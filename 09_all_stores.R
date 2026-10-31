# 09_all_stores.R — run the step-1 stockout detector on all 10 stores; build reconstructed demand
# Reconstructed demand = observed sales, except on flagged stockout days (p < 1e-4, gaps <= 60 days),
# where demand is drawn from Poisson(lambda) at the item's pre-gap normal rate.
suppressMessages(library(data.table))
set.seed(6780)
sales <- fread("data/sales_train_evaluation.csv")
meta  <- sales[, .(id, item_id, dept_id, cat_id, store_id, state_id)]
M <- as.matrix(sales[, grep("^d_", names(sales)), with = FALSE]); storage.mode(M) <- "integer"; rm(sales); invisible(gc())
nd <- ncol(M)
meta[, first_sale := apply(M > 0, 1, function(r) which(r)[1])]
lines <- readLines("R/02_stockout_detect.R"); s <- grep("^detect <- function", lines); f <- grep("^}", lines); f <- f[f > s][1]
WIN <- 56; MINHIST <- 28; eval(parse(text = lines[s:f]))
FL <- matrix(FALSE, nrow(M), nd); R <- M * 1.0
runs_all <- list(); closed_by_store <- list()
for (st in unique(meta$store_id)) {
  rows <- which(meta$store_id == st)
  closed <- which(colSums(M[rows, ]) == 0); closed_by_store[[st]] <- closed
  for (k in rows) {
    r <- detect(M[k, ], meta$first_sale[k]); if (is.null(r)) next
    r <- r[p < 1e-4 & L <= 60 & end < nd]; if (!nrow(r)) next
    for (q in seq_len(nrow(r))) { dd <- setdiff(r$start[q]:r$end[q], closed)
      FL[k, dd] <- TRUE; R[k, dd] <- rpois(length(dd), r$lambda[q]) }
    r[, idx := k]; runs_all[[length(runs_all) + 1]] <- r
  }
  cat(st, "done\n")
}
runs <- rbindlist(runs_all); runs <- cbind(runs, meta[runs$idx, .(item_id, cat_id, store_id, state_id)])
# lost revenue per store (same method as step 1, scaled by CA_1 precision estimate 0.66)
cal <- fread("data/calendar.csv"); runs[, wk := cal$wm_yr_wk[start]]
pr <- fread("data/sell_prices.csv")
runs <- merge(runs, pr[, .(store_id, item_id, wk = wm_yr_wk, sell_price)], by = c("store_id", "item_id", "wk"), all.x = TRUE)
runs[, lost_rev := lambda * L * sell_price]
yrs <- nd / 365.25
# store revenue
P <- dcast(pr, store_id + item_id ~ wm_yr_wk, value.var = "sell_price")
key <- paste(meta$store_id, meta$item_id); Pm <- as.matrix(P[, -(1:2)]); rownames(Pm) <- paste(P$store_id, P$item_id)
wk <- as.character(cal$wm_yr_wk[1:nd]); pm <- Pm[key, wk]; pm[is.na(pm)] <- 0
meta[, revenue := rowSums(M * pm)]
st <- merge(runs[, .(stockout_runs = .N, stockout_days = sum(L), lost_rev = sum(lost_rev, na.rm = TRUE) * 0.66), by = store_id],
            meta[, .(revenue = sum(revenue), onsale_days = sum(nd - first_sale + 1)), by = store_id], by = "store_id")
st[, `:=`(share_days_flagged = round(stockout_days / onsale_days, 4), lost_per_year = round(lost_rev / yrs), revenue_per_year = round(revenue / yrs),
          lost_pct = round(100 * lost_rev / revenue, 2))]
print(st[, .(store_id, share_days_flagged, revenue_per_year, lost_per_year, lost_pct)])
cat("All stores: lost/yr $", round(sum(st$lost_rev) / yrs), " of revenue/yr $", round(sum(st$revenue) / yrs), " =", round(100 * sum(st$lost_rev) / sum(st$revenue), 2), "%\n")
fwrite(st, "out/all_stores_stockouts.csv")
saveRDS(list(meta = meta, M = M, R = R, FL = FL, pm = pm, closed = closed_by_store), "out/all_stores.rds")

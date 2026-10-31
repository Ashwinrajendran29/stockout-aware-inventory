# 03_robustness.R — how sensitive is the stockout signal? separate delistings from stockouts
suppressMessages(library(data.table))
d <- readRDS("out/ca1.rds"); meta <- d$meta; M <- d$M
r <- readRDS("out/ca1_runs.rds"); runs <- r$runs
nd <- ncol(M)
runs[, type := fifelse(end == nd, "ongoing at data end (possible delisting)",
               fifelse(L > 60, "very long gap (>60 days)", "stockout-length gap (<=60 days)"))]
onsale <- 4768932
tab <- runs[flag==TRUE, .(runs=.N, days=sum(L), share_of_onsale_days=round(sum(L)/onsale,4), lost_rev=round(sum(lost_rev))), by=type]
print(tab)
# stricter thresholds, real vs placebo B (lumpy), counting only gaps <= 60 days
thr <- rbindlist(lapply(c(0.01,0.001,0.0001), function(a) {
  real <- runs[p < a & L <= 60 & end < nd, sum(L)]
  pl   <- r$sruns_nb[p < a & L <= 60, sum(L)]
  data.table(alpha=a, real_share=round(real/onsale,4), placebo_lumpy_share=round(pl/onsale,4),
             excess_share=round((real-pl)/onsale,4), precision_est=round(1-pl/real,2))
}))
print(thr); fwrite(thr,"out/threshold_table.csv"); fwrite(tab,"out/run_types.csv")
# store revenue for scale
cal <- fread("data/calendar.csv")
pr <- fread("data/sell_prices.csv")[store_id=="CA_1"]
wk <- cal$wm_yr_wk[1:nd]
P <- dcast(pr, item_id ~ wm_yr_wk, value.var="sell_price")
Pm <- as.matrix(P[,-1]); rownames(Pm) <- P$item_id
pm <- Pm[meta$item_id, as.character(wk)]; pm[is.na(pm)] <- 0
rev <- sum(M * pm)
yrs <- nd/365.25
cat("CA_1 total revenue (all items, 5.3 yrs): $", round(rev), " per year: $", round(rev/yrs), "\n")
best <- runs[p < 1e-4 & L <= 60 & end < nd]
precision <- thr[alpha==1e-4]$precision_est
cons <- sum(best$lost_rev) * precision
cat("Conservative lost revenue (alpha=0.0001, gaps<=60d, scaled by est. precision", precision, "): $", round(cons),
    " = $", round(cons/yrs), "/yr = ", round(100*cons/rev,1), "% of revenue\n")
bycat <- best[, .(runs=.N, days=sum(L), lost_rev_conservative=round(sum(lost_rev)*precision)), by=cat_id][order(-lost_rev_conservative)]
print(bycat); fwrite(bycat,"out/lost_by_cat.csv")
saveRDS(list(rev=rev, yrs=yrs, cons=cons, precision=precision), "out/headline.rds")

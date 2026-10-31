# 02_stockout_detect.R — probability-based stockout detector (pilot: store CA_1)
# Idea: if an item normally sells lambda units/day, a run of L zero-sale days has
# probability exp(-lambda*L) under a Poisson model. Very unlikely runs (p < ALPHA)
# are flagged as probable stockouts (censored demand), not true zero demand.
suppressMessages(library(data.table))
set.seed(6780)
ALPHA <- 0.01; WIN <- 56; MINHIST <- 28
d <- readRDS("out/ca1.rds"); meta <- d$meta; M <- d$M
cal <- fread("data/calendar.csv"); cal[, day := .I]
closed <- which(colSums(M) == 0)                  # store closed (e.g. Christmas)
cat("Store-closed days excluded:", length(closed), "\n")

detect <- function(x, first) {
  x <- unname(as.integer(x)); n <- length(x); out <- list()
  csum <- c(0, cumsum(x)); i <- first
  while (i <= n) {
    if (x[i] == 0 && !(i %in% closed)) {
      j <- i; while (j < n && (x[j+1] == 0 || (j+1) %in% closed)) j <- j + 1
      L <- sum(!(i:j %in% closed))
      h0 <- max(first, i - WIN)
      if (i - h0 >= MINHIST) {
        lam <- (csum[i] - csum[h0]) / (i - h0)
        p <- exp(-lam * L)
        out[[length(out)+1]] <- c(start=i, end=j, L=L, lambda=lam, p=p)
      }
      i <- j + 1
    } else i <- i + 1
  }
  if (length(out)) as.data.table(do.call(rbind, out)) else NULL
}
runs <- rbindlist(lapply(seq_len(nrow(M)), function(k) {
  r <- detect(M[k,], meta$first_sale[k]); if (!is.null(r)) r[, idx := k]; r }))
runs[, flag := p < ALPHA]
runs <- cbind(runs, meta[runs$idx, .(id, item_id, dept_id, cat_id)])

# ---- Placebo tests: simulate demand with NO stockouts and rerun the detector ----
# (a) Poisson at each item's constant long-run on-sale rate
# (b) Negative binomial with the same mean and the item's observed variance
#     (stricter: real retail demand is lumpier than Poisson)
placebo <- function(kind) {
  S <- t(sapply(seq_len(nrow(M)), function(k) {
    f <- meta$first_sale[k]; x <- M[k, f:ncol(M)]; mu <- mean(x); v <- var(x)
    y <- integer(ncol(M))
    n <- ncol(M) - f + 1
    y[f:ncol(M)] <- if (kind=="pois" || v <= mu) rpois(n, mu) else rnbinom(n, mu=mu, size=mu^2/(v-mu))
    y[closed] <- 0L; y }))
  r <- rbindlist(lapply(seq_len(nrow(S)), function(k) { o <- detect(S[k,], meta$first_sale[k]); if(!is.null(o)) o[, idx:=k]; o }))
  r[, flag := p < ALPHA]; r
}
sruns_p <- placebo("pois"); sruns_nb <- placebo("nb")

onsale_days <- sum(ncol(M) - meta$first_sale + 1L - sapply(meta$first_sale, function(f) sum(closed >= f)))
zero_days <- sum(runs$L)
res <- data.table(
  metric=c("On-sale item-days","Zero-sale item-days (after launch)","Flagged stockout runs","Flagged stockout item-days",
           "Share of on-sale days flagged","Share of zero days that look like stockouts",
           "Placebo A, Poisson no-stockout world: share of on-sale days flagged",
           "Placebo B, lumpy (neg. binomial) no-stockout world: share flagged"),
  value=c(onsale_days, zero_days, sum(runs$flag), sum(runs[flag==TRUE]$L),
          round(sum(runs[flag==TRUE]$L)/onsale_days,4), round(sum(runs[flag==TRUE]$L)/zero_days,4),
          round(sum(sruns_p[flag==TRUE]$L)/onsale_days,4), round(sum(sruns_nb[flag==TRUE]$L)/onsale_days,4)))
print(res); fwrite(res, "out/stockout_summary.csv")

# ---- Lost sales estimate: expected units during flagged runs x item price ----
pr <- fread("data/sell_prices.csv")[store_id=="CA_1"]
runs[, wk := cal$wm_yr_wk[start]]
runs <- merge(runs, pr[, .(item_id, wk=wm_yr_wk, sell_price)], by=c("item_id","wk"), all.x=TRUE)
runs[, lost_units := lambda * L]
runs[, lost_rev := lost_units * sell_price]
fl <- runs[flag==TRUE]
years <- (ncol(M) - median(meta$first_sale)) / 365
bycat <- fl[, .(runs=.N, stockout_days=sum(L), lost_units=round(sum(lost_units)), lost_revenue=round(sum(lost_rev, na.rm=TRUE))), by=cat_id][order(-lost_revenue)]
print(bycat); fwrite(bycat, "out/stockout_by_cat.csv")
cat("Run length of flagged runs (days): median", median(fl$L), " p90", quantile(fl$L,.9), "\n")
cat("Price missing for", round(mean(is.na(fl$sell_price)),3), "of flagged runs\n")
saveRDS(list(runs=runs, sruns_p=sruns_p, sruns_nb=sruns_nb, closed=closed), "out/ca1_runs.rds")

# 06_models.R — Step 2: CART and random forest forecasts for CA_1, with and without stockout correction
# Holdout = last 28 days (d_1914–d_1941, Apr 25–May 22 2016). Training = the 365 days before.
# All sales features are lagged >= 28 days, so every test-day forecast uses only data known at the forecast date.
suppressMessages({library(data.table); library(rpart); library(ranger)})
set.seed(6780)
d <- readRDS("out/ca1.rds"); meta <- d$meta; M <- d$M
H <- 28; nd <- ncol(M); T0 <- nd - H; TRAIN_DAYS <- 365; LAG <- 28
cal <- fread("data/calendar.csv")[1:nd]; cal[, day := .I]
closed <- which(colSums(M) == 0)

# --- detector from step 1 (same rule: p < 1e-4, gaps <= 60 days) ---
lines <- readLines("R/02_stockout_detect.R"); s <- grep("^detect <- function", lines); f <- grep("^}", lines); f <- f[f > s][1]
WIN <- 56; MINHIST <- 28; eval(parse(text = lines[s:f]))
flag_matrix <- function(X, upto) {
  F <- matrix(FALSE, nrow(X), ncol(X))
  for (k in seq_len(nrow(X))) { r <- detect(X[k, 1:upto], meta$first_sale[k])
    if (!is.null(r)) { r <- r[p < 1e-4 & L <= 60]; for (q in seq_len(nrow(r))) F[k, r$start[q]:r$end[q]] <- TRUE } }
  F }
Ftrain <- flag_matrix(M, T0)                 # stockouts known at forecast time
Ffull  <- flag_matrix(M, nd)                 # used ONLY to exclude stockout test days from scoring
cat("Training-period stockout item-days flagged:", sum(Ftrain), "\n")

# --- rolling means ending LAG days before each day; corrected version ignores flagged/closed days ---
roll_lag <- function(X, w, mask = NULL) {
  Y <- X * 1.0; if (!is.null(mask)) Y[mask] <- NA
  Y[, closed] <- NA
  out <- t(apply(Y, 1, function(r) frollmean(r, w, na.rm = TRUE, align = "right")))
  cbind(matrix(NA_real_, nrow(X), LAG), out[, 1:(ncol(X) - LAG)])   # shift by LAG
}
days <- (T0 - TRAIN_DAYS + 1):nd
build <- function(mask) {
  r7 <- roll_lag(M, 7, mask); r28 <- roll_lag(M, 28, mask); r91 <- roll_lag(M, 91, mask)
  l28 <- cbind(matrix(NA_real_, nrow(M), LAG), (M * 1.0)[, 1:(nd - LAG)])
  if (!is.null(mask)) { mm <- cbind(matrix(FALSE, nrow(M), LAG), mask[, 1:(nd - LAG)]); l28[mm] <- r28[mm] }
  data.table(idx = rep(seq_len(nrow(M)), times = length(days)), day = rep(days, each = nrow(M)),
             lag28 = as.vector(l28[, days]), rm7 = as.vector(r7[, days]), rm28 = as.vector(r28[, days]), rm91 = as.vector(r91[, days]))
}
pr <- fread("data/sell_prices.csv")[store_id == "CA_1"]
pr[, max_price := max(sell_price), by = item_id]
pr[, prev_price := shift(sell_price), by = item_id]
add_static <- function(D) {
  D[, `:=`(item_id = meta$item_id[idx], dept_id = factor(meta$dept_id[idx]), cat_id = factor(meta$cat_id[idx]))]
  D[, `:=`(wday = cal$wday[day], month = cal$month[day], snap = cal$snap_CA[day],
           event = factor(fifelse(cal$event_type_1[day] == "", "none", cal$event_type_1[day])), wk = cal$wm_yr_wk[day])]
  D <- merge(D, pr[, .(item_id, wk = wm_yr_wk, sell_price, rel_price = sell_price / max_price,
                       price_chg = sell_price / prev_price - 1)], by = c("item_id", "wk"), all.x = TRUE)
  D[is.na(price_chg), price_chg := 0]
  # item weekday profile from the 182 days BEFORE the training window (no overlap with any training or test target)
  pw <- (T0 - TRAIN_DAYS - 181):(T0 - TRAIN_DAYS)
  prof <- rbindlist(lapply(1:7, function(w) { dd <- pw[cal$wday[pw] == w]
    data.table(idx = seq_len(nrow(M)), wday = w, wsum = rowSums(M[, dd, drop = FALSE])) }))
  prof[, wday_idx := wsum / (sum(wsum) / 7), by = idx][!is.finite(wday_idx), wday_idx := 1]
  D <- merge(D, prof[, .(idx, wday, wday_idx)], by = c("idx", "wday"), all.x = TRUE)
  D[, units := M[cbind(idx, day)]]
  D[, `:=`(launched = day >= meta$first_sale[idx], closed_day = day %in% closed,
           so_train = Ftrain[cbind(idx, pmin(day, T0))] & day <= T0, so_test = Ffull[cbind(idx, day)] & day > T0)]
  D <- D[launched == TRUE & closed_day == FALSE & !is.na(sell_price) & !is.na(rm91)]
  # corrected windows can be all-stockout: fall back to the next longer window
  D[is.na(rm28), rm28 := rm91]; D[is.na(rm7), rm7 := rm28]; D[is.na(lag28), lag28 := rm28]
  D
}
Draw <- add_static(build(NULL)); Dcor <- add_static(build(Ftrain))
cat("Rows (raw):", nrow(Draw), " train:", Draw[day <= T0, .N], " test:", Draw[day > T0, .N], "\n")
saveRDS(list(Draw = Draw, Dcor = Dcor, T0 = T0), "out/step2_features.rds")

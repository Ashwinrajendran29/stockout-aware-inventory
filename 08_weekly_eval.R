# 08_weekly_eval.R — score forecasts on weekly item totals (the level inventory is ordered at)
suppressMessages(library(data.table))
P <- readRDS("out/step2_predictions.rds"); T0 <- min(P$day) - 1
d <- readRDS("out/ca1.rds"); M <- d$M
H <- 28; last <- (T0-27):T0
ma28 <- data.table(idx = seq_len(nrow(M)), ma28 = rowMeans(M[, last]))
P <- merge(P, ma28, by = "idx")
P[, week := (day - T0 - 1) %/% 7 + 1]
cols <- c(ma28 = "28-day average", cart_raw = "CART", rf_raw = "Random forest", rf_cor = "Random forest, stockout-corrected")
# keep item-weeks with no stockout test days, so weekly actuals are real demand
W <- P[, c(list(units = sum(units), so = any(so_test), affected = affected[1]), lapply(.SD, sum)), by = .(idx, week), .SDcols = names(cols)][so == FALSE]
sc <- function(Q) rbindlist(lapply(names(cols), function(c) { e <- Q[[c]] - Q$units
  data.table(model = cols[[c]], MAE = round(mean(abs(e)), 2), RMSE = round(sqrt(mean(e^2)), 2), bias = round(mean(e), 2),
             under_share = round(mean(e < 0), 3)) }))
cat("Item-weeks scored:", nrow(W), "\n"); a <- sc(W); print(a)
cat("Affected items:\n"); b <- sc(W[affected == TRUE]); print(b)
fwrite(a, "out/step2_weekly_all.csv"); fwrite(b, "out/step2_weekly_affected.csv")

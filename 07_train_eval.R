# 07_train_eval.R — Step 2: train CART + random forest (raw vs stockout-corrected) and score on the 28-day holdout
suppressMessages({library(data.table); library(rpart); library(ranger)})
set.seed(6780)
S <- readRDS("out/step2_features.rds"); Draw <- S$Draw; Dcor <- S$Dcor; T0 <- S$T0
fml <- units ~ wday_idx + lag28 + rm7 + rm28 + rm91 + dept_id + cat_id + wday + month + snap + event + sell_price + rel_price + price_chg
fit_pair <- function(Dtr) list(
  cart = rpart(fml, data = Dtr, method = "anova", control = rpart.control(cp = 5e-4, minbucket = 200, xval = 0)),
  rf   = ranger(fml, data = Dtr, num.trees = 100, min.node.size = 100, mtry = 4, importance = "impurity", num.threads = 2))
t1 <- Sys.time()
raw <- fit_pair(Draw[day <= T0])
cor <- fit_pair(Dcor[day <= T0 & so_train == FALSE])     # corrected: stockout days are not treated as demand = 0
cat("Training time:", round(as.numeric(difftime(Sys.time(), t1, units = "mins")), 1), "min\n")
te_raw <- Draw[day > T0]; te_cor <- Dcor[day > T0]
stopifnot(identical(te_raw$idx, te_cor$idx), identical(te_raw$day, te_cor$day))
P <- data.table(idx = te_raw$idx, day = te_raw$day, units = te_raw$units, so_test = te_raw$so_test,
  cart_raw = predict(raw$cart, te_raw), rf_raw = predict(raw$rf, te_raw)$predictions,
  cart_cor = predict(cor$cart, te_cor), rf_cor = predict(cor$rf, te_cor)$predictions)
# affected items = had a flagged stockout in the last 28 training days (same definition as step 1)
aff_idx <- unique(Draw[day > T0 - 28 & day <= T0 & so_train == TRUE, idx]); P[, affected := idx %in% aff_idx]
saveRDS(P, "out/step2_predictions.rds")
score <- function(Q, col, nm) { Q <- Q[so_test == FALSE]; e <- Q[[col]] - Q$units
  data.table(model = nm, MAE = round(mean(abs(e)), 3), RMSE = round(sqrt(mean(e^2)), 3), bias = round(mean(e), 3), days = nrow(Q)) }
cols <- c(cart_raw = "CART", cart_cor = "CART, stockout-corrected", rf_raw = "Random forest", rf_cor = "Random forest, stockout-corrected")
res_all <- rbindlist(lapply(names(cols), function(c) score(P, c, cols[[c]])))
res_aff <- rbindlist(lapply(names(cols), function(c) score(P[affected == TRUE], c, cols[[c]])))
base <- fread("out/fair_all.csv"); base_aff <- fread("out/fair_affected.csv")
cat("\nAll items (non-stockout test days):\n"); print(rbind(base[, .(model = method, MAE, RMSE, bias, days = days_scored)], res_all))
cat("\nAffected items:", length(aff_idx), "\n"); print(rbind(base_aff[, .(model = method, MAE, RMSE, bias, days = days_scored)], res_aff))
fwrite(res_all, "out/step2_all.csv"); fwrite(res_aff, "out/step2_affected.csv")
imp <- data.table(feature = names(cor$rf$variable.importance), importance = cor$rf$variable.importance)[order(-importance)]
imp[, share := round(importance / sum(importance), 3)]; print(imp[, .(feature, share)]); fwrite(imp, "out/step2_importance.csv")
cat("CART leaves (corrected):", sum(cor$cart$frame$var == "<leaf>"), "\n")
saveRDS(list(cart_cor = cor$cart), "out/step2_cart.rds")

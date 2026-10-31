# 11_price_events.R — Step 3: how price, events and SNAP days move demand (all 10 stores)
# Stockout days are excluded so that empty shelves are not mistaken for low demand.
suppressMessages(library(data.table))
A <- readRDS("out/all_stores.rds"); meta <- A$meta; M <- A$M; FL <- A$FL; pm <- A$pm; closed <- A$closed
nd <- ncol(M); cal <- fread("data/calendar.csv")[1:nd]; cal[, day := .I]; cal[, date := as.IDate(date)]

# two-way fixed-effects OLS by alternating demeaning, with cluster-robust SE (clusters = item-store)
demean2 <- function(DT, vars, f1, f2, tol = 1e-8, maxit = 200) {
  X <- DT[, c(vars, f1, f2), with = FALSE]
  for (v in vars) { x <- X[[v]]
    for (it in 1:maxit) { old <- x
      x <- x - ave(x, X[[f1]], FUN = mean); x <- x - ave(x, X[[f2]], FUN = mean)
      if (max(abs(x - old)) < tol) break }
    set(X, j = v, value = x) }
  X }
fe_ols <- function(DT, y, xs, f1, f2, cl) {
  X <- demean2(DT, c(y, xs), f1, f2); Xm <- as.matrix(X[, ..xs]); yy <- X[[y]]
  XtX <- crossprod(Xm); b <- solve(XtX, crossprod(Xm, yy)); e <- yy - Xm %*% b
  S <- rowsum(Xm * as.vector(e), DT[[cl]]); V <- solve(XtX) %*% crossprod(S) %*% solve(XtX)
  data.table(term = xs, estimate = as.vector(b), se = sqrt(diag(V)), n = nrow(DT)) }

# ---- 1. Price elasticity: weekly item-store panel ----
wk_of_day <- cal$wm_yr_wk
valid <- !FL & (M >= 0)
for (st in names(closed)) valid[meta$store_id == st, closed[[st]]] <- FALSE
valid[col(M) < meta$first_sale] <- FALSE
wks <- unique(wk_of_day); wk_idx <- match(wk_of_day, wks)
U <- M * valid
WU <- t(rowsum(t(U), wk_idx)); WN <- t(rowsum(t(valid * 1), wk_idx))         # weekly units, valid days
WP <- t(rowsum(t(pm), wk_idx)) / t(rowsum(t((pm > 0) * 1), wk_idx))          # weekly price
panel <- data.table(idx = rep(seq_len(nrow(M)), times = length(wks)), wk = rep(seq_along(wks), each = nrow(M)),
                    units = as.vector(WU), ndays = as.vector(WN), price = as.vector(WP))
panel <- panel[ndays >= 4 & is.finite(price) & price > 0]
panel[, units7 := units * 7 / ndays]
panel[, `:=`(store = meta$store_id[idx], cat = meta$cat_id[idx])]
panel[, store_wk := paste(store, wk)]
panel[, pv := uniqueN(price), by = idx]; panel <- panel[pv > 1]                # items whose price ever changes
panel[, `:=`(ly = log1p(units7), lp = log(price))]
el <- rbindlist(lapply(c("FOODS", "HOUSEHOLD", "HOBBIES"), function(cc)
  fe_ols(panel[cat == cc], "ly", "lp", "idx", "store_wk", "idx")[, cat := cc]))
el[, `:=`(items = sapply(cat, function(cc) panel[cat == cc, uniqueN(idx)]))]
cat("Price elasticity (log weekly units on log price; item-store and store-week fixed effects):\n"); print(el)
fwrite(el, "out/step3_elasticity.csv")

# ---- 2. Event uplift: store x category daily demand vs the same weekday one week before/after ----
R <- A$R; ok <- matrix(TRUE, nrow(R), nd)
sc <- paste(meta$store_id, meta$cat_id)
daily <- rowsum(R, sc)                                                        # 30 store-category rows x days
ev_days <- cal[event_name_1 != "", .(day, event = event_name_1, type = event_type_1)]
ev_days <- ev_days[day > 7 & day <= nd - 7]
upl <- rbindlist(lapply(seq_len(nrow(ev_days)), function(i) { d <- ev_days$day[i]
  if (cal$event_name_1[d - 7] != "" || cal$event_name_1[d + 7] != "") return(NULL)
  base <- (daily[, d - 7] + daily[, d + 7]) / 2
  data.table(event = ev_days$event[i], type = ev_days$type[i], sc = rownames(daily), uplift = daily[, d] / base - 1)[is.finite(uplift) & daily[, d] > 0] }))
upl[, cat := sub(".* ", "", sc)]
ev <- upl[, .(uplift = round(mean(uplift), 3), n = .N), by = .(event, type, cat)]
ev_type <- upl[, .(uplift = round(mean(uplift), 3)), by = .(type, cat)][order(type, cat)]
cat("\nEvent uplift by type and category:\n"); print(dcast(ev_type, type ~ cat, value.var = "uplift"))
top <- upl[cat == "FOODS", .(uplift = round(mean(uplift), 3), n = .N), by = event][order(-abs(uplift))][1:8]
cat("\nLargest FOODS event effects:\n"); print(top)
fwrite(ev, "out/step3_events.csv"); fwrite(ev_type, "out/step3_event_types.csv")

# ---- 3. SNAP days: a natural experiment (each state has its own SNAP calendar) ----
# log daily store-category sales on the store's SNAP flag, with store and DATE fixed effects.
# Date FE absorb everything common to all stores that day (weekday, payday, weather nationwide, holidays),
# so the effect is identified only from states having SNAP on different days of the month.
S <- data.table(sc = rep(rownames(daily), times = nd), day = rep(1:nd, each = nrow(daily)), sales = as.vector(daily))
S[, `:=`(store = sub(" .*", "", sc), cat = sub(".* ", "", sc))]
S[, state := substr(store, 1, 2)]
S[, snap := fifelse(state == "CA", cal$snap_CA[day], fifelse(state == "TX", cal$snap_TX[day], cal$snap_WI[day]))]
S <- S[sales > 0]
sn <- rbindlist(lapply(c("FOODS", "HOUSEHOLD", "HOBBIES"), function(cc) {
  D <- S[cat == cc][, ly := log(sales)][, dayf := day]
  r <- fe_ols(D, "ly", "snap", "store", "dayf", "dayf"); r[, cat := cc]; r }))
sn[, effect_pct := round(100 * (exp(estimate) - 1), 2)]
cat("\nSNAP effect (% change in daily sales):\n"); print(sn)
fwrite(sn, "out/step3_snap.csv")

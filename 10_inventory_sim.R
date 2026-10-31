# 10_inventory_sim.R — Step 4: simulate one year of weekly reordering for all 30,490 items under three policies
#
# Demand each day = reconstructed demand from 09 (observed sales; Poisson-imputed on flagged stockout days).
# The store reviews each item weekly (R = 7 days); orders arrive after a lead time of L = 2 days.
# Order-up-to level S = mu * (R + L) + safety stock; order = S - (on hand + on order), if positive.
# The policy only sees its OWN recorded sales (sales = min(stock, demand)), like a real store.
#   P1 Fixed safety stock : mu from last 28 days of recorded sales; safety stock = k days of mu
#   P2 Forecast-based     : mu, sigma from last 28 days of recorded sales; safety = z * sigma * sqrt(R+L)
#   P3 Stockout-aware     : as P2, but days the shelf ran empty are left out of mu and sigma
# Supply outages: on days the detector flagged a real stockout (step 1/09), the supplier cannot deliver and the shelf
#   is empty for EVERY policy (same outages for all). The question is which ordering rule recovers better afterwards.
# Costs (assumptions): holding = 25%/yr of price; a lost sale costs 30% of price (lost gross margin).
suppressMessages(library(data.table))
A <- readRDS("out/all_stores.rds"); meta <- A$meta; R <- A$R; FL <- A$FL; pm <- A$pm; M <- A$M
nd <- ncol(R); SIM <- (nd - 363):nd; HIST <- 28; RV <- 7; LT <- 2
keep <- which(meta$first_sale <= min(SIM) - 91)         # items selling for 3+ months before the sim starts
cat("Items simulated:", length(keep), "of", nrow(meta), "\n")
D  <- R[keep, SIM]; n <- nrow(D)
# price per item-day: fill missing weeks with the item's nearest known price
P <- pm[keep, SIM]; P[P == 0] <- NA
P <- t(apply(P, 1, function(x) { if (all(is.na(x))) return(rep(1, length(x))); i <- which(!is.na(x)); x[seq_along(x)] <- x[i[findInterval(seq_along(x), i, all.inside = TRUE)]]; x[is.na(x)] <- x[i[1]]; x }))
# history before the sim: observed (censored) sales, and stockout flags from the detector
H0  <- M[keep, (min(SIM) - HIST):(min(SIM) - 1)] * 1.0
FL0 <- FL[keep, (min(SIM) - HIST):(min(SIM) - 1)]
OUT <- FL[keep, SIM]                                   # replayed supply outages
cat("Outage item-days in sim year:", sum(OUT), "\n")

simulate <- function(policy, z = 1.65, kdays = 3, hold_rate = 0.25, margin = 0.30, outages = TRUE) {
  hist_s <- H0; hist_c <- FL0                          # rolling 28-day buffer: sales, censored?
  onhand <- rep(0, n); pipe <- matrix(0, n, LT)
  tot <- c(demand = 0, sales = 0, lost_units = 0, hold_cost = 0, lost_cost = 0, inv_value = 0, stockout_itemdays = 0, lost_outage = 0)
  mu_prev <- rowMeans(H0); held <- rep(0, n)
  it <- list(demand = numeric(n), sales = numeric(n), hold = numeric(n), lostc = numeric(n), inv = numeric(n))
  est <- function() {
    if (policy == "P3") {
      x <- hist_s; x[hist_c] <- NA; cnt <- rowSums(!is.na(x))
      mu <- rowMeans(x, na.rm = TRUE); sg <- sqrt(rowSums((x - mu)^2, na.rm = TRUE) / pmax(cnt - 1, 1))
      bad <- cnt < 7; mu[bad] <- pmax(mu_prev[bad], rowMeans(hist_s[bad, , drop = FALSE])); sg[bad] <- sqrt(mu[bad])
    } else { mu <- rowMeans(hist_s); sg <- sqrt(rowSums((hist_s - mu)^2) / (HIST - 1)) }
    list(mu = mu, sg = sg) }
  for (t in seq_along(SIM)) {
    if ((t - 1) %% RV == 0) {                          # weekly review
      e <- est(); mu_prev <- e$mu
      S <- e$mu * (RV + LT) + if (policy == "P1") kdays * e$mu else z * e$sg * sqrt(RV + LT)
      if (t == 1) onhand <- ceiling(S)                 # start each policy at its own target level
      q <- pmax(0, ceiling(S - onhand - rowSums(pipe) - held)); pipe[, LT] <- pipe[, LT] + q
    }
    arr <- pipe[, 1]; pipe <- cbind(pipe[, -1, drop = FALSE], 0)
    if (outages) { o <- OUT[, t]                                   # outage: shelf empty, deliveries held at supplier
      held[o] <- held[o] + arr[o]; arr[o] <- 0; onhand[o] <- 0
      arr[!o] <- arr[!o] + held[!o]; held[!o] <- 0 }                 # held stock arrives when the outage ends
    onhand <- onhand + arr
    d <- D[, t]; s <- pmin(onhand, d); lost <- d - s; onhand <- onhand - s
    cens <- onhand == 0 & lost > 0
    hist_s <- cbind(hist_s[, -1], s); hist_c <- cbind(hist_c[, -1], cens)
    p <- P[, t]
    lo <- if (outages) sum(lost[o]) else 0
    tot <- tot + c(sum(d), sum(s), sum(lost), sum(onhand * p * hold_rate / 365), sum(lost * p * margin), sum(onhand * p), sum(cens), lo)
    it$demand <- it$demand + d; it$sales <- it$sales + s; it$hold <- it$hold + onhand * p * hold_rate / 365
    it$lostc <- it$lostc + lost * p * margin; it$inv <- it$inv + onhand * p
  }
  tot["inv_value"] <- tot["inv_value"] / length(SIM)
  it$inv <- it$inv / length(SIM); attr(tot, "items") <- it
  tot
}
row <- function(policy, label, setting, ...) { r <- simulate(policy, ...)
  data.table(policy = label, setting = setting, fill_rate = r[["sales"]] / r[["demand"]], lost_units = r[["lost_units"]],
             hold_cost = r[["hold_cost"]], lost_cost = r[["lost_cost"]], total_cost = r[["hold_cost"]] + r[["lost_cost"]],
             avg_inventory_value = r[["inv_value"]], stockout_itemdays = r[["stockout_itemdays"]],
             lost_outside_outages = r[["lost_units"]] - r[["lost_outage"]]) }
t1 <- Sys.time()
res <- rbind(
  rbindlist(lapply(c(1, 2, 3, 5, 7), function(k) row("P1", "Fixed safety stock", paste(k, "days"), kdays = k))),
  rbindlist(lapply(c(0.84, 1.28, 1.65, 2.05, 2.33), function(z) row("P2", "Forecast-based", paste0("z=", z), z = z))),
  rbindlist(lapply(c(0.84, 1.28, 1.65, 2.05, 2.33), function(z) row("P3", "Stockout-aware", paste0("z=", z), z = z))))
# perfect supply (no outages): does stockout-awareness ever hurt?
perfect <- rbind(row("P2", "Forecast-based", "z=1.65, no outages", z = 1.65, outages = FALSE),
                 row("P3", "Stockout-aware", "z=1.65, no outages", z = 1.65, outages = FALSE))
# cost-assumption sensitivity at z = 1.65
sens <- rbindlist(lapply(list(c(0.20, 0.20), c(0.25, 0.30), c(0.30, 0.40), c(0.35, 0.20)), function(a)
  rbind(row("P2", "Forecast-based", sprintf("hold %.0f%%, margin %.0f%%", 100*a[1], 100*a[2]), z = 1.65, hold_rate = a[1], margin = a[2]),
        row("P3", "Stockout-aware", sprintf("hold %.0f%%, margin %.0f%%", 100*a[1], 100*a[2]), z = 1.65, hold_rate = a[1], margin = a[2]))))
fwrite(perfect, "out/sim_perfect_supply.csv"); fwrite(sens, "out/sim_sensitivity.csv")
# per-store and per-category breakdown at the default settings (for the dashboard)
by_store <- rbindlist(lapply(list(c("P1", "Fixed safety stock", "3 days"), c("P2", "Forecast-based", "z=1.65"), c("P3", "Stockout-aware", "z=1.65")), function(a) {
  r <- if (a[1] == "P1") simulate("P1", kdays = 3) else simulate(a[1], z = 1.65); it <- attr(r, "items")
  data.table(store = meta$store_id[keep], cat = meta$cat_id[keep], demand = it$demand, sales = it$sales, hold = it$hold, lostc = it$lostc, inv = it$inv)[
    , .(demand = sum(demand), sales = sum(sales), fill_rate = sum(sales) / sum(demand), total_cost = sum(hold) + sum(lostc), hold_cost = sum(hold), lost_cost = sum(lostc), avg_inventory_value = sum(inv)), by = .(store, cat)][, policy := a[2]] }))
fwrite(by_store, "out/sim_by_store_cat.csv")
cat("\nPerfect supply:\n"); print(perfect[, .(policy, fill_rate = round(fill_rate, 4), total_cost = round(total_cost))])
cat("\nSensitivity:\n"); print(sens[, .(policy, setting, total_cost = round(total_cost))])
cat("Sim time:", round(as.numeric(difftime(Sys.time(), t1, units = "mins")), 1), "min\n")
res[, `:=`(fill_rate = round(fill_rate, 4), total_cost = round(total_cost), hold_cost = round(hold_cost), lost_cost = round(lost_cost), avg_inventory_value = round(avg_inventory_value))]
print(res[, .(policy, setting, fill_rate, total_cost, hold_cost, lost_cost, avg_inventory_value, lost_outside_outages)])
fwrite(res, "out/sim_frontier.csv")
saveRDS(list(keep = keep), "out/sim_meta.rds")

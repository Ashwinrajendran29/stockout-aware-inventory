# 05_charts.R — figures for the exploration report
suppressMessages({library(data.table); library(ggplot2)})
OUT <- "out"
BLUE <- "#2a78d6"; ORANGE <- "#eb6834"; GRAY <- "#a8a7a0"; INK <- "#0b0b0b"; INK2 <- "#52514e"
th <- theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(color = "#e6e5e0", linewidth = .4), axis.text = element_text(color = INK2),
  axis.title = element_text(color = INK2, size = 10), plot.title = element_text(color = INK, face = "bold", size = 13),
  plot.subtitle = element_text(color = INK2, size = 10), legend.position = "top", legend.justification = "left",
  plot.background = element_rect(fill = "#fcfcfb", color = NA), plot.margin = margin(12, 16, 10, 12))
d <- readRDS(file.path(OUT,"ca1.rds")); meta <- d$meta; M <- d$M
runs <- readRDS(file.path(OUT,"ca1_runs.rds"))$runs
cal <- fread("data/calendar.csv"); cal[, date := as.IDate(date)]

# A — example item with a flagged stockout
cand <- runs[cat_id=="FOODS" & p < 1e-6 & L >= 7 & L <= 21 & lambda >= 4 & start > 1500 & end < 1941][order(p)]
ex <- cand[1]; k <- ex$idx; w <- (ex$start-90):(ex$end+45)
ts <- data.table(date = cal$date[w], units = M[k, w])
fl <- runs[idx==k & p < 1e-4 & L <= 60 & start <= max(w) & end >= min(w)]
pA <- ggplot(ts, aes(date, units)) +
  geom_rect(data = fl, inherit.aes = FALSE, aes(xmin = cal$date[start]-0.5, xmax = cal$date[end]+0.5, ymin = -Inf, ymax = Inf), fill = ORANGE, alpha = .18) +
  geom_line(color = BLUE, linewidth = .7) +
  annotate("label", x = min(ts$date)+2, y = max(ts$units)*1.12, hjust = 0, vjust = 1, label.size = 0, fill = "#fcfcfb", size = 3.4, color = INK2,
           label = sprintf("Big shaded gap (%s to %s): %d days of zero sales\nnormal rate %.0f units/day\nchance if demand were normal: < 1 in a trillion", format(cal$date[ex$start], "%b %d"), format(cal$date[ex$end], "%b %d"), ex$L, ex$lambda)) +
  scale_y_continuous(expand = expansion(c(0.02, 0.15))) + labs(title = sprintf("%s at store CA_1: a probable stockout", ex$item_id), subtitle = sprintf("%s to %s. Shaded = flagged as stockout (p < 0.0001)", format(min(ts$date), "%b %Y"), format(max(ts$date), "%b %Y")), x = NULL, y = "Units sold per day") + th
ggsave(file.path(OUT,"fig_example.png"), pA, width = 8, height = 3.6, dpi = 160)

# B — real vs no-stockout simulation, by strictness
thr <- fread(file.path(OUT,"threshold_table.csv"))
bl <- melt(thr[, .(alpha = factor(c("p < 0.01","p < 0.001","p < 0.0001"), levels=c("p < 0.01","p < 0.001","p < 0.0001")),
                   `Real CA_1 data` = real_share, `Simulated: no stockouts` = placebo_lumpy_share)], id.vars = "alpha")
pB <- ggplot(bl, aes(alpha, value, fill = variable)) +
  geom_col(position = position_dodge(width = .78), width = .72, color = "#fcfcfb", linewidth = .6) +
  geom_text(aes(label = sprintf("%.1f%%", 100*value)), position = position_dodge(width = .78), vjust = -.5, size = 3.4, color = INK2) +
  scale_fill_manual(values = c(BLUE, GRAY), name = NULL) + scale_y_continuous(labels = function(x) paste0(100*x,"%"), expand = expansion(c(0,.12))) +
  labs(title = "The signal survives a stricter test", subtitle = "Share of on-sale item-days flagged (gaps up to 60 days)", x = "Detector strictness", y = NULL) + th
ggsave(file.path(OUT,"fig_validation.png"), pB, width = 8, height = 3.8, dpi = 160)

# C — conservative lost revenue by category (p < 0.0001, scaled by estimated precision)
lc <- runs[p < 1e-4 & L <= 60 & end < 1941, .(lost = sum(lost_rev) * 0.66 / (1941/365.25)), by = cat_id][order(lost)]
lc[, cat_id := factor(cat_id, levels = cat_id)]
pC <- ggplot(lc, aes(lost, cat_id)) + geom_col(fill = BLUE, width = .55) +
  geom_text(aes(label = paste0("$", formatC(round(lost, -2), big.mark = ",", format = "d"))), hjust = -.1, size = 3.6, color = INK2) +
  scale_x_continuous(labels = function(x) paste0("$", x/1000, "K"), expand = expansion(c(0,.2))) +
  labs(title = "Estimated lost sales per year at CA_1", subtitle = "Conservative estimate, by category", x = NULL, y = NULL) + th +
  theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(color = "#e6e5e0", linewidth = .4))
ggsave(file.path(OUT,"fig_lost.png"), pC, width = 8, height = 2.8, dpi = 160)

# D — forecast bias on items that had a stockout
fa <- fread(file.path(OUT,"fair_affected.csv"))
fa[, method := factor(c("28-day average\n(standard)","28-day average\n(stockout days removed)"), levels = c("28-day average\n(standard)","28-day average\n(stockout days removed)"))]
pD <- ggplot(fa, aes(method, bias)) + geom_hline(yintercept = 0, color = INK2, linewidth = .5) +
  geom_col(fill = c(ORANGE, BLUE), width = .5) +
  geom_text(aes(label = sprintf("%+.2f units/day", bias), vjust = ifelse(bias < 0, 1.5, -.6)), size = 3.6, color = INK2) +
  scale_y_continuous(limits = c(-.8, .3)) +
  labs(title = "Removing stockout days fixes systematic under-forecasting", subtitle = "Average forecast error on 307 items that had a recent stockout (below 0 = forecast too low)", x = NULL, y = "Forecast minus actual") + th
ggsave(file.path(OUT,"fig_bias.png"), pD, width = 8, height = 3.6, dpi = 160)
cat("Example item:", ex$item_id, "L", ex$L, "lambda", round(ex$lambda,2), "p", signif(ex$p,2), "\n")
print(lc)

# E — Step 2: weekly under-forecasting on items with a recent stockout, by model
if (file.exists(file.path(OUT, "step2_weekly_affected.csv"))) {
  wa <- fread(file.path(OUT, "step2_weekly_affected.csv"))
  wa[, model := factor(model, levels = rev(model))]
  wa[, grp := fifelse(grepl("corrected", model), "Stockout-corrected", "Standard")]
  pE <- ggplot(wa, aes(under_share, model, fill = grp)) +
    geom_col(width = .55) + geom_vline(xintercept = .5, color = INK2, linewidth = .4, linetype = "22") +
    geom_text(aes(label = sprintf("%.0f%%  (bias %+.1f/wk)", 100*under_share, bias)), hjust = -.04, size = 3.4, color = INK2) +
    annotate("text", x = .5, y = 4.45, label = "50% = unbiased", size = 3, color = INK2, hjust = -.05) +
    scale_fill_manual(values = c(Standard = GRAY, `Stockout-corrected` = BLUE), name = NULL) +
    scale_x_continuous(labels = function(x) paste0(100*x, "%"), limits = c(0, 1), expand = expansion(c(0, 0))) +
    labs(title = "Fixing the data beats a fancier model",
         subtitle = "Share of weeks forecast too low, 307 items with a recent stockout", x = NULL, y = NULL) + th +
    theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(color = "#e6e5e0", linewidth = .4), plot.title.position = "plot")
  ggsave(file.path(OUT, "fig_models.png"), pE, width = 8, height = 3.2, dpi = 160)
}

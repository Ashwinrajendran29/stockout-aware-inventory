# 12_final_charts.R — figures for steps 3, 4 and the all-store results
suppressMessages({library(data.table); library(ggplot2)})
OUT <- "out"
BLUE <- "#2a78d6"; ORANGE <- "#eb6834"; GRAY <- "#a8a7a0"; INK <- "#0b0b0b"; INK2 <- "#52514e"
th <- theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank(),
  panel.grid.major = element_line(color = "#e6e5e0", linewidth = .4), axis.text = element_text(color = INK2),
  axis.title = element_text(color = INK2, size = 10), plot.title = element_text(color = INK, face = "bold", size = 13),
  plot.subtitle = element_text(color = INK2, size = 10), legend.position = "top", legend.justification = "left",
  plot.title.position = "plot", plot.background = element_rect(fill = "#fcfcfb", color = NA), plot.margin = margin(12, 16, 10, 12))

# F — policy frontier: annual cost vs fill rate (all 10 stores, with replayed outages)
fr <- fread(file.path(OUT, "sim_frontier.csv"))
fr[, policy := factor(policy, levels = c("Fixed safety stock", "Forecast-based", "Stockout-aware"))]
ends <- fr[, .SD[which.max(fill_rate)], by = policy]
pF <- ggplot(fr, aes(fill_rate, total_cost / 1e6, color = policy)) +
  geom_line(linewidth = .9) + geom_point(size = 2.6, stroke = 1.2, fill = "#fcfcfb", shape = 21) +
  geom_text(data = ends, aes(label = policy), hjust = -.08, size = 3.5, show.legend = FALSE) +
  scale_color_manual(values = c(GRAY, ORANGE, BLUE), name = NULL) +
  scale_x_continuous(labels = function(x) paste0(round(100*x), "%"), expand = expansion(c(.03, .32))) +
  scale_y_continuous(labels = function(x) paste0("$", x, "M")) +
  labs(title = "Stockout-aware ordering gives more service for less money",
       subtitle = "One simulated year, 29,906 items in 10 stores. Each point is a safety-stock setting; lower and further right is better.",
       x = "Fill rate (share of demand served)", y = "Annual cost (holding + lost margin)") + th + theme(legend.position = "none")
ggsave(file.path(OUT, "fig_frontier.png"), pF, width = 8.5, height = 4.4, dpi = 160)

# G — SNAP natural experiment with placebo categories
sn <- fread(file.path(OUT, "step3_snap.csv"))
sn[, `:=`(lo = 100*(exp(estimate - 1.96*se) - 1), hi = 100*(exp(estimate + 1.96*se) - 1))]
sn[, cat := factor(cat, levels = c("HOBBIES", "HOUSEHOLD", "FOODS"))]
pG <- ggplot(sn, aes(effect_pct, cat)) + geom_vline(xintercept = 0, color = INK2, linewidth = .4) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = .18, color = INK2, linewidth = .6) +
  geom_point(aes(fill = cat == "FOODS"), shape = 21, size = 4, color = "#fcfcfb", stroke = 1.2) +
  geom_label(aes(label = sprintf("%+.1f%%", effect_pct)), vjust = -.9, size = 3.4, color = INK2, fill = "#fcfcfb", label.size = 0) +
  scale_fill_manual(values = c(`TRUE` = BLUE, `FALSE` = GRAY), guide = "none") +
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(-3, 14)) +
  labs(title = "SNAP days lift food sales by 11%, and nothing else",
       subtitle = "Effect of a SNAP day on daily store sales, 95% interval. SNAP buys food only, so non-food is a built-in check.",
       x = "Change in daily sales on SNAP days", y = NULL) + th + theme(panel.grid.major.y = element_blank())
ggsave(file.path(OUT, "fig_snap.png"), pG, width = 8.5, height = 3.2, dpi = 160)

# H — lost sales by store
st <- fread(file.path(OUT, "all_stores_stockouts.csv"))[order(lost_pct)]
st[, store_id := factor(store_id, levels = store_id)]
pH <- ggplot(st, aes(lost_pct, store_id)) + geom_col(fill = BLUE, width = .6) +
  geom_text(aes(label = sprintf("%.1f%%  ($%sK/yr)", lost_pct, formatC(round(lost_per_year / 1000), big.mark = ","))), hjust = -.06, size = 3.3, color = INK2) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 5.6), expand = expansion(c(0, 0))) +
  labs(title = "Every store loses 2.7% to 4.1% of sales to stockouts",
       subtitle = "Estimated lost sales as a share of revenue, 2011 to 2016 average", x = NULL, y = NULL) + th +
  theme(panel.grid.major.y = element_blank())
ggsave(file.path(OUT, "fig_stores.png"), pH, width = 8.5, height = 3.8, dpi = 160)

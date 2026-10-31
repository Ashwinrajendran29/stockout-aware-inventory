# 13_dashboard_data.R — tidy CSVs for the interactive dashboard
suppressMessages(library(data.table))
dir.create("dashboard", showWarnings = FALSE)
st <- fread("out/all_stores_stockouts.csv")
fwrite(st[, .(store = store_id, state = substr(store_id, 1, 2), revenue_per_year, lost_per_year,
              lost_pct = round(lost_pct / 100, 4), flagged_day_share = share_days_flagged)], "dashboard/stockout_losses.csv")
b <- fread("out/sim_by_store_cat.csv")
fwrite(b[, .(store, category = cat, policy, demand_units = round(demand), sales_units = round(sales),
             holding_cost = round(hold_cost), lost_margin = round(lost_cost), total_cost = round(total_cost),
             avg_inventory_value = round(avg_inventory_value))], "dashboard/policy_results.csv")
f <- fread("out/sim_frontier.csv")
fwrite(f[, .(policy, setting, fill_rate = round(fill_rate, 4), total_cost = round(total_cost),
             avg_inventory_value = round(avg_inventory_value))], "dashboard/policy_frontier.csv")
w <- fread("out/step2_weekly_affected.csv")
fwrite(w[, .(model, weeks_under_forecast = under_share, bias_units_per_week = bias, weekly_mae = MAE)], "dashboard/forecast_bias.csv")
s <- fread("out/step3_snap.csv")
fwrite(s[, .(category = cat, effect = round(exp(estimate) - 1, 4), low = round(exp(estimate - 1.96 * se) - 1, 4),
             high = round(exp(estimate + 1.96 * se) - 1, 4))], "dashboard/snap_effect.csv")
e <- fread("out/step3_elasticity.csv")
fwrite(e[, .(category = cat, elasticity = round(estimate, 2), std_error = round(se, 2), items)], "dashboard/price_elasticity.csv")

# Stockout-Aware Inventory Optimization

An end-to-end AI decision system for retail replenishment, built on Walmart's M5 data (30,490 product-store series, 10 stores, 2011–2016). BANA 6780 AI for Business, Fall 2026.

**Live dashboard:** https://ashwinrajendran29.github.io/stockout-aware-inventory/

**The problem:** an empty shelf records zero sales, so forecasts read stockouts as zero demand, order less, and cause the next stockout.

## Results

| Finding | Result |
|---|---|
| Cost of stockouts (10 stores) | ~$1.27M/yr, 3.5% of revenue (2.7%–4.1% by store) |
| Best forecasting model | Random forest; beats a 28-day average by 1.6% MAE |
| Effect of stockout correction | Weeks under-forecast on affected items: 58–65% → 40% |
| SNAP days (natural experiment) | Food sales +10.7%; household 0.0%, hobbies +0.6% (placebos) |
| Price elasticity | FOODS −0.91, HOUSEHOLD −0.40, HOBBIES −0.42 |
| Inventory simulation (29,906 items, 1 year) | Stockout-aware ordering: −4.5% annual cost (~$102K), +1.0 pt fill rate; same service with 8.7% less inventory |

## How it works

1. **Detect stockouts** — flag zero-sale runs with probability `exp(-λL) < 0.0001` under the item's normal rate λ; validated against simulated no-stockout data.
2. **Forecast** — CART (`rpart`) and random forest (`ranger`) with lagged sales, calendar, SNAP and price features, with and without stockout correction.
3. **Explain demand** — two-way fixed-effects regressions for price elasticity; SNAP calendars as a natural experiment.
4. **Decide orders** — simulate a year of weekly order-up-to replenishment under fixed safety stock, forecast-based, and stockout-aware policies, with real outages replayed.

## Run it

1. Install R, then: `install.packages(c("data.table", "ggplot2", "rpart", "ranger"))`
2. Download the M5 Forecasting – Accuracy data from Kaggle and put `sales_train_evaluation.csv`, `calendar.csv` and `sell_prices.csv` in `data/`.
3. From this folder, run the scripts in this order (about 12 minutes in total):

| Script | What it does |
|---|---|
| `R/01_load_explore.R` | Load data; zero-sales overview; save the CA_1 pilot slice |
| `R/02_stockout_detect.R` | Stockout detector and placebo simulations (CA_1) |
| `R/03_robustness.R` | Detector thresholds, long gaps, lost revenue (CA_1) |
| `R/04_baselines.R` | Baseline forecasts on the 28-day holdout |
| `R/06_models.R` | Leakage-free feature tables (raw and stockout-corrected) |
| `R/07_train_eval.R` | Train and score CART and random forest (~5 min) |
| `R/08_weekly_eval.R` | Weekly-level accuracy |
| `R/05_charts.R` | Stage 1–2 figures |
| `R/09_all_stores.R` | Detector on all 10 stores; reconstructed demand |
| `R/10_inventory_sim.R` | Inventory simulation, perfect-supply check, cost sensitivity |
| `R/11_price_events.R` | Price elasticity, event effects, SNAP natural experiment |
| `R/12_final_charts.R` | Stage 3–4 figures |
| `R/13_dashboard_data.R` | Summary CSVs that feed the interactive dashboard (`dashboard/`) |

Outputs (tables as CSV, figures as PNG) are written to `out/`. A clean rerun reproduces every result file exactly (seed 6780).

## Data

The M5 data is not included in this repository: Kaggle's competition rules do not allow redistributing it. Download it from the competition page on Kaggle.

## AI use

Claude (Anthropic) was used to write the R code, run the analysis, produce the figures and draft the write-up.

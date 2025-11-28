# Walmart Store Sales

## Data

Weekly panel at **Store × Week** granularity with target **`Weekly_Sales`** and covariates (`Holiday_Flag`, `Temperature`, `Fuel_Price`, `CPI`, `Unemployment`). One row = one **store-week**.

---

## Phase 1 — EDA & Integrity (done)

* **Schema normalized:** snake_case columns; `date` parsed; `store` as factor; `holiday_flag` as {Non-Holiday, Holiday}.
* **Key integrity:** exactly **one row per (store, date)**.
* **Cadence:** weekly; non-7-day gaps flagged per store (informational).
* **Profiles saved:**

  * Distributions (sales & covariates)
  * Total sales over time
  * Holiday vs non-holiday trajectories
  * Top stores by sales
  * Per-store small multiples
  * Numeric correlation matrix
* **Calendar added:** `week`, `month`, `year`, `is_q4`.

**Outcome:** data is clean, keyed, and temporally coherent for feature building and time-aware CV.

---

## Phase 2 — Leakage-Safe Features (done)

All history features are computed **within store** using **only past weeks** (no look-ahead).

**History**

* Lags: `lag1`, `lag2`, `lag4`, `lag8`
* Past-only moving averages: `ma4`, `ma8` (rolling mean, then shifted by one week)
* Expanding stats up to t-1: `store_mean_to_prev`, `store_sd_to_prev`

**Seasonality & Calendar**

* Fourier harmonics: `sin1`, `cos1`, `sin2`, `cos2` (week-of-year)
* `is_q4`, `holiday_flag`, numeric `is_holiday_num`

**Exogenous**

* `temperature`, `fuel_price`, `cpi`, `unemployment`

**Pre-declared interactions**

* `inter_holiday_lag1 = is_holiday_num * lag1`
* `inter_temp_holiday = temperature * is_holiday_num`
* `inter_cpi_unemp = cpi * unemployment`

**Row eligibility**

* Trimmed to observations with sufficient history (`lag8`, `ma8`, `store_mean_to_prev` not NA).

**Leakage audits (passed)**

* Early-row NA check (pre-trim)
* **Rolling correctness:** `ma4 == mean(lag1..lag4)` where defined
* Weekly spacing audit (informational)

**Artifacts**

* `data/walmart_features.rds`
* `data/walmart_features.csv`

---

## Ready for Phase 3 — Modeling

* **Splits:** Train = 2010–2011 (with **blocked rolling CV** inside); Test = 2012.
* **Models to compare:** **LASSO**, **Ridge**, **Gradient Boosting**, **ARIMA** (per-store; optional ARIMAX).
* **Within-fold preprocessing:** build model matrices / scaling / encodings **inside CV** only.
* **Report:** MAE/RMSE on holdout, selection stability (LASSO), coefficient rank stability (Ridge), importance + PDP/ALE (GBM), ARIMA vs GBM per-store comparison.

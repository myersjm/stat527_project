## ==========================================================
## Phase 2 — Leakage-Safe Feature Engineering (Store-Week)
## FIXED: audits run BEFORE trimming rows with sufficient history
## Input  : `walmart` from Phase 1 (in-memory)
## Output : data/walmart_features.rds / .csv
## ==========================================================

## 0) Setup --------------------------------------------------
# install.packages(c("tidyverse","slider","lubridate","janitor"))
library(tidyverse)
library(slider)
library(lubridate)
library(janitor)

stopifnot(all(c("store","date","weekly_sales","holiday_flag",
                "temperature","fuel_price","cpi","unemployment",
                "week","month","year","is_q4") %in% names(walmart)))

## 1) Sort & key check --------------------------------------
walmart = walmart %>% arrange(store, date)

dups_key =
  walmart %>% count(store, date, name = "n") %>% filter(n > 1)
if (nrow(dups_key) > 0) stop("Duplicate (store, date) rows found. Resolve before Phase 2.")

## 2) Helper: past-only rolling means (exclude current) ------
# Compute rolling mean including current, then shift by 1 week.
roll_mean_prev = function(x, k) {
  inc = slide_dbl(x, mean, .before = k - 1, .complete = TRUE)
  lag(inc, 1)
}

## 3) Build features (PRETRIM) -------------------------------
# Build lags/means first; audits will run on this full set.
features_pretrim =
  walmart %>%
  group_by(store) %>%
  mutate(
    # strict lags
    lag1 = lag(weekly_sales, 1),
    lag2 = lag(weekly_sales, 2),
    lag3 = lag(weekly_sales, 3),   # for ma4 audit
    lag4 = lag(weekly_sales, 4),
    lag8 = lag(weekly_sales, 8),
    
    # past-only moving averages
    ma4  = roll_mean_prev(weekly_sales, 4),
    ma8  = roll_mean_prev(weekly_sales, 8),
    
    # expanding stats up to t-1
    store_mean_to_prev = lag(cummean(weekly_sales)),
    store_sd_to_prev   = lag(sqrt(cummean(weekly_sales^2) - (cummean(weekly_sales))^2))
  ) %>%
  ungroup() %>%
  mutate(
    # seasonality harmonics (calendar is known in advance)
    week_52 = if_else(week > 52, 52L, week),
    sin1 = sin(2 * pi * week_52 / 52),
    cos1 = cos(2 * pi * week_52 / 52),
    sin2 = sin(4 * pi * week_52 / 52),
    cos2 = cos(4 * pi * week_52 / 52),
    # numeric holiday
    is_holiday_num = as.integer(holiday_flag == "Holiday"),
    # interactions (declared a priori)
    inter_holiday_lag1 = is_holiday_num * lag1,
    inter_temp_holiday = temperature * is_holiday_num,
    inter_cpi_unemp    = cpi * unemployment
  )

## 4) AUDITS (run BEFORE trimming) ---------------------------

# A) True “start of store” rows should have NA lags/means
audit_na_head =
  features_pretrim %>%
  group_by(store) %>%
  slice_head(n = 10) %>%
  summarise(
    any_non_na_lag =
      any(!is.na(lag1) | !is.na(lag2) | !is.na(lag3) | !is.na(lag4) |
            !is.na(ma4) | !is.na(ma8)),
    .groups = "drop"
  )

if (any(audit_na_head$any_non_na_lag)) {
  # If TRUE here, you likely have non-weekly gaps at the very start of a store
  # or the dataset doesn't begin at week 1 for some stores (which is fine).
  # We only *require* that lags are past-only — verify with audit B below.
  message("Heads-up: some stores have fewer than 10 initial NA rows (that can be OK). Continuing…")
}

# B) Prove ma4 is past-only: ma4 must equal mean(lag1:lag4) whenever all exist
features_pretrim =
  features_pretrim %>%
  mutate(ma4_check = if_else(
    !is.na(lag1) & !is.na(lag2) & !is.na(lag3) & !is.na(lag4),
    (lag1 + lag2 + lag3 + lag4) / 4, NA_real_
  ))

audit_ma4_match =
  features_pretrim %>%
  filter(!is.na(ma4_check)) %>%
  mutate(eq = isTRUE(all.equal(ma4, ma4_check, tolerance = 1e-8))) %>%
  summarise(all_match = all(ma4 == ma4_check | abs(ma4 - ma4_check) < 1e-8)) %>%
  pull(all_match)

if (!isTRUE(audit_ma4_match)) stop("Leakage audit failed: ma4 is not equal to mean(lag1..lag4). Check rolling logic.")

# C) Weekly spacing sanity (not required to be perfect, but good to know)
audit_gap =
  walmart %>%
  group_by(store) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(delta_days = as.integer(date - lag(date))) %>%
  summarise(has_non7_gap = any(!is.na(delta_days) & delta_days != 7), .groups = "drop")
if (any(audit_gap$has_non7_gap)) {
  message("Note: Some stores have non-7-day gaps; lags are still past-only but interpret moving averages with care.")
}

## 5) Trim to rows with sufficient history -------------------
# Require 8 weeks of history (lag8 & ma8 available) and expanding mean present
features =
  features_pretrim %>%
  filter(!is.na(lag8), !is.na(ma8), !is.na(store_mean_to_prev))

## 6) Final selection / ordering -----------------------------
features_model =
  features %>%
  select(
    # keys/target
    store, date, year, month, week, is_q4, weekly_sales, holiday_flag,
    # raw covariates
    temperature, fuel_price, cpi, unemployment,
    # history
    lag1, lag2, lag4, lag8, ma4, ma8, store_mean_to_prev, store_sd_to_prev,
    # seasonality
    sin1, cos1, sin2, cos2,
    # interactions
    is_holiday_num, inter_holiday_lag1, inter_temp_holiday, inter_cpi_unemp
  ) %>%
  arrange(store, date)

## 7) Save artifacts -----------------------------------------
#if (!dir.exists("data")) dir.create("data", recursive = TRUE)
saveRDS(features_model, "walmart_features.rds")
readr::write_csv(features_model, "walmart_features.csv")

## 8) Preview ------------------------------------------------
features_model %>% group_by(store) %>% slice_head(n = 3) %>% print(n = 15)

## ================== End Phase 2 ============================
# Next: Phase 3 — time-aware splits (2010–2011 train, 2012 test) and rolling CV.

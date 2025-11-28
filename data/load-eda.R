## ==========================================================
## Exploratory Data Analysis: Walmart Weekly Store Data
## Phase 1 (finalized for your schema)
## Starting object: `Walmart` (spec_tbl_df with 8 columns)
## ==========================================================

## 0) Setup --------------------------------------------------

# install.packages(c("tidyverse","skimr","janitor","lubridate"))
library(tidyverse)
library(skimr)
library(janitor)
library(lubridate)

theme_set(theme_minimal())

## 1) Load from in-memory `Walmart` and Standardize ----------

# Your str(Walmart) shows camel-case names and Date as "dd-mm-YYYY"
# We'll: (a) clean names to snake_case, (b) parse Date with dmy, (c) cast types.

walmart_raw = Walmart

walmart =
  walmart_raw %>%
  clean_names() %>%  # store, date, weekly_sales, holiday_flag, temperature, fuel_price, cpi, unemployment
  mutate(
    # Parse "05-02-2010" as 05 Feb 2010
    date = dmy(date),
    store = as.factor(store),
    holiday_flag = factor(holiday_flag, levels = c(0, 1),
                          labels = c("Non-Holiday", "Holiday"))
  )

## 2) Structural & Sanity Checks -----------------------------

dim(walmart)
names(walmart)
glimpse(walmart)
skim(walmart)

# Ensure one row per (store, date)
dups_key =
  walmart %>%
  count(store, date, name = "n") %>%
  filter(n > 1)
dups_key
# If any rows appear here, resolve before proceeding.

## 3) Data Quality Diagnostics ------------------------------

# 3.1 Missingness profile
missing_summary =
  walmart %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_n") %>%
  arrange(desc(missing_n))
missing_summary

# 3.2 Range checks for key numeric fields
range_summary =
  walmart %>%
  summarise(
    min_weekly_sales = min(weekly_sales, na.rm = TRUE),
    max_weekly_sales = max(weekly_sales, na.rm = TRUE),
    min_temperature  = min(temperature, na.rm = TRUE),
    max_temperature  = max(temperature, na.rm = TRUE),
    min_fuel_price   = min(fuel_price, na.rm = TRUE),
    max_fuel_price   = max(fuel_price, na.rm = TRUE),
    min_cpi          = min(cpi, na.rm = TRUE),
    max_cpi          = max(cpi, na.rm = TRUE),
    min_unemployment = min(unemployment, na.rm = TRUE),
    max_unemployment = max(unemployment, na.rm = TRUE)
  )
range_summary

# 3.3 Constant / near-constant numeric columns
consts =
  walmart %>%
  summarise(across(where(is.numeric), ~ n_distinct(., na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "var", values_to = "ndistinct") %>%
  arrange(ndistinct)
consts

# 3.4 Negative or implausible values in sales
summary(walmart$weekly_sales)

## 4) Calendar Enrichment (for later phases) ----------------

walmart =
  walmart %>%
  mutate(
    week  = isoweek(date),
    month = month(date),
    year  = year(date),
    is_q4 = month %in% c(10, 11, 12)
  )

## 5) Weekly Continuity per Store (gap check) ----------------

# Expect 7-day jumps between weekly rows within each store
gaps =
  walmart %>%
  arrange(store, date) %>%
  group_by(store) %>%
  mutate(delta_days = as.integer(date - lag(date))) %>%
  summarise(
    has_gap = any(!is.na(delta_days) & delta_days != 7),
    min_date = min(date), max_date = max(date),
    n_weeks = n(), .groups = "drop"
  )
gaps
gaps %>% filter(has_gap)

## 6) Univariate Distributions ------------------------------

numeric_cols = c("weekly_sales","temperature","fuel_price","cpi","unemployment")

# 6.1 Numeric distributions
p_numeric_hist =
  walmart %>%
  pivot_longer(all_of(numeric_cols), names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ metric, scales = "free_x") +
  labs(title = "Distributions of Key Numeric Metrics", x = NULL, y = "Count")

p_numeric_hist
ggsave("wm_numeric_distributions.png", p_numeric_hist, width = 10, height = 6, dpi = 300, bg = "white")

# 6.2 Categorical frequencies
walmart %>% count(store, sort = TRUE)
walmart %>% count(holiday_flag, sort = TRUE)

## 7) Time-Series Behaviour ---------------------------------

# 7.1 Aggregate weekly sales over time (all stores)
daily_total =
  walmart %>%
  group_by(date) %>%
  summarise(total_weekly_sales = sum(weekly_sales, na.rm = TRUE), .groups = "drop")

p_total_sales_ts =
  daily_total %>%
  ggplot(aes(x = date, y = total_weekly_sales)) +
  geom_line() +
  labs(title = "Total Weekly Sales Over Time (All Stores)", x = "Date", y = "Total Weekly Sales")

p_total_sales_ts
ggsave("wm_total_weekly_sales_ts.png", p_total_sales_ts, width = 10, height = 5, dpi = 300, bg = "white")

# 7.2 Holiday vs non-holiday average sales over time
holiday_ts =
  walmart %>%
  group_by(date, holiday_flag) %>%
  summarise(avg_weekly_sales = mean(weekly_sales, na.rm = TRUE), .groups = "drop")

p_holiday_ts =
  holiday_ts %>%
  ggplot(aes(x = date, y = avg_weekly_sales, color = holiday_flag)) +
  geom_line() +
  labs(title = "Average Weekly Sales by Holiday Flag Over Time", x = "Date", y = "Average Weekly Sales", color = "Holiday")

p_holiday_ts
ggsave("wm_weekly_sales_by_holiday_ts.png", p_holiday_ts, width = 10, height = 5, dpi = 300, bg = "white")

# 7.3 Overall holiday impact (aggregated)
holiday_summary =
  walmart %>%
  group_by(holiday_flag) %>%
  summarise(
    avg_weekly_sales    = mean(weekly_sales, na.rm = TRUE),
    median_weekly_sales = median(weekly_sales, na.rm = TRUE),
    n = n(), .groups = "drop"
  )
holiday_summary

p_holiday_bar =
  holiday_summary %>%
  ggplot(aes(x = holiday_flag, y = avg_weekly_sales, fill = holiday_flag)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Average Weekly Sales: Holiday vs Non-Holiday", x = NULL, y = "Average Weekly Sales")

p_holiday_bar
ggsave("wm_holiday_vs_nonholiday_sales.png", p_holiday_bar, width = 6, height = 4, dpi = 300, bg = "white")

## 8) Store Performance -------------------------------------

# 8.1 Store-level totals
store_summary =
  walmart %>%
  group_by(store) %>%
  summarise(
    total_weekly_sales = sum(weekly_sales, na.rm = TRUE),
    avg_weekly_sales   = mean(weekly_sales, na.rm = TRUE),
    n_weeks            = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(total_weekly_sales))
head(store_summary, 10)

p_store_total =
  store_summary %>%
  slice_max(total_weekly_sales, n = 20) %>%
  ggplot(aes(x = reorder(store, total_weekly_sales), y = total_weekly_sales)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 20 Stores by Total Weekly Sales", x = "Store", y = "Total Weekly Sales")

p_store_total
ggsave("wm_top_stores_total_sales.png", p_store_total, width = 7, height = 6, dpi = 300, bg = "white")

# 8.2 Small multiples: per-store sales over time
p_small_mult =
  walmart %>%
  group_by(store, date) %>%
  summarise(sales = sum(weekly_sales, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(date, sales)) +
  geom_line() +
  facet_wrap(~ store, scales = "free_y") +
  labs(title = "Weekly Sales over Time by Store", x = NULL, y = "Weekly Sales")

p_small_mult
ggsave("wm_store_small_multiples.png", p_small_mult, width = 12, height = 8, dpi = 300, bg = "white")

## 9) Relationships / Drivers --------------------------------

# 9.1 Correlations among numeric covariates
num_cols = c("weekly_sales","temperature","fuel_price","cpi","unemployment")
cor_mat =
  walmart %>%
  select(all_of(num_cols)) %>%
  cor(use = "pairwise.complete.obs")
cor_mat

# 9.2 Bivariate plots
p_temp_sales =
  walmart %>%
  ggplot(aes(x = temperature, y = weekly_sales)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Temperature vs Weekly Sales", x = "Temperature", y = "Weekly Sales")
p_temp_sales
ggsave("wm_temperature_vs_weekly_sales.png", p_temp_sales, width = 7, height = 5, dpi = 300, bg = "white")

p_fuel_sales =
  walmart %>%
  ggplot(aes(x = fuel_price, y = weekly_sales)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Fuel Price vs Weekly Sales", x = "Fuel Price", y = "Weekly Sales")
p_fuel_sales
ggsave("wm_fuel_price_vs_weekly_sales.png", p_fuel_sales, width = 7, height = 5, dpi = 300, bg = "white")

p_cpi_sales =
  walmart %>%
  ggplot(aes(x = cpi, y = weekly_sales)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "CPI vs Weekly Sales", x = "CPI", y = "Weekly Sales")
p_cpi_sales
ggsave("wm_cpi_vs_weekly_sales.png", p_cpi_sales, width = 7, height = 5, dpi = 300, bg = "white")

p_unemp_sales =
  walmart %>%
  ggplot(aes(x = unemployment, y = weekly_sales)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Unemployment vs Weekly Sales", x = "Unemployment Rate", y = "Weekly Sales")
p_unemp_sales
ggsave("wm_unemployment_vs_weekly_sales.png", p_unemp_sales, width = 7, height = 5, dpi = 300, bg = "white")

## 10) Holiday Flag Sanity Check -----------------------------

table(walmart$holiday_flag, walmart$year, useNA = "ifany")

## ================== End of Phase 1 ========================
# If `dups_key` has 0 rows, `gaps$has_gap` is FALSE for most stores (or explained),
# and plots/tables saved, Phase 1 is complete. Proceed to Phase 2.

## ==========================================================
## Exploratory Data Analysis: Walmart Weekly Store Data
## ==========================================================

## 0. Setup --------------------------------------------------

# Run installs once if needed:
# install.packages("tidyverse")
# install.packages("skimr")
# install.packages("janitor")

library(tidyverse)
library(skimr)
library(janitor)

theme_set(theme_minimal())

## 1. Load & Standardize -------------------------------------
# Assuming you already did something like:
# walmart = readr::read_csv("walmart.csv")

walmart = Walmart %>%
  clean_names()    # store, date, weekly_sales, etc.

## 2. Structural & Sanity Checks -----------------------------

dim(walmart)
names(walmart)
glimpse(walmart)
skim(walmart)

# If using readr, you can also inspect parsing issues:
# problems(walmart)

## 3. Data Quality Diagnostics ------------------------------

# 3.1 Missingness profile
missing_summary =
  walmart %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "missing_n") %>%
  arrange(desc(missing_n))

missing_summary

# 3.2 Duplicate row check
duplicate_check =
  walmart %>%
  summarise(duplicate_rows = n() - n_distinct(across(everything())))

duplicate_check

# 3.3 Range checks for key numeric fields
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

## 4. Type Alignment -----------------------------------------

walmart = walmart %>%
  mutate(
    # Dates are like "05-02-2010" -> 05 Feb 2010 (dd-mm-YYYY)
    date = as.Date(date, format = "%d-%m-%Y"),
    store = as.factor(store),
    holiday_flag = factor(holiday_flag,
                          levels = c(0, 1),
                          labels = c("Non-Holiday", "Holiday"))
  )

## 5. Univariate Distributions -------------------------------

numeric_cols = c(
  "weekly_sales",
  "temperature",
  "fuel_price",
  "cpi",
  "unemployment"
)

# 5.1 Numeric distributions
p_numeric_hist =
  walmart %>%
  pivot_longer(all_of(numeric_cols),
               names_to = "metric",
               values_to = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ metric, scales = "free_x") +
  labs(
    title = "Distributions of Key Numeric Metrics",
    x = NULL,
    y = "Count"
  )

p_numeric_hist
ggsave("wm_numeric_distributions.png", p_numeric_hist,
       width = 10, height = 6, dpi = 300, bg = "white")

# 5.2 Categorical frequencies (tabular)
walmart %>% count(store, sort = TRUE)
walmart %>% count(holiday_flag, sort = TRUE)

## 6. Time-Series Behaviour ----------------------------------

# 6.1 Aggregate weekly sales over time (across all stores)
daily_total =
  walmart %>%
  group_by(date) %>%
  summarise(
    total_weekly_sales = sum(weekly_sales, na.rm = TRUE),
    .groups = "drop"
  )

p_total_sales_ts =
  daily_total %>%
  ggplot(aes(x = date, y = total_weekly_sales)) +
  geom_line() +
  labs(
    title = "Total Weekly Sales Over Time (All Stores)",
    x = "Date",
    y = "Total Weekly Sales"
  )

p_total_sales_ts
ggsave("wm_total_weekly_sales_ts.png", p_total_sales_ts,
       width = 10, height = 5, dpi = 300, bg = "white")

# 6.2 Holiday vs non-holiday average sales over time
holiday_ts =
  walmart %>%
  group_by(date, holiday_flag) %>%
  summarise(
    avg_weekly_sales = mean(weekly_sales, na.rm = TRUE),
    .groups = "drop"
  )

p_holiday_ts =
  holiday_ts %>%
  ggplot(aes(x = date, y = avg_weekly_sales, color = holiday_flag)) +
  geom_line() +
  labs(
    title = "Average Weekly Sales by Holiday Flag Over Time",
    x = "Date",
    y = "Average Weekly Sales",
    color = "Holiday"
  )

p_holiday_ts
ggsave("wm_weekly_sales_by_holiday_ts.png", p_holiday_ts,
       width = 10, height = 5, dpi = 300, bg = "white")

# 6.3 Overall holiday impact (aggregated)
holiday_summary =
  walmart %>%
  group_by(holiday_flag) %>%
  summarise(
    avg_weekly_sales = mean(weekly_sales, na.rm = TRUE),
    median_weekly_sales = median(weekly_sales, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

holiday_summary

p_holiday_bar =
  holiday_summary %>%
  ggplot(aes(x = holiday_flag, y = avg_weekly_sales, fill = holiday_flag)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Average Weekly Sales: Holiday vs Non-Holiday",
    x = NULL,
    y = "Average Weekly Sales"
  )

p_holiday_bar
ggsave("wm_holiday_vs_nonholiday_sales.png", p_holiday_bar,
       width = 6, height = 4, dpi = 300, bg = "white")

## 7. Store Performance --------------------------------------

# 7.1 Store-level total sales
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
  ggplot(aes(x = reorder(store, total_weekly_sales),
             y = total_weekly_sales)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 20 Stores by Total Weekly Sales",
    x = "Store",
    y = "Total Weekly Sales"
  )

p_store_total
ggsave("wm_top_stores_total_sales.png", p_store_total,
       width = 7, height = 6, dpi = 300, bg = "white")

## 8. Relationships / Drivers --------------------------------

# 8.1 Temperature vs Weekly Sales
p_temp_sales =
  walmart %>%
  ggplot(aes(x = temperature, y = weekly_sales)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Temperature vs Weekly Sales",
    x = "Temperature",
    y = "Weekly Sales"
  )

p_temp_sales
ggsave("wm_temperature_vs_weekly_sales.png", p_temp_sales,
       width = 7, height = 5, dpi = 300, bg = "white")

# 8.2 Fuel Price vs Weekly Sales
p_fuel_sales =
  walmart %>%
  ggplot(aes(x = fuel_price, y = weekly_sales)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Fuel Price vs Weekly Sales",
    x = "Fuel Price",
    y = "Weekly Sales"
  )

p_fuel_sales
ggsave("wm_fuel_price_vs_weekly_sales.png", p_fuel_sales,
       width = 7, height = 5, dpi = 300, bg = "white")

# 8.3 CPI vs Weekly Sales
p_cpi_sales =
  walmart %>%
  ggplot(aes(x = cpi, y = weekly_sales)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "CPI vs Weekly Sales",
    x = "CPI",
    y = "Weekly Sales"
  )

p_cpi_sales
ggsave("wm_cpi_vs_weekly_sales.png", p_cpi_sales,
       width = 7, height = 5, dpi = 300, bg = "white")

# 8.4 Unemployment vs Weekly Sales
p_unemp_sales =
  walmart %>%
  ggplot(aes(x = unemployment, y = weekly_sales)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Unemployment vs Weekly Sales",
    x = "Unemployment Rate",
    y = "Weekly Sales"
  )

p_unemp_sales
ggsave("wm_unemployment_vs_weekly_sales.png", p_unemp_sales,
       width = 7, height = 5, dpi = 300, bg = "white")

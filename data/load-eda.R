## ==========================================================
## Exploratory Data Analysis: Retail Sales & Inventory
## ==========================================================

## 0. Setup --------------------------------------------------

# Run installs once (commented out intentionally)
# install.packages("tidyverse")
# install.packages("skimr")
# install.packages("janitor")

library(tidyverse)
library(skimr)
library(janitor)

## 1. Load & Standardize -------------------------------------
# Assumes `sales` already exists in the environment.
# If reading from CSV:
sales = sales_data

sales = sales %>%
  clean_names()    # snake_case for robust downstream use

## 2. Structural & Sanity Checks -----------------------------

dim(sales)
names(sales)
glimpse(sales)
skim(sales)

# Parsing issues (if loaded with readr)
# problems(sales)

## 3. Data Quality Diagnostics ------------------------------

# 3.1 Missingness profile
missing_summary =
  sales %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "missing_n") %>%
  arrange(desc(missing_n))

missing_summary

# 3.2 Duplicate row check
duplicate_check =
  sales %>%
  summarise(duplicate_rows = n() - n_distinct(across(everything())))

duplicate_check

# 3.3 Range checks for key numeric fields
range_summary =
  sales %>%
  summarise(
    min_units_sold = min(units_sold, na.rm = TRUE),
    max_units_sold = max(units_sold, na.rm = TRUE),
    min_price = min(price, na.rm = TRUE),
    max_price = max(price, na.rm = TRUE),
    min_discount = min(discount, na.rm = TRUE),
    max_discount = max(discount, na.rm = TRUE),
    min_inventory = min(inventory_level, na.rm = TRUE),
    max_inventory = max(inventory_level, na.rm = TRUE)
  )

range_summary

## 4. Type Alignment -----------------------------------------

sales = sales %>%
  mutate(
    date = as.Date(date),
    store_id = as.factor(store_id),
    product_id = as.factor(product_id),
    category = as.factor(category),
    region = as.factor(region),
    weather_condition = as.factor(weather_condition),
    seasonality = as.factor(seasonality),
    promotion_flag = factor(promotion,
                            levels = c(0, 1),
                            labels = c("No Promo", "Promo")),
    epidemic_flag = factor(epidemic,
                           levels = c(0, 1),
                           labels = c("No", "Yes"))
  )

## 5. Univariate Distributions -------------------------------

numeric_cols = c(
  "units_sold", "units_ordered", "price",
  "discount", "inventory_level",
  "competitor_pricing", "demand"
)

# 5.1 Numeric distributions
p_numeric_hist =
  sales %>%
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
ggsave("eda_numeric_distributions.png", p_numeric_hist,
       width = 10, height = 6, dpi = 300, bg = "white")

# 5.2 Categorical frequencies (tabular views)
sales %>% count(category, sort = TRUE)
sales %>% count(region, sort = TRUE)
sales %>% count(weather_condition, sort = TRUE)
sales %>% count(seasonality, sort = TRUE)
sales %>% count(promotion_flag, sort = TRUE)
sales %>% count(epidemic_flag, sort = TRUE)

## 6. Time Series Behaviour ----------------------------------

# 6.1 Aggregate daily demand and sales volume
daily =
  sales %>%
  group_by(date) %>%
  summarise(
    total_units_sold = sum(units_sold, na.rm = TRUE),
    total_demand = sum(demand, na.rm = TRUE),
    .groups = "drop"
  )

# Units sold over time
p_daily_units =
  daily %>%
  ggplot(aes(x = date, y = total_units_sold)) +
  geom_line() +
  labs(
    title = "Daily Total Units Sold",
    x = "Date",
    y = "Units Sold"
  )

p_daily_units
ggsave("eda_daily_units_sold.png", p_daily_units,
       width = 10, height = 5, dpi = 300, bg = "white")

# Demand over time
p_daily_demand =
  daily %>%
  ggplot(aes(x = date, y = total_demand)) +
  geom_line() +
  labs(
    title = "Daily Total Demand",
    x = "Date",
    y = "Demand"
  )

p_daily_demand
ggsave("eda_daily_demand.png", p_daily_demand,
       width = 10, height = 5, dpi = 300, bg = "white")

# 6.2 Seasonality summary
seasonality_summary =
  sales %>%
  group_by(seasonality) %>%
  summarise(
    avg_units_sold = mean(units_sold, na.rm = TRUE),
    avg_demand = mean(demand, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_units_sold))

seasonality_summary

## 7. Store / Region / Category Performance -----------------

# 7.1 Top stores by units_sold
top_stores =
  sales %>%
  group_by(store_id) %>%
  summarise(
    total_units_sold = sum(units_sold, na.rm = TRUE),
    total_demand = sum(demand, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_units_sold)) %>%
  head(15)

top_stores

# 7.2 Region vs Category heatmap
p_region_category =
  sales %>%
  group_by(region, category) %>%
  summarise(
    total_units_sold = sum(units_sold, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = region, y = category, fill = total_units_sold)) +
  geom_tile() +
  scale_fill_continuous() +
  labs(
    title = "Total Units Sold by Region and Category",
    x = "Region",
    y = "Category",
    fill = "Units Sold"
  )

p_region_category
ggsave("eda_region_category_heatmap.png", p_region_category,
       width = 8, height = 6, dpi = 300, bg = "white")

## 8. Key Relationships / Drivers ---------------------------

# 8.1 Price vs Units Sold
p_price_units =
  sales %>%
  ggplot(aes(x = price, y = units_sold)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Price vs Units Sold",
    x = "Price",
    y = "Units Sold"
  )

p_price_units
ggsave("eda_price_vs_units_sold.png", p_price_units,
       width = 7, height = 5, dpi = 300, bg = "white")

# 8.2 Discount vs Units Sold
p_discount_units =
  sales %>%
  ggplot(aes(x = discount, y = units_sold)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Discount vs Units Sold",
    x = "Discount",
    y = "Units Sold"
  )

p_discount_units
ggsave("eda_discount_vs_units_sold.png", p_discount_units,
       width = 7, height = 5, dpi = 300, bg = "white")

# 8.3 Competitor Pricing vs Units Sold
p_competitor_units =
  sales %>%
  ggplot(aes(x = competitor_pricing, y = units_sold)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Competitor Pricing vs Units Sold",
    x = "Competitor Pricing",
    y = "Units Sold"
  )

p_competitor_units
ggsave("eda_competitor_vs_units_sold.png", p_competitor_units,
       width = 7, height = 5, dpi = 300, bg = "white")

# 8.4 Promotion vs performance (tabular)
promotion_effect =
  sales %>%
  group_by(promotion_flag) %>%
  summarise(
    avg_units_sold = mean(units_sold, na.rm = TRUE),
    avg_demand = mean(demand, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

promotion_effect

## 9. Inventory vs Demand / Stock Risk ----------------------

# 9.1 Overall gap & understock rate
inventory_gap_summary =
  sales %>%
  mutate(gap = inventory_level - demand) %>%
  summarise(
    avg_gap = mean(gap, na.rm = TRUE),
    understock_rate = mean(gap < 0, na.rm = TRUE)
  )

inventory_gap_summary

# 9.2 Understock risk by product
understock_by_product =
  sales %>%
  mutate(gap = inventory_level - demand) %>%
  group_by(product_id) %>%
  summarise(
    understock_rate = mean(gap < 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(understock_rate)) %>%
  head(20)

understock_by_product

# 9.3 Visual: Inventory vs Demand (optional)
p_inventory_demand =
  sales %>%
  mutate(gap = inventory_level - demand) %>%
  ggplot(aes(x = demand, y = inventory_level)) +
  geom_point() +
  labs(
    title = "Inventory Level vs Demand",
    x = "Demand",
    y = "Inventory Level"
  )

p_inventory_demand
ggsave("eda_inventory_vs_demand.png", p_inventory_demand,
       width = 7, height = 5, dpi = 300, bg = "white")

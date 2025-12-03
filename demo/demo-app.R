#############################
# app.R  (single-file Shiny)
#############################

# -------- 1. Packages --------
library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)
library(glue)
library(Matrix)
library(xgboost)

# -------- 2. Check required objects --------
if (!exists("Walmart")) {
  stop("Please load the Walmart dataset (object named 'Walmart') before running this app.")
}
if (!exists("lasso")) {
  stop("Please have a glmnet Lasso model object named 'lasso' in the environment.")
}
if (!exists("xgboost_model")) {
  stop("Please have an xgboost model object named 'xgboost_model' in the environment.")
}
if (!exists("test_wape_arimax")) {
  stop("Please have a numeric scalar 'test_wape_arimax' (ARIMAX WAPE) in the environment.")
}

# -------- 3. Prepare Walmart data --------
# Expected columns in Walmart:
# Store, Date (character), Weekly_Sales, Holiday_Flag, Temperature, Fuel_Price, CPI, Unemployment

sales_ts = Walmart |>
  mutate(
    # If dates are actually month-day-year, replace dmy() with mdy()
    Date = dmy(Date),
    IsHoliday = Holiday_Flag == 1
  ) |>
  arrange(Store, Date)

# Global train/test split (you can change this)
split_date = max(sales_ts$Date, na.rm = TRUE) - 52  # last ~1 year as test

# Distinct store list (for selectInput)
stores = sort(unique(sales_ts$Store))

# -------- 4. Model performance (WAPE only) --------
# We have a real WAPE for ARIMAX; Lasso/GBM are placeholders (close to ARIMAX)
# so you can see structure. Replace wape_lasso / wape_gbm with your true values later.

set.seed(123)

wape_arimax = as.numeric(test_wape_arimax)

wape_lasso = wape_arimax * runif(1, 0.9, 1.2)  # placeholder
wape_gbm   = wape_arimax * runif(1, 0.9, 1.2)  # placeholder

model_perf = expand_grid(
  Store = stores,
  Model = c("Lasso", "GBM", "ARIMAX"),
  Horizon = c(1, 4, 8, 12)
) |>
  mutate(
    Metric = "WAPE",
    Value = case_when(
      Model == "Lasso"  ~ wape_lasso,
      Model == "GBM"    ~ wape_gbm,
      Model == "ARIMAX" ~ wape_arimax
    ),
    n_features = case_when(
      Model == "Lasso"  ~ lasso$df,
      Model == "GBM"    ~ xgboost_model$nfeatures,
      TRUE              ~ 0L
    ),
    train_time = NA_real_  # optional, fill with real times if you have them
  )

# -------- 5. Dummy forecasts (for visualization only) --------
# If you have real test predictions, replace this whole block with them.

forecasts = sales_ts |>
  mutate(DataSet = if_else(Date <= split_date, "Train", "Test")) |>
  filter(DataSet == "Test") |>
  crossing(
    Model = c("Lasso", "GBM", "ARIMAX"),
    Horizon = c(1, 4, 8, 12)
  ) |>
  mutate(
    Pred = case_when(
      Model == "Lasso"  ~ Weekly_Sales * runif(n(), 0.9, 1.1),
      Model == "GBM"    ~ Weekly_Sales * runif(n(), 0.92, 1.08),
      Model == "ARIMAX" ~ Weekly_Sales * runif(n(), 0.95, 1.05)
    ),
    Lower = pmin(Weekly_Sales, Pred) * 0.95,
    Upper = pmax(Weekly_Sales, Pred) * 1.05
  )

# -------- 6. Feature importance from real models --------

## 6a. Lasso coefficients
lasso_beta_mat = as.matrix(lasso$beta)
lasso_features = rownames(lasso_beta_mat)
lasso_coefs    = as.numeric(lasso_beta_mat[, 1])

feat_imp_lasso = tibble(
  Model = "Lasso",
  Feature = lasso_features,
  Coefficient = lasso_coefs,
  Importance = NA_real_
)

## 6b. XGBoost importance
xgb_imp = xgb.importance(model = xgboost_model)

feat_imp_gbm = xgb_imp |>
  as_tibble() |>
  mutate(
    Model = "GBM",
    Coefficient = NA_real_,
    Importance = Gain
  ) |>
  select(Model, Feature, Coefficient, Importance)

## 6c. Combine & normalize GBM importance
feat_imp = bind_rows(feat_imp_lasso, feat_imp_gbm) |>
  group_by(Model) |>
  mutate(
    Importance = if_else(
      Model == "GBM" & !is.na(Importance),
      Importance / sum(Importance, na.rm = TRUE),
      Importance
    )
  ) |>
  ungroup()

# -------- 7. UI --------
ui = navbarPage(
  "Retail Sales Forecasting: Variable Selection",
  theme = shinytheme("flatly"),
  
  tabPanel(
    "Overview",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput("store", "Store", choices = stores, selected = stores[1]),
        dateRangeInput(
          "date_range",
          "History window",
          start = min(sales_ts$Date, na.rm = TRUE),
          end = max(sales_ts$Date, na.rm = TRUE)
        )
      ),
      mainPanel(
        width = 9,
        fluidRow(
          column(4, uiOutput("box_best_wape")),
          column(4, uiOutput("box_best_sparsity"))
        ),
        hr(),
        plotOutput("ts_plot_overview", height = "350px")
      )
    )
  ),
  
  tabPanel(
    "Model Comparison",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        # Use a dropdown with the actual horizons we have
        selectInput(
          "horizon_mc", "Forecast horizon (weeks)",
          choices = c(1, 4, 8, 12),
          selected = 4
        ),
        checkboxGroupInput(
          "models_mc",
          "Models to show",
          choices = c("Lasso", "GBM", "ARIMAX"),
          selected = c("Lasso", "GBM", "ARIMAX")
        )
      ),
      mainPanel(
        width = 9,
        plotOutput("model_perf_plot", height = "350px"),
        hr(),
        tableOutput("model_summary_table")
      )
    )
  ),
  
  tabPanel(
    "Forecast Explorer",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput(
          "model_fx",
          "Model",
          choices = c("Lasso", "GBM", "ARIMAX"),
          selected = "GBM"
        ),
        selectInput(
          "horizon_fx", "Forecast horizon (weeks)",
          choices = c(1, 4, 8, 12),
          selected = 4
        ),
        checkboxInput("show_pi", "Show prediction interval (if available)", TRUE)
      ),
      mainPanel(
        width = 9,
        plotOutput("forecast_plot", height = "350px")
      )
    )
  ),
  
  tabPanel(
    "Feature Interpretation",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput(
          "model_feat",
          "Model",
          choices = c("Lasso", "GBM"),
          selected = "Lasso"
        ),
        sliderInput(
          "top_k",
          "Top features",
          min = 3, max = 20, value = 10, step = 1
        )
      ),
      mainPanel(
        width = 9,
        plotOutput("feature_plot", height = "350px")
        # Text summary still commented out
        # ,hr(), verbatimTextOutput("feature_summary_text")
      )
    )
  )
)

# -------- 8. Server --------
server = function(input, output, session) {
  
  # ----- Reactive: filtered time series for selected store -----
  ts_selected = reactive({
    sales_ts |>
      filter(Store == input$store) |>
      arrange(Date) |>
      mutate(
        DataSet = if_else(Date <= split_date, "Train", "Test")
      )
  })
  
  # ----- Reactive: model performance subset -----
  model_perf_selected = reactive({
    model_perf |>
      filter(Store == input$store)
  })
  
  # ----- Helper: best model by WAPE -----
  get_best_wape_model = function(df) {
    df_filtered = df |>
      filter(Metric == "WAPE") |>
      group_by(Model) |>
      summarise(
        Value = mean(Value, na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(Value)
    
    if (nrow(df_filtered) == 0) return(NULL)
    df_filtered[1, , drop = FALSE]
  }
  
  # ----- Info boxes (Overview) -----
  output$box_best_wape = renderUI({
    df = model_perf_selected()
    if (nrow(df) == 0) {
      return(NULL)
    }
    best = get_best_wape_model(df)
    if (is.null(best)) {
      return(NULL)
    }
    
    div(
      class = "well",
      style = "background-color:#f5f5f5;",
  h4("Best WAPE model"),
p(strong(best$Model)),
p(glue("WAPE: {round(best$Value * 100, 1)}%"))
)
})

output$box_best_sparsity = renderUI({
  df = model_perf_selected()
  if (nrow(df) == 0) {
    return(NULL)
  }
  
  sparsity = df |>
    group_by(Model) |>
    summarise(
      avg_features = mean(n_features, na.rm = TRUE),
      .groups = "drop"
    ) |>
    filter(avg_features > 0) |>
    arrange(avg_features)
  
  if (nrow(sparsity) == 0) {
    return(
      div(
        class = "well",
        style = "background-color:#f5f5f5;",
        h4("Sparsity"),
        p("No feature counts available.")
      )
    )
  }
  
  best_sparse = sparsity[1, , drop = FALSE]
  
  div(
    class = "well",
    style = "background-color:#f5f5f5;",
    h4("Most sparse model"),
    p(strong(best_sparse$Model)),
    p(glue("Avg. # features: {round(best_sparse$avg_features, 1)}"))
  )
})

# ----- Overview time series plot -----
output$ts_plot_overview = renderPlot({
  df = ts_selected()
  
  # Apply date filter from UI
  df = df |>
    filter(Date >= input$date_range[1], Date <= input$date_range[2])
  
  ggplot(df, aes(x = Date, y = Weekly_Sales, color = DataSet)) +
    geom_line(linewidth = 1) +
    scale_y_continuous(labels = dollar_format()) +
    scale_color_manual(values = c("Train" = "#1f77b4", "Test" = "#ff7f0e")) +
    labs(
      x = "Week",
      y = "Weekly Sales",
      color = "Set",
      title = glue("Store {unique(df$Store)}")
    ) +
    theme_minimal(base_size = 14)
})

# ----- Model Comparison: performance plot (WAPE only) -----
output$model_perf_plot = renderPlot({
  df = model_perf_selected() |>
    filter(
      Metric == "WAPE",
      Horizon == as.numeric(input$horizon_mc),
      Model %in% input$models_mc
    )
  
  if (nrow(df) == 0) {
    return(NULL)
  }
  
  ggplot(df, aes(x = Model, y = Value * 100, fill = Model)) +
    geom_col(width = 0.6) +
    labs(
      x = NULL,
      y = "WAPE (%)",
      title = glue("WAPE by model (Store {input$store}, horizon = {input$horizon_mc} weeks)")
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none")
})

# ----- Model Comparison: summary table (WAPE only) -----
output$model_summary_table = renderTable({
  df = model_perf_selected() |>
    filter(Metric == "WAPE") |>
    group_by(Model) |>
    summarise(
      avg_wape = mean(Value, na.rm = TRUE),
      avg_features = mean(n_features, na.rm = TRUE),
      avg_train_time = mean(train_time, na.rm = TRUE),
      .groups = "drop"
    ) |>
    filter(Model %in% input$models_mc) |>
    arrange(avg_wape)
  
  if (nrow(df) == 0) {
    return(NULL)
  }
  
  df |>
    mutate(
      avg_wape = round(avg_wape * 100, 1),
      avg_features = round(avg_features, 1),
      avg_train_time = round(avg_train_time, 2)
    ) |>
    rename(
      Model = Model,
      `WAPE (%)` = avg_wape,
      `# Features` = avg_features,
      `Train time (s)` = avg_train_time
    )
})

# ----- Forecast Explorer: forecast plot -----
output$forecast_plot = renderPlot({
  df_ts = ts_selected()
  
  df_fx = forecasts |>
    filter(
      Store == input$store,
      Model == input$model_fx,
      Horizon == as.numeric(input$horizon_fx)
    )
  
  if (nrow(df_fx) == 0) {
    return(NULL)
  }
  
  df_plot = df_ts |>
    left_join(
      df_fx |> select(Date, Pred, Lower, Upper),
      by = "Date"
    )
  
  gg = ggplot(df_plot, aes(x = Date)) +
    geom_line(aes(y = Weekly_Sales, color = "Actual"), linewidth = 1)
  
  if (input$show_pi) {
    gg = gg +
      geom_ribbon(
        aes(ymin = Lower, ymax = Upper, fill = "PI"),
        alpha = 0.2
      )
  }
  
  gg = gg +
    geom_line(aes(y = Pred, color = "Forecast"), linewidth = 1, linetype = "dashed") +
    scale_color_manual(
      values = c("Actual" = "#1f77b4", "Forecast" = "#d62728"),
      breaks = c("Actual", "Forecast")
    ) +
    scale_fill_manual(
      values = c("PI" = "#d62728"),
      labels = c("Prediction interval"),
      guide = if (input$show_pi) "legend" else "none"
    ) +
    scale_y_continuous(labels = dollar_format()) +
    labs(
      x = "Week",
      y = "Weekly_Sales",
      color = NULL,
      fill = NULL,
      title = glue(
        "Forecast vs Actual – Store {input$store}, {input$model_fx}, horizon {input$horizon_fx} weeks"
      )
    ) +
    theme_minimal(base_size = 14)
  
  gg
})

# ----- Feature Interpretation: feature plot -----
output$feature_plot = renderPlot({
  df = feat_imp |>
    filter(Model == input$model_feat)
  
  if (nrow(df) == 0) {
    return(NULL)
  }
  
  if (input$model_feat == "GBM") {
    df = df |>
      arrange(desc(Importance)) |>
      head(input$top_k)
    
    ggplot(df, aes(x = reorder(Feature, Importance), y = Importance)) +
      geom_col(width = 0.7) +
      coord_flip() +
      labs(
        x = NULL,
        y = "Relative importance (Gain, normalized)",
        title = glue("Top {nrow(df)} XGBoost features by importance")
      ) +
      theme_minimal(base_size = 14)
    
  } else {
    df = df |>
      filter(!is.na(Coefficient)) |>
      mutate(abs_coef = abs(Coefficient)) |>
      arrange(desc(abs_coef)) |>
      head(input$top_k)
    
    ggplot(df, aes(x = reorder(Feature, Coefficient), y = Coefficient)) +
      geom_col(
        aes(fill = Coefficient > 0),
        width = 0.7
      ) +
      scale_fill_manual(
        values = c("TRUE" = "#1f77b4", "FALSE" = "#d62728"),
        labels = c("Negative", "Positive")
      ) +
      coord_flip() +
      labs(
        x = NULL,
        y = "Coefficient",
        fill = "Sign",
        title = glue("Top {nrow(df)} Lasso coefficients")
      ) +
      theme_minimal(base_size = 14)
  }
})

# Feature text summary still commented out
# output$feature_summary_text = renderPrint({ ... })
}

# -------- 9. Run app --------
shinyApp(ui = ui, server = server)

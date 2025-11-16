library(xgboost)
library(tidymodels)
library(modeltime)
library(tidyverse)
library(timetk)

rssi_train <- sensors_split$RSSI_01$train |>
  arrange(rdtimestamp) |>
  select(
    rdtimestamp, rssi,
    soiltemp, soilhum,
    season_spring, season_summer, season_autumn
  )

rssi_test <- sensors_split$RSSI_01$test |>
  arrange(rdtimestamp) |>
  select(
    rdtimestamp, rssi,
    soiltemp, soilhum,
    season_spring, season_summer, season_autumn
  )

rssi_train  |> 
  plot_time_series(rdtimestamp, rssi, .interactive = FALSE)

# ARIMA
model_fit_arima_xreg <- arima_reg() |>
  set_engine("auto_arima") |>
  fit(
    rssi ~ rdtimestamp+soiltemp + soilhum +
      season_spring + season_summer + season_autumn,
    data = rssi_train)

rec_ml <- recipe(
  rssi ~ rdtimestamp +
    soiltemp + soilhum +
    season_spring + season_summer + season_autumn,
  data = rssi_train
) |>
  # Cria assinatura temporal (ano, mês, dia da semana, hora, etc.)
  step_timeseries_signature(rdtimestamp) |>
  # Remove colunas de data que não interessam ou que podem dar colinearidade forte
  step_rm(contains("iso"), contains("xts"), contains("minute"),
          contains("second"), contains("am.pm")) |>
  # Normaliza preditores (para XGBoost ajuda; RF não precisa, mas não atrapalha)
  step_normalize(all_numeric_predictors(), -all_outcomes())

# Random Forest
spec_rf <- rand_forest(
  mode  = "regression",
  trees = 500
) |>
  set_engine("ranger")

wflw_rf <- workflow() |>
  add_model(spec_rf) |>
  add_recipe(rec_ml) |>
  fit(data = rssi_train)

# arima xboost

model_spec_arima_boost <- arima_boost(
  # hiperparâmetros do XGBoost
  trees      = 10,
  learn_rate = 0.05,
  tree_depth = 6,
  min_n      = 10
  # (pode deixar seasonal_period = "auto" implícito)
) |>
  # usa Auto ARIMA + XGBoost Errors
  set_engine("auto_arima_xgboost")


model_fit_arima_boost <- model_spec_arima_boost |>
  fit(
    rssi ~ rdtimestamp +
      soiltemp + soilhum +
      season_spring + season_summer + season_autumn,
    data = rssi_train
  )


models_tbl <- modeltime_table(
  model_fit_arima_xreg,  
  wflw_rf,
  model_fit_arima_boost
) |>
  update_model_description(1, "ARIMA+ALL") |>
  update_model_description(2, "Random Forest ") |> 
  update_model_description(3, "ARIMA+XBoost")


calibration_tbl <- models_tbl |>
  modeltime_calibrate(new_data = rssi_test)

accuracy_tbl <- calibration_tbl |>
  modeltime_accuracy() |>
  arrange(rmse)

accuracy_tbl

forecast_tbl <- calibration_tbl |>
  modeltime_forecast(
    new_data    = rssi_test,
    actual_data = bind_rows(rssi_train, rssi_test)
  )

forecast_tbl |>
  plot_modeltime_forecast(.interactive = TRUE)



# modelos de machine learning 

library(tidymodels)
library(modeltime)
library(timetk)
library(xgboost)
library(ranger)

df_train <- tibble::tibble(
  data = rssi_train_list$Tinovi04$rdtimestamp,
  rssi = rssi_train_list$Tinovi04$rssi,
  temp = rssi_train_list$Tinovi04$airtemp,
  hum  = rssi_train_list$Tinovi04$airhum
)

df_test <- tibble::tibble(
  data = rssi_test_list$Tinovi04$rdtimestamp,
  rssi = rssi_test_list$Tinovi04$rssi,
  temp = rssi_test_list$Tinovi04$airtemp,
  hum  = rssi_test_list$Tinovi04$airhum
)



n_train <- nrow(df_train)

df_all <- bind_rows(df_train, df_test) %>%
  arrange(data) %>%
  tk_augment_lags(rssi, .lags = 1:5) %>%
  drop_na()

train_ml <- df_all %>% slice(1:(n_train - 5))
test_ml  <- df_all %>% slice((n_train - 5 + 1):n())

df_test_aligned <- test_ml %>% select(data, rssi, temp, hum)

rec <- recipe(rssi ~ ., data = train_ml) %>%
  update_role(data, new_role = "id") %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())

spec_xgb <- boost_tree(
  mode = "regression",
  trees = 500,
  tree_depth = 6,
  learn_rate = 0.05,
  min_n = 5,
  loss_reduction = 0
) %>%
  set_engine("xgboost")

wf_xgb <- workflow() %>%
  add_recipe(rec) %>%
  add_model(spec_xgb)

fit_xgb <- fit(wf_xgb, data = train_ml)

pred_xgb <- predict(fit_xgb, new_data = test_ml) %>%
  bind_cols(df_test_aligned %>% select(data, rssi)) %>%
  rename(pred = .pred)



# =========================================================
# 5) Elastic Net autoregressivo
#    mixture = 1  -> LASSO
#    mixture = 0  -> Ridge
#    mixture entre 0 e 1 -> Elastic Net
# =========================================================
spec_enet <- linear_reg(
  mode = "regression",
  penalty = 0.01,
  mixture = 0.5
) %>%
  set_engine("glmnet")

wf_enet <- workflow() %>%
  add_recipe(rec) %>%
  add_model(spec_enet)

fit_enet <- fit(wf_enet, data = train_ml)

pred_enet <- predict(fit_enet, new_data = test_ml) %>%
  bind_cols(df_test_aligned %>% select(data, rssi)) %>%
  rename(pred = .pred)

resultados_ml <- bind_rows(
  avaliar_modelo(pred_xgb$rssi,  pred_xgb$pred,  "XGBoost-AR"),
  avaliar_modelo(pred_enet$rssi, pred_enet$pred, "ElasticNet-AR")
)

resultados_ml




model_arima_xgb <- arima_boost(
  mode = "regression",
  seasonal_period = 24,
  trees = 300,
  tree_depth = 6,
  learn_rate = 0.05,
  min_n = 2,
  stop_iter = 20
) %>%
  set_engine(
    "auto_arima_xgboost",
    objective = "reg:squarederror",
    nthread = 1
  )

fit_arima_xgb <- model_arima_xgb %>%
  fit(rssi ~ data + temp + hum, data = df_train)

pred_arima_xgb <- predict(fit_arima_xgb, new_data = df_test) %>%
  bind_cols(df_test %>% select(data, rssi)) %>%
  rename(pred = .pred)

metricas_arima_xgb <- tibble(
  modelo = "ARIMA + XGBoost",
  MAE  = yardstick::mae_vec(pred_arima_xgb$rssi, pred_arima_xgb$pred),
  RMSE = yardstick::rmse_vec(pred_arima_xgb$rssi, pred_arima_xgb$pred),
  MAPE = yardstick::mape_vec(pred_arima_xgb$rssi, pred_arima_xgb$pred)
)

metricas_arima_xgb

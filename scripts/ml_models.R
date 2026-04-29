########################
## Hybrid ARIMA models ##
########################

library(tidyverse)
library(forecast)
library(randomForest)
library(e1071)
library(nnet)

# ============================
# 1. Configurações iniciais
# ============================

m <- length(rssi_train_list)
sensors <- names(rssi_train_list)

metric_cols <- c(
  "ARIMA-ANN",
  "ARIMA-RF",
  "ARIMA-SVM",
  "ARIMA-TH"
)

MAE <- MAPE <- RMSE <- COR <- matrix(NA, m, length(metric_cols))

colnames(MAE) <- colnames(MAPE) <- colnames(RMSE) <- colnames(COR) <- metric_cols
rownames(MAE) <- rownames(MAPE) <- rownames(RMSE) <- rownames(COR) <- sensors

order_arima <- matrix(NA, m, 3)
rownames(order_arima) <- sensors
colnames(order_arima) <- c("p", "d", "q")

#plots_dir <- "plots_hybrid"
#dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

n_lags <- 4

# ============================
# 2. Função para criar resíduos defasados
# ============================

create_lagged_residuals <- function(res, n_lags = 4) {
  
  res <- as.numeric(res)
  
  df <- data.frame(res = res)
  
  for (j in 1:n_lags) {
    df[[paste0("lag", j)]] <- dplyr::lag(res, j)
  }
  
  df <- na.omit(df)
  
  for (j in names(df)) {
    df[[j]] <- as.numeric(df[[j]])
  }
  
  return(df)
}

# ============================
# 3. Loop por sensor
# ============================

for (i in seq_along(sensors)) {
  
  df_tr <- rssi_train_list[[i]]
  df_te <- rssi_test_list[[i]]
  
  RSSI <- as.numeric(df_tr$rssi)
  RSSI_test <- as.numeric(df_te$rssi)
  
  # Covariáveis: temperatura + umidade
  Xth <- cbind(
    T  = as.numeric(df_tr$airtemp),
    RH = as.numeric(df_tr$airhum)
  )
  
  Xth_t <- cbind(
    T  = as.numeric(df_te$airtemp),
    RH = as.numeric(df_te$airhum)
  )
  
  # ============================
  # 3.1 ARIMAX com temperatura + umidade
  # ============================
  
  a_base <- auto.arima(
    RSSI,
    allowdrift = FALSE,
    seasonal = TRUE,
    xreg = Xth
  )
  
  ord <- arimaorder(a_base)
  order_arima[i, ] <- ord
  
  a_th <- Arima(
    RSSI,
    order = ord,
    xreg = Xth
  )
  
  # ============================
  # 3.2 Modelos híbridos nos resíduos do ARIMA-TH
  # ============================
  
  res_train <- as.numeric(residuals(a_th))
  res_train_data <- create_lagged_residuals(res_train, n_lags)
  
  x_train <- res_train_data[, paste0("lag", 1:n_lags)]
  y_train <- res_train_data$res
  
  set.seed(10)
  
  ann_model <- nnet::nnet(
    x = x_train,
    y = y_train,
    size = 5,
    linout = TRUE,
    trace = FALSE,
    maxit = 1000
  )
  
  rf_model <- randomForest::randomForest(
    x = x_train,
    y = y_train
  )
  
  svm_model <- e1071::svm(
    x = x_train,
    y = y_train
  )
  
  # ============================
  # 3.3 One-step-ahead no teste
  # ============================
  
  new_th <- Arima(
    RSSI_test,
    xreg = Xth_t,
    model = a_th
  )
  
  fitted_th <- as.numeric(new_th$fitted)
  res_test <- as.numeric(residuals(new_th))
  
  # ============================
  # 3.4 Previsão dos resíduos do ARIMA-TH
  # ============================
  
  res_test_full <- c(
    tail(res_train, n_lags),
    res_test
  )
  
  res_test_data <- create_lagged_residuals(res_test_full, n_lags)
  x_test <- res_test_data[, paste0("lag", 1:n_lags)]
  
  pred_res_ann <- as.numeric(predict(ann_model, x_test))
  pred_res_rf  <- as.numeric(predict(rf_model, x_test))
  pred_res_svm <- as.numeric(predict(svm_model, x_test))
  
  # ============================
  # 3.5 Alinhamento dos comprimentos
  # ============================
  
  n_common <- min(
    length(RSSI_test),
    length(fitted_th),
    length(pred_res_ann),
    length(pred_res_rf),
    length(pred_res_svm)
  )
  
  RSSI_test_use <- tail(RSSI_test, n_common)
  fitted_th_use <- tail(fitted_th, n_common)
  
  pred_res_ann <- tail(pred_res_ann, n_common)
  pred_res_rf  <- tail(pred_res_rf, n_common)
  pred_res_svm <- tail(pred_res_svm, n_common)
  
  # ============================
  # 3.6 Previsões híbridas
  # ============================
  
  f_ann <- fitted_th_use + pred_res_ann
  f_rf  <- fitted_th_use + pred_res_rf
  f_svm <- fitted_th_use + pred_res_svm
  
  # ============================
  # 3.7 Métricas
  # ============================
  
  MAE[i, ] <- c(
    forecast::accuracy(f_ann, RSSI_test_use)[3],
    forecast::accuracy(f_rf, RSSI_test_use)[3],
    forecast::accuracy(f_svm, RSSI_test_use)[3],
    forecast::accuracy(fitted_th_use, RSSI_test_use)[3]
  )
  
  MAPE[i, ] <- c(
    forecast::accuracy(f_ann, RSSI_test_use)[5],
    forecast::accuracy(f_rf, RSSI_test_use)[5],
    forecast::accuracy(f_svm, RSSI_test_use)[5],
    forecast::accuracy(fitted_th_use, RSSI_test_use)[5]
  )
  
  RMSE[i, ] <- c(
    forecast::accuracy(f_ann, RSSI_test_use)[2],
    forecast::accuracy(f_rf, RSSI_test_use)[2],
    forecast::accuracy(f_svm, RSSI_test_use)[2],
    forecast::accuracy(fitted_th_use, RSSI_test_use)[2]
  )
  
  COR[i, ] <- c(
    cor(RSSI_test_use, f_ann, use = "complete.obs"),
    cor(RSSI_test_use, f_rf, use = "complete.obs"),
    cor(RSSI_test_use, f_svm, use = "complete.obs"),
    cor(RSSI_test_use, fitted_th_use, use = "complete.obs")
  )
  
  # ============================
  # 3.8 Gráfico por sensor
  # ============================
  
  # timestamp_use <- tail(as.POSIXct(df_te$rdtimestamp), n_common)
  # 
  # plot_df <- data.frame(
  #   rdtimestamp = timestamp_use,
  #   Observed = RSSI_test_use,
  #   `ARIMA-ANN` = f_ann,
  #   `ARIMA-RF` = f_rf,
  #   `ARIMA-SVM` = f_svm,
  #   `ARIMA-TH` = fitted_th_use
  # )
  # 
  # plot_long <- plot_df %>%
  #   pivot_longer(
  #     cols = -rdtimestamp,
  #     names_to = "models",
  #     values_to = "value"
  #   )
  # 
  # p <- ggplot(plot_long, aes(x = rdtimestamp, y = value, color = models)) +
  #   geom_line(na.rm = TRUE) +
  #   labs(
  #     title = paste("Sensor:", sensors[i], "- Hybrid models out-of-sample"),
  #     x = NULL,
  #     y = "RSSI"
  #   ) +
  #   theme_bw()
  # 
  # safe_name <- gsub("[^A-Za-z0-9_-]", "_", sensors[i])
  # 
  # ggsave(
  #   filename = file.path(plots_dir, paste0("hybrid_overlay_", safe_name, ".png")),
  #   plot = p,
  #   width = 12,
  #   height = 5,
  #   dpi = 150
  # )
  # 
  assign(
    paste0("result0", i),
    t(data.frame(
      MAE = MAE[i, ],
      MAPE = MAPE[i, ],
      RMSE = RMSE[i, ],
      COR = COR[i, ]
    ))
  )
}

# ============================
# 4. Resultados principais
# ============================

print(order_arima)

print(round(MAE, 5))
print(round(MAPE, 5))
print(round(RMSE, 5))
print(round(COR, 5))

# ============================
# 5. Diferenças percentuais em relação ao ARIMA-TH
# ============================

MAE_AUM <- (
  MAE[, "ARIMA-TH"] -
    MAE[, c("ARIMA-ANN", "ARIMA-RF", "ARIMA-SVM")]
) / MAE[, "ARIMA-TH"] * 100

M_AUM <- (
  MAPE[, "ARIMA-TH"] -
    MAPE[, c("ARIMA-ANN", "ARIMA-RF", "ARIMA-SVM")]
) / MAPE[, "ARIMA-TH"] * 100

RMSE_AUM <- (
  RMSE[, "ARIMA-TH"] -
    RMSE[, c("ARIMA-ANN", "ARIMA-RF", "ARIMA-SVM")]
) / RMSE[, "ARIMA-TH"] * 100

COR_AUM <- (
  COR[, c("ARIMA-ANN", "ARIMA-RF", "ARIMA-SVM")] -
    COR[, "ARIMA-TH"]
) / COR[, "ARIMA-TH"] * 100

print(round(MAE_AUM, 5))
print(round(M_AUM, 5))
print(round(RMSE_AUM, 5))
print(round(COR_AUM, 5))

# ============================
# 6. Dados organizados para gráfico
# ============================

MAE_all <- data.frame(
  values = c(
    MAE_AUM[, 1], M_AUM[, 1], RMSE_AUM[, 1], COR_AUM[, 1],
    MAE_AUM[, 2], M_AUM[, 2], RMSE_AUM[, 2], COR_AUM[, 2],
    MAE_AUM[, 3], M_AUM[, 3], RMSE_AUM[, 3], COR_AUM[, 3]
  ),
  measure = rep(
    c(
      rep("MAE", m),
      rep("MAPE", m),
      rep("RMSE", m),
      rep("COR", m)
    ),
    3
  ),
  sensor = rep(sensors, 12),
  model = rep(
    c("ARIMA-ANN", "ARIMA-RF", "ARIMA-SVM"),
    each = 4 * m
  )
)

MAE_all <- MAE_all %>%
  mutate(
    plot_text = case_when(
      values < 0 ~ -0.2,
      values >= 0 ~ values + 0.2
    )
  )

print(MAE_all)



# ============================
# 7. Counting the times the models were the best option
# ============================

count <- apply(
  cbind(
    apply(result01[1:3, ], 1, rank) == 1,
    COR = rank(result01[4, ]) == ncol(result01)
  ),
  1,
  sum
)

for (i in 2:m) {
  
  r <- get(paste0("result0", i))
  
  r <- apply(
    cbind(
      apply(r[1:3, ], 1, rank) == 1,
      COR = rank(r[4, ]) == ncol(r)
    ),
    1,
    sum
  )
  
  count <- abind::abind(count, r, along = 2)
}

count <- abind::abind(
  count,
  apply(count, 1, sum),
  along = 2
)

colnames(count) <- c(rownames(MAPE), "Overall")

print(count)

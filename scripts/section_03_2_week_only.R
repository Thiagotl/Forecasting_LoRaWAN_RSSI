# Script sem modelagem com dummies
m <- length(rssi_train_list_week)
sensors <- names(rssi_train_list_week)

order_arima <- matrix(NA, m, 3)

# Removido ARIMA-DUM das colunas de métricas
metric_cols <- c("ARIMA-TH","ARIMA-Temp","ARIMA-H","ARIMA")
MAE <- MAPE <- RMSE <- COR <- matrix(NA, m, length(metric_cols))
colnames(MAE) <- colnames(MAPE) <- colnames(RMSE) <- colnames(COR) <- metric_cols

rownames(order_arima) <- rownames(MAE) <- rownames(MAPE) <- rownames(RMSE) <- rownames(COR) <- sensors

# Cov_names sem as dummies
cov_names <- c("T","RH")
p_cov <- length(cov_names)

Xsig <- matrix("", m, p_cov)
rownames(Xsig) <- sensors
colnames(Xsig) <- cov_names

for (i in seq_along(sensors)) {
  
  df_tr <- rssi_train_list_week[[i]]
  df_te <- rssi_test_list_week[[i]]
  
  RSSI <- df_tr$rssi
  RSSI_test <- df_te$rssi
  
  # Matrizes apenas com T e RH (sem dummies)
  X <- cbind(
    df_tr$airtemp,
    df_tr$airhum
  )
  
  Xtest <- cbind(
    df_te$airtemp,
    df_te$airhum
  )
  
  colnames(X) <- colnames(Xtest) <- cov_names
  
  Xth   <- X[, 1:2, drop = FALSE]
  Xth_t <- Xtest[, 1:2, drop = FALSE]
  
  Xtemp   <- X[, 1, drop = FALSE]
  Xtemp_t <- Xtest[, 1, drop = FALSE]
  
  Xhum   <- X[, 2, drop = FALSE]
  Xhum_t <- Xtest[, 2, drop = FALSE]
  
  # Modelo principal com T e RH
  a01 <- auto.arima(RSSI, xreg = Xth, allowdrift = FALSE)
  ord <- arimaorder(a01)
  order_arima[i, ] <- ord
  
  # Testes de significância
  ct <- lmtest::coeftest(a01)
  pvals <- ct[, 4]
  names(pvals) <- rownames(ct)
  
  bhat <- stats::coef(a01)
  
  pick_name <- function(v) {
    if (v %in% names(bhat)) return(v)
    vx <- paste0("xreg", v)
    if (vx %in% names(bhat)) return(vx)
    return(NA_character_)
  }
  
  for (v in c("T", "RH")) {
    nm <- pick_name(v)
    if (!is.na(nm) && !is.na(pvals[nm]) && pvals[nm] < 0.05) {
      dir <- ifelse(bhat[nm] >= 0, "Positive", "Negative")
      Xsig[i, v] <- paste0(dir)
    } else {
      Xsig[i, v] <- ""
    }
  }
  
  # Modelos alternativos (sem dummies)
  a02 <- Arima(RSSI, order = ord, xreg = Xtemp)
  a03 <- Arima(RSSI, order = ord, xreg = Xhum)
  a04 <- Arima(RSSI, order = ord)
  
  # Previsões (sem o modelo com dummies)
  new1 <- Arima(RSSI_test, xreg = Xth_t, model = a01)
  new2 <- Arima(RSSI_test, xreg = Xtemp_t, model = a02)
  new3 <- Arima(RSSI_test, xreg = Xhum_t, model = a03)
  new4 <- Arima(RSSI_test, model = a04)
  
  # Métricas (apenas 4 modelos)
  MAPE[i, ] <- c(
    forecast::accuracy(new1$fitted, RSSI_test)[5],
    forecast::accuracy(new2$fitted, RSSI_test)[5],
    forecast::accuracy(new3$fitted, RSSI_test)[5],
    forecast::accuracy(new4$fitted, RSSI_test)[5]
  )
  
  RMSE[i, ] <- c(
    forecast::accuracy(new1$fitted, RSSI_test)[2],
    forecast::accuracy(new2$fitted, RSSI_test)[2],
    forecast::accuracy(new3$fitted, RSSI_test)[2],
    forecast::accuracy(new4$fitted, RSSI_test)[2]
  )
  
  MAE[i, ] <- c(
    forecast::accuracy(new1$fitted, RSSI_test)[3],
    forecast::accuracy(new2$fitted, RSSI_test)[3],
    forecast::accuracy(new3$fitted, RSSI_test)[3],
    forecast::accuracy(new4$fitted, RSSI_test)[3]
  )
  
  COR[i, ] <- c(
    cor(RSSI_test, new1$fitted, use = "complete.obs"),
    cor(RSSI_test, new2$fitted, use = "complete.obs"),
    cor(RSSI_test, new3$fitted, use = "complete.obs"),
    cor(RSSI_test, new4$fitted, use = "complete.obs")
  )
  
  # Objetos xts (apenas 4)
  RSSI_test_xts <- xts::xts(RSSI_test, order.by = df_te$rdtimestamp)
  new1fit <- xts::xts(new1$fitted, order.by = df_te$rdtimestamp)
  new2fit <- xts::xts(new2$fitted, order.by = df_te$rdtimestamp)
  new3fit <- xts::xts(new3$fitted, order.by = df_te$rdtimestamp)
  new4fit <- xts::xts(new4$fitted, order.by = df_te$rdtimestamp)
  
  assign(
    paste0("result0", i),
    t(data.frame(
      MAE  = MAE[i, ],
      MAPE = MAPE[i, ],
      RMSE = RMSE[i, ],
      COR  = COR[i, ]
    ))
  )
}

print(cbind(order_arima, Xsig))

# Cálculo das melhorias (AUM) - ajustado para 4 modelos
base_col <- which(metric_cols == "ARIMA")
comp_cols <- which(metric_cols %in% c("ARIMA-TH","ARIMA-Temp","ARIMA-H"))

MAE_AUM  <- (MAE[, base_col]  - MAE[, comp_cols, drop = FALSE])  / MAE[, base_col]
M_AUM    <- (MAPE[, base_col] - MAPE[, comp_cols, drop = FALSE]) / MAPE[, base_col]
RMSE_AUM <- (RMSE[, base_col] - RMSE[, comp_cols, drop = FALSE]) / RMSE[, base_col]
COR_AUM  <- (COR[, comp_cols, drop = FALSE] - COR[, base_col])   / COR[, base_col]

# Combinar resultados
result <- cbind(get("result01"), rbind(
  MAE_AUM[1, ], M_AUM[1, ], RMSE_AUM[1, ], COR_AUM[1, ]
) * 100)

for (i in 2:m) {
  r <- cbind(get(paste0("result0", i)), rbind(
    MAE_AUM[i, ], M_AUM[i, ], RMSE_AUM[i, ], COR_AUM[i, ]
  ) * 100)
  result <- abind::abind(result, r, along = 1)
}

# Nomes das colunas de melhoria
aum_cols <- paste0(metric_cols[comp_cols], "_AUM")
colnames(result) <- c(metric_cols, aum_cols)

measures <- rownames(get("result01"))
rownames(result) <- paste(
  rep(sensors[1:m], each = length(measures)),
  rep(measures, times = m),
  sep = " | "
)

print(result, digits = 5)


## lag models

m <- length(rssi_train_list_day)
sensors <- names(rssi_train_list_day)

order_arima <- matrix(NA, m, 3)

metric_cols <- c("ARIMA-TH","ARIMA-Temp","ARIMA-H","ARIMA","ARIMA-DUM")
MAE <- MAPE <- RMSE <- COR <- matrix(NA, m, length(metric_cols))
colnames(MAE) <- colnames(MAPE) <- colnames(RMSE) <- colnames(COR) <- metric_cols

rownames(order_arima) <- rownames(MAE) <- rownames(MAPE) <-
  rownames(RMSE) <- rownames(COR) <- sensors


cov_names <- c("T","RH","dum_Summer","dum_Autumn","dum_Winter")
p_cov <- length(cov_names)

Xsig <- matrix("", m, p_cov)
rownames(Xsig) <- sensors
colnames(Xsig) <- cov_names

plots_dir2 <- "plots_hour_test2"
dir.create(plots_dir2, showWarnings = FALSE, recursive = TRUE)

for (i in seq_along(sensors)) {
  cat("\nProcessando sensor:", sensors[i], "\n")
  df_tr <- rssi_train_list_day[[i]]
  df_te <- rssi_test_list_day[[i]]
  cat("  Dados de treino - RSSI:",
      "NAs:", sum(is.na(df_tr$rssi)),
      "Infinitos:", sum(is.infinite(df_tr$rssi)),
      "Comprimento:", length(df_tr$rssi), "\n")
  # Check for variation in the training RSSI
  if (sd(df_tr$rssi, na.rm = TRUE) == 0) {
    cat("  AVISO: RSSI constante nos dados de treino!\n")
  }
  RSSI <- df_tr$rssi
  RSSI_test <- df_te$rssi
  
  ## ------------------------------------------------------------------------
  ## Construct lagged covariates

  temp_combined <- c(df_tr$airtemp, df_te$airtemp)
  hum_combined  <- c(df_tr$airhum,  df_te$airhum)
  # Create lagged series: shift by one with the first element repeated
  temp_lag_combined <- c(temp_combined[1], head(temp_combined, -1))
  hum_lag_combined  <- c(hum_combined[1],  head(hum_combined,  -1))
  # Partition lagged covariates back into training and test sets
  n_tr <- nrow(df_tr)
  n_te <- nrow(df_te)
  temp_lag_tr <- temp_lag_combined[1:n_tr]
  hum_lag_tr  <- hum_lag_combined[1:n_tr]
  temp_lag_te <- temp_lag_combined[(n_tr + 1):(n_tr + n_te)]
  hum_lag_te  <- hum_lag_combined[(n_tr + 1):(n_tr + n_te)]
  ## ------------------------------------------------------------------------
  
  X <- cbind(
    temp_lag_tr,
    hum_lag_tr,
    df_tr$dum_Summer,
    df_tr$dum_Autumn,
    df_tr$dum_Winter
  )
  Xtest <- cbind(
    temp_lag_te,
    hum_lag_te,
    df_te$dum_Summer,
    df_te$dum_Autumn,
    df_te$dum_Winter
  )
  colnames(X) <- colnames(Xtest) <- cov_names
  
  ## Subsets for different model configurations
  Xth   <- X[, 1:2, drop = FALSE]   # both lagged temperature and humidity
  Xth_t <- Xtest[, 1:2, drop = FALSE]
  Xtemp   <- X[, 1, drop = FALSE]   # only lagged temperature
  Xtemp_t <- Xtest[, 1, drop = FALSE]
  Xhum    <- X[, 2, drop = FALSE]   # only lagged humidity
  Xhum_t  <- Xtest[, 2, drop = FALSE]
  Xdum    <- X[, 3:5, drop = FALSE] # seasonal dummies (no lag)
  Xdum_t  <- Xtest[, 3:5, drop = FALSE]
  

  a01 <- auto.arima(RSSI, xreg = Xth, allowdrift = FALSE)
  ord <- arimaorder(a01)
  order_arima[i, ] <- ord
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

  Xsig[i, c("dum_Summer","dum_Autumn","dum_Winter")] <- ""
 
  
  a02 <- Arima(RSSI, order = ord, xreg = Xtemp)
  a03 <- Arima(RSSI, order = ord, xreg = Xhum)
  a04 <- Arima(RSSI, order = ord)
  a05 <- Arima(RSSI, order = ord, xreg = Xdum)
  
  
  new1 <- Arima(RSSI_test, xreg = Xth_t, model = a01)
  new2 <- Arima(RSSI_test, xreg = Xtemp_t, model = a02)
  new3 <- Arima(RSSI_test, xreg = Xhum_t, model = a03)
  new4 <- Arima(RSSI_test, model = a04)
  new5 <- Arima(RSSI_test, xreg = Xdum_t, model = a05)
  ## Accuracy metrics: MAPE, RMSE, MAE, COR
  MAPE[i, ] <- c(
    forecast::accuracy(new1$fitted, RSSI_test)[5],
    forecast::accuracy(new2$fitted, RSSI_test)[5],
    forecast::accuracy(new3$fitted, RSSI_test)[5],
    forecast::accuracy(new4$fitted, RSSI_test)[5],
    forecast::accuracy(new5$fitted, RSSI_test)[5]
  )
  RMSE[i, ] <- c(
    forecast::accuracy(new1$fitted, RSSI_test)[2],
    forecast::accuracy(new2$fitted, RSSI_test)[2],
    forecast::accuracy(new3$fitted, RSSI_test)[2],
    forecast::accuracy(new4$fitted, RSSI_test)[2],
    forecast::accuracy(new5$fitted, RSSI_test)[2]
  )
  MAE[i, ] <- c(
    forecast::accuracy(new1$fitted, RSSI_test)[3],
    forecast::accuracy(new2$fitted, RSSI_test)[3],
    forecast::accuracy(new3$fitted, RSSI_test)[3],
    forecast::accuracy(new4$fitted, RSSI_test)[3],
    forecast::accuracy(new5$fitted, RSSI_test)[3]
  )
  COR[i, ] <- c(
    cor(RSSI_test, new1$fitted, use = "complete.obs"),
    cor(RSSI_test, new2$fitted, use = "complete.obs"),
    cor(RSSI_test, new3$fitted, use = "complete.obs"),
    cor(RSSI_test, new4$fitted, use = "complete.obs"),
    cor(RSSI_test, new5$fitted, use = "complete.obs")
  )
  
  RSSI_test_xts <- xts::xts(RSSI_test, order.by = df_te$rdtimestamp)
  new1fit <- xts::xts(new1$fitted, order.by = df_te$rdtimestamp)
  new2fit <- xts::xts(new2$fitted, order.by = df_te$rdtimestamp)
  new3fit <- xts::xts(new3$fitted, order.by = df_te$rdtimestamp)
  new4fit <- xts::xts(new4$fitted, order.by = df_te$rdtimestamp)
  new5fit <- xts::xts(new5$fitted, order.by = df_te$rdtimestamp)
  
  plot_df <- data.frame(
    rdtimestamp = as.POSIXct(df_te$rdtimestamp),
    Observed    = as.numeric(RSSI_test),
    `ARIMA-TH`   = as.numeric(new1$fitted),
    `ARIMA-Temp` = as.numeric(new2$fitted),
    `ARIMA-H`    = as.numeric(new3$fitted),
    `ARIMA`      = as.numeric(new4$fitted),
    `ARIMA-DUM`  = as.numeric(new5$fitted)
  )
  plot_long <- plot_df %>%
    pivot_longer(
      cols = -rdtimestamp,
      names_to = "series",
      values_to = "value"
    )
  p <- ggplot(plot_long, aes(x = rdtimestamp, y = value, color = series)) +
    geom_line(na.rm = TRUE) +
    labs(
      title = paste("Sensor:", sensors[i], "- Teste (Observado vs Modelos)"),
      x = NULL, y = "RSSI"
    ) +
    theme_bw()
  safe_name <- gsub("[^A-Za-z0-9_-]", "_", sensors[i])
  ggsave(
    filename = file.path(plots_dir2, paste0("test_overlay_", safe_name, ".png")),
    plot = p, width = 12, height = 5, dpi = 150
  )
  
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

base_col <- which(metric_cols == "ARIMA")
comp_cols <- which(metric_cols %in% c("ARIMA-TH","ARIMA-Temp","ARIMA-H","ARIMA-DUM"))
MAE_AUM  <- (MAE[, base_col]  - MAE[, comp_cols, drop = FALSE])  / MAE[, base_col]
M_AUM    <- (MAPE[, base_col] - MAPE[, comp_cols, drop = FALSE]) / MAPE[, base_col]
RMSE_AUM <- (RMSE[, base_col] - RMSE[, comp_cols, drop = FALSE]) / RMSE[, base_col]
COR_AUM  <- (COR[, comp_cols, drop = FALSE] - COR[, base_col])   / COR[, base_col]
result <- cbind(get("result01"), rbind(
  MAE_AUM[1, ], M_AUM[1, ], RMSE_AUM[1, ], COR_AUM[1, ]
) * 100)
for (i in 2:m) {
  r <- cbind(get(paste0("result0", i)), rbind(
    MAE_AUM[i, ], M_AUM[i, ], RMSE_AUM[i, ], COR_AUM[i, ]
  ) * 100)
  result <- abind::abind(result, r, along = 1)
}
aum_cols <- paste0(metric_cols[comp_cols], "_AUM")
colnames(result) <- c(metric_cols, aum_cols)
measures <- rownames(get("result01"))
rownames(result) <- paste(
  rep(sensors[1:m], each = length(measures)),
  rep(measures, times = m),
  sep = " | "
)
print(result, digits = 5)

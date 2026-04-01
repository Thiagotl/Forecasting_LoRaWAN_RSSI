# teste de sazonalidade 

# sensors
sensors_train <- readr::read_delim("train_radio_values.csv", 
                                   delim = ",", 
                                   escape_double = FALSE, 
                                   trim_ws = TRUE) |> 
  dplyr::mutate(rdtimestamp= as.POSIXct(rdtimestamp, tz = "GMT", origin="1970-01-01 00:00:00"))

# hour
sensors_hour_train <- sensors_train |> 
  group_by( nodeid, rdtimestamp = floor_date(rdtimestamp, "hour") ) |> 
  summarise( rssi = mean(rssi, na.rm = TRUE), 
             snr = mean(snr, na.rm = TRUE), 
             .groups = 'drop' ) |> select(-snr)


new_sensors <- sensors_hour_train |> 
  dplyr::filter(
    rdtimestamp >= as.POSIXct("2024-11-16 00:00:00"),
    rdtimestamp <= as.POSIXct("2025-02-02 00:00:00")
  )



env_train <- read_delim(
  "train_env_values.csv",
  delim = ",",
  escape_double = FALSE,
  trim_ws = TRUE
) |> 
  dplyr::mutate(rdtimestamp= as.POSIXct(rdtimestamp, tz = "GMT", origin="1970-01-01 00:00:00")) 
  
env_hour_train <- env_train |>
  group_by(rdtimestamp = floor_date(rdtimestamp, "hour")) |>
  summarise(
    airtemp  = mean(airtemp, na.rm = TRUE),
    airhum   = mean(airhum,  na.rm = TRUE)
  )



## Selecionar o mesmo periodo de 10 de novembro 24 a 5 de março 25---

new_env <- env_hour_train |> 
  dplyr::filter(
    rdtimestamp >= as.POSIXct("2024-11-16 00:00:00"),
    rdtimestamp <= as.POSIXct("2025-02-02 00:00:00")
    )


# tinovi - soil
tinovi01_RSSI <- new_sensors |> 
  dplyr::filter(nodeid == "tinovi-01") 

tinovi02_RSSI <- new_sensors |> 
  dplyr::filter(nodeid == "tinovi-02") 

tinovi03_RSSI <- new_sensors |> 
  dplyr::filter(nodeid == "tinovi-03") 

tinovi04_RSSI <- new_sensors |>
  dplyr::filter(nodeid == "tinovi-04") 

tinovi05_RSSI <- new_sensors |> 
  dplyr::filter(nodeid == "tinovi-05") 

tinovi06_RSSI <- new_sensors |> 
  dplyr::filter(nodeid == "tinovi-06") 

# milesight - air

milesight01_RSSI <- new_sensors |> 
  dplyr::filter(nodeid == "milesight-01") 

milesight02_RSSI <- new_sensors |> 
  dplyr::filter(nodeid == "milesight-02") 



tinovi01_RSSI <- inner_join(tinovi01_RSSI, new_env, by = "rdtimestamp") 
tinovi02_RSSI <- inner_join(tinovi02_RSSI, new_env, by = "rdtimestamp") 
tinovi03_RSSI <- inner_join(tinovi03_RSSI, new_env, by = "rdtimestamp") 
tinovi04_RSSI <- inner_join(tinovi04_RSSI, new_env, by = "rdtimestamp") 
tinovi05_RSSI <- inner_join(tinovi05_RSSI, new_env, by = "rdtimestamp") 
tinovi06_RSSI <- inner_join(tinovi06_RSSI, new_env, by = "rdtimestamp") 
milesight01_RSSI <- inner_join(milesight01_RSSI, new_env, by = "rdtimestamp")
milesight02_RSSI <- inner_join(milesight02_RSSI, new_env, by = "rdtimestamp")


rssi_list <- list(
  Tinovi01    = tinovi01_RSSI,
  Tinovi02    = tinovi02_RSSI,
  Tinovi03    = tinovi03_RSSI,
  Tinovi04    = tinovi04_RSSI,
  Tinovi05    = tinovi05_RSSI,
  Tinovi06    = tinovi06_RSSI,
  Milesight01 = milesight01_RSSI,
  Milesight02 = milesight02_RSSI
)



library(purrr)



split_train_test <- function(df, prop = .8){
  
  n_train <- floor(nrow(df) * prop)
  
  list(
    train = df[1:n_train, ],
    test  = df[(n_train + 1):nrow(df), ]
  )
  
}

splits <- map(rssi_list, split_train_test, prop = 0.8)

rssi_train_list <- map(splits, "train")
rssi_test_list  <- map(splits, "test")



####################
## Fitting ARIMAX ##
####################

order_arima <- matrix(NA, 8, 3)

MAE  <- MAPE <- RMSE <- COR <- matrix(NA, 8, 4)
colnames(MAE) <- colnames(MAPE) <- colnames(RMSE) <- colnames(COR) <-
  c("ARIMAX(T+RH)", "ARIMAX(T)", "ARIMAX(RH)", "ARIMA")
rownames(MAE) <- rownames(MAPE) <- rownames(RMSE) <- rownames(COR) <-
  rownames(order_arima) <- names(rssi_list)

Xsig   <- matrix("",    8, 2)
sinal  <- matrix("",    8, 2)
values <- matrix(NA,    8, 2)
rownames(Xsig) <- rownames(sinal) <- rownames(values) <- names(rssi_list)
colnames(Xsig) <- colnames(sinal) <- colnames(values) <- c("T", "RH")

for (i in 1:8) {
  
  df_train <- rssi_train_list[[i]]
  df_test  <- rssi_test_list[[i]]
  
  RSSI      <- df_train$rssi
  RSSI_test <- df_test$rssi
  
  # Covariáveis: T+RH, só T, só RH
  X_both  <- cbind(df_train$airtemp, df_train$airhum)
  X_T     <- df_train$airtemp
  X_RH    <- df_train$airhum
  
  Xtest_both <- cbind(df_test$airtemp, df_test$airhum)
  Xtest_T    <- df_test$airtemp
  Xtest_RH   <- df_test$airhum
  
  # ── 1. auto.arima com T+RH para definir a ordem ──────────────────────────
  a_auto <- auto.arima(RSSI, xreg = X_both, allowdrift = FALSE)
  ord    <- arimaorder(a_auto)
  order_arima[i, ] <- ord
  
  # Significância dos coeficientes de xreg
  idx_xreg <- (length(a_auto$coef) - 1):length(a_auto$coef)   # últimos 2
  tcoef    <- coeftest(a_auto)[idx_xreg, 4] < 0.05
  Xsig[i, ]  <- ifelse(tcoef, c("T", "RH"), "")
  sinal[i, ] <- ifelse(tcoef,
                       ifelse(coef(a_auto)[idx_xreg] < 0, "negative", "positive"),
                       "")
  values[i, ] <- coef(a_auto)[idx_xreg]
  
  # ── 2. Ajuste dos 4 modelos com a mesma ordem ─────────────────────────────
  # Modelo 1 – ARIMAX(T+RH)
  m1 <- Arima(RSSI, order = ord, xreg = X_both)
  
  # Modelo 2 – ARIMAX(T)
  m2 <- Arima(RSSI, order = ord, xreg = X_T)
  
  # Modelo 3 – ARIMAX(RH)
  m3 <- Arima(RSSI, order = ord, xreg = X_RH)
  
  # Modelo 4 – ARIMA puro
  m4 <- Arima(RSSI, order = ord)
  
  # ── 3. One-step-ahead no conjunto de teste ────────────────────────────────
  new1 <- Arima(RSSI_test, xreg = Xtest_both, model = m1)
  new2 <- Arima(RSSI_test, xreg = Xtest_T,    model = m2)
  new3 <- Arima(RSSI_test, xreg = Xtest_RH,   model = m3)
  new4 <- Arima(RSSI_test,                     model = m4)
  
  # ── 4. Métricas ───────────────────────────────────────────────────────────
  acc <- function(new) forecast::accuracy(new$fitted, RSSI_test)[1, ]
  
  MAPE[i, ] <- c(acc(new1)["MAPE"], acc(new2)["MAPE"],
                 acc(new3)["MAPE"], acc(new4)["MAPE"])
  RMSE[i, ] <- c(acc(new1)["RMSE"], acc(new2)["RMSE"],
                 acc(new3)["RMSE"], acc(new4)["RMSE"])
  MAE[i, ]  <- c(acc(new1)["MAE"],  acc(new2)["MAE"],
                 acc(new3)["MAE"],  acc(new4)["MAE"])
  COR[i, ]  <- c(cor(RSSI_test, new1$fitted), cor(RSSI_test, new2$fitted),
                 cor(RSSI_test, new3$fitted), cor(RSSI_test, new4$fitted))
  
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

# ── Resumo ────────────────────────────────────────────────────────────────────
colnames(sinal) <- c("AirTemp", "AirHum")
print(cbind(order_arima, sinal))

############################
## Percentage differences ##
############################

MAE_AUM  <- (MAE[, 4]  - MAE[, 1:3])  / MAE[, 4]
MAPE_AUM <- (MAPE[, 4] - MAPE[, 1:3]) / MAPE[, 4]
RMSE_AUM <- (RMSE[, 4] - RMSE[, 1:3]) / RMSE[, 4]
COR_AUM  <- (COR[, 1:3] - COR[, 4])   / COR[, 4]

########################
## Organizing TABLE V ##
########################

result <- cbind(
  result01,
  rbind(
    MAE_AUM[1, ],
    MAPE_AUM[1, ],
    RMSE_AUM[1, ],
    COR_AUM[1, ]
  ) * 100
)

for(i in 2:8){
  r <- cbind(
    get(paste0("result0", i)),
    rbind(
      MAE_AUM[i, ],
      MAPE_AUM[i, ],
      RMSE_AUM[i, ],
      COR_AUM[i, ]
    ) * 100
  )
  
  result <- abind::abind(result, r, along = 1)
}

colnames(result) <- c(
  "ARIMA-(T+RH)", "ARIMA-T", "ARIMA-RH", "ARIMA",
  "Diff.% (T+H)", "Diff.% T", "Diff.%"
)

print(result, digits = 5)  # TABLE V

result_df <- data.frame(
  
)

########################
## Organizing TABLE VI ##
########################

count <- apply(
  cbind(
    apply(result01[1:3, ], 1, rank) == 1,
    COR = rank(result01[4, ]) == 4
  ),
  1,
  sum
)

for(i in 2:8){
  r <- get(paste0("result0", i))
  
  r <- apply(
    cbind(
      apply(r[1:3, ], 1, rank) == 1,
      COR = rank(r[4, ]) == 4
    ),
    1,
    sum
  )
  
  count <- abind::abind(count, r, along = 2)
}

count <- abind::abind(count, apply(count, 1, sum), along = 2)

colnames(count) <- c(rownames(MAE), "Overall")
rownames(count) <- colnames(MAE)

print(t(count))  # TABLE VI


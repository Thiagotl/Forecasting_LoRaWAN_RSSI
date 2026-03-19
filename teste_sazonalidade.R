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
    rdtimestamp >= as.POSIXct("2024-11-10 00:00:00"),
    rdtimestamp <= as.POSIXct("2025-03-05 23:59:59")
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
    rdtimestamp >= as.POSIXct("2024-11-10 00:00:00"),
    rdtimestamp <= as.POSIXct("2025-03-05 23:59:59")
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
  c("ARIMA-COV", "ARIMA-COV*", "ARIMA-COV**", "ARIMA")

rownames(MAE) <- rownames(MAPE) <- rownames(RMSE) <- rownames(COR) <-
  rownames(order_arima) <- names(rssi_list)

Xsig   <- matrix("",    8, 2)
sinal  <- matrix(FALSE, 8, 2)
values <- matrix(NA,    8, 2)

rownames(Xsig) <- rownames(sinal) <- rownames(values) <- names(rssi_list)
colnames(Xsig) <- colnames(sinal) <- colnames(values) <- c("T", "RH")

coeffs <- matrix(NA, 8, 4)

for(i in 1:8){
  
  df_train <- rssi_train_list[[i]]
  df_test  <- rssi_test_list[[i]]
  
  RSSI <- df_train$rssi
  RSSI_test <- df_test$rssi
  
  X <- cbind(df_train$airtemp, df_train$airhum)
  Xtest <- cbind(df_test$airtemp, df_test$airhum)
  
  Xchoosed  <- X[, 1, drop = FALSE]      
  Xchoosedt <- Xtest[, 1, drop = FALSE]
  
  # fitting the algorithms
  a01 <- auto.arima(RSSI, xreg = X, allowdrift = FALSE)
  
  idx_xreg <- (length(a01$coef) - ncol(X) + 1):length(a01$coef)
  
  tcoef <- coeftest(a01)[idx_xreg, 4] < 0.05
  
  order_arima[i, ] <- arimaorder(a01)
  
  #Xsig[i, ]   <- c(c("T", "RH")[tcoef], rep("", 2 - sum(tcoef)))
  Xsig[i, ] <- ifelse(tcoef, c("T", "RH"), "")
  #sinal[i, ]  <- (coef(a01) < 0)[idx_xreg]
  
  sinal[i, ] <- ifelse(
    tcoef,
    ifelse(coef(a01)[idx_xreg] < 0, "negative", "positive"),
    ""
  )
  
  values[i, ] <- coef(a01)[idx_xreg]
  
  # modelo com covariáveis significativas
  if(sum(tcoef) > 0){
    Xnew  <- X[, tcoef, drop = FALSE]
    Xnewt <- Xtest[, tcoef, drop = FALSE]
    a02   <- Arima(RSSI, order = arimaorder(a01), xreg = Xnew)
    new2  <- Arima(RSSI_test, xreg = Xnewt, model = a02)
  } else {
    a02  <- Arima(RSSI, order = arimaorder(a01))
    new2 <- Arima(RSSI_test, model = a02)
  }
  
  # modelo sem covariáveis
  a03 <- Arima(RSSI, order = arimaorder(a01))
  
  # modelo com 1 covariável (airtemp)
  a04 <- Arima(RSSI, order = arimaorder(a01), xreg = Xchoosed)
  
  # forecasting / one-step-ahead
  new1 <- Arima(RSSI_test, xreg = Xtest, model = a01)
  new3 <- Arima(RSSI_test, model = a03)
  new4 <- Arima(RSSI_test, xreg = Xchoosedt, model = a04)
  
  MAPE[i, ] <- c(
    forecast::accuracy(new1$fitted, RSSI_test)[1, "MAPE"],
    forecast::accuracy(new2$fitted, RSSI_test)[1, "MAPE"],
    forecast::accuracy(new4$fitted, RSSI_test)[1, "MAPE"],
    forecast::accuracy(new3$fitted, RSSI_test)[1, "MAPE"]
  )
  
  RMSE[i, ] <- c(
    forecast::accuracy(new1$fitted, RSSI_test)[1, "RMSE"],
    forecast::accuracy(new2$fitted, RSSI_test)[1, "RMSE"],
    forecast::accuracy(new4$fitted, RSSI_test)[1, "RMSE"],
    forecast::accuracy(new3$fitted, RSSI_test)[1, "RMSE"]
  )
  
  MAE[i, ] <- c(
    forecast::accuracy(new1$fitted, RSSI_test)[1, "MAE"],
    forecast::accuracy(new2$fitted, RSSI_test)[1, "MAE"],
    forecast::accuracy(new4$fitted, RSSI_test)[1, "MAE"],
    forecast::accuracy(new3$fitted, RSSI_test)[1, "MAE"]
  )
  
  COR[i,]<-c(cor(RSSI_test,new1$fitted),
             cor(RSSI_test,new2$fitted),
             cor(RSSI_test,new4$fitted),
             cor(RSSI_test,new3$fitted)
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
  "ARIMA-COV", "ARIMA-COV*", "ARIMA-COV**", "ARIMA",
  "Diff.% COV", "Diff.% COV*", "Diff.% COV**"
)

print(result, digits = 6)  # TABLE V

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


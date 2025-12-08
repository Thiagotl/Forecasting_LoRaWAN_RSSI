# DATA SET ADJUSTMENT AND DESCRIPTIVE ANALYSIS

library(tidyverse)
library(xts)
library(lmtest)
library(forecast)
library(stats)
library(knitr)
library(kableExtra)
library(lubridate)



### Train data set - Nodes and Environment ---- 

# sensors
sensors_train <- readr::read_delim("train_radio_values.csv", 
                             delim = ",", escape_double = FALSE, trim_ws = TRUE) |> 
  dplyr::mutate(rdtimestamp= as.POSIXct(rdtimestamp, tz = "GMT",
                                      origin="1970-01-01 00:00:00"))

sensors_hour_train <- sensors_train |> 
  group_by(
    nodeid,
    rdtimestamp = floor_date(rdtimestamp, "hour")
  ) |> 
  summarise(
    rssi = mean(rssi, na.rm = TRUE),
    snr = mean(snr, na.rm = TRUE),
    .groups = 'drop'
  )

#environment
env_train <- readr::read_delim("train_env_values.csv", 
                             delim = ",", escape_double = FALSE, trim_ws = TRUE) |> 
  dplyr::mutate(rdtimestamp= as.POSIXct(rdtimestamp, tz = "GMT",
                                        origin="1970-01-01 00:00:00"))


env_hour_train <- env_train |>
  group_by(
    rdtimestamp = floor_date(rdtimestamp, "hour") 
  ) |> 
  summarise(
    soiltemp = mean(soiltemp, na.rm = T),
    soilhum  = mean(soilhum, na.rm = T),
    airtemp  = mean(airtemp, na.rm = T),
    airhum   = mean(airhum, na.rm = T),
    .groups = 'drop'
  )

### Test data set - Nodes and Environment ---- 

sensors_test <- readr::read_delim("test_radio_vals_after.csv", 
                                   delim = ",", escape_double = FALSE, trim_ws = TRUE) |> 
  dplyr::mutate(rdtimestamp= as.POSIXct(rdtimestamp, tz = "GMT",
                                        origin="1970-01-01 00:00:00"))
sensors_hour_test <- sensors_test |> 
  group_by(
    nodeid,
    rdtimestamp = floor_date(rdtimestamp, "hour")
  ) |> 
  summarise(
    rssi = mean(rssi, na.rm = TRUE),
    snr = mean(snr, na.rm = TRUE),
    .groups = 'drop'
  )


env_test <- readr::read_delim("test_env_vals_after.csv", 
                               delim = ",", escape_double = FALSE, trim_ws = TRUE) |> 
  dplyr::mutate(rdtimestamp= as.POSIXct(rdtimestamp, tz = "GMT",
                                        origin="1970-01-01 00:00:00"))

env_hour_test <- env_test |>
  group_by(
    rdtimestamp = floor_date(rdtimestamp, "hour") 
  ) |> 
  summarise(
    soiltemp = mean(soiltemp, na.rm = T),
    soilhum  = mean(soilhum, na.rm = T),
    airtemp  = mean(airtemp, na.rm = T),
    airhum   = mean(airhum, na.rm = T),
    .groups = 'drop'
  )
  

### Select the RSSI's values - Train ----

# tinovi - soil
tinovi01_RSSI_train <- sensors_hour_train |> 
  dplyr::filter(nodeid == "tinovi-01") 

tinovi02_RSSI_train <- sensors_hour_train |> 
  dplyr::filter(nodeid == "tinovi-02")

tinovi03_RSSI_train <- sensors_hour_train |> 
  dplyr::filter(nodeid == "tinovi-03")

tinovi04_RSSI_train <- sensors_hour_train |>
  dplyr::filter(nodeid == "tinovi-04")

tinovi05_RSSI_train <- sensors_hour_train |> 
  dplyr::filter(nodeid == "tinovi-05")

tinovi06_RSSI_train <- sensors_hour_train |> 
  dplyr::filter(nodeid == "tinovi-06")

# milesight - air

milesight01_RSSI_train <- sensors_hour_train |> 
  dplyr::filter(nodeid == "milesight-01")

milesight02_RSSI_train <- sensors_hour_train |> 
  dplyr::filter(nodeid == "milesight-02")


## Joins ----

# tinovi01_RSSI <- inner_join(tinovi01_RSSI, combined_hourly_env, by = "rdtimestamp") 
# tinovi02_RSSI <- inner_join(tinovi02_RSSI, combined_hourly_env, by = "rdtimestamp") 
# tinovi03_RSSI <- inner_join(tinovi03_RSSI, combined_hourly_env, by = "rdtimestamp") 
# tinovi04_RSSI <- inner_join(tinovi04_RSSI, combined_hourly_env, by = "rdtimestamp") 
# tinovi05_RSSI <- inner_join(tinovi05_RSSI, combined_hourly_env, by = "rdtimestamp") 
# tinovi06_RSSI <- inner_join(tinovi06_RSSI, combined_hourly_env, by = "rdtimestamp") 
# milesight01_RSSI <- inner_join(milesight01_RSSI, combined_hourly_env, by = "rdtimestamp")
# milesight02_RSSI <- inner_join(milesight02_RSSI, combined_hourly_env, by = "rdtimestamp")


rss_train_list <- list(
  Tinovi01    = tinovi01_RSSI_train,
  Tinovi02    = tinovi02_RSSI_train,
  Tinovi03    = tinovi03_RSSI_train,
  Tinovi04    = tinovi04_RSSI_train,
  Tinovi05    = tinovi05_RSSI_train,
  Tinovi06    = tinovi06_RSSI_train,
  Milesight01 = milesight01_RSSI_train,
  Milesight02 = milesight02_RSSI_train
)


## Pearson's correlation ----

psych::corr.test(tinovi01_RSSI[, c(3:8)])
psych::corr.test(tinovi02_RSSI[, c(3:8)])
psych::corr.test(tinovi03_RSSI[, c(3:8)])
psych::corr.test(tinovi04_RSSI[, c(3:8)])
psych::corr.test(tinovi05_RSSI[, c(3:8)])
psych::corr.test(tinovi06_RSSI[, c(3:8)])
psych::corr.test(milesight01_RSSI[, c(3:8)])
psych::corr.test(milesight02_RSSI[, c(3:8)])



# make_corr_table <- function(data, caption) {
#   ct  <- psych::corr.test(data[, 3:8])
#   tab <- round(ct$r, 2)
#   
#   kbl(
#     tab,
#     format  = "latex",
#     booktabs = TRUE,
#     caption = caption
#   ) |>
#     kable_classic(full_width = FALSE)
# }
# 
# make_corr_table(tinovi01_RSSI, "Correlation matrix - Tinovi 02")
# make_corr_table(tinovi02_RSSI, "Correlation matrix - Tinovi 02")
# make_corr_table(tinovi03_RSSI, "Correlation matrix - Tinovi 03")
# make_corr_table(tinovi04_RSSI, "Correlation matrix - Tinovi 04")
# make_corr_table(tinovi05_RSSI, "Correlation matrix - Tinovi 05")
# make_corr_table(tinovi06_RSSI, "Correlation matrix - Tinovi 06")
# make_corr_table(milesight01_RSSI, "Correlation matrix - Milesight 01")
# make_corr_table(milesight02_RSSI, "Correlation matrix - Milesight 02")


triangular_corr_table <- function(data, caption, type = c("lower", "upper")) {
  type <- match.arg(type)
  
  ct <- psych::corr.test(data[, 3:8])
  M  <- round(ct$r, 2)
  
  if (type == "lower") {
    M[upper.tri(M)] <- ""
  } else {
    M[lower.tri(M)] <- ""
  }
  
  M <- as.data.frame(M)
  
  kbl(
    M,
    format  = "latex",
    booktabs = TRUE,
    caption = caption
  ) |>
    kable_classic(full_width = FALSE)
}

triangular_corr_table(tinovi01_RSSI, "Correlation matrix - Tinovi 01")
triangular_corr_table(tinovi02_RSSI, "Correlation matrix - Tinovi 02")
triangular_corr_table(tinovi03_RSSI, "Correlation matrix - Tinovi 03")
triangular_corr_table(tinovi04_RSSI, "Correlation matrix - Tinovi 04")
triangular_corr_table(tinovi05_RSSI, "Correlation matrix - Tinovi 05")
triangular_corr_table(tinovi06_RSSI, "Correlation matrix - Tinovi 06")
triangular_corr_table(milesight01_RSSI, "Correlation matrix - Milesight 01")
triangular_corr_table(milesight02_RSSI, "Correlation matrix - Milesight 02")



### Time series Figures ----

# RSSI_01 <- xts(tinovi01_RSSI$rssi, order.by=tinovi01_RSSI$rdtimestamp)
# RSSI_02 <- xts(tinovi02_RSSI$rssi, order.by = tinovi02_RSSI$rdtimestamp)
# RSSI_03 <- xts(tinovi03_RSSI$rssi, order.by = tinovi03_RSSI$rdtimestamp)
# RSSI_04 <- xts(tinovi04_RSSI$rssi, order.by = tinovi04_RSSI$rdtimestamp)
# RSSI_05 <- xts(tinovi05_RSSI$rssi, order.by = tinovi05_RSSI$rdtimestamp)
# RSSI_06 <- xts(tinovi06_RSSI$rssi, order.by = tinovi06_RSSI$rdtimestamp)
# RSSI_07 <- xts(milesight01_RSSI$rssi, order.by=milesight01_RSSI$rdtimestamp)
# RSSI_08 <- xts(milesight02_RSSI$rssi, order.by=milesight02_RSSI$rdtimestamp)
#
# {plot(RSSI_01,main="", yaxis.right=FALSE, grid.col = "white",
#       format.labels="%b-%Y", main.timespan = FALSE,
#       cex.axis=1.2,
#       lwd=0.5,ylim=c(-115,-42),ylab="",cex.lab=1.2)
#   par(cex.lab=1.2, cex.axis=1.2, cex.main=1.2, cex.sub=1.2)
#   lines(RSSI_02,main="RSSI 02",col=2)
#   lines(RSSI_03,main="RSSI 02",col=3)
#   lines(RSSI_04,main="RSSI 02",col=4)
#   addLegend("topright",
#             legend.names=c("RSSI 01","RSSI 02","RSSI 03","RSSI 04"),
#             col=1:4, cex=1.2,
#             lwd=rep(.5,4),
#             ncol=2,
#             bg="white")
# }
# 
# {
#   plot(RSSI_05,main="", yaxis.right=FALSE, grid.col = "white",
#        format.labels="%b-%Y", main.timespan = FALSE,
#        cex.axis=1.2,
#        lwd=0.5,ylim=c(-115,-42),ylab="",cex.lab=1.2)
#   par(cex.lab=1.2, cex.axis=1.2, cex.main=1.2, cex.sub=1.2)
#   lines(RSSI_06,main="",col=2)
#   lines(RSSI_07,main="",col=3)
#   lines(RSSI_08,main="",col=4)
#   addLegend("topright",
#             legend.names=c("RSSI 05","RSSI 06","RSSI 07","RSSI 08"),
#             col=1:4, cex=1.2,
#             lwd=rep(.5,4),
#             ncol=2,
#             bg="white")
# }

### Train and Test sets -----


add_season_dummies <- function(df, time_col = "rdtimestamp") {
  df |>
    mutate(
      month  = month(.data[[time_col]]),
      season = case_when(
        month %in% c(12, 1, 2, 3) ~ "winter",  # dez–mar
        month %in% 4:5            ~ "spring",  # abr–mai
        month %in% 6:8            ~ "summer",  # jun–ago
        TRUE                      ~ "autumn"   # set–nov
      ),
      season = factor(season, levels = c("winter", "spring", "summer", "autumn")),
      # winter é referência → não criamos dummy pra ele
      season_spring = as.integer(season == "spring"),
      season_summer = as.integer(season == "summer"),
      season_autumn = as.integer(season == "autumn")
    ) |>
    select(-month, -season)
}



split_train_test <- function(df, time_col = "rdtimestamp", prop_train = 0.8){
  
  df_ordered <- df[order(df[[time_col]]),]
  
  n_total <- nrow(df_ordered)
  n_train <- floor(prop_train * n_total)
  
  train <- df_ordered[1:n_train, ]
  test <- df_ordered[(n_train+1):n_total, ]
  
  return(list(train = train, test = test))
  
}

# Data Sets List
sensors_list <- list(
  RSSI_01 = tinovi01_RSSI,
  RSSI_02 = tinovi02_RSSI,
  RSSI_03 = tinovi03_RSSI,
  RSSI_04 = tinovi04_RSSI,
  RSSI_05 = tinovi05_RSSI,
  RSSI_06 = tinovi06_RSSI
  #RSSI_07 = milesight01_RSSI,
  #RSSI_08 = milesight02_RSSI
)


sensors_list <- lapply(sensors_list, add_season_dummies)

sensors_split <- lapply(sensors_list, split_train_test)


###Auxiliary Functions 

rss_col  <- "rssi"
#cov_cols <- c("soiltemp","soilhum")
cov_cols <- c(
  
  "soiltemp",
  "soilhum",
  #"airtemp",
  #"airhum",
  "season_spring",
  "season_summer",
  "season_autumn"
)

time_col <- "rdtimestamp"

make_x <- function(df) {
  as.matrix(df[, cov_cols, drop = FALSE])
}


sensor_names <- names(sensors_list)
n_sens <- length(sensor_names)
n_cov  <- length(cov_cols)

cov_labels <- c("T", "RH", "Spring", "Summer", "Autumn")
stopifnot(length(cov_labels) == n_cov)

order_arima <- matrix(NA, n_sens, 3,
                      dimnames = list(sensor_names, c("p","d","q")))

MAE  <- MAPE <- RMSE <- COR <- matrix(NA, n_sens, 4,
                                      dimnames = list(sensor_names,
                                                      c("ARIMA-ALL", "ARIMA-TEMP", "ARIMA-HUM", "ARIMA")))

Xsig   <- matrix("",       n_sens, n_cov,
                 dimnames = list(sensor_names, cov_labels))
sinal  <- matrix("",       n_sens, n_cov,
                 dimnames = list(sensor_names, cov_labels))
values <- matrix(NA_real_, n_sens, n_cov,
                 dimnames = list(sensor_names, cov_labels))


Winter_sig <- rep(NA, n_sens)
names(Winter_sig) <- sensor_names


for (i in seq_len(n_sens)) {
  
  nm <- sensor_names[i]
  tr <- sensors_split[[nm]]$train
  te <- sensors_split[[nm]]$test
  
  tr <- tr[order(tr[[time_col]]), ]
  te <- te[order(te[[time_col]]), ]
  
  y_tr <- tr[[rss_col]]
  y_te <- te[[rss_col]]
  
  X_tr <- make_x(tr)   
  X_te <- make_x(te)
  
  a01 <- auto.arima(y_tr, xreg = X_tr, allowdrift = FALSE)
  ord <- arimaorder(a01)
  order_arima[i, ] <- ord
  
  ct <- lmtest::coeftest(a01)
  
  idx_cov <- (nrow(ct) - n_cov + 1):nrow(ct)
  pvals   <- ct[idx_cov, 4]
  coefs   <- ct[idx_cov, 1]
  sig     <- pvals < 0.05
  
  
  Xsig[i, ]   <- ifelse(sig, cov_labels, "")
  sinal[i, ]  <- ifelse(sig, ifelse(coefs > 0, "+", "-"), "")
  values[i, ] <- coefs
  
  
  int_idx <- which(rownames(ct) %in% c("intercept", "mean"))
  if (length(int_idx) == 1L) {
    Winter_sig[i] <- ct[int_idx, 4] < 0.05   
  } else {
    Winter_sig[i] <- NA                      
  }
 
  
  Xtemp_tr <- X_tr[, "soiltemp", drop = FALSE]
  Xtemp_te <- X_te[, "soiltemp",  drop = FALSE]
  
  Xhum_tr  <- X_tr[, "soilhum",   drop = FALSE]
  Xhum_te  <- X_te[, "soilhum",   drop = FALSE]
  
  a02 <- forecast::Arima(y_tr, order = ord, xreg = Xtemp_tr)  # ARIMA-TEMP
  a03 <- forecast::Arima(y_tr, order = ord)                   # ARIMA
  a04 <- forecast::Arima(y_tr, order = ord, xreg = Xhum_tr)   # ARIMA-HUM
  
  RSSI_test <- y_te
  
  new1 <- forecast::Arima(RSSI_test, model = a01, xreg = X_te)
  new2 <- forecast::Arima(RSSI_test, model = a02, xreg = Xtemp_te)
  new3 <- forecast::Arima(RSSI_test, model = a03)
  new4 <- forecast::Arima(RSSI_test, model = a04, xreg = Xhum_te)
  
  acc1 <- forecast::accuracy(RSSI_test, new1$fitted)
  acc2 <- forecast::accuracy(RSSI_test, new2$fitted)
  acc3 <- forecast::accuracy(RSSI_test, new4$fitted)
  acc4 <- forecast::accuracy(RSSI_test, new3$fitted)
  
  MAPE[i, ] <- c(acc1[5], acc2[5], acc3[5], acc4[5])
  RMSE[i, ] <- c(acc1[2], acc2[2], acc3[2], acc4[2])
  MAE[i, ]  <- c(acc1[3], acc2[3], acc3[3], acc4[3])
  
  COR[i, ] <- c(
    cor(RSSI_test, new1$fitted, use = "complete.obs"),
    cor(RSSI_test, new2$fitted, use = "complete.obs"),
    cor(RSSI_test, new3$fitted, use = "complete.obs"),
    cor(RSSI_test, new4$fitted, use = "complete.obs")
  )
  
  
  assign(
    paste0("result0", i),
    t(data.frame(MAE = MAE[i, ], MAPE = MAPE[i, ], RMSE = RMSE[i, ], COR = COR[i, ]))
  )
}

print(cbind(order_arima,Xsig))

#Winter = ifelse(Winter_sig, "Winter", ""

# Calculating the percentage difference with respect to ARIMA
MAE_AUM<-(MAE[,4]-MAE[,1:3])/MAE[,4]
M_AUM<-(MAPE[,4]-MAPE[,1:3])/MAPE[,4]
RMSE_AUM<-(RMSE[,4]-RMSE[,1:3])/RMSE[,4]
COR_AUM<-(COR[,1:3]-COR[,4])/COR[,4]

# organizing the table
result<- cbind(result01,rbind(
  MAE_AUM[1,],M_AUM[1,],RMSE_AUM[1,],COR_AUM[1,]
)*100
)

for(i in 2:6){ #8
  r<-cbind(get(paste0("result0",i)),rbind(
    MAE_AUM[i,],M_AUM[i,],RMSE_AUM[i,],COR_AUM[i,]
  )*100
  )
  result<-abind::abind(result,r,along = 1)
}
print(result,digits=4) # TABLE V

# Counting the times the models were the best option
count<-apply(cbind(apply(result01[1:3,], 1, rank)==1,
                   COR=rank(result01[4,])==4),1,sum)
for(i in 2:6){#8
  r<-get(paste0("result0",i))
  r<-apply(cbind(apply(r[1:3,], 1, rank)==1,
                 COR=rank(r[4,])==4),1,sum)
  count<-abind::abind(count,r,along = 2)
}
count<-abind::abind(count,apply(count,1,sum),along = 2)
colnames(count)<-c(rownames(MAPE),"Overall")

print(t(count)) # TABLE VI


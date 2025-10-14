# DATA SET ADJUSTMENT AND DESCRIPTIVE ANALYSIS

library(tidyverse)
library(xts)
library(lmtest)
library(forecast)
library(stats)

### Data preparation----
sensors <- readr::read_delim("dataset_sensors.csv", 
                             delim = ",", escape_double = FALSE, trim_ws = TRUE) |> 
  dplyr::mutate(timestamp= as.POSIXct(timestamp, tz = "GMT",
                                       origin="1970-01-01 00:00:00")) #|>relocate(time, .after = timestamp) 


#View(sensors)
attach(sensors)
na_sums <- colSums(is.na(sensors))
print(na_sums) # lora_snr = 33 NAs 

combined_hourly_data <- sensors |> 
  group_by(
    nodeid,
    time_hour = floor_date(timestamp, "hour")
  ) |> 
  summarise(
    temperature_mean = mean(temperature, na.rm = TRUE),
    humidity_mean = mean(humidity, na.rm = TRUE),
    lora_rssi_mean = mean(lora_rssi, na.rm = TRUE),
    lora_snr_mean = mean(lora_snr, na.rm = TRUE),
    .groups = 'drop'
  )


# Select the RSSI's values

tinovi01_RSSI <- combined_hourly_data |> 
  dplyr::filter(nodeid == "tinovi-01")
summary(tinovi01_RSSI[,c(3:6)])

tinovi02_RSSI <- combined_hourly_data |> 
  dplyr::filter(nodeid == "tinovi-02")
summary(tinovi02_RSSI[,c(3:6)])

tinovi03_RSSI <- combined_hourly_data |> 
  dplyr::filter(nodeid == "tinovi-03")
summary(tinovi03_RSSI[,c(3:6)])

tinovi04_RSSI <- combined_hourly_data |> 
  dplyr::filter(nodeid == "tinovi-04")
summary(tinovi04_RSSI[,c(3:6)])

tinovi05_RSSI <- combined_hourly_data |> 
  dplyr::filter(nodeid == "tinovi-05")
summary(tinovi05_RSSI[,c(3:6)])

tinovi06_RSSI <- combined_hourly_data |> 
  dplyr::filter(nodeid == "tinovi-06")
summary(tinovi06_RSSI[,c(3:6)])

milesight01_RSSI <- combined_hourly_data |> 
  dplyr::filter(nodeid == "milesight-01")
summary(milesight01_RSSI[,c(3:6)])

milesight02_RSSI <- combined_hourly_data |> 
  dplyr::filter(nodeid == "milesight-02")
summary(milesight02_RSSI[,c(3:6)])
 

### Descriptive Analysis ----

### Pearson's Correlation ----

### Time series Figures ----

RSSI_01 <- xts(tinovi01_RSSI$lora_rssi_mean, order.by=tinovi01_RSSI$time_hour)
RSSI_02 <- xts(tinovi02_RSSI$lora_rssi_mean, order.by = tinovi02_RSSI$time_hour)
RSSI_03 <- xts(tinovi03_RSSI$lora_rssi_mean, order.by = tinovi03_RSSI$time_hour)
RSSI_04 <- xts(tinovi04_RSSI$lora_rssi_mean, order.by = tinovi04_RSSI$time_hour)
RSSI_05 <- xts(tinovi05_RSSI$lora_rssi_mean, order.by = tinovi05_RSSI$time_hour)
RSSI_06 <- xts(tinovi06_RSSI$lora_rssi_mean, order.by = tinovi06_RSSI$time_hour)
RSSI_07 <- xts(milesight01_RSSI$lora_rssi_mean, order.by=milesight01_RSSI$time_hour)
RSSI_08 <- xts(milesight02_RSSI$lora_rssi_mean, order.by=milesight02_RSSI$time_hour)

{plot(RSSI_01,main="", yaxis.right=FALSE, grid.col = "white",
      format.labels="%b-%Y", main.timespan = FALSE,
      cex.axis=1.2,
      lwd=0.5,ylim=c(-115,-42),ylab="",cex.lab=1.2)
  par(cex.lab=1.2, cex.axis=1.2, cex.main=1.2, cex.sub=1.2) 
  lines(RSSI_02,main="RSSI 02",col=2)
  lines(RSSI_03,main="RSSI 02",col=3)
  lines(RSSI_04,main="RSSI 02",col=4)
  addLegend("topright",
            legend.names=c("RSSI 01","RSSI 02","RSSI 03","RSSI 04"),
            col=1:4, cex=1.2,
            lwd=rep(.5,4),
            ncol=2,
            bg="white")
}

{
  plot(RSSI_05,main="", yaxis.right=FALSE, grid.col = "white",
       format.labels="%b-%Y", main.timespan = FALSE,
       cex.axis=1.2,
       lwd=0.5,ylim=c(-115,-42),ylab="",cex.lab=1.2)
  par(cex.lab=1.2, cex.axis=1.2, cex.main=1.2, cex.sub=1.2) 
  lines(RSSI_06,main="",col=2)
  lines(RSSI_07,main="",col=3)
  lines(RSSI_08,main="",col=4)
  addLegend("topright",
            legend.names=c("RSSI 05","RSSI 06","RSSI 07","RSSI 08"),
            col=1:4, cex=1.2,
            lwd=rep(.5,4),
            ncol=2,
            bg="white")
}


### Train and Test sets -----

split_train_test <- function(df, time_col = "time_hour", prop_train = 0.8){
  
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
  RSSI_06 = tinovi06_RSSI,
  RSSI_07 = milesight01_RSSI,
  RSSI_08 = milesight02_RSSI
)

sensors_split <- lapply(sensors_list, split_train_test)

### Auxiliary Functions ----

rss_col  <- "lora_rssi_mean"                
cov_cols <- c("temperature_mean","humidity_mean")  
time_col <- "time_hour"

make_x <- function(df) {
  as.matrix(df[, cov_cols, drop = FALSE])
}

pvals_from_arima <- function(fit) {
  se <- sqrt(diag(vcov(fit)))
  cf <- coef(fit)
  z  <- cf / se
  2 * pnorm(abs(z), lower.tail = FALSE)
}

best_single_cov <- function(X, y) {
  if (ncol(X) == 0) return(NULL)
  cs <- apply(X, 2, function(x) abs(cor(x, y, use = "complete.obs")))
  which.max(cs)
}

acc_metrics <- function(y, fitted_vec) {
  a <- forecast::accuracy(forecast::na.interp(y), forecast::na.interp(fitted_vec))
  c(MAE = a[3], MAPE = a[5], RMSE = a[2], COR = suppressWarnings(cor(y, fitted_vec, use = "complete.obs")))
}


sensor_names <- names(sensors_list)                 
n_sens <- length(sensor_names)

order_arima <- matrix(NA, n_sens, 3, dimnames = list(sensor_names, c("p","d","q")))
MAE  <- MAPE <- RMSE <- COR <- matrix(NA, n_sens, 4,
                                      dimnames = list(sensor_names, c("ARIMA-COV","ARIMA-COV*","ARIMA-COV**","ARIMA"))
)
Xsig   <- matrix("", n_sens, 4, dimnames = list(sensor_names, c("COVs*","COVs**","","")))
sinal  <- matrix(NA, n_sens, 2, dimnames = list(sensor_names, c("sign(COV*)","sign(COV**)")))
values <- matrix(NA, n_sens, 2, dimnames = list(sensor_names, c("coef(COV*)","coef(COV**)")))

for (i in seq_len(n_sens)) {
  nm <- sensor_names[i]
  tr <- sensors_split[[nm]]$train
  te <- sensors_split[[nm]]$test
  
  # Aqui deu problema - verificar !!!!!!!
  tr <- tr[order(tr[[time_col]]), ]
  te <- te[order(te[[time_col]]), ]
  
  y_tr <- tr[[rss_col]]
  y_te <- te[[rss_col]]
  X_tr <- make_x(tr)
  X_te <- make_x(te)
  
  # ARIMA-COV 
  fit_auto <- auto.arima(y_tr, xreg = X_tr, allowdrift = FALSE)
  order_arima[i, ] <- arimaorder(fit_auto)
  
  # ARIMA-COV* (Significant at 5%)
  pv  <- pvals_from_arima(fit_auto)
  # nomes de coeficientes das covariáveis são os colnames(X_tr)
  cov_names <- colnames(X_tr)
  pv_cov    <- pv[names(pv) %in% cov_names]
  keep_sig  <- names(pv_cov)[pv_cov < 0.05]
  X_sig_tr  <- if (length(keep_sig)) as.matrix(X_tr[, keep_sig, drop = FALSE]) else NULL
  X_sig_te  <- if (length(keep_sig)) as.matrix(X_te[, keep_sig, drop = FALSE]) else NULL
  
  fit_sig <- if (!is.null(X_sig_tr)) Arima(y_tr, order = arimaorder(fit_auto), xreg = X_sig_tr) else Arima(y_tr, order = arimaorder(fit_auto))
  
  # Get names/signs/values
  Xsig[i, "COVs*"] <- if (length(keep_sig)) paste(keep_sig, collapse = ",") else "-"
  if (!is.null(X_sig_tr)) {
    cf <- coef(fit_sig)[keep_sig]
    sinal[i, "sign(COV*)"]  <- paste(ifelse(cf < 0, "-", "+"), collapse = "")
    values[i, "coef(COV*)"] <- paste(round(cf, 4), collapse = ",")
  }
  
  # ARIMA-COV** (uma covariável "melhor": maior |cor| com y) VVerificar se ta certo !!!
  if (ncol(X_tr) > 0) {
    j <- best_single_cov(X_tr, y_tr)
    X_best_tr <- as.matrix(X_tr[, j, drop = FALSE])
    X_best_te <- as.matrix(X_te[, j, drop = FALSE])
    fit_best  <- Arima(y_tr, order = arimaorder(fit_auto), xreg = X_best_tr)
    Xsig[i, "COVs**"] <- colnames(X_best_tr)
    cf2 <- coef(fit_best)[colnames(X_best_tr)]
    sinal[i, "sign(COV**)"]  <- ifelse(cf2 < 0, "-", "+")
    values[i, "coef(COV**)"] <- round(cf2, 4)
  } else {
    fit_best <- Arima(y_tr, order = arimaorder(fit_auto))
    Xsig[i, "COVs**"] <- "-"
  }
  
  # ARIMA (Whithout Covariates)
  fit_nox <- Arima(y_tr, order = arimaorder(fit_auto))
  
  # ---- "one-step-ahead" ---- 
  pred_cov  <- Arima(y_te, xreg = X_te,      model = fit_auto)$fitted
  pred_sig  <- if (!is.null(X_sig_te)) Arima(y_te, xreg = X_sig_te, model = fit_sig)$fitted else Arima(y_te, model = fit_sig)$fitted
  pred_best <- if (exists("X_best_te")) Arima(y_te, xreg = X_best_te, model = fit_best)$fitted else Arima(y_te, model = fit_best)$fitted
  pred_nox  <- Arima(y_te,                    model = fit_nox)$fitted
  
  # ---- Metrics ----
  m1 <- acc_metrics(y_te, pred_cov)
  m2 <- acc_metrics(y_te, pred_sig)
  m3 <- acc_metrics(y_te, pred_best)
  m4 <- acc_metrics(y_te, pred_nox)
  
  MAE [i, ] <- c(m1["MAE"],  m2["MAE"],  m3["MAE"],  m4["MAE"])
  MAPE[i, ] <- c(m1["MAPE"], m2["MAPE"], m3["MAPE"], m4["MAPE"])
  RMSE[i, ] <- c(m1["RMSE"], m2["RMSE"], m3["RMSE"], m4["RMSE"])
  COR [i, ] <- c(m1["COR"],  m2["COR"],  m3["COR"],  m4["COR"])
}

print(cbind(order_arima, Xsig))
print(MAE)
print(MAPE)
print(RMSE)
print(COR)
#############


# =========================
MAE_AUM  <- (MAE[,4]  - MAE[,1:3])  / MAE[,4]
MAPE_AUM <- (MAPE[,4] - MAPE[,1:3]) / MAPE[,4]
RMSE_AUM <- (RMSE[,4] - RMSE[,1:3]) / RMSE[,4]
COR_AUM  <- (COR[,1:3] - COR[,4])   / COR[,4]


make_result_by_sensor <- function(i) {
  base <- rbind(MAE[i,], MAPE[i,], RMSE[i,], COR[i,])
  diffs <- rbind(MAE_AUM[i,], MAPE_AUM[i,], RMSE_AUM[i,], COR_AUM[i,]) * 100
  cbind(base, diffs)
}

result_list <- lapply(seq_along(sensor_names), function(i) make_result_by_sensor(i))
names(result_list) <- sensor_names







# best_counts <- matrix(0L, nrow = 4, ncol = 4,
#                       dimnames = list(c("MAE","MAPE","RMSE","COR"),
#                                       colnames(MAE)))  
# 
# for (i in seq_along(sensor_names)) {
#   r <- rbind(MAE[i,], MAPE[i,], RMSE[i,], COR[i,])
#   # MAE/MAPE/RMSE
#   for (row in 1:3) {
#     j <- which.min(r[row, ])
#     best_counts[row, j] <- best_counts[row, j] + 1
#   }
#   # COR 
#   j <- which.max(r[4, ])
#   best_counts[4, j] <- best_counts[4, j] + 1
# }
# 
# best_counts <- cbind(best_counts) #, Overall = rowSums(best_counts)
# print(best_counts)
# 
# 
# 
# 
# 
# MAE_all <- data.frame(
#   values = c((MAE[,3]-MAE[,1])/MAE[,3],
#              (MAPE[,3]-MAPE[,1])/MAPE[,3],
#              (RMSE[,3]-RMSE[,1])/RMSE[,3],
#              (COR[,1]-COR[,3])/COR[,3]) * 100,
#   measure = rep(c("MAE","MAPE","RMSE","COR"), each = length(sensor_names)),
#   model   = rep(sensor_names, times = 4)
# ) |>
#   mutate(plot_text = ifelse(values < 0, 0.2, values + 0.2))
# 
# 


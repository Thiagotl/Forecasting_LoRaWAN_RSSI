# DATA SET ADJUSTMENT AND DESCRIPTIVE ANALYSIS

library(tidyverse)
library(xts)
library(lmtest)
library(forecast)

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


order_arima<-matrix(NA,8,3)
MAE<-MAPE<-RMSE<-COR<-matrix(NA,8,4)
colnames(MAE)<-colnames(MAPE)<-colnames(RMSE)<-
  colnames(COR)<-c("ARIMA-COV","ARIMA-COV*","ARIMA-COV**","ARIMA")
rownames(MAPE)<-rownames(RMSE)<-rownames(order_arima)<-
  rownames(COR)<-names(sensors_list)

Xsig<-values<-sinal<-matrix(0,8,4)
rownames(Xsig)<-names(sensors_list)




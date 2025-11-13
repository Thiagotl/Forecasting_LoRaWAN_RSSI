# DATA SET ADJUSTMENT AND DESCRIPTIVE ANALYSIS

library(tidyverse)
library(xts)
library(lmtest)
library(forecast)
library(stats)



### Nodes ---- 
sensors <- readr::read_delim("new_data/radio_values.csv", 
                             delim = ",", escape_double = FALSE, trim_ws = TRUE) |> 
  dplyr::mutate(rdtimestamp= as.POSIXct(rdtimestamp, tz = "GMT",
                                      origin="1970-01-01 00:00:00"))

# start 03-23-2024 
# final 03-23-2025

na_sums <- colSums(is.na(sensors)) # snr=8 

combined_hourly_sensors <- sensors |> 
  group_by(
    nodeid,
    rdtimestamp = floor_date(rdtimestamp, "hour")
  ) |> 
  summarise(
    rssi = mean(rssi, na.rm = TRUE),
    snr = mean(snr, na.rm = TRUE),
    .groups = 'drop'
  )


### Environment ----
env <- readr::read_delim("new_data/env_values.csv", 
                         delim = ",", escape_double = FALSE, trim_ws = TRUE) |> 
  dplyr::mutate(rdtimestamp= as.POSIXct(rdtimestamp, tz = "GMT",
                                        origin="1970-01-01 00:00:00"))

combined_hourly_env <- env |>
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
  

summary(combined_hourly_env[,-1])

# Select the RSSI's values

tinovi01_RSSI <- combined_hourly_sensors |> 
  dplyr::filter(nodeid == "tinovi-01") 

tinovi02_RSSI <- combined_hourly_sensors |> 
  dplyr::filter(nodeid == "tinovi-02")

tinovi03_RSSI <- combined_hourly_sensors |> 
  dplyr::filter(nodeid == "tinovi-03")

tinovi04_RSSI <- combined_hourly_sensors |> # n = 8634
  dplyr::filter(nodeid == "tinovi-04")

tinovi05_RSSI <- combined_hourly_sensors |> 
  dplyr::filter(nodeid == "tinovi-05")

tinovi06_RSSI <- combined_hourly_sensors |> 
  dplyr::filter(nodeid == "tinovi-06")

milesight01_RSSI <- combined_hourly_sensors |> 
  dplyr::filter(nodeid == "milesight-01")

milesight02_RSSI <- combined_hourly_sensors |> 
  dplyr::filter(nodeid == "milesight-02")


## Joins ----

tinovi01_RSSI <- inner_join(tinovi01_RSSI, combined_hourly_env, by = "rdtimestamp") 
tinovi02_RSSI <- inner_join(tinovi02_RSSI, combined_hourly_env, by = "rdtimestamp") 
tinovi03_RSSI <- inner_join(tinovi03_RSSI, combined_hourly_env, by = "rdtimestamp") 
tinovi04_RSSI <- inner_join(tinovi04_RSSI, combined_hourly_env, by = "rdtimestamp") 
tinovi05_RSSI <- inner_join(tinovi05_RSSI, combined_hourly_env, by = "rdtimestamp") 
tinovi06_RSSI <- inner_join(tinovi06_RSSI, combined_hourly_env, by = "rdtimestamp") 
milesight01_RSSI <- inner_join(milesight01_RSSI, combined_hourly_env, by = "rdtimestamp")
milesight02_RSSI <- inner_join(milesight02_RSSI, combined_hourly_env, by = "rdtimestamp")


## Pearson's correlation ----

psych::corr.test(tinovi02_RSSI[, c(3:8)])
psych::corr.test(tinovi03_RSSI[, c(3:8)])
psych::corr.test(tinovi04_RSSI[, c(3:8)])
psych::corr.test(tinovi05_RSSI[, c(3:8)])
psych::corr.test(tinovi06_RSSI[, c(3:8)])
psych::corr.test(milesight01_RSSI[, c(3:8)])
psych::corr.test(milesight02_RSSI[, c(3:8)])

### Train and Test sets -----

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
  RSSI_06 = tinovi06_RSSI,
  RSSI_07 = milesight01_RSSI,
  RSSI_08 = milesight02_RSSI
)

sensors_split <- lapply(sensors_list, split_train_test)


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

### ----






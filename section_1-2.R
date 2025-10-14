library(tidyverse)
library(xts)
library(lmtest)
library(forecast)
library(stats)

# Seasonality as a Covariate

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








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

tinovi04_RSSI <- combined_hourly_sensors |> 
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




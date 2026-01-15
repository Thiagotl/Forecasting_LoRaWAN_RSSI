library(tidyverse)
library(xts)
library(lmtest)
library(forecast)
library(stats)
library(knitr)
library(kableExtra)
library(lubridate)



### Training data set - Nodes and Environment ---- 

# sensors
sensors_train <- readr::read_delim("train_radio_values.csv", 
                                   delim = ",", escape_double = FALSE, trim_ws = TRUE) |> 
  dplyr::mutate(rdtimestamp= as.POSIXct(rdtimestamp, tz = "GMT",
                                        origin="1970-01-01 00:00:00"))
## day

sensors_day_train <- sensors_train |> 
  group_by(
    nodeid,
    rdtimestamp = floor_date(rdtimestamp, "day")
  ) |>
  summarise(
    rssi = mean(rssi, na.rm = TRUE),
    snr  = mean(snr,  na.rm = TRUE),
    .groups = "drop"
  )

#environment
env_train <- readr::read_delim("train_env_values.csv", 
                               delim = ",", escape_double = FALSE, trim_ws = TRUE) |> 
  dplyr::mutate(rdtimestamp= as.POSIXct(rdtimestamp, tz = "GMT",
                                        origin="1970-01-01 00:00:00"))


env_day_train <- env_train |>
  group_by(
    rdtimestamp = floor_date(rdtimestamp, "day") 
  ) |> 
  summarise(
    soiltemp = mean(soiltemp, na.rm = T),
    soilhum  = mean(soilhum, na.rm = T),
    airtemp  = mean(airtemp, na.rm = T),
    airhum   = mean(airhum, na.rm = T),
    .groups = 'drop'
  ) |> select(-c(soiltemp, soilhum))

### Testing data set - Nodes and Environment ---- 

sensors_test <- readr::read_delim("test_radio_vals_after.csv", 
                                  delim = ",", escape_double = FALSE, trim_ws = TRUE) |> 
  dplyr::mutate(rdtimestamp= as.POSIXct(rdtimestamp, tz = "GMT",
                                        origin="1970-01-01 00:00:00"))
sensors_day_test <- sensors_test |> 
  group_by(
    nodeid,
    rdtimestamp = floor_date(rdtimestamp, "day")
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

env_day_test <- env_test |>
  group_by(
    rdtimestamp = floor_date(rdtimestamp, "day") 
  ) |> 
  summarise(
    soiltemp = mean(soiltemp, na.rm = T),
    soilhum  = mean(soilhum, na.rm = T),
    airtemp  = mean(airtemp, na.rm = T),
    airhum   = mean(airhum, na.rm = T),
    .groups = 'drop'
  ) |> select(-c(soiltemp, soilhum))


### Select the RSSI's values - Training Set (Day)----

# tinovi - soil
tinovi01_RSSI_train <- sensors_day_train |> 
  dplyr::filter(nodeid == "tinovi-01") |> select(-snr)

tinovi02_RSSI_train <- sensors_day_train |> 
  dplyr::filter(nodeid == "tinovi-02") |> select(-snr)

tinovi03_RSSI_train <- sensors_day_train |> 
  dplyr::filter(nodeid == "tinovi-03") |> select(-snr)

tinovi04_RSSI_train <- sensors_day_train |>
  dplyr::filter(nodeid == "tinovi-04") |> select(-snr)

tinovi05_RSSI_train <- sensors_day_train |> 
  dplyr::filter(nodeid == "tinovi-05") |> select(-snr)

tinovi06_RSSI_train <- sensors_day_train |> 
  dplyr::filter(nodeid == "tinovi-06") |> select(-snr)

# milesight - air

milesight01_RSSI_train <- sensors_day_train |> 
  dplyr::filter(nodeid == "milesight-01") |> select(-snr)

milesight02_RSSI_train <- sensors_day_train |> 
  dplyr::filter(nodeid == "milesight-02") |> select(-snr)


### Select the RSSI's values - testing ----

# tinovi - soil
tinovi01_RSSI_test <- sensors_hour_test |>
  dplyr::filter(nodeid == "tinovi-01") |> select(-snr)

tinovi02_RSSI_test <- sensors_hour_test |>
  dplyr::filter(nodeid == "tinovi-02") |> select(-snr)

tinovi03_RSSI_test <- sensors_hour_test |>
  dplyr::filter(nodeid == "tinovi-03") |> select(-snr)

tinovi04_RSSI_test <- sensors_hour_test |>
  dplyr::filter(nodeid == "tinovi-04") |> select(-snr)

tinovi05_RSSI_test <- sensors_hour_test|>
  dplyr::filter(nodeid == "tinovi-05") |> select(-snr)

tinovi06_RSSI_test <- sensors_hour_test|>
  dplyr::filter(nodeid == "tinovi-06") |> select(-snr)

# milesight - air

milesight01_RSSI_test <- sensors_hour_test |>
  dplyr::filter(nodeid == "milesight-01") |> select(-snr)

milesight02_RSSI_test <- sensors_hour_test |>
  dplyr::filter(nodeid == "milesight-02") |> select(-snr)







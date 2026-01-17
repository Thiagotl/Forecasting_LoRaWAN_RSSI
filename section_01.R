# DATA SET ADJUSTMENT AND DESCRIPTIVE ANALYSIS

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

# environment

env_train <- read_delim(
  "train_env_values.csv",
  delim = ",",
  escape_double = FALSE,
  trim_ws = TRUE
) |> 
  mutate(
    rdtimestamp = as.POSIXct(rdtimestamp, tz = "GMT", origin = "1970-01-01 00:00:00"),
    ts = rdtimestamp,
    mmdd = month(ts) * 100 + day(ts),
    season = case_when(
      mmdd >= 321  & mmdd < 621  ~ "Spring",
      mmdd >= 621  & mmdd < 922  ~ "Summer",
      mmdd >= 922  & mmdd < 1221 ~ "Autumn",
      TRUE                      ~ "Winter"
    ),
    season = factor(season, levels = c("Spring","Summer","Autumn","Winter"))
  ) |> 
  select(-ts, -mmdd)

env_hour_train <- env_train |>
  group_by(rdtimestamp = floor_date(rdtimestamp, "hour")) |>
  summarise(
    airtemp  = mean(airtemp, na.rm = TRUE),
    airhum   = mean(airhum,  na.rm = TRUE),
    
    # estação da hora
    season = first(season),
    
    # dummies (Spring é referência)
    dum_Summer = as.integer(first(season) == "Summer"),
    dum_Autumn = as.integer(first(season) == "Autumn"),
    dum_Winter = as.integer(first(season) == "Winter"),
    
    .groups = "drop"
  )


### Testing data set - Nodes and Environment ---- 

sensors_test <- readr::read_delim("test_radio_vals_after.csv", delim = ",", escape_double = FALSE, trim_ws = TRUE) |> 
  dplyr::mutate(rdtimestamp= as.POSIXct(rdtimestamp, tz = "GMT", origin="1970-01-01 00:00:00"))

sensors_hour_test <- sensors_test |> 
  group_by( nodeid, rdtimestamp = floor_date(rdtimestamp, "hour") ) |> 
  summarise( rssi = mean(rssi, na.rm = TRUE), 
             snr = mean(snr, na.rm = TRUE), 
             .groups = 'drop' ) |>  select(-snr)

env_test <- read_delim(
  "test_env_vals_after.csv",
  delim = ",", escape_double = FALSE, trim_ws = TRUE
) |> 
  mutate(
    rdtimestamp = as.POSIXct(rdtimestamp, tz = "GMT", origin = "1970-01-01 00:00:00"),
    ts   = rdtimestamp,
    mmdd = month(ts) * 100 + day(ts),
    season = case_when(
      mmdd >= 321  & mmdd < 621  ~ "Spring",
      mmdd >= 621  & mmdd < 922  ~ "Summer",
      mmdd >= 922  & mmdd < 1221 ~ "Autumn",
      TRUE                      ~ "Winter"
    ),
    season = factor(season, levels = c("Spring","Summer","Autumn","Winter"))
  ) |> 
  select(-ts, -mmdd)

env_hour_test <- env_test |>
  group_by(rdtimestamp = floor_date(rdtimestamp, "hour")) |>
  summarise(
    airtemp  = mean(airtemp, na.rm = TRUE),
    airhum   = mean(airhum,  na.rm = TRUE),
    season = first(season),
    dum_Summer = as.integer(first(season) == "Summer"),
    dum_Autumn = as.integer(first(season) == "Autumn"),
    dum_Winter = as.integer(first(season) == "Winter"),
    .groups = "drop"
  )


### Select the RSSI's values - Training Set ----

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


### Select the RSSI's values - testing ----

# tinovi - soil
tinovi01_RSSI_test <- sensors_hour_test |>
  dplyr::filter(nodeid == "tinovi-01") 

tinovi02_RSSI_test <- sensors_hour_test |>
  dplyr::filter(nodeid == "tinovi-02") 

tinovi03_RSSI_test <- sensors_hour_test |>
  dplyr::filter(nodeid == "tinovi-03") 

tinovi04_RSSI_test <- sensors_hour_test |>
  dplyr::filter(nodeid == "tinovi-04") 

tinovi05_RSSI_test <- sensors_hour_test|>
  dplyr::filter(nodeid == "tinovi-05") 

tinovi06_RSSI_test <- sensors_hour_test|>
  dplyr::filter(nodeid == "tinovi-06") 

# milesight - air

milesight01_RSSI_test <- sensors_hour_test |>
  dplyr::filter(nodeid == "milesight-01") 

milesight02_RSSI_test <- sensors_hour_test |>
  dplyr::filter(nodeid == "milesight-02") 


summary(milesight02_RSSI_test[3])
dim(milesight02_RSSI_test)

## Joins training sets ----

tinovi01_RSSI_train <- inner_join(tinovi01_RSSI_train, env_hour_train, by = "rdtimestamp") 
tinovi02_RSSI_train <- inner_join(tinovi02_RSSI_train, env_hour_train, by = "rdtimestamp") 
tinovi03_RSSI_train <- inner_join(tinovi03_RSSI_train, env_hour_train, by = "rdtimestamp") 
tinovi04_RSSI_train <- inner_join(tinovi04_RSSI_train, env_hour_train, by = "rdtimestamp") 
tinovi05_RSSI_train <- inner_join(tinovi05_RSSI_train, env_hour_train, by = "rdtimestamp") 
tinovi06_RSSI_train <- inner_join(tinovi06_RSSI_train, env_hour_train, by = "rdtimestamp") 
milesight01_RSSI_train <- inner_join(milesight01_RSSI_train, env_hour_train, by = "rdtimestamp")
milesight02_RSSI_train <- inner_join(milesight02_RSSI_train, env_hour_train, by = "rdtimestamp")


rssi_train_list <- list(
  Tinovi01    = tinovi01_RSSI_train,
  Tinovi02    = tinovi02_RSSI_train,
  Tinovi03    = tinovi03_RSSI_train,
  Tinovi04    = tinovi04_RSSI_train,
  Tinovi05    = tinovi05_RSSI_train,
  Tinovi06    = tinovi06_RSSI_train,
  Milesight01 = milesight01_RSSI_train,
  Milesight02 = milesight02_RSSI_train
)


## Joins testing sets ----

tinovi01_RSSI_test <- inner_join(tinovi01_RSSI_test, env_hour_test, by = "rdtimestamp") 
tinovi02_RSSI_test <- inner_join(tinovi02_RSSI_test, env_hour_test, by = "rdtimestamp") 
tinovi03_RSSI_test <- inner_join(tinovi03_RSSI_test, env_hour_test, by = "rdtimestamp") 
tinovi04_RSSI_test <- inner_join(tinovi04_RSSI_test, env_hour_test, by = "rdtimestamp") 
tinovi05_RSSI_test <- inner_join(tinovi05_RSSI_test, env_hour_test, by = "rdtimestamp") 
tinovi06_RSSI_test <- inner_join(tinovi06_RSSI_test, env_hour_test, by = "rdtimestamp") 
milesight01_RSSI_test <- inner_join(milesight01_RSSI_test, env_hour_test, by = "rdtimestamp")
milesight02_RSSI_test <- inner_join(milesight02_RSSI_test, env_hour_test, by = "rdtimestamp")


rssi_test_list <- list(
  Tinovi01    = tinovi01_RSSI_test,
  Tinovi02    = tinovi02_RSSI_test,
  Tinovi03    = tinovi03_RSSI_test,
  Tinovi04    = tinovi04_RSSI_test,
  Tinovi05    = tinovi05_RSSI_test,
  Tinovi06    = tinovi06_RSSI_test,
  Milesight01 = milesight01_RSSI_test,
  Milesight02 = milesight02_RSSI_test
)


## Pearson's correlation ----
# 
# psych::corr.test(tinovi01_RSSI_train[, c(3:5)])
# psych::corr.test(tinovi02_RSSI_train[, c(3:5)])
# psych::corr.test(tinovi03_RSSI_train[, c(3:5)])
# psych::corr.test(tinovi04_RSSI_train[, c(3:5)])
# psych::corr.test(tinovi05_RSSI_train[, c(3:5)])
# psych::corr.test(tinovi06_RSSI_train[, c(3:5)])
# psych::corr.test(milesight01_RSSI_train[, c(3:5)])
# psych::corr.test(milesight02_RSSI_train[, c(3:5)])



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


# triangular_corr_table <- function(data, caption, type = c("lower", "upper")) {
#   type <- match.arg(type)
#   
#   ct <- psych::corr.test(data[, 3:8])
#   M  <- round(ct$r, 2)
#   
#   if (type == "lower") {
#     M[upper.tri(M)] <- ""
#   } else {
#     M[lower.tri(M)] <- ""
#   }
#   
#   M <- as.data.frame(M)
#   
#   kbl(
#     M,
#     format  = "latex",
#     booktabs = TRUE,
#     caption = caption
#   ) |>
#     kable_classic(full_width = FALSE)
# }
# 
# triangular_corr_table(tinovi01_RSSI, "Correlation matrix - Tinovi 01")
# triangular_corr_table(tinovi02_RSSI, "Correlation matrix - Tinovi 02")
# triangular_corr_table(tinovi03_RSSI, "Correlation matrix - Tinovi 03")
# triangular_corr_table(tinovi04_RSSI, "Correlation matrix - Tinovi 04")
# triangular_corr_table(tinovi05_RSSI, "Correlation matrix - Tinovi 05")
# triangular_corr_table(tinovi06_RSSI, "Correlation matrix - Tinovi 06")
# triangular_corr_table(milesight01_RSSI, "Correlation matrix - Milesight 01")
# triangular_corr_table(milesight02_RSSI, "Correlation matrix - Milesight 02")


### Training data set - Nodes and Environment ---- 

# sensors
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
tinovi01_RSSI_train_d <- sensors_day_train |> 
  dplyr::filter(nodeid == "tinovi-01") |> select(-snr)

tinovi02_RSSI_train_d <- sensors_day_train |> 
  dplyr::filter(nodeid == "tinovi-02") |> select(-snr)

tinovi03_RSSI_train_d <- sensors_day_train |> 
  dplyr::filter(nodeid == "tinovi-03") |> select(-snr)

tinovi04_RSSI_train_d <- sensors_day_train |>
  dplyr::filter(nodeid == "tinovi-04") |> select(-snr)

tinovi05_RSSI_train_d <- sensors_day_train |> 
  dplyr::filter(nodeid == "tinovi-05") |> select(-snr)

tinovi06_RSSI_train_d <- sensors_day_train |> 
  dplyr::filter(nodeid == "tinovi-06") |> select(-snr)

# milesight - air

milesight01_RSSI_train_d <- sensors_day_train |> 
  dplyr::filter(nodeid == "milesight-01") |> select(-snr)

milesight02_RSSI_train_d <- sensors_day_train |> 
  dplyr::filter(nodeid == "milesight-02") |> select(-snr)


### Select the RSSI's values - Testing Set (day)----

# tinovi - soil
tinovi01_RSSI_test_d <- sensors_day_test |>
  dplyr::filter(nodeid == "tinovi-01") |> select(-snr)

tinovi02_RSSI_test_d <- sensors_day_test |>
  dplyr::filter(nodeid == "tinovi-02") |> select(-snr)

tinovi03_RSSI_test_d <- sensors_day_test |>
  dplyr::filter(nodeid == "tinovi-03") |> select(-snr)

tinovi04_RSSI_test_d <- sensors_day_test |>
  dplyr::filter(nodeid == "tinovi-04") |> select(-snr)

tinovi05_RSSI_test_d <- sensors_day_test|>
  dplyr::filter(nodeid == "tinovi-05") |> select(-snr)

tinovi06_RSSI_test_d <- sensors_day_test|>
  dplyr::filter(nodeid == "tinovi-06") |> select(-snr)

# milesight - air

milesight01_RSSI_test_d <- sensors_day_test |>
  dplyr::filter(nodeid == "milesight-01") |> select(-snr)

milesight02_RSSI_test_d <- sensors_day_test |>
  dplyr::filter(nodeid == "milesight-02") |> select(-snr)











# DATA SET ADJUSTMENT AND DESCRIPTIVE ANALYSIS

library(tidyverse)

### Data preparation----
sensors <- readr::read_delim("sensors.csv", 
                             delim = ",", escape_double = FALSE, trim_ws = TRUE) |> 
  dplyr::mutate(timestamp = as.POSIXct(timestamp, tz = "GMT",
                                       origin="1970-01-01 00:00:00"))
#View(sensors)
attach(sensors)

na_sums <- colSums(is.na(sensors))
print(na_sums) # lora_snr = 29 NAs 

# Select the RSSI's values

tinovi01_RSSI <- sensors |> 
  dplyr::filter(nodeid == "tinovi-01")
summary(tinovi01_RSSI[,c(6:7)])

tinovi02_RSSI <- sensors |> 
  dplyr::filter(nodeid == "tinovi-02")

tinovi03_RSSI <- sensors |> 
  dplyr::filter(nodeid == "tinovi-03")

tinovi04_RSSI <- sensors |> 
  dplyr::filter(nodeid == "tinovi-04")

tinovi05_RSSI <- sensors |> 
  dplyr::filter(nodeid == "tinovi-05")

tinovi06_RSSI <- sensors |> 
  dplyr::filter(nodeid == "tinovi-06")

milesight01_RSSI <- sensors |> 
  dplyr::filter(nodeid == "milesight-01")

milesight02_RSSI <- sensors |> 
  dplyr::filter(nodeid == "milesight-02")
 

### DESCRIPTIVE ANALYSIS ----

summary1 <- apply(sensors[, c(3:4)], 2, summary)

combined_df <- dplyr::bind_rows(tinovi01_RSSI,tinovi02_RSSI, tinovi03_RSSI,
                                tinovi04_RSSI, tinovi05_RSSI, tinovi06_RSSI,
                                milesight01_RSSI, milesight02_RSSI)

summary2 <- combined_df  |> 
  group_by(nodeid) |> 
  summarise(
    across(c(lora_rssi, lora_snr),
           list(
             Min = ~min(., na.rm = TRUE),
             Q1  = ~quantile(., probs = 0.25, na.rm = TRUE),
             Median = ~median(., na.rm = TRUE),
             Mean = ~mean(., na.rm = TRUE),
             Q3 = ~quantile(., probs = 0.75, na.rm = TRUE),
             Max = ~max(., na.rm = TRUE)
           )
    )
  )

### Pearson's Correlation ----

correlation <- sensors[, c()]













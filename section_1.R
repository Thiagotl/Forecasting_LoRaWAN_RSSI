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

# RSSI_01 <- xts(tinovi01_RSSI$lora_rssi_mean, order.by=tinovi01_RSSI$time_hour)
# RSSI_02 <- xts(tinovi02_RSSI$lora_rssi_mean, order.by = tinovi02_RSSI$time_hour)
# RSSI_03 <- xts(tinovi03_RSSI$lora_rssi_mean, order.by = tinovi03_RSSI$time_hour)
# RSSI_04 <- xts(tinovi04_RSSI$lora_rssi_mean, order.by = tinovi04_RSSI$time_hour)
# RSSI_05 <- xts(tinovi05_RSSI$lora_rssi_mean, order.by = tinovi05_RSSI$time_hour)
# RSSI_06 <- xts(tinovi06_RSSI$lora_rssi_mean, order.by = tinovi06_RSSI$time_hour)
# RSSI_07 <- xts(milesight01_RSSI$lora_rssi_mean, order.by=milesight01_RSSI$time_hour)
# RSSI_08 <- xts(milesight02_RSSI$lora_rssi_mean, order.by=milesight02_RSSI$time_hour)
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


sensor_names <- names(sensors_list)                 
n_sens <- length(sensor_names)

order_arima <- matrix(NA, 8, 3)
MAE <- MAPE <- RMSE <- COR <- matrix(NA, 8, 4)
colnames(MAE) <- colnames(MAPE) <- colnames(RMSE) <- colnames(COR) <-
  c("ARIMA-TEMP+HUM","ARIMA-TEMP","ARIMA-HUM","ARIMA")
rownames(MAPE) <- rownames(RMSE) <- rownames(order_arima) <- rownames(COR) <- sensor_names
Xsig <- values <- sinal <- matrix("", 8, 4)
rownames(Xsig) <- sensor_names

for (i in seq_len(n_sens)){
  
  nm <- sensor_names[i]
  tr <- sensors_split[[nm]]$train
  te <- sensors_split[[nm]]$test
  
  tr <- tr[order(tr[[time_col]]), ]
  te <- te[order(te[[time_col]]), ]
  
  y_tr <- tr[[rss_col]]
  y_te <- te[[rss_col]]
  X <- make_x(tr)
  Xtest <- make_x(te)
  
  #Xchoosed<-X[,1]
  #Xchoosedt<-Xtest[,1]
  
  # ARIMA-COMPLETO
  a01<-assign(paste0("arimax0",i), auto.arima(y_tr,xreg = X,allowdrift=FALSE))
  tcoef<-(coeftest(a01)<0.05)[(length(a01$coef)-dim(X)[2]+1):length(a01$coef),4]
  #Xnew<-X[,tcoef]
  order_arima[i, ] <- arimaorder(a01)
  Xsig[i,]<-c(c("T","RH")[tcoef],rep("",4-sum(tcoef)))
  sinal[i,]<-(coef(a01)<0)[(length(a01$coef)-dim(X)[2]+1):length(a01$coef)]
  values[i,]<-(coef(a01))[(length(a01$coef)-dim(X)[2]+1):length(a01$coef)]
  
  #Xnewt<-Xtest[,tcoef]
  
  Xnew <-X[, 1] # Temperature
  a02<-Arima(y_tr,arimaorder(a01),xreg=Xnew) # ARIMA Temperature
  
  a03<-Arima(y_tr,arimaorder(a01)) # ARIMA without covariates
  
  Xnew2 <-X[, 2] # Humidity
  a04<-Arima(y_tr,order=arimaorder(a01),xreg=Xnew2) # ARIMA Humidity
  
  
  # forecasting
  
  RSSI_test <- y_te
  
  # COMPLETO 
  new1<-assign(paste0("arima_cov0",i),
               Arima(RSSI_test ,xreg = Xtest,model=a01)) #one-step-ahead
  
  Xnewt <- Xtest[, 1] # Temperature
  new2<-assign(paste0("arima_covstar",i),
               Arima(RSSI_test ,xreg = Xnewt,model=a02)) #one-step-ahead
  
  # Without covariates
  new3<-assign(paste0("arima_pred0",i),
               Arima(RSSI_test ,model=a03)) #one-step-ahead
  # Humidity
  Xnewt2 <- Xtest[, 2]
  new4<-assign(paste0("arima_cov2star0",i),
               Arima(RSSI_test ,xreg=Xnewt2,model=a04)) #one-step-ahead
  
  MAPE[i,]<-c(forecast::accuracy(RSSI_test,new1$fitted)[5],
              forecast::accuracy(RSSI_test,new2$fitted)[5],
              forecast::accuracy(RSSI_test,new4$fitted)[5],
              forecast::accuracy(RSSI_test,new3$fitted)[5]
  )
  RMSE[i,]<-c(forecast::accuracy(RSSI_test,new1$fitted)[2],
              forecast::accuracy(RSSI_test,new2$fitted)[2],
              forecast::accuracy(RSSI_test,new4$fitted)[2],
              forecast::accuracy(RSSI_test,new3$fitted)[2]
  )
  COR[i,]<-c(cor(RSSI_test,new1$fitted),
             cor(RSSI_test,new2$fitted),
             cor(RSSI_test,new4$fitted),
             cor(RSSI_test,new3$fitted)
  )
  MAE[i,]<-c(forecast::accuracy(RSSI_test,new1$fitted)[3],
             forecast::accuracy(RSSI_test,new2$fitted)[3],
             forecast::accuracy(RSSI_test,new4$fitted)[3],
             forecast::accuracy(RSSI_test,new3$fitted)[3]
  )
  
  
  assign(paste0("result0",i),
         t(data.frame(MAE=MAE[i,],MAPE=MAPE[i,],RMSE=RMSE[i,],COR=COR[i,]))
  )
  
}

print(cbind(order_arima,Xsig))

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
for(i in 2:8){
  r<-cbind(get(paste0("result0",i)),rbind(
    MAE_AUM[i,],M_AUM[i,],RMSE_AUM[i,],COR_AUM[i,]
  )*100
  )
  result<-abind::abind(result,r,along = 1)
}
print(result,digits=7) # TABLE V


# Counting the times the models were the best option
count<-apply(cbind(apply(result01[1:3,], 1, rank)==1,
                   COR=rank(result01[4,])==4),1,sum)
for(i in 2:8){
  r<-get(paste0("result0",i))
  r<-apply(cbind(apply(r[1:3,], 1, rank)==1,
                 COR=rank(r[4,])==4),1,sum)
  count<-abind::abind(count,r,along = 2)
}
count<-abind::abind(count,apply(count,1,sum),along = 2)
colnames(count)<-c(rownames(MAPE),"Overall")

print(t(count)) # TABLE VI



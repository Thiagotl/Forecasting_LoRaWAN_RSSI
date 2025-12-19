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
  ) |> select(-c(soiltemp, soilhum))

### Testing data set - Nodes and Environment ---- 

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
  ) |> select(-c(soiltemp, soilhum))
  

### Select the RSSI's values - Training Set ----

# tinovi - soil
tinovi01_RSSI_train <- sensors_hour_train |> 
  dplyr::filter(nodeid == "tinovi-01") |> select(-snr)

tinovi02_RSSI_train <- sensors_hour_train |> 
  dplyr::filter(nodeid == "tinovi-02") |> select(-snr)

tinovi03_RSSI_train <- sensors_hour_train |> 
  dplyr::filter(nodeid == "tinovi-03") |> select(-snr)

tinovi04_RSSI_train <- sensors_hour_train |>
  dplyr::filter(nodeid == "tinovi-04") |> select(-snr)

tinovi05_RSSI_train <- sensors_hour_train |> 
  dplyr::filter(nodeid == "tinovi-05") |> select(-snr)

tinovi06_RSSI_train <- sensors_hour_train |> 
  dplyr::filter(nodeid == "tinovi-06") |> select(-snr)

# milesight - air

milesight01_RSSI_train <- sensors_hour_train |> 
  dplyr::filter(nodeid == "milesight-01") |> select(-snr)

milesight02_RSSI_train <- sensors_hour_train |> 
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

psych::corr.test(tinovi01_RSSI_train[, c(3:5)])
psych::corr.test(tinovi02_RSSI_train[, c(3:5)])
psych::corr.test(tinovi03_RSSI_train[, c(3:5)])
psych::corr.test(tinovi04_RSSI_train[, c(3:5)])
psych::corr.test(tinovi05_RSSI_train[, c(3:5)])
psych::corr.test(tinovi06_RSSI_train[, c(3:5)])
psych::corr.test(milesight01_RSSI_train[, c(3:5)])
psych::corr.test(milesight02_RSSI_train[, c(3:5)])



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



####################
## Fitting ARIMAX ##
####################

m <- length(rssi_train_list)
sensors <- names(rssi_train_list)

order_arima <- matrix(NA, m, 3)

MAE <- MAPE <- RMSE <- COR <- matrix(NA, m, 3)
colnames(MAE) <- colnames(MAPE) <- colnames(RMSE) <- colnames(COR) <-
  c("ARIMA-COV","ARIMA-COV*","ARIMA-COV*","ARIMA")

rownames(order_arima) <- rownames(MAE) <- rownames(MAPE) <- rownames(RMSE) <- rownames(COR) <- sensors

# deixei 4 colunas pra manter o “layout” do seu output, embora agora só existam 2 covariáveis (T, RH)
Xsig   <- matrix("", m, 2)
values <- matrix(NA, m, 2)
sinal  <- matrix(NA, m, 2)
rownames(Xsig) <- sensors

#coeffs <- matrix(0, m, 6)  # (não é usado no trecho; mantido pra preservar estrutura)

for (i in seq_along(sensors)) {
  
  df_tr <- rssi_train_list[[i]]
  df_te <- rssi_test_list[[i]]
  
  RSSI <- df_tr$rssi
  
  # covariáveis disponíveis nos seus data sets
  X     <- cbind(df_tr$airtemp, df_tr$airhum)
  Xtest <- cbind(df_te$airtemp, df_te$airhum)
  
  #Xchoosed  <- X[, 1]
  #Xchoosedt <- Xtest[, 1]
  
  # fitting the algorithms
  a01 <- assign(paste0("arimax0", i),
                auto.arima(RSSI, xreg = X, allowdrift = FALSE))
  
  # seleção por significância dos coeficientes do xreg (últimos k coeficientes)
  k <- ncol(X)
  pvals <- coeftest(a01)[, 4]
  tcoef <- pvals[(length(a01$coef) - k + 1):length(a01$coef)] < 0.05
  
  # (opcional, mas evita quebrar quando nenhuma covariável é significativa)
  if (sum(tcoef) == 0) tcoef[1] <- TRUE  # comente esta linha se você preferir “sem COV*”
  
  Xnew  <- X[, tcoef, drop = FALSE]
  Xnewt <- Xtest[, tcoef, drop = FALSE]
  
  order_arima[i, ] <- arimaorder(a01)
  
  Xsig[i, ] <- c(c("T", "RH")[tcoef], rep("", 2 - sum(tcoef)))
  
  sign_vec <- (coef(a01) < 0)[(length(a01$coef) - k + 1):length(a01$coef)]
  val_vec  <- (coef(a01))[(length(a01$coef) - k + 1):length(a01$coef)]
  sinal[i, ]  <- c(sign_vec, rep(NA, 2 - length(sign_vec)))
  values[i, ] <- c(val_vec,  rep(NA, 2 - length(val_vec)))
  
  a02 <- Arima(RSSI, arimaorder(a01), xreg = Xnew) # Significativo
  
  a03 <- Arima(RSSI, arimaorder(a01)) # ARIMA
  
  a04 <- Arima(RSSI, order = arimaorder(a01), xreg = Xchoosed)
  
  # forecasting (one-step-ahead)
  RSSI_test <- df_te$rssi
  
  new1 <- assign(paste0("arima_cov0", i),
                 Arima(RSSI_test, xreg = Xtest, model = a01))
  
  new2 <- assign(paste0("arima_covstar", i),
                 Arima(RSSI_test, xreg = Xnewt, model = a02))
  
  new3 <- assign(paste0("arima_pred0", i),
                 Arima(RSSI_test, model = a03))
  
  new4 <- assign(paste0("arima_cov2star0", i),
                Arima(RSSI_test, xreg = Xchoosedt, model = a04))
  
  MAPE[i, ] <- c(
    forecast::accuracy(RSSI_test, new1$fitted)[5],
    forecast::accuracy(RSSI_test, new2$fitted)[5],
    forecast::accuracy(RSSI_test, new4$fitted)[5],
    forecast::accuracy(RSSI_test, new3$fitted)[5]
  )
  
  RMSE[i, ] <- c(
    forecast::accuracy(RSSI_test, new1$fitted)[2],
    forecast::accuracy(RSSI_test, new2$fitted)[2],
    forecast::accuracy(RSSI_test, new4$fitted)[2],
    forecast::accuracy(RSSI_test, new3$fitted)[2]
  )
  
  COR[i, ] <- c(
    cor(RSSI_test, new1$fitted),
    cor(RSSI_test, new2$fitted),
    cor(RSSI_test, new4$fitted),
    cor(RSSI_test, new3$fitted)
  )
  
  MAE[i, ] <- c(
    forecast::accuracy(RSSI_test, new1$fitted)[3],
    forecast::accuracy(RSSI_test, new2$fitted)[3],
    forecast::accuracy(RSSI_test, new4$fitted)[3],
    forecast::accuracy(RSSI_test, new3$fitted)[3]
  )
  
  # xts com os timestamps do TESTE (substitui data$timestamp e n)
  RSSI_test <- xts(RSSI_test, order.by = df_te$rdtimestamp)
  new1fit   <- xts(new1$fitted, order.by = df_te$rdtimestamp)
  new2fit   <- xts(new2$fitted, order.by = df_te$rdtimestamp)
  new3fit   <- xts(new3$fitted, order.by = df_te$rdtimestamp)
  new4fit   <- xts(new4$fitted, order.by = df_te$rdtimestamp)
  
  assign(paste0("result0", i),
         t(data.frame(MAE = MAE[i, ], MAPE = MAPE[i, ], RMSE = RMSE[i, ], COR = COR[i, ])))
}

print(cbind(order_arima, Xsig))


colnames(values)<-c("T","RH")
values<-as.data.frame(values)
ggplot(stack(values), aes(x = ind, y = values)) +
  geom_boxplot() +
  labs(title="",x="Weather parameter", 
       y = expression(paste(beta,"-coefficient estimates"))) +
  geom_hline(yintercept=0, linetype=2, 
             color = "grey0", size=.3)+
  theme(axis.title.y = element_text(color=1,size=15),
        axis.title.x = element_text(color=1,size=15),
        axis.text.x = element_text(color=1,size=15),
        axis.text.y = element_text(color=1,size=15),
        panel.background = element_rect(fill = "white", 
                                        colour = "black"))



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
print(result)



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

print(t(count)) 



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













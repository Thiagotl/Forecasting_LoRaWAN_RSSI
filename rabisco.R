data<-tinovi01_RSSI

data<-data[1:8759,]
n<-round(dim(data)[1]*.8) 
dim(data)[1]-n

datatrain<-cbind(data[1:n,])  
colnames(datatrain)<-paste0(colnames(datatrain),"_train")

datatest<-cbind(data[(n+1):(dim(data)[1]),])  
colnames(datatest)<-paste0(colnames(datatest),"_test")

attach(datatrain)
attach(datatest)
attach(data)

X<-cbind(datatrain$temperature_mean_train, datatrain$humidity_mean_train)
Xtest<-cbind(datatest$temperature_mean_test, datatest$humidity_mean_test)


fit <- auto.arima(datatrain$lora_rssi_mean_train, xreg = X, seasonal = TRUE)
summary(fit)
coeftest(fit)

fit2<-Arima(datatest$lora_rssi_mean_test, model=fit, xreg = Xtest)
summary(fit2)
coeftest(fit2)

####

X1<-cbind(datatrain$temperature_mean_train)
X1test<-cbind(datatest$temperature_mean_test)


fit11 <- auto.arima(datatrain$lora_rssi_mean_train, xreg = X1, seasonal = TRUE)
summary(fit11)
coeftest(fit11)

fit22<-Arima(datatest$lora_rssi_mean_test, model=fit11, xreg = X1test)
summary(fit22)
coeftest(fit22)

e_full  <- datatest$lora_rssi_mean_test - as.numeric(fit2$fitted)
e_best  <- datatest$lora_rssi_mean_test - as.numeric(fit22$fitted)

# Diebold–Mariano: requer 'forecast'
forecast::dm.test(e_full, e_best, alternative = "two.sided",
                  h = 1, power = 2)

library(rugarch)

rssit <- rssi_train_list$Tinovi01$rssi

temp <- rssi_train_list$Tinovi01$airtemp

hum <- rssi_train_list$Tinovi01$airhum

Xreg <- as.matrix(cbind(temp, hum))

best_fit_arfima <- autoarfima(rssit, ar.max = 2, ma.max = 2, 
                              criterion = "AIC",
                              method = "full", arfima = T,
                              external.regressors = Xreg,
                              solver = "solnp", return.all = FALSE)


best_fit_arfima
fdGPH(rssit, bandw.exp = 0.5)
fdSperio(rssit, bandw.exp = 0.5, beta = 0.9)
acf(rssit)
plot.ts(rssit)

tseries::adf.test(rssit)
tseries::pp.test(rssit)


y<-diff(rssit)
plot.ts(y)
tseries::adf.test(y)
# spec <- ugarchspec(
#   variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
#   mean.model     = list(
#     armaOrder = c(1,1),      # ordens AR e MA (ajuste conforme necessário)
#     include.mean = TRUE,
#     arfima = TRUE,           # ativa a diferença fracionária
#     external.regressors = as.matrix(temp)
#     ),
#   distribution.model = "std"  # escolha a distribuição (norm, std, sstd, etc.)
# )
# 
# fit <- ugarchfit(spec = spec, data = rssit)
# summary(fit)


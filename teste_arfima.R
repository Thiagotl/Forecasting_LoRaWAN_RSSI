
library(rugarch)

rssit <- rssi_train_list$Tinovi02$rssi

temp <- rssi_train_list$Tinovi02$airtemp

hum <- rssi_train_list$Tinovi02$airhum

Xreg <- as.matrix(cbind(temp, hum))

best_fit_arfima <- autoarfima(rssit, ar.max = 2, ma.max = 2, 
                              criterion = "AIC",
                              method = "full", arfima = T,
                              external.regressors = as.matrix(temp),
                              solver = "solnp", return.all = FALSE)


best_fit_arfima


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


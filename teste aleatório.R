
treino <-  rssi_train_list$Tinovi01$rssi

teste <- rssi_test_list$Tinovi01$rssi


X <- cbind(
  rssi_train_list$Tinovi01$airtemp,
  rssi_train_list$Tinovi01$airhum
  #rssi_train_list$Tinovi01$dum_Summer,
  #rssi_train_list$Tinovi01$dum_Autumn,
  #rssi_train_list$Tinovi01$dum_Winter
)

Xtest <- cbind(
  rssi_test_list$Tinovi01$airtemp,
  rssi_test_list$Tinovi01$airhum
  #rssi_test_list$Tinovi01$dum_Summer,
  #rssi_test_list$Tinovi01$dum_Autumn,
  #rssi_test_list$Tinovi01$dum_Winter
)

pacf(treino)


RSSI <- ts(treino, frequency = 24)
RSSI_teste <- ts(teste, frequency = 24)

a01 <- forecast::auto.arima(RSSI, xreg = X, allowdrift = FALSE)

coeftest(a01)

or <- arimaorder(a01)

new <- Arima(teste, xreg = Xtest, order = or, model = a01)


checkresiduals(a01)

h <- 24
h_i <- min(h, length(teste), nrow(Xtest)) 


autoplot(a01)

findfrequency(RSSI)
gglagplot(RSSI)
# -----------------------
# Forecast 24 passos (fora da amostra, sem atualizar)
# -----------------------
h <- 24
h_i <- min(h, length(teste), nrow(Xtest))

# forecast 24 passos (fora da amostra, sem atualizar)
fc <- forecast::forecast(a01, h = h_i, xreg = Xtest[seq_len(h_i), , drop = FALSE])

df_plot <- data.frame(
  step = 1:h_i,
  Observed = as.numeric(teste[seq_len(h_i)]),
  Forecast = as.numeric(fc$mean)
)

df_long <- tidyr::pivot_longer(df_plot, cols = c("Observed","Forecast"),
                               names_to = "series", values_to = "value")

ggplot(df_long, aes(x = step, y = value, color = series)) +
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  labs(title = "24 passos fora da amostra", x = "Passo à frente", y = "RSSI", color = NULL) +
  theme_bw()

autoplot(fc)

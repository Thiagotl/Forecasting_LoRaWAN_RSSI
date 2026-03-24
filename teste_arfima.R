
library(rugarch)


rssi_full_list <- purrr::map2(
  rssi_train_list,
  rssi_test_list,
  ~ dplyr::bind_rows(.x, .y)
)


results_lsm <- purrr::map(rssi_full_list, ~ tseries::kpss.test(.x[["rssi"]]))



df_results_lsm <- tibble::tibble(
  sensors = names(rssi_full_list),
  method  = purrr::map_chr(results_lsm, ~ .x$method),
  p_value = purrr::map_dbl(results_lsm, ~ unname(.x$p.value))
) |> 
  dplyr::mutate(
    result  = ifelse(p_value < 0.05, "No Stationary", "Stationary")
  )

print(tseries::adf.test(rssit))    # ADF: H0 = raiz unitária
print(tseries::kpss.test(rssit))   # KPSS: H0 = estacionária


purrr::map(rssi_full_list, ~ Acf(.x[["rssi"]]))


## ARFIMA MODEL ###----

rssit      <- rssi_train_list$Tinovi04$rssi
temp       <- rssi_train_list$Tinovi04$airtemp
hum        <- rssi_train_list$Tinovi04$airhum
Xreg       <- as.matrix(cbind(temp, hum))

rssitest   <- rssi_test_list$Tinovi04$rssi
temp_test  <- rssi_test_list$Tinovi04$airtemp
hum_test   <- rssi_test_list$Tinovi04$airhum
Xreg_test  <- as.matrix(cbind(temp_test, hum_test))

h <- length(rssitest)

y_all <- c(rssit, rssitest)
X_all <- rbind(Xreg, Xreg_test)

fracdiff::fracdiff(rssit)


acf(rssit, lag.max = 48)
pacf(rssit, lag.max = 48)


d_fdGPH <- fracdiff::fdGPH(rssit)    # GPH (Geweke-Porter-Hudak)
d_fdSperio <- fracdiff::fdSperio(rssit)  # Spectral regression
cat("\nEstimativa de d (GPH):", d_fdGPH$d, "\n")
cat("Estimativa de d (Sperio):", d_fdSperio$d, "\n")

y_all <- c(rssit, rssitest)
X_all <- rbind(Xreg, Xreg_test)
h <- length(rssitest)

arfima_model <- autoarfima(
  data = rssit,
  ar.max = 2,
  ma.max = 2,
  criterion = "AIC",
  method = "full",
  arfima = TRUE,
  external.regressors = Xreg,
  distribution.model = "norm",
  solver = "solnp",
  return.all = FALSE
)


best_ar <- arfima_model$fit@model$modelinc["ar"]
best_ma <- arfima_model$fit@model$modelinc["ma"]

spec_arfima <- arfimaspec(
  mean.model = list(
    armaOrder           = c(best_ar, best_ma),
    include.mean        = TRUE,
    arfima              = TRUE,
    external.regressors = X_all          
  ),
  distribution.model = "norm"
)


fit_arfima <- arfimafit(
  spec = spec_arfima,
  data = y_all,
  out.sample = h,
  solver = "solnp",
  fit.control = list(scale = 1)
)

fc_arfima <- arfimaforecast(
  fit_arfima,
  n.ahead = 1,
  n.roll = h - 1
)

pred_arfima <- as.numeric(fitted(fc_arfima)[1, ])

# =========================
# 4) ARFIMA-GARCH final
# =========================
spec_arfima_garch <- ugarchspec(
  variance.model = list(
    model      = "sGARCH",
    garchOrder = c(1, 1)
  ),
  mean.model = list(
    armaOrder           = c(best_ar, best_ma),
    include.mean        = TRUE,
    arfima              = TRUE,
    external.regressors = X_all         
  ),
  distribution.model = "std"
)

fit_arfima_garch <- ugarchfit(
  spec = spec_arfima_garch,
  data = y_all,
  out.sample = h,
  solver = "hybrid",
  fit.control = list(scale = 1)
)

fc_arfima_garch <- ugarchforecast(
  fit_arfima_garch,
  n.ahead = 1,
  n.roll = h - 1
)

pred_arfima_garch <- as.numeric(fitted(fc_arfima_garch)[1, ])

time_test <- rssi_test_list$Tinovi04$rdtimestamp

#time_test <- tail(rssi_test_list$Tinovi04$rdtimestamp, h)
#obs_test  <- tail(rssitest, h)

df_plot <- data.frame(
  time = time_test,
  Observed = rssitest,
  ARFIMA = pred_arfima,
  ARFIMA_GARCH = pred_arfima_garch
)

df_long <- reshape(
  df_plot,
  varying = c("Observed", "ARFIMA", "ARFIMA_GARCH"),
  v.names = "value",
  timevar = "series",
  times = c("Observed", "ARFIMA", "ARFIMA_GARCH"),
  direction = "long"
)


calc_metrics <- function(actual, predicted, model_name = "Modelo") {
  erro  <- actual - predicted
  mae   <- mean(abs(erro))
  mape  <- mean(abs(erro / actual)) * 100
  rmse  <- sqrt(mean(erro^2))
  
  cat("---------------------------------------------\n")
  cat(" Accuracy metrics —", model_name, "\n")
  cat("---------------------------------------------\n")
  cat("  MAE  :", round(mae,  4), "\n")
  cat("  MAPE :", round(mape, 4), "%\n")
  cat("  RMSE :", round(rmse, 4), "\n\n")
  
  invisible(data.frame(
    model = model_name,
    MAE   = mae,
    MAPE  = mape,
    RMSE  = rmse
  ))
}

metrics_arfima       <- calc_metrics(rssitest, pred_arfima,       "ARFIMA")
metrics_arfima_garch <- calc_metrics(rssitest, pred_arfima_garch, "ARFIMA-GARCH")

# Tabela comparativa
metrics_table <- rbind(metrics_arfima, metrics_arfima_garch)
cat("=============================================\n")
cat(" Models \n")
cat("=============================================\n")
print(metrics_table, row.names = FALSE)



ggplot(df_long, aes(x = time, y = value, color = series, linetype = series)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(
    values = c(
      "Observed" = "black",
      "ARFIMA" = "#CC0000",
      "ARFIMA_GARCH" = "#0018A8"
    ),
    labels = c(
      "Observed" = "Observed",
      "ARFIMA" = "ARFIMA",
      "ARFIMA_GARCH" = "ARFIMA-GARCH"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Observed" = "solid",
      "ARFIMA" = "longdash",
      "ARFIMA_GARCH" = "twodash"
    ),
    labels = c(
      "Observed" = "Observed",
      "ARFIMA" = "ARFIMA",
      "ARFIMA_GARCH" = "ARFIMA-GARCH"
    )
  ) +
  labs(
    title = "Observed vs Forecasts on the Test Set",
    x = "Time",
    y = "RSSI",
    color = NULL,
    linetype = NULL
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = c(0.85, 0.85),
    legend.background = element_blank(),
    legend.key = element_blank(),
    axis.title = element_text(face = "bold")
  )


## Residual analysis ----



res_arfima <- residuals(fit_arfima, standardize = TRUE)

Box.test(res_arfima, lag = 24, type = "Ljung-Box")
Box.test(res_arfima^2, lag = 24, type = "Ljung-Box")


acf(res_arfima,    main = "ACF — Residuals ARFIMA")
pacf(res_arfima,   main = "PACF — Residuals ARFIMA")
qqnorm(res_arfima); qqline(res_arfima, col = "red")


res_garch <- residuals(fit_arfima_garch, standardize = TRUE)

Box.test(res_garch, lag = 24, type = "Ljung-Box")
Box.test(res_garch^2, lag = 24, type = "Ljung-Box")

acf(res_garch,    main = "ACF — Residuals ARFIMA+GARCH")
pacf(res_garch,   main = "PACF — Residuals ARFIMA+GARCH")
qqnorm(res_garch); qqline(res_garch, col = "red")



rolling_forecast <- function(train_y, test_y, train_x = NULL,
                             test_x = NULL, fit_fn, ...) {
  n_test <- length(test_y)
  preds  <- numeric(n_test)
  
  for (i in seq_len(n_test)) {
    # Dados disponíveis até o momento i
    y_cur <- c(train_y, test_y[seq_len(i - 1)])
    
    x_cur <- if (!is.null(train_x)) {
      rbind(train_x,
            if (i > 1) test_x[seq_len(i - 1), , drop = FALSE]
            else NULL)
    } else NULL
    
    x_new <- if (!is.null(test_x)) {
      test_x[i, , drop = FALSE]
    } else NULL
    
    preds[i] <- fit_fn(y_cur, x_cur, x_new, ...)
  }
  preds
}


arima_sel <- auto.arima(
  rssit,
  xreg           = Xreg,
  seasonal       = FALSE,      # sem componente sazonal
  stepwise       = FALSE,      # busca completa
  approximation  = FALSE,
  ic             = "aic",
  max.p          = 5,
  max.q          = 5,
  max.d          = 2,
  trace          = TRUE
)
print(summary(arima_sel))

# Ordem selecionada
p_arima <- arima_sel$arma[1]
d_arima <- arima_sel$arma[6]
q_arima <- arima_sel$arma[2]


fit_arima <- function(y, x_train, x_new, p, d, q) {
  y_ts  <- ts(y, frequency = 24)
  model <- Arima(y_ts, order = c(p, d, q), xreg = x_train)
  as.numeric(forecast(model, h = 1, xreg = x_new)$mean)
}
coeftest(arima_sel)

pred_arima <- rolling_forecast(
  train_y = rssit,
  test_y  = rssitest,
  train_x = Xreg,
  test_x  = Xreg_test,
  fit_fn  = fit_arima,
  p = p_arima, d = d_arima, q = q_arima
)


sarima_sel <- auto.arima(
  rssit,
  xreg           = Xreg,
  seasonal       = TRUE,       # componente sazonal ativo
  stepwise       = FALSE,
  approximation  = FALSE,
  ic             = "aic",
  max.p          = 3,
  max.q          = 3,
  max.d          = 2,
  max.P          = 2,          # AR sazonal
  max.Q          = 2,          # MA sazonal
  max.D          = 1,          # diferença sazonal
  trace          = TRUE
)
print(summary(sarima_sel))

# Ordens selecionadas
p_s  <- sarima_sel$arma[1]; d_s  <- sarima_sel$arma[6]; q_s  <- sarima_sel$arma[2]
P_s  <- sarima_sel$arma[3]; D_s  <- sarima_sel$arma[7]; Q_s  <- sarima_sel$arma[4]


fit_sarima <- function(y, x_train, x_new, p, d, q, P, D, Q) {
  y_ts  <- ts(y, frequency = 24)
  model <- Arima(y_ts,
                 order    = c(p, d, q),
                 seasonal = list(order = c(P, D, Q), period = 24),
                 xreg     = x_train)
  as.numeric(forecast(model, h = 1, xreg = x_new)$mean)
}


pred_sarima <- rolling_forecast(
  train_y = rssit,
  test_y  = rssitest,
  train_x = Xreg,
  test_x  = Xreg_test,
  fit_fn  = fit_sarima,
  p = p_s, d = d_s, q = q_s,
  P = P_s, D = D_s, Q = Q_s
)


checkresiduals(arima_sel)
Box.test(residuals(arima_sel), lag = 24, type = "Ljung-Box")
Box.test(residuals(arima_sel)^2, lag = 24, type = "Ljung-Box")

checkresiduals(sarima_sel)
Box.test(residuals(sarima_sel), lag = 24, type = "Ljung-Box")
Box.test(residuals(sarima_sel)^2, lag = 24, type = "Ljung-Box")


metrics_arima  <- calc_metrics(rssitest, pred_arima,  "ARIMA")
metrics_sarima <- calc_metrics(rssitest, pred_sarima, "SARIMA")


all_metrics <- list()

if (exists("metrics_arima"))        all_metrics[["ARIMA"]]        <- metrics_arima
if (exists("metrics_sarima"))       all_metrics[["SARIMA"]]       <- metrics_sarima
if (exists("metrics_arfima"))       all_metrics[["ARFIMA"]]       <- metrics_arfima
if (exists("metrics_arfima_garch")) all_metrics[["ARFIMA-GARCH"]] <- metrics_arfima_garch

metrics_all <- do.call(rbind, all_metrics)

print(metrics_all[order(metrics_all$RMSE), ], row.names = FALSE)



# =============================================================================
# Comparative plot — ggplot2
# =============================================================================

# Build data.frame with available forecasts
idx <- seq_along(rssitest)

df <- data.frame(Index = idx, Observed = rssitest)

if (exists("pred_arima"))        df$ARIMA          <- pred_arima
if (exists("pred_sarima"))       df$SARIMA         <- pred_sarima
if (exists("pred_arfima"))       df$ARFIMA         <- pred_arfima
if (exists("pred_arfima_garch")) df$`ARFIMA-GARCH` <- pred_arfima_garch

# Long format for ggplot
df_long <- df |>
  pivot_longer(-Index, names_to = "Model", values_to = "RSSI") |>
  mutate(
    Model = factor(Model, levels = c("Observed", "ARIMA", "SARIMA",
                                     "ARFIMA", "ARFIMA-GARCH"))
  )

# Colour palette, line types and widths
colours <- c(
  "Observed"     = "#000000",
  "ARIMA"        = "#0057FF",
  "SARIMA"       = "#FF6B00",
  "ARFIMA"       = "#00A82D",
  "ARFIMA-GARCH" = "#CC0000"
)

linetypes <- c(
  "Observed"     = "solid",
  "ARIMA"        = "dashed",
  "SARIMA"       = "dotdash",
  "ARFIMA"       = "dotted",
  "ARFIMA-GARCH" = "longdash"
)

linewidths <- c(
  "Observed"     = 1.4,
  "ARIMA"        = 1.1,
  "SARIMA"       = 1.1,
  "ARFIMA"       = 1.1,
  "ARFIMA-GARCH" = 1.1
)

ggplot(df_long, aes(x = Index, y = RSSI,
                    colour    = Model,
                    linetype  = Model,
                    linewidth = Model)) +
  geom_line() +
  scale_colour_manual(values = colours,      breaks = levels(df_long$Model)) +
  scale_linetype_manual(values = linetypes,  breaks = levels(df_long$Model)) +
  scale_linewidth_manual(values = linewidths, breaks = levels(df_long$Model)) +
  labs(
    title     = "One-Step-Ahead Rolling Forecast — Tinovi04",
    subtitle  = "Model comparison | Hourly RSSI series",
    x         = "Index",
    y         = "RSSI",
    colour    = "Model",
    linetype  = "Model",
    linewidth = "Model"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(colour = "grey40", size = 11),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    legend.key.width = unit(1.8, "cm"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey92")
  ) +
  guides(
    colour    = guide_legend(nrow = 1),
    linetype  = guide_legend(nrow = 1),
    linewidth = guide_legend(nrow = 1)
  )



#### 24 passos ----
h_plot <- 24

df <- data.frame(
  Time = tail(rssi_test_list$Tinovi04$rdtimestamp, h_plot),
  Observed = tail(rssitest, h_plot)
)

if (exists("pred_sarima"))       df$SARIMA         <- tail(pred_sarima, h_plot)
if (exists("pred_arfima"))       df$ARFIMA         <- tail(pred_arfima, h_plot)
if (exists("pred_arfima_garch")) df$`ARFIMA-GARCH` <- tail(pred_arfima_garch, h_plot)

df_long <- df |>
  pivot_longer(-Time, names_to = "Model", values_to = "RSSI") |>
  mutate(
    Model = factor(Model, levels = c("Observed", "SARIMA",
                                     "ARFIMA", "ARFIMA-GARCH"))
  )

colours <- c(
  "Observed"     = "#000000",
  "SARIMA"       = "#FF6B00",
  "ARFIMA"       = "#00A82D",
  "ARFIMA-GARCH" = "#CC0000"
)

linetypes <- c(
  "Observed"     = "solid",
  "SARIMA"       = "dotdash",
  "ARFIMA"       = "dotted",
  "ARFIMA-GARCH" = "longdash"
)

linewidths <- c(
  "Observed"     = 1.4,
  "SARIMA"       = 1.1,
  "ARFIMA"       = 1.1,
  "ARFIMA-GARCH" = 1.1
)

ggplot(df_long, aes(x = Time, y = RSSI,
                    colour = Model,
                    linetype = Model,
                    linewidth = Model)) +
  geom_line() +
  scale_colour_manual(values = colours, breaks = levels(df_long$Model)) +
  scale_linetype_manual(values = linetypes, breaks = levels(df_long$Model)) +
  scale_linewidth_manual(values = linewidths, breaks = levels(df_long$Model)) +
  labs(
    title = "One-Step-Ahead Rolling Forecast — Tinovi04",
    #subtitle = "Last 24 observations",
    x = "Time",
    y = "RSSI",
    colour = "Model",
    linetype = "Model",
    linewidth = "Model"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(colour = "grey40", size = 11),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.width = unit(1.8, "cm"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey92")
  ) +
  guides(
    colour = guide_legend(nrow = 1),
    linetype = guide_legend(nrow = 1),
    linewidth = guide_legend(nrow = 1)
  )



# library(arfima)
# 
# fit_arfima <- arfima(
#   z = rssit,
#   order = c(2, 0, 2),
#   xreg = Xreg,
#   dmean = FALSE,
#   back = TRUE,
#   lmodel = list(dfixed = 0.4999)
# )
# 
# 
# bestModes(fit_arfima, 1)
# 
# 
# summary(fit_arfima)
# 
# modo1 <- fit_arfima$modes[[1]]
# modo1$dfrac
# 
# 
# str(modo1)
# 
# save(modo1, fit_arfima, file = "arfima_resultados.RData")

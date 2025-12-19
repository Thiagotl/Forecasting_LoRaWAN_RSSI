
library(forecast)
library(tsibble)
library(feasts)
#library(ggseas)  devtools::install_github("ellisp/ggseas/pkg") # para visualização de séries temporais

locale_original <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "English")

#Sys.setlocale("LC_TIME", locale_original) # - Restaurar localização original (opcional)


### Adjusts ----
dados <- sensors_hour_train |>
  mutate(
    hours = hour(rdtimestamp ),          
    week_day = wday(rdtimestamp, label = TRUE, abbr = TRUE, week_start = 1),
    week_day_num = lubridate::wday(rdtimestamp, week_start = 1),
    #weeks = week(rdtimestamp ),        
    months = month(rdtimestamp, label = TRUE, abbr = TRUE),
    months_num = month(rdtimestamp),
    day_month = day(rdtimestamp),
    
    season = case_when(
      months_num %in% c(12, 1, 2)  ~ "Winter",
      months_num %in% c(3, 4, 5)   ~ "Spring",
      months_num %in% c(6, 7, 8)   ~ "Summer",
      months_num %in% c(9, 10, 11) ~ "Fall"
    ),
    season = factor(season, levels = c("Winter","Spring","Summer","Fall")),
    
    day_period = case_when(
      hours >= 6  & hours < 12 ~ "Morning (6–11h)",
      hours >= 12 & hours < 18 ~ "Afternoon (12–17h)",
      hours >= 18 & hours < 24 ~ "Evening (18–23h)",
      TRUE                     ~ "Night (0–5h)"
    ),
    day_period = factor(
      day_period,
      levels = c("Night (0–5h)", "Morning (6–11h)", 
                 "Afternoon (12–17h)", "Evening (18–23h)")
    )
    
  ) |> filter(nodeid == "tinovi-06") #milesight-0

#dados <- tinovi01_RSSI_train[-c(1),]

ggplot(dados, aes(x = week_day, y = rssi, fill = week_day)) + # mesma coisa do boxplot de cima
  geom_boxplot(alpha = 0.7) +
  scale_fill_viridis_d(option = "plasma") +
  labs(
    title = "RSSI Distribution by Day of the Week",
    #subtitle = "Dados coletados a cada 15 minutos durante um ano",
    x = "Weekdays",
    y = "RSSI Values - tinovi-01",
    fill = "Day"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none" # Remove legenda se quiser
  )


ggplot(dados, aes(x = months, y = rssi, fill = months)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_viridis_d(option = "plasma") +
  labs(
    title = "RSSI Distribution by Month",
    #subtitle = "Análise de sazonalidade mensal",
    x = "Months",
    y = " RSSI Values - Milesight 02"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none" # Remove legenda se quiser
  )


p6 <- ggplot(dados, aes(x = factor(hours), y = rssi)) +
  geom_boxplot(fill = "lightgreen") +
  labs(
    title = "RSSI Distribution by hour",
    x = "Hours (0-23)",
    y = "RSSI Values - Tinovi 06"
  ) +
  theme_minimal()


# Boxplot colorido por estação
ggplot(dados, aes(x = months, y = rssi, fill = season)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("lightblue", "lightgreen", "gold", "orange")) +
  labs(
    title = "RSSI Distribution by Season",
    #subtitle = "Dados coletados na Itália (hemisfério norte)",
    x = "Months",
    y = "RSSI Values - Milesight 02",
    fill = "Season"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Boxplot facetado muito bommmmmmm
ggplot(dados, aes(x = week_day, y = rssi, fill = day_period)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ day_period, ncol = 2) +
  scale_fill_viridis_d(option = "plasma") +
  labs(
    title = "RSSI Distribution: Month × Time of Day",
    subtitle = "Monthly Seasonality Analysis and Daily Patterns",
    x = "Weekdays",
    y = "RSSI Values - Tinovi 3",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

### Graphs ----


library(cowplot)

plots <- list(p1, p2, p3, p4, p5, p6, p7, p8)


# estacao do ano
leg <- get_legend(
  plots[[1]] + theme(legend.position = "bottom")
)

# 2) remove a legenda de todos - estacao do ano
plots_noleg <- lapply(plots, \(p) p + theme(legend.position = "none"))



plots <- lapply(plots, \(p) p + theme(
  plot.margin = margin(2, 2, 2, 2, "mm"),
  plot.title  = element_text(size = 10),
  axis.title  = element_text(size = 9),
  axis.text   = element_text(size = 8),
  legend.position = "none"
))


# 3) monta a grade 4x2 - estacao do ano

grid <- plot_grid(plotlist = plots_noleg, ncol = 4)

grid <- plot_grid(
  plotlist = plots,
  ncol = 4
  )


final <- plot_grid(
  grid, leg,
  ncol = 1,
  rel_heights = c(1, 0.10)  # ajuste 0.08~0.15 se precisar
)



ggsave(
  "rssi_hours_plot.pdf",
  grid,
  width  = 11.69,
  height = 8.27,
  units  = "in",
  dpi    = 300
)


###
dados_ts <- dados %>%
  as_tsibble(index = rdtimestamp)
dados_ts <- dados %>%
  as_tsibble(index = rdtimestamp) |> fill_gaps(.full = TRUE)

# esse aqui é interessante
dados_ts %>%
  gg_subseries(y = rssi, period = "1d") +
  labs(
    title = "",
    subtitle = "",
    y = "RSSI"
  )

dados_ts %>%
  gg_season(y = rssi, period = "1m") +
  labs(
    title = "",
    y = "RSSI",
    x = "hour"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 23, by = 3))




ts_data <- ts(dados$rssi,
              frequency = 24)

dec <- decompose(ts_data)
autoplot(dec)

# Opção 1: Decomposição com duas sazonalidades
decomp <- mstl(ts_data,
               s.window = "periodic",
               iterate = 2)

# Opção 2: Especificar múltiplas frequências explicitamente
decomp <- mstl(ts_data,
               lambda = NULL,  
               s.window = 24,  
               iterate = 2)    

autoplot(decomp)

monthplot(ts_data,choice = "seasonal")

y <- msts(ts_data, seasonal.periods = c(24, 168))
fit <- mstl(
  y,
  s.window = c(13, 13),      
  s.degree = 1,
  t.degree = 1,
  robust = TRUE,
  inner = 2,
  outer = 1
)
autoplot(fit)


###  another tests----

library(xgboost)
library(tidymodels)
library(modeltime)
library(tidyverse)
library(timetk)

rssi_train <- sensors_split$RSSI_01$train |>
  arrange(rdtimestamp) |>
  select(
    rdtimestamp, rssi,
    soiltemp, soilhum,
    season_spring, season_summer, season_autumn
  )

rssi_test <- sensors_split$RSSI_01$test |>
  arrange(rdtimestamp) |>
  select(
    rdtimestamp, rssi,
    soiltemp, soilhum,
    season_spring, season_summer, season_autumn
  )

rssi_train  |> 
  plot_time_series(rdtimestamp, rssi, .interactive = TRUE)

# ARIMA
model_fit_arima_xreg <- arima_reg() |>
  set_engine("auto_arima") |>
  fit(
    rssi ~ rdtimestamp+soiltemp + soilhum +
      season_spring + season_summer + season_autumn,
    data = rssi_train)

rec_ml <- recipe(
  rssi ~ rdtimestamp +
    soiltemp + soilhum +
    season_spring + season_summer + season_autumn,
  data = rssi_train
) |>
  # Cria assinatura temporal (ano, mês, dia da semana, hora, etc.)
  step_timeseries_signature(rdtimestamp) |>
  # Remove colunas de data que não interessam ou que podem dar colinearidade forte
  step_rm(contains("iso"), contains("xts"), contains("minute"),
          contains("second"), contains("am.pm")) |>
  # Normaliza preditores (para XGBoost ajuda; RF não precisa, mas não atrapalha)
  step_normalize(all_numeric_predictors(), -all_outcomes())

# Random Forest
spec_rf <- rand_forest(
  mode  = "regression",
  trees = 500
) |>
  set_engine("ranger")

wflw_rf <- workflow() |>
  add_model(spec_rf) |>
  add_recipe(rec_ml) |>
  fit(data = rssi_train)

# arima xboost

model_spec_arima_boost <- arima_boost(
  # hiperparâmetros do XGBoost
  trees      = 10,
  learn_rate = 0.05,
  tree_depth = 6,
  min_n      = 10
  # (pode deixar seasonal_period = "auto" implícito)
) |>
  # usa Auto ARIMA + XGBoost Errors
  set_engine("auto_arima_xgboost")


model_fit_arima_boost <- model_spec_arima_boost |>
  fit(
    rssi ~ rdtimestamp +
      soiltemp + soilhum +
      season_spring + season_summer + season_autumn,
    data = rssi_train
  )


models_tbl <- modeltime_table(
  model_fit_arima_xreg,  
  wflw_rf,
  model_fit_arima_boost
) |>
  update_model_description(1, "ARIMA+ALL") |>
  update_model_description(2, "Random Forest ") |> 
  update_model_description(3, "ARIMA+XBoost")


calibration_tbl <- models_tbl |>
  modeltime_calibrate(new_data = rssi_test)

accuracy_tbl <- calibration_tbl |>
  modeltime_accuracy() |>
  arrange(rmse)

accuracy_tbl

forecast_tbl <- calibration_tbl |>
  modeltime_forecast(
    new_data    = rssi_test,
    actual_data = bind_rows(rssi_train, rssi_test)
  )

forecast_tbl |>
  plot_modeltime_forecast(.interactive = TRUE)





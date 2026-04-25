
### PLOTS ----

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
    
  ) |> filter(nodeid == "milesight-02") #milesight-0

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


p <- ggplot(dados, aes(x = factor(hours), y = rssi)) +
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
p8 <- ggplot(dados, aes(x = months, y = rssi, fill = day_period)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ day_period, ncol = 2) +
  scale_fill_viridis_d(option = "plasma") +
  labs(
    #title = "RSSI Distribution: Month × Time of Day",
    #subtitle = "Monthly Seasonality Analysis and Daily Patterns",
    x = "Months",
    y = "RSSI Values - Milesight 02",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

### Graphs ----


library(cowplot)

plots <- list(p1, p2, p3, p4)
plots2 <- list(p5, p6, p7, p8)


# estacao do ano
leg <- get_legend(
  plots[[1]] + theme(legend.position = "bottom")
)

# 2) remove a legenda de todos - estacao do ano
plots_noleg <- lapply(plots, \(p) p + theme(legend.position = "none"))



plots <- lapply(plots2, \(p) p + theme(
  plot.margin = margin(2, 2, 2, 2, "mm"),
  plot.title  = element_text(size = 8),
  axis.title  = element_text(size = 9),
  axis.text   = element_text(size = 8),
  legend.position = "none"
))


# 3) monta a grade 4x2 - estacao do ano

grid <- plot_grid(plotlist = plots, ncol = 4)

grid <- plot_grid(
  plotlist = plots
  )


final <- plot_grid(
  grid, leg,
  ncol = 1,
  rel_heights = c(1, 0.10)  # ajuste 0.08~0.15 se precisar
)



ggsave(
  "plots2_months.pdf",
  grid,
  width  = 10,
  height = 12,
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


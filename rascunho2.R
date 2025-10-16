# ============================
# 0) Pacotes
# ============================

library(purrr)
library(forecast)
library(xts)
library(abind)
library(tidyverse)


### Data preparation----
sensors <- readr::read_delim("dataset_sensors.csv", 
                             delim = ",", escape_double = FALSE, trim_ws = TRUE) |> 
  dplyr::mutate(timestamp= as.POSIXct(timestamp, tz = "GMT",
                                      origin="1970-01-01 00:00:00")) 


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
# ==========================================================
# 1) Helpers para unir lateralmente (full join por time_hour)
# ==========================================================
# função auxiliar: pega um df (train/test) de um sensor e renomeia as colunas
# adicionando _<tag> (ex.: _RSSI_01), mantendo apenas a chave time_hour
rename_for_tag <- function(df, tag) {
  key <- "time_hour"
  measure_cols <- setdiff(names(df), c("nodeid", key))
  df %>%
    select(all_of(c(key, measure_cols))) %>%
    rename_with(~ paste0(.x, "_", tag), all_of(measure_cols))
}

# junta (full join) uma lista nomeada de dfs já renomeados por time_hour
reduce_full_by_time <- function(lst) {
  reduce(lst, ~ full_join(.x, .y, by = "time_hour")) %>%
    arrange(time_hour)
}

# =================================================================
# 2) Geração de train_wide e test_wide a partir de sensors_split
#    (sensors_split deve existir no ambiente, como você já tem)
# =================================================================
train_wide <-
  sensors_split %>%
  imap(~ rename_for_tag(.x$train, .y)) %>%   # .y é o nome (RSSI_01, ...)
  reduce_full_by_time()

test_wide  <-
  sensors_split %>%
  imap(~ rename_for_tag(.x$test, .y)) %>%
  reduce_full_by_time()

# Verifique os nomes criados (ex.: temperature_mean_RSSI_01, humidity_mean_RSSI_01, lora_rssi_mean_RSSI_01, etc.)
# names(train_wide); names(test_wide)

# ==========================================================
# 3) Empilha train/test em 'data' e define n (tamanho do treino)
# ==========================================================
stopifnot("time_hour" %in% names(train_wide), "time_hour" %in% names(test_wide))
train_wide <- arrange(train_wide, time_hour)
test_wide  <- arrange(test_wide,  time_hour)

n    <- nrow(train_wide)
data <- bind_rows(train_wide, test_wide)

# ==========================================================
# 4) Mapeamento de nomes e utilitários ARIMAX
#    (covariáveis: SOMENTE temperatura e umidade)
# ==========================================================
sensor_tags <- sprintf("RSSI_%02d", 1:8)

col_y <- function(tag) paste0("lora_rssi_mean_", tag)


col_xs <- function(tag) c(
  paste0("temperature_mean_", tag),
  paste0("humidity_mean_"   , tag)
)

# checa existência das colunas e cria NA se faltar (mantém alinhamento e NAs)
ensure_cols <- function(df, cols) {
  miss <- setdiff(cols, names(df))
  if (length(miss)) df[miss] <- NA_real_
  df[cols]
}

# helper p-valor para coeficientes do ARIMA (z = beta / se)
pvals_from_fit <- function(fit) {
  vc <- tryCatch(vcov(fit), error = function(e) NULL)
  if (is.null(vc)) return(rep(NA_real_, length(coef(fit))))
  se <- sqrt(diag(vc))
  z  <- coef(fit) / se
  2 * pnorm(abs(z), lower.tail = FALSE)
}

# ==========================================================
# 5) Ajustes por sensor e métricas
# ==========================================================
order_arima <- matrix(NA, 8, 3)
MAE <- MAPE <- RMSE <- COR <- matrix(NA, 8, 4)
colnames(MAE) <- colnames(MAPE) <- colnames(RMSE) <- colnames(COR) <-
  c("ARIMA-COV","ARIMA-COV*","ARIMA-COV**","ARIMA")
rownames(MAPE) <- rownames(RMSE) <- rownames(order_arima) <- rownames(COR) <- sensor_tags
Xsig <- values <- sinal <- matrix("", 8, 4)
rownames(Xsig) <- sensor_tags

for (i in seq_along(sensor_tags)) {
  tag <- sensor_tags[i]
  
  # Train and test sets
  y_tr <- ensure_cols(train_wide, col_y(tag))[[1]]
  y_te <- ensure_cols(test_wide,  col_y(tag))[[1]]
  
  # Covariates
  X_tr_raw <- ensure_cols(train_wide, col_xs(tag))
  X_te_raw <- ensure_cols(test_wide,  col_xs(tag))
  
  # Remove col with NA's
  keep_tr <- colSums(!is.na(X_tr_raw)) > 0
  keep_te <- colSums(!is.na(X_te_raw)) > 0
  keep    <- keep_tr | keep_te
  X_tr    <- as.matrix(X_tr_raw[, keep, drop = FALSE])
  X_te    <- as.matrix(X_te_raw[, keep, drop = FALSE])
  
  # rótulos legíveis das covariáveis
  xlabels <- c("T","RH")[keep]
  
  # --- Ajustes ---------------------------------------------------------------
  a01 <- auto.arima(y_tr, xreg = X_tr, allowdrift=FALSE)
  
  # significância das covariáveis no a01
  pv <- pvals_from_fit(a01)
  k  <- ncol(X_tr)
  sel_idx <- if (k > 0) tail(seq_along(coef(a01)), k) else integer(0)
  
  tcoef_logical <- logical(0)
  if (length(sel_idx)) {
    tcoef_logical <- pv[sel_idx] < 0.05
  }
  
  Xnew_tr <- if (any(tcoef_logical)) X_tr[, tcoef_logical, drop = FALSE] else NULL
  Xnew_te <- if (any(tcoef_logical)) X_te[, tcoef_logical, drop = FALSE] else NULL
  
  order_arima[i, ] <- arimaorder(a01)
  
  # registra quais variáveis ficaram significativas
  sig_names <- if (length(tcoef_logical)) xlabels[tcoef_logical] else character(0)
  Xsig[i, 1:length(sig_names)] <- sig_names
  
  # sinal/valor dos coeficientes significativos
  if (length(sel_idx)) {
    coefs_x <- coef(a01)[sel_idx]
    if (any(tcoef_logical)) {
      vals   <- coefs_x[tcoef_logical]
      signs  <- ifelse(vals < 0, "-", "+")
      values[i, 1:length(vals)] <- round(vals, 4)
      sinal[i,  1:length(vals)] <- signs
    }
  }
  
  # Modelos derivados
  a02 <- Arima(y_tr, order = arimaorder(a01), xreg = Xnew_tr)  # só sig.
  a03 <- Arima(y_tr, order = arimaorder(a01))                   # sem xreg
  
  # "covariável única escolhida" = primeira coluna de X (se existir)
  X1_tr <- if (ncol(X_tr) >= 1) X_tr[, 1, drop = FALSE] else NULL
  X1_te <- if (ncol(X_te) >= 1) X_te[, 1, drop = FALSE] else NULL
  a04   <- Arima(y_tr, order = arimaorder(a01), xreg = X1_tr)
  
  # Previsão one-step-ahead no conjunto de teste
  new1 <- Arima(y_te, xreg = X_te,   model = a01)
  new2 <- Arima(y_te, xreg = Xnew_te, model = a02)
  new3 <- Arima(y_te,                  model = a03)
  new4 <- Arima(y_te, xreg = X1_te,    model = a04)
  
  # Métricas (MAPE, RMSE, COR, MAE)
  MAPE[i, ] <- c(accuracy(y_te, new1$fitted)[5],
                 accuracy(y_te, new2$fitted)[5],
                 accuracy(y_te, new4$fitted)[5],
                 accuracy(y_te, new3$fitted)[5])
  
  RMSE[i, ] <- c(accuracy(y_te, new1$fitted)[2],
                 accuracy(y_te, new2$fitted)[2],
                 accuracy(y_te, new4$fitted)[2],
                 accuracy(y_te, new3$fitted)[2])
  
  COR[i, ]  <- c(cor(y_te, new1$fitted, use = "complete.obs"),
                 cor(y_te, new2$fitted, use = "complete.obs"),
                 cor(y_te, new4$fitted, use = "complete.obs"),
                 cor(y_te, new3$fitted, use = "complete.obs"))
  
  MAE[i, ]  <- c(accuracy(y_te, new1$fitted)[3],
                 accuracy(y_te, new2$fitted)[3],
                 accuracy(y_te, new4$fitted)[3],
                 accuracy(y_te, new3$fitted)[3])
  
  # Séries em xts (opcional, para gráficos)
  tt_test <- test_wide$time_hour
  RSSI_test_xts <- xts(y_te, order.by = tt_test)
  new1fit_xts   <- xts(new1$fitted, order.by = tt_test)
  new2fit_xts   <- xts(new2$fitted, order.by = tt_test)
  new3fit_xts   <- xts(new3$fitted, order.by = tt_test)
  new4fit_xts   <- xts(new4$fitted, order.by = tt_test)
  
  assign(paste0("result0", i),
         t(data.frame(MAE = MAE[i,], MAPE = MAPE[i,], RMSE = RMSE[i,], COR = COR[i,])))
}

print(cbind(order_arima, Xsig))  # Tabela de ordens + covariáveis significativas

# ==========================================================
# 6) Pós-processamento: difs % vs ARIMA e contagem de vitórias
# ==========================================================
MAE_AUM  <- (MAE[,4]  - MAE[,1:3])  / MAE[,4]
M_AUM    <- (MAPE[,4] - MAPE[,1:3]) / MAPE[,4]
RMSE_AUM <- (RMSE[,4] - RMSE[,1:3]) / RMSE[,4]
COR_AUM  <- (COR[,1:3] - COR[,4])   / COR[,4]

# Tabela V (por sensor)
result <- cbind(
  result01,
  rbind(MAE_AUM[1,], M_AUM[1,], RMSE_AUM[1,], COR_AUM[1,]) * 100
)
for (i in 2:8) {
  r <- cbind(
    get(paste0("result0", i)),
    rbind(MAE_AUM[i,], M_AUM[i,], RMSE_AUM[i,], COR_AUM[i,]) * 100
  )
  result <- abind::abind(result, r, along = 1)
}
print(result, digits = 3)  # TABLE V

# Tabela VI (contagem de vitórias)
count <- apply(
  cbind(
    apply(result01[1:3,], 1, rank) == 1,   # MAE/MAPE/RMSE -> menor é melhor
    COR = rank(result01[4,]) == 4          # COR -> maior é melhor
  ),
  1, sum
)
for (i in 2:8) {
  r <- get(paste0("result0", i))
  r <- apply(
    cbind(
      apply(r[1:3,], 1, rank) == 1,
      COR = rank(r[4,]) == 4
    ),
    1, sum
  )
  count <- abind::abind(count, r, along = 2)
}
count <- abind::abind(count, apply(count, 1, sum), along = 2)
colnames(count) <- c(sensor_tags, "Overall")
print(t(count))  # TABLE VI

# ==========================================================
# 7) Diferença % ARIMAX_all (coluna 3) vs ARIMAX_Temp (coluna 1)
#    pronto para ggplot
# ==========================================================
MAE_all <- data.frame(
  values = c(
    (MAE[,3]  - MAE[,1])  / MAE[,3],
    (MAPE[,3] - MAPE[,1]) / MAPE[,3],
    (RMSE[,3] - RMSE[,1]) / RMSE[,3],
    (COR[,1]  - COR[,3])  / COR[,3]
  ) * 100,
  measure = c(rep("MAE",  8), rep("MAPE", 8), rep("RMSE", 8), rep("COR", 8)),
  model   = rep(sensor_tags, 4)
) %>%
  mutate(
    plot_text = dplyr::case_when(
      values < 0 ~ 0.2,
      values > 0 ~ values + 0.2,
      TRUE       ~ 0.2
    )
  )

# MAE_all agora está pronto para o gráfico
# Exemplo (opcional):
# library(ggplot2)
# ggplot(MAE_all, aes(x = model, y = values, fill = measure)) +
#   geom_bar(stat = "identity", position = "dodge")

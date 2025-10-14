library(dplyr)
library(purrr)
library(tidyr)

# função auxiliar: pega um df (train/test) de um sensor e renomeia as colunas
# adicionando _<tag> (ex.: _RSSI_01), mantendo apenas a chave time_hour
rename_for_tag <- function(df, tag) {
  # chave para união
  key <- "time_hour"
  # quais colunas de medidas manter? (todas menos nodeid e a chave)
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

# --- TRAIN --------------------------------------------------------------------
train_wide <-
  sensors_split %>%
  imap(~ rename_for_tag(.x$train, .y)) %>%   # .y é o nome (RSSI_01, ...)
  reduce_full_by_time()

# --- TEST ---------------------------------------------------------------------
test_wide  <-
  sensors_split %>%
  imap(~ rename_for_tag(.x$test, .y)) %>%
  reduce_full_by_time()

# Verifique os nomes criados (ex.: temperature_mean_RSSI_01, humidity_mean_RSSI_01, lora_rssi_mean_RSSI_01, etc.)
names(train_wide)
names(test_wide)


library(dplyr)
library(purrr)
library(tidyr)
library(forecast)
library(xts)

# -----------------------------------------------------------
# 1) Empilha train/test em um único 'data' e define n
# -----------------------------------------------------------
stopifnot("time_hour" %in% names(train_wide), "time_hour" %in% names(test_wide))
train_wide <- arrange(train_wide, time_hour)
test_wide  <- arrange(test_wide,  time_hour)

n    <- nrow(train_wide)
data <- bind_rows(train_wide, test_wide)

# -----------------------------------------------------------
# 2) Utilitários e mapeamento de nomes
#    (usamos covariáveis por sensor: temp, hum, snr)
# -----------------------------------------------------------
sensor_tags <- sprintf("RSSI_%02d", 1:8)

col_y   <- function(tag) paste0("lora_rssi_mean_", tag)
col_xs  <- function(tag) c(
  paste0("temperature_mean_", tag),
  paste0("humidity_mean_"   , tag),
  paste0("lora_snr_mean_"   , tag)   # opcional como covariável extra
)

# checa existência das colunas e cria NA se faltar (mantém alinhamento e NAs)
ensure_cols <- function(df, cols) {
  miss <- setdiff(cols, names(df))
  if (length(miss)) df[miss] <- NA_real_
  df[cols]
}

# -----------------------------------------------------------
# 3) Constrói objetos compatíveis com seu código legado
#    (RSSI_0i_train/test e X/Xtest DENTRO do loop)
# -----------------------------------------------------------

# Tabelas-name para resultados (mesma forma do seu script)
order_arima <- matrix(NA, 8, 3)
MAE <- MAPE <- RMSE <- COR <- matrix(NA, 8, 4)
colnames(MAE) <- colnames(MAPE) <- colnames(RMSE) <- colnames(COR) <-
  c("ARIMA-COV","ARIMA-COV*","ARIMA-COV**","ARIMA")
rownames(MAPE) <- rownames(RMSE) <- rownames(order_arima) <- rownames(COR) <- sensor_tags
Xsig <- values <- sinal <- matrix("", 8, 4)
rownames(Xsig) <- sensor_tags

# helper p-valor para coeficientes do ARIMA (z = beta / se)
pvals_from_fit <- function(fit) {
  vc <- tryCatch(vcov(fit), error = function(e) NULL)
  if (is.null(vc)) return(rep(NA_real_, length(coef(fit))))
  se <- sqrt(diag(vc))
  z  <- coef(fit) / se
  2 * pnorm(abs(z), lower.tail = FALSE)
}

for (i in seq_along(sensor_tags)) {
  tag <- sensor_tags[i]
  
  # --- Série alvo (treino/teste) --------------------------------------------
  y_tr <- ensure_cols(train_wide, col_y(tag))[[1]]
  y_te <- ensure_cols(test_wide,  col_y(tag))[[1]]
  
  # --- Covariáveis por sensor (temp, hum, snr) ------------------------------
  X_tr_raw <- ensure_cols(train_wide, col_xs(tag))
  X_te_raw <- ensure_cols(test_wide,  col_xs(tag))
  
  # remove colunas completamente NA (mantém o “sem excluir dados”: linhas continuam)
  keep_tr <- colSums(!is.na(X_tr_raw)) > 0
  keep_te <- colSums(!is.na(X_te_raw)) > 0
  keep    <- keep_tr | keep_te
  X_tr    <- as.matrix(X_tr_raw[, keep, drop = FALSE])
  X_te    <- as.matrix(X_te_raw[, keep, drop = FALSE])
  
  # nomes legíveis das covariáveis
  xlabels <- c("T","RH","SNR")[keep]
  
  # --- Ajustes ---------------------------------------------------------------
  # auto.arima com todas covariáveis disponíveis
  a01 <- auto.arima(y_tr, xreg = X_tr, allowdrift = FALSE)
  
  # significância das covariáveis no a01
  pv <- pvals_from_fit(a01)
  # pega apenas a cauda das covariáveis (últimos k coef são de xreg)
  k  <- ncol(X_tr)
  sel_idx <- if (k > 0) {
    tail(seq_along(coef(a01)), k)
  } else integer(0)
  
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
  
  # Previsão one-step-ahead no conjunto de teste (como no seu código)
  new1 <- Arima(y_te, xreg = X_te,   model = a01)
  new2 <- Arima(y_te, xreg = Xnew_te, model = a02)
  new3 <- Arima(y_te,                  model = a03)
  new4 <- Arima(y_te, xreg = X1_te,    model = a04)
  
  # Métricas (MAPE, RMSE, COR, MAE) na ordem do seu script
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
  
  # Série temporal (se quiser em xts para plot posterior)
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

library(dplyr)
library(abind)

# -----------------------------------------------------------
# 1) Diferenças percentuais vs ARIMA (coluna 4)
# -----------------------------------------------------------
MAE_AUM  <- (MAE[,4]  - MAE[,1:3])  / MAE[,4]
M_AUM    <- (MAPE[,4] - MAPE[,1:3]) / MAPE[,4]
RMSE_AUM <- (RMSE[,4] - RMSE[,1:3]) / RMSE[,4]
COR_AUM  <- (COR[,1:3] - COR[,4])   / COR[,4]

# -----------------------------------------------------------
# 2) Tabela organizada por sensor (result01 … result08) + % dif.
#    Mantém a mesma estrutura do seu legado
# -----------------------------------------------------------
# sensor 1
result <- cbind(
  result01,
  rbind(MAE_AUM[1,], M_AUM[1,], RMSE_AUM[1,], COR_AUM[1,]) * 100
)

# sensores 2..8
for (i in 2:8) {
  r <- cbind(
    get(paste0("result0", i)),
    rbind(MAE_AUM[i,], M_AUM[i,], RMSE_AUM[i,], COR_AUM[i,]) * 100
  )
  result <- abind::abind(result, r, along = 1)
}
print(result, digits = 3)  # TABLE V

# -----------------------------------------------------------
# 3) Contagem de "vitórias" dos modelos (melhor = rank 1; para COR, maior = rank 4)
# -----------------------------------------------------------
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

# adiciona coluna "Overall" com o total por métrica
count <- abind::abind(count, apply(count, 1, sum), along = 2)

# rótulos das colunas = nomes dos sensores + Overall
sensor_tags <- rownames(MAPE)  # esperado "RSSI_01"…"RSSI_08"
colnames(count) <- c(sensor_tags, "Overall")

print(t(count))  # TABLE VI

# -----------------------------------------------------------
# 4) Diferença % do ARIMAX_all (coluna 3) em relação ao ARIMAX_Temp (coluna 1)
#    (mesma lógica do seu script)
# -----------------------------------------------------------
MAE_all <- data.frame(
  values = c(
    (MAE[,3]  - MAE[,1])  / MAE[,3],
    (MAPE[,3] - MAPE[,1]) / MAPE[,3],
    (RMSE[,3] - RMSE[,1]) / RMSE[,3],
    (COR[,1]  - COR[,3])  / COR[,3]
  ) * 100,
  measure = c(rep("MAE",  8), rep("MAPE", 8), rep("RMSE", 8), rep("COR", 8)),
  model   = rep(sensor_tags, 4)  # <— substitui colnames(data[,6:13])
)

MAE_all <- MAE_all %>%
  mutate(
    plot_text = dplyr::case_when(
      values < 0 ~ 0.2,
      values > 0 ~ values + 0.2,
      TRUE       ~ 0.2
    )
  )

# Agora 'MAE_all' já está pronto para o seu ggplot
# (mantém coerência com os nomes RSSI_01…RSSI_08)

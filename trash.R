make_summary_rssi <- function(df, sensor_name, var_label = "RSSI"){
  
  num_df <- df |> select(where(is.numeric))
  
  x <- num_df[[1]]
  s <- summary(x)
  
  if ("NA's" %in% names(s)) {
    s <- s[names(s) != "NA's"]
  }
  
  out <- as.data.frame(t(s))
  
  out$N   <- sum(!is.na(x))
  out$NAs <- sum(is.na(x))
  
  out$Sensor <- sensor_name
  out$Env    <- var_label
  
  
  out <- out |>
    relocate(Sensor, Env, N, NAs)
  
  num_cols <- intersect(
    c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max."),
    colnames(out)
  )
  out[num_cols] <- lapply(out[num_cols], round, 4)
  
  rownames(out) <- NULL
  
  return(out)
  
}

summary_all <- purrr::imap_dfr(
  rss_train_list,
  ~ make_summary_rssi(.x, sensor_name = .y, var_label = "RSSI")
)

summary_all <- summary_all |> 
  arrange(Sensor)

rownames(summary_all) <- NULL


kbl(
  summary_all,
  format    = "latex",
  booktabs  = TRUE,
  row.names = FALSE,
  caption   = "Resumo descritivo da série RSSI por sensor"
) %>%
  kable_classic(full_width = FALSE) %>%
  collapse_rows(
    columns = 1,   # coluna Sensor
    valign  = "middle"
  )





make_summary_df <- function(df, sensor_name, drop_cols = 1:2) {
  
  # pega só as colunas numéricas (independente da posição)
  num_df <- df |>
    dplyr::select(where(is.numeric))
  
  # se quiser manter a lógica de "tirar as 2 primeiras" especificamente:
  # num_df <- df[, -drop_cols, drop = FALSE]
  # num_df <- num_df[, sapply(num_df, is.numeric), drop = FALSE]
  
  # summary de cada coluna numérica
  sum_mat <- sapply(num_df, summary)
  
  out <- as.data.frame(t(sum_mat))
  
  out$Sensor <- sensor_name
  out$Env    <- rownames(out)
  
  out <- out |>
    dplyr::relocate(Sensor, Env)
  
  # arredonda apenas colunas numéricas
  out <- out |>
    dplyr::mutate(
      dplyr::across(
        where(is.numeric),
        ~ round(.x, 4)
      )
    )
  
  rownames(out) <- NULL
  
  return(out)
}



summary_all <- purrr::imap_dfr(
  rss_list,
  ~ make_summary_df(.x, sensor_name = .y)
)


summary_all <- summary_all |>
  arrange(Sensor, Env)

rownames(summary_all) <- NULL

kbl(
  summary_all,
  format   = "latex",
  booktabs = TRUE,
  row.names = FALSE,
  caption  = ""
) |> 
  kable_classic(full_width = FALSE) |>
  collapse_rows(
    columns = 1,  
    valign  = "middle"
  )





# add_season_dummies <- function(df, time_col = "rdtimestamp") {
df |>
  mutate(
    month  = month(.data[[time_col]]),
    season = case_when(
      month %in% c(12, 1, 2, 3) ~ "winter",  # dez–mar
      month %in% 4:5            ~ "spring",  # abr–mai
      month %in% 6:8            ~ "summer",  # jun–ago
      TRUE                      ~ "autumn"   # set–nov
    ),
    season = factor(season, levels = c("winter", "spring", "summer", "autumn")),
    # winter é referência → não criamos dummy pra ele
    season_spring = as.integer(season == "spring"),
    season_summer = as.integer(season == "summer"),
    season_autumn = as.integer(season == "autumn")
  ) |>
  select(-month, -season)
}

# split_train_test <- function(df, time_col = "rdtimestamp", prop_train = 0.8){
#   
#   df_ordered <- df[order(df[[time_col]]),]
#   
#   n_total <- nrow(df_ordered)
#   n_train <- floor(prop_train * n_total)
#   
#   train <- df_ordered[1:n_train, ]
#   test <- df_ordered[(n_train+1):n_total, ]
#   
#   return(list(train = train, test = test))
#   
# }


sensors_list <- lapply(sensors_list, add_season_dummies)

sensors_split <- lapply(sensors_list, split_train_test)


###Auxiliary Functions 

rss_col  <- "rssi"
cov_cols <- c("soiltemp","soilhum")


time_col <- "rdtimestamp"

make_x <- function(df) {
  as.matrix(df[, cov_cols, drop = FALSE])
}


sensor_names <- names(sensors_list)
n_sens <- length(sensor_names)
n_cov  <- length(cov_cols)

cov_labels <- c("T", "RH", "Spring", "Summer", "Autumn")
stopifnot(length(cov_labels) == n_cov)

order_arima <- matrix(NA, n_sens, 3,
                      dimnames = list(sensor_names, c("p","d","q")))

MAE  <- MAPE <- RMSE <- COR <- matrix(NA, n_sens, 4,
                                      dimnames = list(sensor_names,
                                                      c("ARIMA-ALL", "ARIMA-TEMP", "ARIMA-HUM", "ARIMA")))

Xsig   <- matrix("",       n_sens, n_cov,
                 dimnames = list(sensor_names, cov_labels))
sinal  <- matrix("",       n_sens, n_cov,
                 dimnames = list(sensor_names, cov_labels))
values <- matrix(NA_real_, n_sens, n_cov,
                 dimnames = list(sensor_names, cov_labels))


Winter_sig <- rep(NA, n_sens)
names(Winter_sig) <- sensor_names


for (i in seq_len(n_sens)) {
  
  nm <- sensor_names[i]
  tr <- sensors_split[[nm]]$train
  te <- sensors_split[[nm]]$test
  
  tr <- tr[order(tr[[time_col]]), ]
  te <- te[order(te[[time_col]]), ]
  
  y_tr <- tr[[rss_col]]
  y_te <- te[[rss_col]]
  
  X_tr <- make_x(tr)   
  X_te <- make_x(te)
  
  a01 <- auto.arima(y_tr, xreg = X_tr, allowdrift = FALSE)
  ord <- arimaorder(a01)
  order_arima[i, ] <- ord
  
  ct <- lmtest::coeftest(a01)
  
  idx_cov <- (nrow(ct) - n_cov + 1):nrow(ct)
  pvals   <- ct[idx_cov, 4]
  coefs   <- ct[idx_cov, 1]
  sig     <- pvals < 0.05
  
  
  Xsig[i, ]   <- ifelse(sig, cov_labels, "")
  sinal[i, ]  <- ifelse(sig, ifelse(coefs > 0, "+", "-"), "")
  values[i, ] <- coefs
  
  
  int_idx <- which(rownames(ct) %in% c("intercept", "mean"))
  if (length(int_idx) == 1L) {
    Winter_sig[i] <- ct[int_idx, 4] < 0.05   
  } else {
    Winter_sig[i] <- NA                      
  }
  
  
  Xtemp_tr <- X_tr[, "soiltemp", drop = FALSE]
  Xtemp_te <- X_te[, "soiltemp",  drop = FALSE]
  
  Xhum_tr  <- X_tr[, "soilhum",   drop = FALSE]
  Xhum_te  <- X_te[, "soilhum",   drop = FALSE]
  
  a02 <- forecast::Arima(y_tr, order = ord, xreg = Xtemp_tr)  # ARIMA-TEMP
  a03 <- forecast::Arima(y_tr, order = ord)                   # ARIMA
  a04 <- forecast::Arima(y_tr, order = ord, xreg = Xhum_tr)   # ARIMA-HUM
  
  RSSI_test <- y_te
  
  new1 <- forecast::Arima(RSSI_test, model = a01, xreg = X_te)
  new2 <- forecast::Arima(RSSI_test, model = a02, xreg = Xtemp_te)
  new3 <- forecast::Arima(RSSI_test, model = a03)
  new4 <- forecast::Arima(RSSI_test, model = a04, xreg = Xhum_te)
  
  acc1 <- forecast::accuracy(RSSI_test, new1$fitted)
  acc2 <- forecast::accuracy(RSSI_test, new2$fitted)
  acc3 <- forecast::accuracy(RSSI_test, new4$fitted)
  acc4 <- forecast::accuracy(RSSI_test, new3$fitted)
  
  MAPE[i, ] <- c(acc1[5], acc2[5], acc3[5], acc4[5])
  RMSE[i, ] <- c(acc1[2], acc2[2], acc3[2], acc4[2])
  MAE[i, ]  <- c(acc1[3], acc2[3], acc3[3], acc4[3])
  
  COR[i, ] <- c(
    cor(RSSI_test, new1$fitted, use = "complete.obs"),
    cor(RSSI_test, new2$fitted, use = "complete.obs"),
    cor(RSSI_test, new3$fitted, use = "complete.obs"),
    cor(RSSI_test, new4$fitted, use = "complete.obs")
  )
  
  
  assign(
    paste0("result0", i),
    t(data.frame(MAE = MAE[i, ], MAPE = MAPE[i, ], RMSE = RMSE[i, ], COR = COR[i, ]))
  )
}

print(cbind(order_arima,Xsig))

#Winter = ifelse(Winter_sig, "Winter", ""

# Calculating the percentage difference with respect to ARIMA
MAE_AUM<-(MAE[,4]-MAE[,1:3])/MAE[,4]
M_AUM<-(MAPE[,4]-MAPE[,1:3])/MAPE[,4]
RMSE_AUM<-(RMSE[,4]-RMSE[,1:3])/RMSE[,4]
COR_AUM<-(COR[,1:3]-COR[,4])/COR[,4]

# organizing the table
result<- cbind(result01,rbind(
  MAE_AUM[1,],M_AUM[1,],RMSE_AUM[1,],COR_AUM[1,]
)*100
)

for(i in 2:6){ #8
  r<-cbind(get(paste0("result0",i)),rbind(
    MAE_AUM[i,],M_AUM[i,],RMSE_AUM[i,],COR_AUM[i,]
  )*100
  )
  result<-abind::abind(result,r,along = 1)
}
print(result,digits=4) # TABLE V

# Counting the times the models were the best option
count<-apply(cbind(apply(result01[1:3,], 1, rank)==1,
                   COR=rank(result01[4,])==4),1,sum)
for(i in 2:6){#8
  r<-get(paste0("result0",i))
  r<-apply(cbind(apply(r[1:3,], 1, rank)==1,
                 COR=rank(r[4,])==4),1,sum)
  count<-abind::abind(count,r,along = 2)
}
count<-abind::abind(count,apply(count,1,sum),along = 2)
colnames(count)<-c(rownames(MAPE),"Overall")

print(t(count)) # TABLE VI





##### foi o salvador

# =========================
# DADOS / COLUNAS
# =========================
sensor_names <- intersect(names(rssi_train_list), names(rssi_test_list))
n_sens <- length(sensor_names)

time_col <- "rdtimestamp"
rss_col  <- "rssi"
cov_cols <- c("airtemp", "airhum")

cov_labels <- c("T", "RH")
stopifnot(length(cov_labels) == length(cov_cols))
n_cov <- length(cov_cols)

make_x <- function(df) {
  X <- as.matrix(df[, cov_cols, drop = FALSE])
  colnames(X) <- cov_cols
  X
}

# =========================
# OUTPUTS
# =========================
order_arima <- matrix(NA, n_sens, 3,
                      dimnames = list(sensor_names, c("p","d","q")))

MAE <- MAPE <- RMSE <- COR <- matrix(NA, n_sens, 4,
                                     dimnames = list(sensor_names, c("ARIMA-ALL", "ARIMA-TEMP", "ARIMA-HUM", "ARIMA"))
)

Xsig   <- matrix("",       n_sens, n_cov, dimnames = list(sensor_names, cov_labels))
sinal  <- matrix("",       n_sens, n_cov, dimnames = list(sensor_names, cov_labels))
values <- matrix(NA_real_, n_sens, n_cov, dimnames = list(sensor_names, cov_labels))

# =========================
# LOOP
# =========================
for (i in seq_len(n_sens)) {
  
  nm <- sensor_names[i]
  tr <- rssi_train_list[[nm]]
  te <- rssi_test_list[[nm]]
  
  tr <- tr[order(tr[[time_col]]), ]
  te <- te[order(te[[time_col]]), ]
  
  y_tr <- tr[[rss_col]]
  y_te <- te[[rss_col]]
  
  X_tr <- make_x(tr)
  X_te <- make_x(te)
  
  # (opcional) remove NA
  keep_tr <- complete.cases(y_tr, X_tr)
  keep_te <- complete.cases(y_te, X_te)
  y_tr <- y_tr[keep_tr]; X_tr <- X_tr[keep_tr, , drop = FALSE]
  y_te <- y_te[keep_te]; X_te <- X_te[keep_te, , drop = FALSE]
  
  # ARIMA-ALL
  a01 <- auto.arima(y_tr, xreg = X_tr, allowdrift = FALSE)
  ord <- arimaorder(a01)[c("p","d","q")]
  order_arima[i, ] <- ord
  
  # significância (apenas airtemp/airhum)
  ct <- lmtest::coeftest(a01)
  idx_cov <- match(cov_cols, rownames(ct))  # mantém a ordem airtemp, airhum
  
  pvals <- rep(NA_real_, n_cov)
  coefs <- rep(NA_real_, n_cov)
  
  ok <- !is.na(idx_cov)
  pvals[ok] <- ct[idx_cov[ok], 4]
  coefs[ok] <- ct[idx_cov[ok], 1]
  sig <- !is.na(pvals) & (pvals < 0.05)
  
  Xsig[i, ]   <- ifelse(sig, cov_labels, "")
  sinal[i, ]  <- ifelse(sig, ifelse(coefs > 0, "+", "-"), "")
  values[i, ] <- coefs
  
  # modelos 1-cov e sem cov
  Xtemp_tr <- X_tr[, "airtemp", drop = FALSE]
  Xtemp_te <- X_te[, "airtemp", drop = FALSE]
  
  Xhum_tr  <- X_tr[, "airhum",  drop = FALSE]
  Xhum_te  <- X_te[, "airhum",  drop = FALSE]
  
  a02 <- forecast::Arima(y_tr, order = ord, xreg = Xtemp_tr) # ARIMA-TEMP
  a03 <- forecast::Arima(y_tr, order = ord, xreg = Xhum_tr)  # ARIMA-HUM
  a04 <- forecast::Arima(y_tr, order = ord)                  # ARIMA
  
  # one-step-ahead no teste
  RSSI_test <- y_te
  
  new_all  <- forecast::Arima(RSSI_test, model = a01, xreg = X_te)
  new_temp <- forecast::Arima(RSSI_test, model = a02, xreg = Xtemp_te)
  new_hum  <- forecast::Arima(RSSI_test, model = a03, xreg = Xhum_te)
  new_arim <- forecast::Arima(RSSI_test, model = a04)
  
  acc_all  <- forecast::accuracy(RSSI_test, new_all$fitted)
  acc_temp <- forecast::accuracy(RSSI_test, new_temp$fitted)
  acc_hum  <- forecast::accuracy(RSSI_test, new_hum$fitted)
  acc_arim <- forecast::accuracy(RSSI_test, new_arim$fitted)
  
  MAPE[i, ] <- c(acc_all[5],  acc_temp[5],  acc_hum[5],  acc_arim[5])
  RMSE[i, ] <- c(acc_all[2],  acc_temp[2],  acc_hum[2],  acc_arim[2])
  MAE[i, ]  <- c(acc_all[3],  acc_temp[3],  acc_hum[3],  acc_arim[3])
  
  COR[i, ] <- c(
    cor(RSSI_test, new_all$fitted,  use = "complete.obs"),
    cor(RSSI_test, new_temp$fitted, use = "complete.obs"),
    cor(RSSI_test, new_hum$fitted,  use = "complete.obs"),
    cor(RSSI_test, new_arim$fitted, use = "complete.obs")
  )
  
  assign(
    paste0("result0", i),
    t(data.frame(MAE = MAE[i, ], MAPE = MAPE[i, ], RMSE = RMSE[i, ], COR = COR[i, ]))
  )
}

print(cbind(order_arima, Xsig))

# 
# # Calculating the percentage difference with respect to ARIMA
# MAE_AUM<-(MAE[,4]-MAE[,1:3])/MAE[,4]
# M_AUM<-(MAPE[,4]-MAPE[,1:3])/MAPE[,4]
# RMSE_AUM<-(RMSE[,4]-RMSE[,1:3])/RMSE[,4]
# COR_AUM<-(COR[,1:3]-COR[,4])/COR[,4]
# 
# # organizing the table
# result<- cbind(result01,rbind(
#   MAE_AUM[1,],M_AUM[1,],RMSE_AUM[1,],COR_AUM[1,]
# )*100
# )
# 
# for(i in 2:6){ #8
#   r<-cbind(get(paste0("result0",i)),rbind(
#     MAE_AUM[i,],M_AUM[i,],RMSE_AUM[i,],COR_AUM[i,]
#   )*100
#   )
#   result<-abind::abind(result,r,along = 1)
# }
# print(result,digits=5) # TABLE V
# 
# # Counting the times the models were the best option
# count<-apply(cbind(apply(result01[1:3,], 1, rank)==1,
#                    COR=rank(result01[4,])==4),1,sum)
# for(i in 2:6){#8
#   r<-get(paste0("result0",i))
#   r<-apply(cbind(apply(r[1:3,], 1, rank)==1,
#                  COR=rank(r[4,])==4),1,sum)
#   count<-abind::abind(count,r,along = 2)
# }
# count<-abind::abind(count,apply(count,1,sum),along = 2)
# colnames(count)<-c(rownames(MAPE),"Overall")
# 
# print(t(count))

MAE_AUM  <- (MAE[,4]  - MAE[,1:3])  / MAE[,4]
M_AUM    <- (MAPE[,4] - MAPE[,1:3]) / MAPE[,4]
RMSE_AUM <- (RMSE[,4] - RMSE[,1:3]) / RMSE[,4]
COR_AUM  <- (COR[,1:3] - COR[,4])   / COR[,4]

n_sens <- nrow(MAE)

# organizing the table
result <- cbind(
  get("result01"),
  rbind(MAE_AUM[1,], M_AUM[1,], RMSE_AUM[1,], COR_AUM[1,]) * 100
)

if (n_sens >= 2) {
  for (i in 2:n_sens) {
    r <- cbind(
      get(paste0("result0", i)),
      rbind(MAE_AUM[i,], M_AUM[i,], RMSE_AUM[i,], COR_AUM[i,]) * 100
    )
    result <- abind::abind(result, r, along = 1)
  }
}

print(result, digits = 5) # TABLE V

# Counting the times the models were the best option
result01 <- get("result01")

count <- apply(
  cbind(
    apply(result01[1:3,], 1, rank) == 1,
    COR = rank(result01[4,]) == 4
  ),
  1, sum
)

if (n_sens >= 2) {
  for (i in 2:n_sens) {
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
}

count <- abind::abind(count, apply(count, 1, sum), along = 2)
colnames(count) <- c(rownames(MAPE), "Overall")

print(t(count))



library(dplyr)
library(purrr)
library(psych)

summ_corr_1sensor <- function(df, sensor_name) {
  df2 <- df %>%
    select(rssi, airtemp, airhum) %>%
    mutate(across(everything(), as.numeric))
  
  ct <- psych::corr.test(df2, use = "pairwise", adjust = "holm")
  
  r <- ct$r
  p <- ct$p
  n <- ct$n
  
  n_pair <- function(a, b) {
    if (is.matrix(n)) return(n[a, b])
    as.numeric(n[1])  # quando vem escalar/vetor
  }
  
  tibble(
    sensor = sensor_name,
    
    r_rssi_airtemp = r["rssi","airtemp"],
    p_rssi_airtemp = p["rssi","airtemp"],
    n_rssi_airtemp = n_pair("rssi","airtemp"),
    
    r_rssi_airhum  = r["rssi","airhum"],
    p_rssi_airhum  = p["rssi","airhum"],
    n_rssi_airhum  = n_pair("rssi","airhum"),
    
    r_airtemp_airhum = r["airtemp","airhum"],
    p_airtemp_airhum = p["airtemp","airhum"],
    n_airtemp_airhum = n_pair("airtemp","airhum")
  )
}

corr_within_sensors <- imap_dfr(train_list, summ_corr_1sensor)
corr_within_sensors


library(tidyr)

rssi_wide <- train_list |>
  imap(\(df, sensor) df |> select(rdtimestamp, rssi) |> rename(!!sensor := rssi)) |>
  reduce(full_join, by = "rdtimestamp") |>
  arrange(rdtimestamp)

ct_rssi <- psych::corr.test(
  rssi_wide |> select(-rdtimestamp),
  use = "pairwise",
  adjust = "holm"
)

# matriz de correlação entre sensores
ct_rssi$r

### --- nao apagar daqui saiu os resultados da reuião do dia 23/01/2026


####################
## Fitting ARIMAX ##
####################

m <- length(rssi_train_list_day)
sensors <- names(rssi_train_list_day)

order_arima <- matrix(NA, m, 3)

MAE <- MAPE <- RMSE <- COR <- matrix(NA, m, 5)
colnames(MAE) <- colnames(MAPE) <- colnames(RMSE) <- colnames(COR) <-
  c("ARIMA-COV","ARIMA-COV*","ARIMA-COV**","ARIMA","ARIMA-DUM")

rownames(order_arima) <- rownames(MAE) <-
  rownames(MAPE) <- rownames(RMSE) <- rownames(COR) <- sensors

cov_names <- c("T","RH","dum_Summer","dum_Autumn","dum_Winter")
p_cov <- length(cov_names)

Xsig   <- matrix("", m, p_cov)
values <- matrix(NA, m, p_cov)
sinal  <- matrix(NA, m, p_cov)

rownames(Xsig) <- rownames(values) <- rownames(sinal) <- sensors
colnames(Xsig) <- colnames(values) <- colnames(sinal) <- cov_names

# Matriz para armazenar quais covariáveis foram usadas em cada modelo
cov_used <- matrix("", m, 3)
colnames(cov_used) <- c("ARIMA-COV", "ARIMA-COV*", "ARIMA-COV**")
rownames(cov_used) <- sensors

for (i in seq_along(sensors)) {
  
  df_tr <- rssi_train_list_day[[i]]
  df_te <- rssi_test_list_day[[i]]
  
  RSSI <- df_tr$rssi
  
  X <- cbind(
    df_tr$airtemp,
    df_tr$airhum,
    df_tr$dum_Summer,
    df_tr$dum_Autumn,
    df_tr$dum_Winter
  )
  
  Xtest <- cbind(
    df_te$airtemp,
    df_te$airhum,
    df_te$dum_Summer,
    df_te$dum_Autumn,
    df_te$dum_Winter
  )
  
  colnames(X) <- colnames(Xtest) <- cov_names
  
  Xdum   <- X[, 3:5, drop = FALSE]
  Xdum_t <- Xtest[, 3:5, drop = FALSE]
  
  # Modelo 1: ARIMA com todas as covariáveis
  a01 <- auto.arima(RSSI, xreg = X, allowdrift = FALSE)
  
  k <- ncol(X)
  pvals <- coeftest(a01)[, 4]
  p_xreg <- pvals[(length(a01$coef) - k + 1):length(a01$coef)]
  tcoef  <- p_xreg < 0.05
  
  order_arima[i, ] <- arimaorder(a01)
  
  Xsig[i, ] <- ifelse(tcoef, cov_names, "")
  
  xreg_coef   <- coef(a01)[(length(a01$coef) - k + 1):length(a01$coef)]
  values[i, ] <- xreg_coef
  sinal[i, ]  <- xreg_coef < 0
  
  # ESTRATÉGIA REVISADA PARA EVITAR DUPLICAÇÃO
  # Conta quantas variáveis são significativas
  n_signif <- sum(tcoef)
  
  if (n_signif == 0) {
    # Caso 1: Nenhuma variável significativa
    # ARIMA-COV: usa todas (modelo original)
    # ARIMA-COV*: usa temperatura (primeira)
    # ARIMA-COV**: usa umidade (segunda)
    
    Xnew <- X  # todas
    Xnewt <- Xtest
    
    Xstar <- X[, 1, drop = FALSE]  # temperatura
    Xstart <- Xtest[, 1, drop = FALSE]
    
    Xdblstar <- X[, 2, drop = FALSE]  # umidade
    Xdblstart <- Xtest[, 2, drop = FALSE]
    
    cov_used[i, ] <- c("T,RH,Summer,Autumn,Winter", "T", "RH")
    
  } else if (n_signif == 1) {
    # Caso 2: Apenas uma variável significativa
    # ARIMA-COV: usa todas (modelo original)
    # ARIMA-COV*: usa a variável significativa
    # ARIMA-COV**: usa a variável significativa + temperatura
    
    Xnew <- X  # todas
    Xnewt <- Xtest
    
    idx_signif <- which(tcoef)
    Xstar <- X[, idx_signif, drop = FALSE]  # variável significativa
    Xstart <- Xtest[, idx_signif, drop = FALSE]
    
    # Para evitar duplicação, adiciona temperatura (se não for a mesma)
    if (idx_signif != 1) {
      Xdblstar <- X[, c(idx_signif, 1), drop = FALSE]  # signif + temperatura
      Xdblstart <- Xtest[, c(idx_signif, 1), drop = FALSE]
      cov_names_dblstar <- paste(cov_names[idx_signif], "T", sep = ",")
    } else {
      # Se a significativa for temperatura, adiciona umidade
      Xdblstar <- X[, c(1, 2), drop = FALSE]  # temperatura + umidade
      Xdblstart <- Xtest[, c(1, 2), drop = FALSE]
      cov_names_dblstar <- "T,RH"
    }
    
    cov_used[i, 1] <- "T,RH,Summer,Autumn,Winter"
    cov_used[i, 2] <- cov_names[idx_signif]
    cov_used[i, 3] <- cov_names_dblstar
    
  } else if (n_signif == 2) {
    # Caso 3: Duas variáveis significativas
    # ARIMA-COV: usa todas (modelo original)
    # ARIMA-COV*: usa as duas significativas
    # ARIMA-COV**: usa apenas a mais significativa
    
    Xnew <- X  # todas
    Xnewt <- Xtest
    
    idx_signif <- which(tcoef)
    Xstar <- X[, idx_signif, drop = FALSE]  # ambas significativas
    Xstart <- Xtest[, idx_signif, drop = FALSE]
    
    # Encontra a mais significativa (menor p-valor)
    pvals_signif <- p_xreg[tcoef]
    idx_most_signif <- idx_signif[which.min(pvals_signif)]
    Xdblstar <- X[, idx_most_signif, drop = FALSE]  # mais significativa
    Xdblstart <- Xtest[, idx_most_signif, drop = FALSE]
    
    cov_used[i, 1] <- "T,RH,Summer,Autumn,Winter"
    cov_used[i, 2] <- paste(cov_names[idx_signif], collapse = ",")
    cov_used[i, 3] <- cov_names[idx_most_signif]
    
  } else if (n_signif >= 3) {
    # Caso 4: Três ou mais variáveis significativas
    # ARIMA-COV: usa todas (modelo original)
    # ARIMA-COV*: usa todas as significativas
    # ARIMA-COV**: usa as duas mais significativas
    
    Xnew <- X  # todas
    Xnewt <- Xtest
    
    idx_signif <- which(tcoef)
    Xstar <- X[, idx_signif, drop = FALSE]  # todas significativas
    Xstart <- Xtest[, idx_signif, drop = FALSE]
    
    # Encontra as duas mais significativas
    pvals_signif <- p_xreg[tcoef]
    idx_ordered <- idx_signif[order(pvals_signif)]
    idx_top2 <- idx_ordered[1:2]
    Xdblstar <- X[, idx_top2, drop = FALSE]  # duas mais significativas
    Xdblstart <- Xtest[, idx_top2, drop = FALSE]
    
    cov_used[i, 1] <- "T,RH,Summer,Autumn,Winter"
    cov_used[i, 2] <- paste(cov_names[idx_signif], collapse = ",")
    cov_used[i, 3] <- paste(cov_names[idx_top2], collapse = ",")
  }
  
  # Ajusta os modelos
  a02 <- Arima(RSSI, order = arimaorder(a01), xreg = Xstar)
  a03 <- Arima(RSSI, order = arimaorder(a01))
  a04 <- Arima(RSSI, order = arimaorder(a01), xreg = Xdblstar)
  a05 <- Arima(RSSI, order = arimaorder(a01), xreg = Xdum)
  
  RSSI_test <- df_te$rssi
  
  # Previsões
  new1 <- Arima(RSSI_test, xreg = Xtest, model = a01)
  new2 <- Arima(RSSI_test, xreg = Xstart, model = a02)
  new3 <- Arima(RSSI_test, model = a03)
  new4 <- Arima(RSSI_test, xreg = Xdblstart, model = a04)
  new5 <- Arima(RSSI_test, xreg = Xdum_t, model = a05)
  
  MAPE[i, ] <- c(
    forecast::accuracy(new1$fitted, RSSI_test)[5],
    forecast::accuracy(new2$fitted, RSSI_test)[5],
    forecast::accuracy(new4$fitted, RSSI_test)[5],
    forecast::accuracy(new3$fitted, RSSI_test)[5],
    forecast::accuracy(new5$fitted, RSSI_test)[5]
  )
  
  RMSE[i, ] <- c(
    forecast::accuracy(new1$fitted, RSSI_test)[2],
    forecast::accuracy(new2$fitted, RSSI_test)[2],
    forecast::accuracy(new4$fitted, RSSI_test)[2],
    forecast::accuracy(new3$fitted, RSSI_test)[2],
    forecast::accuracy(new5$fitted, RSSI_test)[2]
  )
  
  MAE[i, ] <- c(
    forecast::accuracy(new1$fitted, RSSI_test)[3],
    forecast::accuracy(new2$fitted, RSSI_test)[3],
    forecast::accuracy(new4$fitted, RSSI_test)[3],
    forecast::accuracy(new3$fitted, RSSI_test)[3],
    forecast::accuracy(new5$fitted, RSSI_test)[3]
  )
  
  COR[i, ] <- c(
    cor(RSSI_test, new1$fitted, use = "complete.obs"),
    cor(RSSI_test, new2$fitted, use = "complete.obs"),
    cor(RSSI_test, new4$fitted, use = "complete.obs"),
    cor(RSSI_test, new3$fitted, use = "complete.obs"),
    cor(RSSI_test, new5$fitted, use = "complete.obs")
  )
  
  RSSI_test_xts <- xts(RSSI_test, order.by = df_te$rdtimestamp)
  new1fit <- xts(new1$fitted, order.by = df_te$rdtimestamp)
  new2fit <- xts(new2$fitted, order.by = df_te$rdtimestamp)
  new3fit <- xts(new3$fitted, order.by = df_te$rdtimestamp)
  new4fit <- xts(new4$fitted, order.by = df_te$rdtimestamp)
  new5fit <- xts(new5$fitted, order.by = df_te$rdtimestamp)
  
  assign(
    paste0("result0", i),
    t(data.frame(
      MAE  = MAE[i, ],
      MAPE = MAPE[i, ],
      RMSE = RMSE[i, ],
      COR  = COR[i, ]
    ))
  )
}

print(cbind(order_arima, Xsig))

# Mostra quais covariáveis foram usadas em cada modelo
cat("\n=== Covariáveis usadas em cada modelo ===\n")
print(cov_used)

MAE_AUM  <- (MAE[, 4]  - MAE[, 1:3])  / MAE[, 4]
M_AUM    <- (MAPE[, 4] - MAPE[, 1:3]) / MAPE[, 4]
RMSE_AUM <- (RMSE[, 4] - RMSE[, 1:3]) / RMSE[, 4]
COR_AUM  <- (COR[, 1:3] - COR[, 4])   / COR[, 4]

result <- cbind(get("result01"), rbind(
  MAE_AUM[1, ], M_AUM[1, ], RMSE_AUM[1, ], COR_AUM[1, ]
) * 100)

for (i in 2:m) {
  r <- cbind(get(paste0("result0", i)), rbind(
    MAE_AUM[i, ], M_AUM[i, ], RMSE_AUM[i, ], COR_AUM[i, ]
  ) * 100)
  result <- abind::abind(result, r, along = 1)
}

metric_cols <- c("ARIMA-COV","ARIMA-COV*","ARIMA-COV**","ARIMA","ARIMA-DUM")
aum_cols    <- paste0(metric_cols[1:3], "_AUM")
colnames(result) <- c(metric_cols, aum_cols)

measures <- rownames(get("result01"))
rownames(result) <- paste(
  rep(sensors[1:m], each = length(measures)),
  rep(measures, times = m),
  sep = " | "
)

cat("\n=== Resultados Finais (sem valores duplicados) ===\n")
print(result, digits = 5)


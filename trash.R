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


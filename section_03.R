
####################
## Fitting ARIMAX ##
####################

m <- length(rssi_train_list_day)
sensors <- names(rssi_train_list_day)

order_arima <- matrix(NA, m, 3)

MAE <- MAPE <- RMSE <- COR <- matrix(NA, m, 4)
colnames(MAE) <- colnames(MAPE) <- colnames(RMSE) <- colnames(COR) <-
  c("ARIMA-COV","ARIMA-COV*","ARIMA-COV*","ARIMA")

rownames(order_arima) <- rownames(MAE) <- 
  rownames(MAPE) <- rownames(RMSE) <- rownames(COR) <- sensors


cov_names <- c("T","RH","dum_Summer","dum_Autumn","dum_Winter")
p_cov <- length(cov_names)

Xsig   <- matrix("", m, p_cov)
values <- matrix(NA, m, p_cov)
sinal  <- matrix(NA, m, p_cov)
#rownames(Xsig) <- sensors

rownames(Xsig) <- rownames(values) <- rownames(sinal) <- sensors
colnames(Xsig) <- colnames(values) <- colnames(sinal) <- cov_names

#coeffs <- matrix(0, m, 6)  

for (i in seq_along(sensors)) {
  
  df_tr <- rssi_train_list_day[[i]]
  df_te <- rssi_test_list_day[[i]]
  
  RSSI <- df_tr$rssi
  
  # covariáveis 
  #X     <- cbind(df_tr$airtemp, df_tr$airhum)
  #Xtest <- cbind(df_te$airtemp, df_te$airhum)
  
  #Xchoosed  <- X[, 1]
  #Xchoosedt <- Xtest[, 1]
  
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
  
  # fitting the algorithms
  a01 <- assign(paste0("arimax0", i),
                auto.arima(RSSI, xreg = X, allowdrift = FALSE))
  
  # seleção por significância dos coeficientes do xreg (últimos k coeficientes)
  k <- ncol(X)
  pvals <- coeftest(a01)[, 4]
  #tcoef <- pvals[(length(a01$coef) - k + 1):length(a01$coef)] < 0.05
  
  p_xreg <- pvals[(length(a01$coef) - k + 1):length(a01$coef)]
  tcoef  <- p_xreg < 0.05
  
  # evita quebrar quando nenhuma covariável é significativa
  if (sum(tcoef) == 0) tcoef[1] <- TRUE  # comente esta linha se você preferir “sem COV*”
  
  Xnew  <- X[, tcoef, drop = FALSE]
  Xnewt <- Xtest[, tcoef, drop = FALSE]
  
  order_arima[i, ] <- arimaorder(a01)
  
  #Xsig[i, ] <- c(c("T", "RH")[tcoef], rep("", 2 - sum(tcoef)))
  
  Xsig[i, ] <- ifelse(tcoef, cov_names, "") # variaveis significativas
  
  #sign_vec <- (coef(a01) < 0)[(length(a01$coef) - k + 1):length(a01$coef)]
  #val_vec  <- (coef(a01))[(length(a01$coef) - k + 1):length(a01$coef)]
  #sinal[i, ]  <- c(sign_vec, rep(NA, 2 - length(sign_vec)))
  #values[i, ] <- c(val_vec,  rep(NA, 2 - length(val_vec)))
  
  xreg_coef   <- coef(a01)[(length(a01$coef) - k + 1):length(a01$coef)]
  values[i, ] <- xreg_coef
  sinal[i, ]  <- xreg_coef < 0
  
  idx_best  <- which.min(p_xreg)
  Xchoosed  <- X[, idx_best]
  Xchoosedt <- Xtest[, idx_best]
  
  a02 <- Arima(RSSI, arimaorder(a01), xreg = Xnew) # Significativo
  
  a03 <- Arima(RSSI, arimaorder(a01)) # ARIMA
  
  a04 <- Arima(RSSI, order = arimaorder(a01), xreg = Xchoosed)
  
  # forecasting (one-step-ahead)
  RSSI_test <- df_te$rssi
  
  new1 <- assign(paste0("arima_cov0", i),
                 Arima(RSSI_test, xreg = Xtest, model = a01))
  
  new2 <- assign(paste0("arima_covstar", i),
                 Arima(RSSI_test, xreg = Xnewt, model = a02))
  
  new3 <- assign(paste0("arima_pred0", i),
                 Arima(RSSI_test, model = a03))
  
  new4 <- assign(paste0("arima_cov2star0", i),
                 Arima(RSSI_test, xreg = Xchoosedt, model = a04))
  
  MAPE[i, ] <- c(
    forecast::accuracy(RSSI_test, new1$fitted)[5],
    forecast::accuracy(RSSI_test, new2$fitted)[5],
    forecast::accuracy(RSSI_test, new4$fitted)[5],
    forecast::accuracy(RSSI_test, new3$fitted)[5]
  )
  
  RMSE[i, ] <- c(
    forecast::accuracy(RSSI_test, new1$fitted)[2],
    forecast::accuracy(RSSI_test, new2$fitted)[2],
    forecast::accuracy(RSSI_test, new4$fitted)[2],
    forecast::accuracy(RSSI_test, new3$fitted)[2]
  )
  
  COR[i, ] <- c(
    cor(RSSI_test, new1$fitted),
    cor(RSSI_test, new2$fitted),
    cor(RSSI_test, new4$fitted),
    cor(RSSI_test, new3$fitted)
  )
  
  MAE[i, ] <- c(
    forecast::accuracy(RSSI_test, new1$fitted)[3],
    forecast::accuracy(RSSI_test, new2$fitted)[3],
    forecast::accuracy(RSSI_test, new4$fitted)[3],
    forecast::accuracy(RSSI_test, new3$fitted)[3]
  )
  
  # xts com os timestamps do TESTE (substitui data$timestamp e n)
  RSSI_test <- xts(RSSI_test, order.by = df_te$rdtimestamp)
  new1fit   <- xts(new1$fitted, order.by = df_te$rdtimestamp)
  new2fit   <- xts(new2$fitted, order.by = df_te$rdtimestamp)
  new3fit   <- xts(new3$fitted, order.by = df_te$rdtimestamp)
  new4fit   <- xts(new4$fitted, order.by = df_te$rdtimestamp)
  
  assign(paste0("result0", i),
         t(data.frame(MAE = MAE[i, ], MAPE = MAPE[i, ], RMSE = RMSE[i, ], COR = COR[i, ])))
}

print(cbind(order_arima, Xsig))


colnames(values)<-c("T","RH")
values<-as.data.frame(values)
ggplot(stack(values), aes(x = ind, y = values)) +
  geom_boxplot() +
  labs(title="",x="Weather parameter", 
       y = expression(paste(beta,"-coefficient estimates"))) +
  geom_hline(yintercept=0, linetype=2, 
             color = "grey0", size=.3)+
  theme(axis.title.y = element_text(color=1,size=15),
        axis.title.x = element_text(color=1,size=15),
        axis.text.x = element_text(color=1,size=15),
        axis.text.y = element_text(color=1,size=15),
        panel.background = element_rect(fill = "white", 
                                        colour = "black"))



MAE_AUM  <- (MAE[,4]  - MAE[,1:3])  / MAE[,4]
M_AUM    <- (MAPE[,4] - MAPE[,1:3]) / MAPE[,4]
RMSE_AUM <- (RMSE[,4] - RMSE[,1:3]) / RMSE[,4]
COR_AUM  <- (COR[,1:3] - COR[,4])   / COR[,4]

# organizing the table
result <- cbind(result01, rbind(
  MAE_AUM[1,], M_AUM[1,], RMSE_AUM[1,], COR_AUM[1,]
) * 100)

for (i in 2:m) {
  r <- cbind(get(paste0("result0", i)), rbind(
    MAE_AUM[i,], M_AUM[i,], RMSE_AUM[i,], COR_AUM[i,]
  ) * 100)
  
  result <- abind::abind(result, r, along = 1)
}



metric_cols <- c("ARIMA-COV","ARIMA-COV*","ARIMA-COV**","ARIMA")
aum_cols    <- paste0(metric_cols[1:3], "_AUM")
colnames(result) <- c(metric_cols, aum_cols)

measures <- rownames(result01)  # MAE, MAPE, RMSE, COR
rownames(result) <- paste(
  rep(sensors[1:m], each = length(measures)),
  rep(measures, times = m),
  sep = " | "
)

print(result, digits = 4)





result_df <- as.data.frame(result) |> round(digits = 4)


kable(result_df, "latex") %>%
  kable_styling() %>%
  save_kable("tabela.tex")

count<-apply(cbind(apply(result01[1:3,], 1, rank)==1,
                   COR=rank(result01[4,])==4),1,sum)
for(i in 2:8){
  r<-get(paste0("result0",i))
  r<-apply(cbind(apply(r[1:3,], 1, rank)==1,
                 COR=rank(r[4,])==4),1,sum)
  count<-abind::abind(count,r,along = 2)
}
count<-abind::abind(count,apply(count,1,sum),along = 2)
colnames(count)<-c(rownames(MAPE),"Overall")

print(t(count)) 



### Time series Figures ----

# RSSI_01 <- xts(tinovi01_RSSI$rssi, order.by=tinovi01_RSSI$rdtimestamp)
# RSSI_02 <- xts(tinovi02_RSSI$rssi, order.by = tinovi02_RSSI$rdtimestamp)
# RSSI_03 <- xts(tinovi03_RSSI$rssi, order.by = tinovi03_RSSI$rdtimestamp)
# RSSI_04 <- xts(tinovi04_RSSI$rssi, order.by = tinovi04_RSSI$rdtimestamp)
# RSSI_05 <- xts(tinovi05_RSSI$rssi, order.by = tinovi05_RSSI$rdtimestamp)
# RSSI_06 <- xts(tinovi06_RSSI$rssi, order.by = tinovi06_RSSI$rdtimestamp)
# RSSI_07 <- xts(milesight01_RSSI$rssi, order.by=milesight01_RSSI$rdtimestamp)
# RSSI_08 <- xts(milesight02_RSSI$rssi, order.by=milesight02_RSSI$rdtimestamp)
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


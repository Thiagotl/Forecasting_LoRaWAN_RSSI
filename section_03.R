
####################
## Fitting ARIMAX ##
####################

m <- length(rssi_train_list)
sensors <- names(rssi_train_list)

order_arima <- matrix(NA, m, 3)

metric_cols <- c("ARIMA-TH","ARIMA-Temp","ARIMA-H","ARIMA","ARIMA-DUM")
MAE <- MAPE <- RMSE <- COR <- matrix(NA, m, length(metric_cols))
colnames(MAE) <- colnames(MAPE) <- colnames(RMSE) <- colnames(COR) <- metric_cols

rownames(order_arima) <- rownames(MAE) <- rownames(MAPE) <- rownames(RMSE) <- rownames(COR) <- sensors

cov_names <- c("T","RH","dum_Summer","dum_Autumn","dum_Winter")
p_cov <- length(cov_names)

Xsig <- matrix("", m, p_cov)
rownames(Xsig) <- sensors
colnames(Xsig) <- cov_names

for (i in seq_along(sensors)) {
  
  df_tr <- rssi_train_list[[i]]
  df_te <- rssi_test_list[[i]]
  
  RSSI <- df_tr$rssi
  RSSI_test <- df_te$rssi
  
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
  
  Xth   <- X[, 1:2, drop = FALSE]
  Xth_t <- Xtest[, 1:2, drop = FALSE]
  
  Xtemp   <- X[, 1, drop = FALSE]
  Xtemp_t <- Xtest[, 1, drop = FALSE]
  
  Xhum   <- X[, 2, drop = FALSE]
  Xhum_t <- Xtest[, 2, drop = FALSE]
  
  Xdum   <- X[, 3:5, drop = FALSE]
  Xdum_t <- Xtest[, 3:5, drop = FALSE]
  
  a01 <- auto.arima(RSSI, xreg = Xth, allowdrift = FALSE)
  ord <- arimaorder(a01)
  order_arima[i, ] <- ord
  
  ct <- lmtest::coeftest(a01)
  pvals <- ct[, 4]
  names(pvals) <- rownames(ct)
  
  bhat <- stats::coef(a01)
  
  pick_name <- function(v) {
    if (v %in% names(bhat)) return(v)
    vx <- paste0("xreg", v)
    if (vx %in% names(bhat)) return(vx)
    return(NA_character_)
  }
  
  for (v in c("T", "RH")) {
    nm <- pick_name(v)
    if (!is.na(nm) && !is.na(pvals[nm]) && pvals[nm] < 0.05) {
      dir <- ifelse(bhat[nm] >= 0, "Positive", "Negative")
      Xsig[i, v] <- paste0(dir)
    } else {
      Xsig[i, v] <- ""
    }
  }
  
  Xsig[i, c("dum_Summer","dum_Autumn","dum_Winter")] <- ""
  
  
  a02 <- Arima(RSSI, order = ord, xreg = Xtemp)
  a03 <- Arima(RSSI, order = ord, xreg = Xhum)
  a04 <- Arima(RSSI, order = ord)
  a05 <- Arima(RSSI, order = ord, xreg = Xdum)
  
  new1 <- Arima(RSSI_test, xreg = Xth_t, model = a01)
  new2 <- Arima(RSSI_test, xreg = Xtemp_t, model = a02)
  new3 <- Arima(RSSI_test, xreg = Xhum_t, model = a03)
  new4 <- Arima(RSSI_test, model = a04)
  new5 <- Arima(RSSI_test, xreg = Xdum_t, model = a05)
  
  MAPE[i, ] <- c(
    forecast::accuracy(new1$fitted, RSSI_test)[5],
    forecast::accuracy(new2$fitted, RSSI_test)[5],
    forecast::accuracy(new3$fitted, RSSI_test)[5],
    forecast::accuracy(new4$fitted, RSSI_test)[5],
    forecast::accuracy(new5$fitted, RSSI_test)[5]
  )
  
  RMSE[i, ] <- c(
    forecast::accuracy(new1$fitted, RSSI_test)[2],
    forecast::accuracy(new2$fitted, RSSI_test)[2],
    forecast::accuracy(new3$fitted, RSSI_test)[2],
    forecast::accuracy(new4$fitted, RSSI_test)[2],
    forecast::accuracy(new5$fitted, RSSI_test)[2]
  )
  
  MAE[i, ] <- c(
    forecast::accuracy(new1$fitted, RSSI_test)[3],
    forecast::accuracy(new2$fitted, RSSI_test)[3],
    forecast::accuracy(new3$fitted, RSSI_test)[3],
    forecast::accuracy(new4$fitted, RSSI_test)[3],
    forecast::accuracy(new5$fitted, RSSI_test)[3]
  )
  
  COR[i, ] <- c(
    cor(RSSI_test, new1$fitted, use = "complete.obs"),
    cor(RSSI_test, new2$fitted, use = "complete.obs"),
    cor(RSSI_test, new3$fitted, use = "complete.obs"),
    cor(RSSI_test, new4$fitted, use = "complete.obs"),
    cor(RSSI_test, new5$fitted, use = "complete.obs")
  )
  
  RSSI_test_xts <- xts::xts(RSSI_test, order.by = df_te$rdtimestamp)
  new1fit <- xts::xts(new1$fitted, order.by = df_te$rdtimestamp)
  new2fit <- xts::xts(new2$fitted, order.by = df_te$rdtimestamp)
  new3fit <- xts::xts(new3$fitted, order.by = df_te$rdtimestamp)
  new4fit <- xts::xts(new4$fitted, order.by = df_te$rdtimestamp)
  new5fit <- xts::xts(new5$fitted, order.by = df_te$rdtimestamp)
  
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



base_col <- which(metric_cols == "ARIMA")
comp_cols <- which(metric_cols %in% c("ARIMA-TH","ARIMA-Temp","ARIMA-H","ARIMA-DUM"))

MAE_AUM  <- (MAE[, base_col]  - MAE[, comp_cols, drop = FALSE])  / MAE[, base_col]
M_AUM    <- (MAPE[, base_col] - MAPE[, comp_cols, drop = FALSE]) / MAPE[, base_col]
RMSE_AUM <- (RMSE[, base_col] - RMSE[, comp_cols, drop = FALSE]) / RMSE[, base_col]
COR_AUM  <- (COR[, comp_cols, drop = FALSE] - COR[, base_col])   / COR[, base_col]

result <- cbind(get("result01"), rbind(
  MAE_AUM[1, ], M_AUM[1, ], RMSE_AUM[1, ], COR_AUM[1, ]
) * 100)

for (i in 2:m) {
  r <- cbind(get(paste0("result0", i)), rbind(
    MAE_AUM[i, ], M_AUM[i, ], RMSE_AUM[i, ], COR_AUM[i, ]
  ) * 100)
  result <- abind::abind(result, r, along = 1)
}

aum_cols <- paste0(metric_cols[comp_cols], "_AUM")
colnames(result) <- c(metric_cols, aum_cols)

measures <- rownames(get("result01"))
rownames(result) <- paste(
  rep(sensors[1:m], each = length(measures)),
  rep(measures, times = m),
  sep = " | "
)

print(result, digits = 5)


# count<-apply(cbind(apply(result01[1:3,], 1, rank)==1,
#                    COR=rank(result01[4,])==4),1,sum)
# for(i in 2:8){
#   r<-get(paste0("result0",i))
#   r<-apply(cbind(apply(r[1:3,], 1, rank)==1,
#                  COR=rank(r[4,])==4),1,sum)
#   count<-abind::abind(count,r,along = 2)
# }
# count<-abind::abind(count,apply(count,1,sum),along = 2)
# colnames(count)<-c(rownames(MAPE),"Overall")
# 
# print(t(count)) 
# 
# 
# 
# 
# 
# result_df <- as.data.frame(result) |> round(digits = 4)
# 
# 
# kable(result_df, "latex") %>%
#   kable_styling() %>%
#   save_kable("tabela.tex")
# 
# 
# colnames(values)<-c("T","RH")
# values<-as.data.frame(values)
# ggplot(stack(values), aes(x = ind, y = values)) +
#   geom_boxplot() +
#   labs(title="",x="Weather parameter", 
#        y = expression(paste(beta,"-coefficient estimates"))) +
#   geom_hline(yintercept=0, linetype=2, 
#              color = "grey0", size=.3)+
#   theme(axis.title.y = element_text(color=1,size=15),
#         axis.title.x = element_text(color=1,size=15),
#         axis.text.x = element_text(color=1,size=15),
#         axis.text.y = element_text(color=1,size=15),
#         panel.background = element_rect(fill = "white", 
#                                         colour = "black"))

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


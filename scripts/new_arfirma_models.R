
library(rugarch)
library(parallel)
library(doParallel)
library(foreach)
## ARFIMA MODEL ###----

rssit      <- rssi_train_list$Tinovi02$rssi#[1:500]
temp       <- rssi_train_list$Tinovi02$airtemp#[1:500]
hum        <- rssi_train_list$Tinovi02$airhum#[1:500]
Xreg       <- as.matrix(cbind(rssi_train_list$Tinovi02[, c(4,5)]))#[1:500, ]


rssitest   <- rssi_test_list$Tinovi02$rssi#[1:50]
temp_test  <- rssi_test_list$Tinovi02$airtemp#[1:50]
hum_test   <- rssi_test_list$Tinovi02$airhum#[1:50]
Xreg_test  <- as.matrix(cbind(rssi_test_list$Tinovi02[, c(4,5)]))#[1:50, ]

y_all <- c(rssit, rssitest)
X_all <- rbind(Xreg, Xreg_test)

h <- length(rssitest)

grid <- expand.grid(p = 0:15, q = 0:15)

df_results <- data.frame(
  p = integer(),
  q = integer(),
  AIC = numeric()
)

pb = txtProgressBar(min = 0, max = nrow(grid), style = 3)

for (k in seq_len(nrow(grid))) {
  
  p <- grid$p[[k]]
  q <- grid$q[[k]]
  
  
  # spec_arfima <- arfimaspec(
  #   mean.model = list(
  #     armaOrder           = c(p, q),
  #     include.mean        = TRUE,
  #     arfima              = TRUE,
  #     external.regressors = X_all          
  #   ),
  #   distribution.model = "norm"
  # )
  
  spec_arfima_garch <- ugarchspec(
    variance.model = list(
      model      = "sGARCH",
      garchOrder = c(1, 1)
    ),
    mean.model = list(
      armaOrder           = c(p, q),
      include.mean        = TRUE,
      arfima              = TRUE,
      external.regressors = X_all         
    ),
    distribution.model = "std"
  )
  
  
  fit_arfima_garch <- tryCatch(ugarchfit(
    spec = spec_arfima_garch,
    data = y_all,
    out.sample = h,
    solver = "hybrid",
    fit.control = list(scale = 1)
    ),
    
    error = function(e){
      message("Error - Order (", p, ",",q,"): ", e$message)
      
      return(NULL)
    }
  )
  
  
  # fit_arfima <- tryCatch(
  #   arfimafit(
  #     spec = spec_arfima,
  #     data = y_all,
  #     out.sample = h,
  #     solver = "solnp",
  #     fit.control = list(scale = 1)
  #   ),
  #   
  #   error = function(e){
  #     message("Error - Order (", p, ",",q,"): ", e$message)
  #     
  #     return(NULL)
  #   }
  # )
  
  
  # if (!is.null(fit_arfima)) {
  #   print(class(fit_arfima))
  #   print(isS4(fit_arfima))
  #   print((infocriteria(fit_arfima)))
  # }

  aic_values <- if (!is.null(fit_arfima_garch)) {
    ic <- tryCatch(
      as.numeric(infocriteria(fit_arfima_garch)),
      error = function(e) {
        message("Failed to extract criteria (", p, ",", q, "): ", e$message)
        return(NULL)
      }
    )
    
    if (!is.null(ic) && length(ic) >= 1) ic[1] else NA_real_
  } else {
    NA_real_
  }
  
  
  df_results <- rbind(
    df_results,
    data.frame(p = p, 
               q = q, 
               AIC = aic_values[1])
  ) 
  
  
  setTxtProgressBar(pb, k)
}


df_results <- df_results[order(df_results$AIC, na.last = TRUE), ]

df_results

best_p <- df_results$p[1]

best_q <- df_results$q[1]

coef(fit_arfima_garch)
coeftest(fit_arfima_garch@fit$matcoef)


fit_arfima_garch@fit$matcoef[, 4] < 0.05



# Ajuste dos modelos -----

spec_arfima_garch <- ugarchspec(
  variance.model = list(
    model      = "fiGARCH",
    garchOrder = c(1, 1)
  ),
  mean.model = list(
    armaOrder           = c(13, 11),
    include.mean        = TRUE,
    arfima              = TRUE,
    external.regressors = X_all        
  ),
  distribution.model = "std"
)


fit_arfima_garch <- tryCatch(ugarchfit(
  spec = spec_arfima_garch,
  data = y_all,
  out.sample = h,
  solver = "hybrid",
  fit.control = list(scale = 1)
),

error = function(e){
  message("Error", e$message)
  
  return(NULL)
}
)

# Forecast rolling one-step-ahead
fc_arfima_garch <- ugarchforecast(
  fit_arfima_garch,
  n.ahead = 1,
  n.roll  = h - 1,
  external.forecasts = list(mregfor = Xreg_test)
)



dim(Xreg_test)        # deve ser h x p
class(Xreg_test)      # deve ser "matrix"
is.matrix(Xreg_test)  # deve ser TRUE

# fitted() em rolling retorna matriz 1 x h
pred_arfima_garch <- as.numeric(fitted(fc_arfima_garch))

# Verificação rápida
stopifnot(length(pred_arfima_garch) == length(rssitest))

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


# NEW NEW NEW NEW NEW NEW NEW -----

n_cores <- max(1, detectCores() - 8)
cl      <- makeCluster(n_cores)
registerDoParallel(cl)

clusterExport(cl, varlist = c("y_all", "h", "grid"))
clusterEvalQ(cl, library(rugarch))

results_list <- foreach(
  k              = seq_len(nrow(grid)),
  .packages      = "rugarch",
  .errorhandling = "pass"
) %dopar% {
  
  p <- grid$p[[k]]
  q <- grid$q[[k]]
  
  spec_arfima_garch <- ugarchspec(
    variance.model = list(
      model      = "sGARCH",
      garchOrder = c(1, 1)
    ),
    mean.model = list(
      armaOrder    = c(p, q),
      include.mean = TRUE,
      arfima       = TRUE
    ),
    distribution.model = "std"
  )
  
  fit_arfima_garch <- tryCatch(
    ugarchfit(
      spec        = spec_arfima_garch,
      data        = y_all,
      out.sample  = h,
      solver      = "hybrid",
      fit.control = list(scale = 1)
    ),
    error = function(e) {
      message("Error - Order (", p, ",", q, "): ", e$message)
      return(NULL)
    }
  )
  
  aic_values <- if (!is.null(fit_arfima_garch)) {
    ic <- tryCatch(
      as.numeric(infocriteria(fit_arfima_garch)),
      error = function(e) {
        message("Failed to extract criteria (", p, ",", q, "): ", e$message)
        return(NULL)
      }
    )
    if (!is.null(ic) && length(ic) >= 1) ic[1] else NA_real_
  } else {
    NA_real_
  }
  
  data.frame(p = p, q = q, AIC = aic_values[1])
}

stopCluster(cl)

df_results <- do.call(rbind, results_list)
df_results <- df_results[order(df_results$AIC, na.last = TRUE), ]
df_results





# NEW NEW NEW NEW NEW NEW NEW -----



y_all <- c(rssit, rssitest)
X_all <- rbind(Xreg, Xreg_test)
h     <- length(rssitest)
grid  <- expand.grid(p = 0:5, q = 0:5)

if (exists("cl")) {
  stopCluster(cl)
  rm(cl)
}

n_cores <- max(1, detectCores() - 8)
cl      <- makeCluster(n_cores, type = "PSOCK")
registerDoParallel(cl)

clusterExport(cl, varlist = c("y_all", "h", "grid"))
clusterEvalQ(cl, library(rugarch))

results_list <- foreach(
  k              = seq_len(nrow(grid)),
  .packages      = "rugarch",
  .errorhandling = "pass",
  .export        = c("y_all", "h", "grid")
) %dopar% {
  
  p <- grid$p[[k]]
  q <- grid$q[[k]]
  
  spec_arfima_garch <- ugarchspec(
    variance.model = list(
      model      = "sGARCH",
      garchOrder = c(1, 1)
    ),
    mean.model = list(
      armaOrder    = c(p, q),
      include.mean = TRUE,
      arfima       = TRUE
    ),
    distribution.model = "std"
  )
  
  fit_arfima_garch <- tryCatch(
    ugarchfit(
      spec        = spec_arfima_garch,
      data        = y_all,
      out.sample  = h,
      solver      = "hybrid",
      fit.control = list(scale = 1)
    ),
    error = function(e) return(NULL)
  )
  
  aic_values <- if (!is.null(fit_arfima_garch)) {
    ic <- tryCatch(
      as.numeric(infocriteria(fit_arfima_garch)),
      error = function(e) return(NULL)
    )
    if (!is.null(ic) && length(ic) >= 1) ic[1] else NA_real_
  } else {
    NA_real_
  }
  
  data.frame(p = p, q = q, AIC = aic_values[1])
}

stopCluster(cl)

df_results <- do.call(rbind, results_list)
df_results <- df_results[order(df_results$AIC, na.last = TRUE), ]
df_results























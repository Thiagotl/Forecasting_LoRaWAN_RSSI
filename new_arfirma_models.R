
library(rugarch)
library(parallel)
library(doParallel)
library(foreach)
## ARFIMA MODEL ###----

rssit      <- rssi_train_list$Tinovi04$rssi#[1:500]
temp       <- rssi_train_list$Tinovi04$airtemp#[1:500]
hum        <- rssi_train_list$Tinovi04$airhum#[1:500]
Xreg       <- as.matrix(rssi_train_list$Tinovi04)#[, c(4,5, 7:9)])#[1:500, ]


rssitest   <- rssi_test_list$Tinovi04$rssi#[1:50]
temp_test  <- rssi_test_list$Tinovi04$airtemp#[1:50]
hum_test   <- rssi_test_list$Tinovi04$airhum#[1:50]
Xreg_test  <- as.matrix(rssi_train_list$Tinovi04)#[, c(4,5, 7:9)])#[1:50, ]

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
      arfima              = TRUE
      #external.regressors = X_all         
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

spec_arfima_garch <- ugarchspec(
  variance.model = list(
    model      = "fiGARCH",
    garchOrder = c(1, 1)
  ),
  mean.model = list(
    armaOrder           = c(5, 5),
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

modelo1 <- fit_arfima_garch
modelo2 <- fit_arfima_garch
modelo3 <- fit_arfima_garch



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

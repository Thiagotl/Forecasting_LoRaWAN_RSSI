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
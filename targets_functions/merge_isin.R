.merge_isin <- function(data,
                        aux_data = NULL,
                        tool,
                        company_col,
                        isin_col,
                        ticker_col) {

  if (identical("S&PS1", tool)) {
  r_idx <- match(x = data[[company_col]], table = aux_data[[company_col]])
  data[[isin_col]] <- aux_data[[isin_col]][r_idx]
  data[[ticker_col]] <- aux_data[[ticker_col]][r_idx]
  
  # missing matches (length 6)
  missing_matches <- list(COMPANYNAME = data[is.na(get(isin_col)), unique(get(company_col))])
  
  # TE Connectivity Ltd. = "CH0102993182" "TEL"
  # Bath & Body Works, Inc. = "US0708301041" "BBWI"
  # Etsy, Inc. = "US29786A1060" "ETSY"
  # Bio-Rad Laboratories, Inc. = "US0905722072" "BIO"
  # American Airlines Group Inc. = "US02376R1023" "AAL"
  # Lam Research Corporation = "US5128071082" "LRCX"
  
  missing_isin <- c(
    "CH0102993182",
    "US0708301041",
    "US29786A1060",
    "US0905722072",
    "US02376R1023",
    "US5128071082"
  )
  
  missing_tickers <- c("TEL", "BBWI", "ETSY", "BIO", "AAL", "LRCX")
  
  missing_matches[[isin_col]] <-  missing_isin
  missing_matches[[ticker_col]] <- missing_tickers
  
  # match the missing
  r_idx <- match(x = data[[company_col]], table = missing_matches[[company_col]])
  
  data[[isin_col]] <- data.table::fifelse(test = is.na(data[[isin_col]]),
                                          yes = missing_matches[[isin_col]][r_idx],
                                          no = data[[isin_col]])
  
  data[[ticker_col]] <- data.table::fifelse(test = is.na(data[[ticker_col]]),
                                            yes = missing_matches[[ticker_col]][r_idx],
                                            no = data[[ticker_col]])
  } else if (identical("BIA_GBS", tool)) {
    r_idx <- match(x = data[["c4fEntityId"]],
                   table = aux_data[["entityAnalyzedId"]])
    
    data[["isin"]] <- aux_data[[isin_col]][r_idx]
    data.table::setcolorder(data, "isin")
  }
  return(data)
}

.finance_data <- function(GICS_data,
                          company_data,
                          isin_data,
                          sales_data) {
  # non-tool data cleaning #####################################################
  # fix columns
  colnames(GICS_data)[c(1, 3, 5, 7)] <- paste(tolower(x = gsub(
    pattern = " |-",
    replacement = "_",
    x = colnames(GICS_data)[c(1, 3, 5, 7)]
  )), "no", sep = "_")
  
  colnames(GICS_data)[c(2, 4, 6, 8)] <- gsub(pattern = "_no",
                                             replacement = "",
                                             colnames(GICS_data)[c(1, 3, 5, 7)])
  
  # fill with NAs
  for (col in 1:6) {
    # very weird white spaces here
    GICS_data[, col] <- lapply(GICS_data[, col], function(row) {
      row <- sapply(row, function(val) {
        if (is.na(val)) {
          return(NA)
        } else {
          cleaned_val <- trimws(val)
          if (nchar(cleaned_val) == 1 & !is.numeric(cleaned_val)) {
            return(NA)
          } else {
            return(cleaned_val)
          }
        }
      })
      return(row)
    })
  }
  
  GICS_data <- zoo::na.locf(GICS_data)
  company_data <- tibble::as_tibble(merge.data.frame(company_data,
                                              GICS_data,
                                              by.x = "GICS.Sector",
                                              by.y = "sector",
                                              all.x = TRUE))
  
  # get ISIN
  r_idx <- match(table = isin_data[["ticker"]],
                 x = company_data[["Symbol"]])
  
  company_data[["isin"]] <- isin_data[["isin"]][r_idx]
  
  # Smurfit Westrock (SW) not matching - manually add ISIN
  company_data[company_data$Symbol == "SW", "isin"] <- "IE00028FXN24"
  company_data <- unique(company_data[, c("Symbol", "Security", "GICS.Sector", "GICS.Sub.Industry", "isin")])  
  
  # revenue
  colnames(sales_data) <- gsub(pattern = " |-", replacement = "_", x = colnames(sales_data))
  colnames(sales_data) <- gsub(pattern = "__", replacement = "", x = colnames(sales_data))
  
  sales_data <- subset(sales_data, Industry_Format == "INDL")
  sales_data <- sales_data[, c("Data_Year_Fiscal", "Ticker_Symbol", "Sales/Turnover_(Net)")]
  colnames(sales_data) <- c("year", "ticker", "sales")
  setDT(sales_data)
  setDT(company_data)

  finance_data <- list(sales_data = sales_data,
                       company_data = company_data)

  return(finance_data)
}
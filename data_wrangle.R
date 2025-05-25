.wrangle_data <- function(data,
                          requested = NULL,
                          tool,
                          company_col,
                          isin_col = NA,
                          ticker_col = NA,
                          year_col = NULL,
                          year = NULL,
                          var_col = "variable",
                          value_col = "value",
                          endpoint,
                          id.vars = NULL,
                          ticker_data = NULL,
                          value_adjusted) {
  # tool data cleaning #########################################################
  if (!is.null(id.vars)) {
    if (is.element(el = tool, set = c("MSCI", "IDL", "BIA_GBS", "GID", "FS", "GIST", "NAlpha"))) {
      if (identical(x = tool, y = "FS")) {
        # match tickers for FS
        r_idx <- match(x = data[["Company"]], table = ticker_data[["Security"]])
        data[["ticker"]] <- ticker_data[["Symbol"]][r_idx]
        ticker_col <- "ticker"
        
        # still some NAs
        data[Company == "APA CORPORATION", ticker := "APA"]
        data[Company == "EstÈe Lauder Companies (The)", ticker := "EL"]
        data[Company == "BrownñForman", ticker := "BF.B"]
        
        id.vars <- c(2, 11)
      } 
      endpoint_col <- grep(pattern = paste0("^", endpoint, "$"), x = colnames(data))
      data <- data[, c(id.vars, endpoint_col), with = FALSE]
      data[, variable := endpoint]

      
      if (identical(x = tool, y = "MSCI")) {
        data <- unique(x = data)
      }
      colnames(data)[colnames(data) == endpoint] <- "value"
    } 
  }

  if (!is.null(year)) {
    if (identical(x = tool, y = "GIST")) {
      # aggregate (over iso)
      data <- data[, .(value = sum(value)), by = c("COMPANY_NAME", "REPORTING_YEAR", "ISIN", "variable")]
      # take most recent
      data <- data[data[, .I[which.max(REPORTING_YEAR)], by = COMPANY_NAME]$V1]
    }
    data <- data[get(year_col) %in% year]
  } else {
    data[["year"]] <- NA
    year_col <- "year"
  }
  
  if (is.na(isin_col)) {
    data[["isin"]] <- NA
    isin_col <- "isin"
  }
  
  if (is.na(ticker_col)) {
    data[["ticker"]] <- NA
    ticker_col <- "ticker"
  }

  tool_specific_names <- c(company_col, isin_col, year_col, ticker_col, var_col, value_col)
  data <- data[, mget(tool_specific_names)]
  colnames(x = data) <- c("company", "isin", "year", "ticker", "variable", "value")

  if (identical(x = tool, y = "FS")) {
    # FS has <0.0001 as a character for some values, sub with numeric 0.00009 for rankings
    data[["value"]] <- gsub(pattern = "<0.0001", replacement = "0.00009", x = data[["value"]])
  }
  
  if (identical(x = endpoint, y = "TOTAL_GLOBAL_PDF")) {
    inject <- subset(requested,
           subset = REPORTING_YEAR == 2022,
           select = c("COMPANY_NAME", "REPORTING_YEAR", "ISIN", "TOTAL_GLOBAL_PDF"))
    inject[["variable"]] <- "TOTAL_GLOBAL_PDF"
    setDT(inject)
    setnames(inject, new = c("company","year","isin","value","variable"))
    inject[, let(ticker = NA)]
    data <- rbindlist(list(data, inject), use.names = TRUE)
  }
  
  data[["value"]] <- as.numeric(x = data[["value"]])
  data[["year"]] <- as.integer(data[["year"]])
  data <- data[variable == endpoint]
  if (identical(x = endpoint, y = "Aggregate Impact: Ecosystem Footprint (HSA)")) {
    inject <- requested[, c("Entity Name", "Ticker", "Ecosystem Footprint (HSA)\r\n(hectares)")]
    inject[["isin"]] <- c("US1696561059", "US25754A2015", "US74460D1090")
    setDT(inject)
    setnames(inject, new = c("company", "ticker", "value", "isin"))
    inject[["variable"]] <- unique(data[["variable"]])
    inject[["year"]] <- NA
    data <- rbindlist(list(data, inject), use.names = TRUE)
  }
  
  data[, let(tool = tool)]
  data[, let(value_adjusted = value_adjusted)]
  data <- unique(data)

  return(data)
}

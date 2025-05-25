.consolidate_data <- function(..., finance_dat) {
  tool_data <- list(...)
  list2env(finance_dat, environment())
  # for each company in each tool, check whether isin matches to master_dat
  # if not attempt to match on ticker
  tool_data <- lapply(
    X = tool_data,
    FUN = function(tool) {
      r_idx <- match(x = tool[["ticker"]], company_data[["Symbol"]])
      
      if (!identical(x = unique(tool[["tool"]]), y = "FS")) {
        tool[["isin_check"]] <- is.element(el = tool[["isin"]], set = company_data[["isin"]])
        
        tool[["isin"]] <- fifelse(test = tool[["isin_check"]],
                                  yes = tool[["isin"]],
                                  no = company_data[["isin"]][r_idx])
        
      } else {
        tool[["isin"]] <- company_data[["isin"]][r_idx]
      }
      # second check
      tool[["isin_check"]] <- is.element(el = tool[["isin"]], set = company_data[["isin"]])
      # tool specific fixes
      #S&P
      if (identical(x = unique(tool[["tool"]]), y = "S&PS1")) {
        tool[ticker == "BRK.A", "isin"] <- "US0846707026"
      } else if (identical(x = unique(tool[["tool"]]), y = "MSCI")) {
        # mismatches here (tool[(!isin_check)]) are not in the S&P500 currently so we toss
        tool <- tool[(isin_check)]
      } else if (identical(x = unique(tool[["tool"]]), y = "IDL")) {
        tool[company == "BIO RAD LABORATORIES INC", "isin"] <- "US0905722072"
        tool[company == "BROWN FORMAN CORP CLASS B", "isin"] <- "CA07784D1033"
        tool[company == "BUNGE LTD", "isin"] <- "CH1300646267"
        tool[company == "CAMPBELL SOUP", "isin"] <- "US1344291091"
        tool[company == "Constellation Energy Generation LLC", "isin"] <- "US21037T1097"
        tool[company == "DuPont de Nemours, Inc.", "isin"] <- "US26614N1028"
        tool[company == "Federal Realty OP LP", "isin"] <- "US3137472060"
        tool[company == "FISERV INC", "isin"] <- "US31620M1062"
        tool[company == "FOX CORP", "isin"] <- "US35137L1052"
        tool[company == "HEALTHPEAK PROPERTIES INC", "isin"] <- "US71943U1043"
        tool[company == "Kimco Realty Corp.", "isin"] <- "US49446R1095"
        tool[company == "Linde Plc", "isin"] <- "IE00BZ12WP82"
        tool[company == "NEWS CORP", "isin"] <- "US65249B1098"
        tool[company == "Paramount Global", "isin"] <- "US92556H2067"
        tool[company == "Ralph Lauren Corp.", "isin"] <- "US7512121010"
        tool[company == "Regency Centers LP", "isin"] <- "US7588491032"
        tool[company == "Tapestry, Inc.", "isin"] <- "US8760301072"

        tool[["isin_check"]] <- is.element(el = tool[["isin"]], set = company_data[["isin"]])
        # remaining mismatches here (tool[(!isin_check)]) are not in the S&P500 currently so we toss
        tool <- tool[(isin_check)]
        
      } else if (identical(x = unique(tool[["tool"]]), y = "BIA_GBS")) {
        tool[company == "Fiserv, Inc.", "isin"] <- "US31620M1062"
        tool[company == "Linde Plc", "isin"] <- "IE00BZ12WP82"
      } else if (identical(x = unique(tool[["tool"]]), y = "GIST")) {
        tool[company == "Brown-Forman Corp.", "isin"] <- "CA07784D1033"
        # remaining mismatches here (tool[(!isin_check)]) are not in the S&P500 currently so we toss
        tool <- tool[(isin_check)]
      } else if (identical(x = unique(tool[["tool"]]), y = "NAlpha")) {
        # filter by ISIN in company_data (many isin present here)
        tool <- tool[isin %in% company_data[["isin"]], ]
      } else if (identical(x = unique(tool[["tool"]]), y = "GID")) {
        # some duplicate tickers without isin
        tool <- tool[isin %in% company_data[["isin"]], ]
      } else if (identical(x = unique(tool[["tool"]]), y = "FS")) {
        # drops duplicate goog fox and nws which is not in sp500
        tool <- tool[isin_check == TRUE]
      }
      
      # final check
      tool[["isin_check"]] <- is.element(el = tool[["isin"]], set = company_data[["isin"]])
      
      if (!all(tool[["isin_check"]])) {
        stop(paste("Still an isin mismatch on", unique(tool[["tool"]])))
      }
      
      tool <- tool[, !"isin_check"]
      return(tool)
    }
  )

  # standardize company names and tickers
  # add NAs for missing entries in each tool
  # add rank
  tool_data <- lapply(
    X = tool_data,
    FUN = function(tool) {
      if (any(is.na(tool[["ticker"]]))) {
        r_idx <- match(tool[["isin"]], company_data[["isin"]])
        tool[["ticker"]] <- company_data[["Symbol"]][r_idx]
      }

      # NA table
      NA_subset <- company_data[!is.element(el = company_data[["Symbol"]], set = tool[["ticker"]])]
      
      missing_dt <- data.table(
        company = NA_subset[["Security"]],
        isin = NA_subset[["isin"]],
        year = NA,
        ticker = NA_subset[["Symbol"]],
        variable = unique(tool[["variable"]]),
        value = NA,
        tool = unique(tool[["tool"]]),
        value_adjusted = unique(tool[["value_adjusted"]])
      )
      
      tool <- rbindlist(list(tool, missing_dt))
      
      if (all(is.na(tool[["year"]]))) {
        rev_21 <- subset(sales_data, year == 2021)
        r_idx <- match(x = tool[["ticker"]], table = rev_21[["ticker"]])
        tool[["sales"]] <- rev_21[["sales"]][r_idx]
      } else {
        tool <- merge.data.table(tool,
                         sales_data,
                         by = c("year", "ticker"),
                         all.x = TRUE)
      }

      r_idx <- match(x = tool[["ticker"]], company_data[["Symbol"]])
      tool[["GICS_sector"]] <- company_data[["GICS.Sector"]][r_idx]
      tool[["GICS_subsector"]] <- company_data[["GICS.Sub.Industry"]][r_idx]

      if (unique(tool$tool) == "S&PS1") {
        BRK <- company_data[Security == "Berkshire Hathaway"]
        tool[ticker == "BRK.A", "GICS_sector"] <- purrr::pluck("Financials")
        tool[ticker == "BRK.A", "GICS_subsector"] <- purrr::pluck("Multi-Sector Holdings")
      }

      if (unique(tool$tool) == "MSCI") {
        r_idx <- match(x = tool[["isin"]], company_data[["isin"]])
        tool[["GICS_sector"]] <- dplyr::case_when(is.na(tool[["GICS_sector"]]) ~ company_data[["GICS.Sector"]][r_idx],
                                                  TRUE ~ tool[["GICS_sector"]])
        tool[["GICS_subsector"]] <- dplyr::case_when(is.na(tool[["GICS_subsector"]]) ~ company_data[["GICS.Sub.Industry"]][r_idx],
                                                  TRUE ~ tool[["GICS_subsector"]])
      }
      return(tool)
    }
  )
  
  return(tool_data)
}
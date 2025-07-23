# Authors: M Cantele

# Targets from here ------------------------------------------------------------

# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:

library(targets)

getwd()

# Set target options:
tar_option_set(
  packages = c(
    "readxl",
    "data.table",
    "tibble",
    "tidyverse",
    "zoo",
    "haven",
    "writexl"
  ),
  format = "qs",
  garbage_collection = TRUE
)

# functions
functions <- list.files(
  "targets_functions/R",
  pattern = "\\.R$",
  full.names = TRUE
)
sapply(functions, targets::tar_source)

list(
  # File tracking ##############################################################
  # modification will cue targets to rerun
  tar_target(
    name = SP_file,
    command = "data/raw_data/s&p/S&P500_Companies_NBD.csv",
    format = "file"
  ),
  tar_target(
    name = SP_isin,
    command = "data/raw_data/s&p/S&P500_ISIN_TICKERS.xlsx",
    format = "file"
  ),
  tar_target(
    name = MSCI_file,
    command = "data/raw_data/msci/msci_all.xlsx",
    format = "file"
  ),
  tar_target(
    name = IDL_file,
    command = "data/raw_data/IDL/idl_cbf_cbfscore.xlsx",
    format = "file"
  ),
  tar_target(
    name = BIA_file,
    command = "data/raw_data/bia-gbs/BIA-GBS_top100.xlsx",
    format = "file"
  ),
  tar_target(
    name = GID_file,
    command = "data/raw_data/GID/gid_sp500.xlsx",
    format = "file"
  ),
  tar_target(
    name = FS_file,
    command = "data/raw_data/fairsupply/fairsupply_results_august.csv",
    format = "file"
  ),
  tar_target(
    name = GIST_file,
    command = "data/raw_data/GIST/GIST_Impact_PDF_SP500_2018_2022.csv",
    format = "file"
  ),
  tar_target(
    name = NA_file,
    command = "data/raw_data/NA/2024-09/companies_risk_matrix_20240930.csv",
    format = "file"
  ),
  tar_target(
    name = requested_tickers,
    command = "data/raw_data/missing_data.xlsx",
    format = "file"
  ),

  # Tool specific loading, wrangling and endpoint selection ####################
  # use "tar_make(target_name)" is read, "tar_load(target_name)" load into env
  # S&P ------------------------------------------------------------------------
  tar_target(name = SP_raw_data, command = {
    data.table::fread(SP_file)
  }),
  tar_target(name = SP_aux_data, command = {
    data.table::setDT(readxl::read_xlsx(path = SP_isin))
  }),
  # merge ISIN and tickers for S&PS1
  tar_target(
    name = SP_merged_data,
    command = .merge_isin(
      data = SP_raw_data,
      aux_data = SP_aux_data,
      tool = "S&PS1",
      company_col = "COMPANYNAME",
      isin_col = "ISIN",
      ticker_col = "TICKERSYMBOL"
    )
  ),
  # unique endpoints here with description (use "tar_read(unique_SP)" to view)
  tar_target(name = unique_SP, command = {
    SP_raw_data[, c("DATAITEMNAME", "DATAITEMDEFINITION")] |>
      as_tibble() |>
      unique() |>
      arrange(DATAITEMNAME)
  }),

  tar_target(name = SP_requested, command = {
    readxl::read_xlsx(path = requested_tickers, sheet = "S&P")
  }),

  # long format, drop unnecessary columns, add tool name, standard var names
  tar_target(
    name = SP_wrangled_data,
    command = .wrangle_data(
      data = SP_merged_data,
      requested = SP_requested,
      tool = "S&PS1",
      endpoint = "Aggregate Impact: Ecosystem Footprint (HSA)",
      company_col = "COMPANYNAME",
      isin_col = "ISIN",
      ticker_col = "TICKERSYMBOL",
      var_col = "DATAITEMNAME",
      value_col = "DATAITEMVALUE",
      value_adjusted = FALSE
    )
  ),

  # MSCI -----------------------------------------------------------------------
  tar_target(name = MSCI_raw_data, command = {
    data.table::setDT(suppressWarnings(readxl::read_xlsx(path = MSCI_file)))
  }),

  # unique endpoints here with description (use "tar_read(unique_MSCI)" to view)
  tar_target(name = unique_MSCI, command = {
    sort(colnames(MSCI_raw_data)[10:ncol(MSCI_raw_data)])
  }),

  tar_target(
    name = MSCI_wrangled_data,
    command = .wrangle_data(
      data = MSCI_raw_data,
      tool = "MSCI",
      endpoint = "GLOBAL_SPECIES_EXTINCT_RECENT",
      company_col = "ISSUER_NAME",
      isin_col = "ISSUER_ISIN",
      ticker_col = "ISSUER_TICKER",
      id.vars = c(1:8),
      value_adjusted = FALSE
    )
  ),

  tar_target(
    name = MSCI_wrangled_data_va,
    command = .wrangle_data(
      data = MSCI_raw_data,
      tool = "MSCI",
      endpoint = "GLOBAL_SPECIES_EXTINCT_INTENS_RECENT",
      company_col = "ISSUER_NAME",
      isin_col = "ISSUER_ISIN",
      ticker_col = "ISSUER_TICKER",
      id.vars = c(1:8),
      value_adjusted = TRUE
    )
  ),

  # IDL ------------------------------------------------------------------------
  tar_target(name = IDL_raw_data, command = {
    data.table::setDT(readxl::read_xlsx(path = IDL_file))
  }),

  # unique endpoints here with description (use "tar_read(unique_IDL)" to view)
  tar_target(name = unique_IDL, command = {
    sort(colnames(IDL_raw_data)[8:ncol(IDL_raw_data)])
  }),

  # turns out I was chunking about 15 IDL companies that were valid
  # IDL provided a revised value for DD but we should stay consistent
  # tar_target(name = IDL_requested,
  #            command = {
  #              readxl::read_xlsx(path = requested_tickers,
  #                                sheet = "IDL")
  #            }),

  tar_target(
    name = IDL_wrangled_data,
    command = .wrangle_data(
      data = IDL_raw_data,
      tool = "IDL",
      endpoint = "CBF Value",
      company_col = "Entity name",
      isin_col = "ISIN",
      ticker_col = NA,
      year_col = "Year",
      year = 2021,
      id.vars = c(1:7),
      value_adjusted = FALSE
    )
  ),

  tar_target(
    name = IDL_wrangled_data_va,
    command = .wrangle_data(
      data = IDL_raw_data,
      tool = "IDL",
      endpoint = "CBF Indicator Sales Value",
      company_col = "Entity name",
      isin_col = "ISIN",
      ticker_col = NA,
      year_col = "Year",
      year = 2021,
      id.vars = c(1:7),
      value_adjusted = TRUE
    )
  ),

  # BIA-GBS --------------------------------------------------------------------
  tar_target(name = BIA_GBS_raw_data, command = {
    data.table::setDT(readxl::read_xlsx(path = BIA_file, sheet = "Entity Data"))
  }),

  tar_target(name = BIA_GBS_aux_data, command = {
    data.table::setDT(readxl::read_xlsx(
      path = BIA_file,
      sheet = "Instrument Mapping"
    ))
  }),

  # merge ISIN and tickers for S&PS1
  tar_target(
    name = BIA_GBS_merged_data,
    command = .merge_isin(
      data = BIA_GBS_raw_data,
      aux_data = BIA_GBS_aux_data,
      tool = "BIA_GBS",
      company_col = "entityAnalyzedName",
      isin_col = "portfolioIsin",
      ticker_col = NA
    )
  ),

  # unique endpoints here with description (use "tar_read(unique_BIA_GBS)" to view)
  tar_target(name = unique_BIA_GBS, command = {
    sort(colnames(BIA_GBS_merged_data)[12:ncol(BIA_GBS_merged_data)])
  }),

  # unsure whether this metric is value-adjusted, going with TRUE
  tar_target(
    name = BIA_GBS_wrangled_data,
    command = .wrangle_data(
      data = BIA_GBS_merged_data,
      tool = "BIA_GBS",
      endpoint = "corpoFINormalizedScore",
      company_col = "entityName",
      isin_col = "isin",
      ticker_col = NA,
      year_col = "reportYear",
      year = c(2017:2021),
      id.vars = c(1:11),
      value_adjusted = FALSE
    )
  ),

  # GID ------------------------------------------------------------------------

  tar_target(
    name = GID_raw_data,
    command = {
      data.table::setDT(
        readxl::read_xlsx(
          path = GID_file,
          sheet = "Total per company" # or "Data delivery", depending on your needs
        )
      )
    }
  ),

  tar_target(
    name = unique_GID,
    command = {
      sort(colnames(GID_raw_data)[c(4, 5)])
    }
  ),

  tar_target(
    name = GID_wrangled_data,
    command = .wrangle_data(
      data = GID_raw_data,
      tool = "GID",
      endpoint = "total_non_monetized",
      company_col = "Security",
      isin_col = "ISIN",
      ticker_col = "Symbol",
      id.vars = c(1:3),
      value_adjusted = FALSE
    )
  ),

  # Value-adjusted creation

  tar_target(
    name = GID_data_value_adjusted,
    command = {
      readxl::read_xlsx(path = GID_file, sheet = "Data delivery") %>%
        dplyr::filter(!unit_intensity == "$PPP2022/USD2022output") %>%
        dplyr::group_by(Symbol, Security, ISIN) %>%
        dplyr::summarise(intensity = sum(intensity), .groups = "drop")
    }
  ),

  tar_target(
    name = GID_wrangled_data_va,
    command = {
      data_dt <- data.table::as.data.table(GID_data_value_adjusted)
      .wrangle_data(
        data = data_dt,
        tool = "GID",
        endpoint = "intensity",
        company_col = "Security",
        isin_col = "ISIN",
        ticker_col = "Symbol",
        id.vars = c(1:3),
        value_adjusted = TRUE
      )
    }
  ),

  # FS -------------------------------------------------------------------------
  tar_target(name = FS_raw_data, command = {
    data.table::fread(FS_file)
  }),

  # unique endpoints here with description (use "tar_read(unique_GID)" to view)
  tar_target(name = unique_FS, command = {
    sort(colnames(FS_raw_data)[c(5, 6)])
  }),

  tar_target(
    name = FS_wrangled_data,
    command = .wrangle_data(
      ticker_data = SP500_company_data,
      data = FS_raw_data,
      tool = "FS",
      endpoint = "nSTAR",
      company_col = "Company",
      id.vars = 2,
      value_adjusted = TRUE
    )
  ),

  # GIST -----------------------------------------------------------------------
  tar_target(name = GIST_raw_data, command = {
    data.table::fread(GIST_file)
  }),

  tar_target(name = unique_GIST, command = {
    sort(colnames(GIST_raw_data)[c(9:ncol(GIST_raw_data))])
  }),

  tar_target(name = GIST_requested, command = {
    readxl::read_xlsx(path = requested_tickers, sheet = "GIST")
  }),

  tar_target(
    name = GIST_wrangled_data,
    command = .wrangle_data(
      data = GIST_raw_data,
      requested = GIST_requested,
      tool = "GIST",
      endpoint = "TOTAL_GLOBAL_PDF",
      company_col = "COMPANY_NAME",
      isin_col = "ISIN",
      year_col = "REPORTING_YEAR",
      year = c(2018:2022),
      id.vars = c(3, 5, 8),
      value_adjusted = FALSE
    )
  ),
  # NAlpha -------------------------------------------------------------------------
  tar_target(name = NA_raw_data, command = {
    data.table::fread(NA_file)
  }),

  tar_target(name = unique_NA, command = {
    sort(colnames(NA_raw_data)[c(4:ncol(NA_raw_data))])
  }),

  tar_target(
    name = NA_wrangled_data,
    command = .wrangle_data(
      data = NA_raw_data,
      tool = "NAlpha",
      endpoint = "l_module_total",
      company_col = "entity_name",
      isin_col = "entity_isin",
      id.vars = c(2:3),
      value_adjusted = TRUE
    )
  ),

  tar_target(
    name = NA_wrangled_data_va,
    command = .wrangle_data(
      data = NA_raw_data,
      tool = "NAlpha",
      endpoint = "b_module_total",
      company_col = "entity_name",
      isin_col = "entity_isin",
      id.vars = c(2:3),
      value_adjusted = FALSE
    )
  ),

  # Master ISIN-company-ticker-GIC dataset created here ################################################
  tar_target(
    name = GICS_file,
    command = "data/finance_data/gics_industry_classification.xlsx",
    format = "file"
  ),

  tar_target(name = GICS_data, command = {
    readxl::read_xlsx(path = GICS_file, skip = 3, sheet = 1)
  }),

  tar_target(
    name = SP500_company_file,
    command = "data/finance_data/SP500_companies.csv",
    format = "file"
  ),

  tar_target(name = SP500_company_data, command = {
    read.csv(file = SP500_company_file)
  }),

  tar_target(
    name = SP500_isin_file,
    command = "data/finance_data/SNP500_ticker_isin.csv",
    format = "file"
  ),

  tar_target(name = SP500_isin_data, command = {
    read.csv(file = SP500_isin_file)
  }),

  tar_target(
    name = revenue_file,
    command = "data/finance_data/revenue_sales_sp500.xlsx",
    format = "file"
  ),

  tar_target(name = revenue_data, command = {
    readxl::read_xlsx(path = revenue_file)
  }),

  # Final steps ----------------------------------------------------------------
  tar_target(
    name = finance_dat,
    command = .finance_data(
      GICS_data = GICS_data,
      company_data = SP500_company_data,
      isin_data = SP500_isin_data,
      sales_data = revenue_data
    )
  ),

  tar_target(
    name = merged_tool_dat,
    command = .consolidate_data(
      SP = SP_wrangled_data,
      MSCI = MSCI_wrangled_data,
      IDL = IDL_wrangled_data,
      BIA_GBS = BIA_GBS_wrangled_data,
      GID = GID_wrangled_data,
      FS = FS_wrangled_data,
      GIST = GIST_wrangled_data,
      NAlpha = NA_wrangled_data,
      IDL_va = IDL_wrangled_data_va,
      MSCI_va = MSCI_wrangled_data_va,
      NAlpha_va = NA_wrangled_data_va,
      GID_va = GID_wrangled_data_va,
      finance_dat = finance_dat
    )
  ),

  tar_target(
    name = va_supplemented_dat,
    command = .add_va(
      missing = c("S&PS1", "BIA_GBS", "FS", "GIST"),
      data = merged_tool_dat
    )
  ),

  tar_target(
    name = final_tool_dat,
    command = {
      .final_clean(data = va_supplemented_dat) |>
        .convert_tickers(col_ticker = "ticker") # rename certain tickers
    }
  ),

  tar_target(
    name = clean_dat_dir,
    command = {
      clean_data_path <- file.path(getwd(), "data", "tool_data", "cleaned_data")
      if (!dir.exists(clean_data_path)) {
        dir.create(clean_data_path, recursive = TRUE)
      }
      return(clean_data_path)
    },
    format = "file"
  ),

  tar_target(
    name = f_dat_file,
    command = {
      fwrite(
        x = final_tool_dat,
        file = file.path(clean_dat_dir, "tool_dat.csv")
      )
    },
    format = "file"
  )
)

.wrangle_GID_va <- function(data) {
  # Ensure 'intensity' is numeric
  data[, intensity := as.numeric(intensity)]
  
  # Filter rows for the desired unit_intensity values
  dt_filtered <- data[
    unit_intensity %in% c("PDF.ha/USD2022output", "MSA-loss.ha/USD2022output")
  ]
  
  # Sum intensity by the first three ID columns (Security, ISIN, Symbol)
  dt_summary <- dt_filtered[
    , .(endpoint = sum(intensity, na.rm = TRUE)), 
    by = .(Security, ISIN, Symbol)
  ]
  
  # Add fixed metadata
  dt_summary[, tool := "GID"]
  dt_summary[, value_adjusted := TRUE]
  
  # Order columns exactly as requested:
  # 1) tool
  # 2) endpoint
  # 3) Security
  # 4) ISIN
  # 5) Symbol
  # 6) value_adjusted
  setcolorder(dt_summary, c("tool", "endpoint", "Security", "ISIN", "Symbol", "value_adjusted"))
  
  return(dt_summary)
}
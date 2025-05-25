aggregate_GID_wrangled_data_va <- function(data, id_vars = names(data)[1:3], value_col = NULL) {
  # Infer the value column if not provided.
  if (is.null(value_col)) {
    inferred_cols <- setdiff(names(data), id_vars)
    if (length(inferred_cols) != 1) {
      stop("Unable to uniquely determine the value column. Please specify the 'value_col' parameter.")
    }
    value_col <- inferred_cols
  }
  
  # Group by id_vars and sum the values in value_col using data.table syntax.
  aggregated_data <- data[, .(aggregated_value = sum(get(value_col), na.rm = TRUE)), by = id_vars]
  return(aggregated_data)
}
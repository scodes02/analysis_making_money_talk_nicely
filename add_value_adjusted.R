.add_va <- function(missing,
                    data) {
  ls_data <- lapply(X = data,
                    FUN = function(indi) {
                      tool <- unique(indi[["tool"]])
                      if (is.element(tool, missing)) {
                        # value-adjusted to aggregate
                        if (is.element(el = tool, set = c("FS"))) {
                          agg_dt <- copy(indi)
                          agg_dt[, value_adjusted := FALSE]
                          agg_dt[, value := value * sales]
                          indi <- rbindlist(list(indi, agg_dt))
                        } else {
                          # aggregate to value-adjusted
                          va_dt <- copy(indi)
                          va_dt[, value_adjusted := TRUE]
                          va_dt[, value := value / sales]
                          indi <- rbindlist(list(indi, va_dt))
                        }
                      } else {
                        indi
                      }
                    })
  
  data <- rbindlist(ls_data, use.names = TRUE)
}



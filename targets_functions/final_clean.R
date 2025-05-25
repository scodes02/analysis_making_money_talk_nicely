.final_clean <- function(data) {

  # split
  ls_data <- split(data, by = c("tool", "value_adjusted"))
  
  ls_data <- lapply(ls_data, function(d) {
    tool <- unique(d$tool)
    d <- d[!duplicated(ticker)]
    if (tool == "GID") {
      d <- d[!ticker %in% c("GOOGL","FOX")]
    } else if (tool == "FS") {
      d <- d[!ticker %in% c("GEV", "FOX", "FIS")]
    }
    
    # ranking
    if (tool == "IDL") {
      d[["rank"]] <- rank(d[["value"]] * -1, na.last = "keep")
    } else {
      d[["rank"]] <- rank(d[["value"]], na.last = "keep")
    }
    data.table::setorderv(d, cols = "rank", na.last = TRUE)
    return(d)
  })

  data <- rbindlist(ls_data)
  return(data)
}
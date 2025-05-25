.convert_tickers <- function(data, col_ticker = "ticker") {
  data[get(col_ticker) %in% c("BRK.B", "BRK.A"), (col_ticker) := "BRK"]
  data[get(col_ticker) %in% c("GOOG", "GOOGL"), (col_ticker) := "GOOGL"]
  data[get(col_ticker) %in% c("FOXA", "FOX"), (col_ticker) := "FOX"]
  data[get(col_ticker) %in% c("NWSA", "NWS"), (col_ticker) := "NWS"]
  data[get(col_ticker) %in% c("BF.A", "BF.B"), (col_ticker) := "BF.A"]
  return(data)
}
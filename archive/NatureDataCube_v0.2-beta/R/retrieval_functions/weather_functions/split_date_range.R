split_date_range <- function(fromdate, todate, by_days = 7) {
  fromdate <- as.Date(fromdate)
  todate   <- as.Date(todate)
  
  starts <- seq(fromdate, todate, by = paste(by_days, "days"))
  ends   <- pmin(starts + by_days - 1, todate)
  
  data.frame(
    from = starts,
    to   = ends
  )
}

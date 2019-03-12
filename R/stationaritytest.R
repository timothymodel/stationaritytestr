#' Stationarity tests
#'
#' This function runs and neatly presents stationarity tests on a series
#' @param x A time series vector with missing data omitted or removed
#' @param l Number of lags for Ljung-Box, Box-Pierce, and ADF Tests
#' @keywords stationarity tests
#' @export
#' @examples
#' stationarity.tests(dataframe$vector, l = 5)

stationarity.tests <- function(x, l){

  require(tseries)

  lb <- Box.test(x, lag = l, type = "Ljung-Box")
  lb.s <- lb$statistic
  lb.p <- lb$p.value
  lb.null <- "No autocorrelation in series"
  lb.interp <- ifelse(lb$statistic < 0.05, "Series may have autocorelation", "Series may not have autocorrelation")

  bp <- Box.test(x, lag = l, type = "Box-Pierce")
  bp.s <- bp$statistic
  bp.p <- bp$p.value
  bp.null <- "No autocorrelation in series"
  bp.interp <- ifelse(lb$statistic < 0.05, "Series may have autocorelation", "Series may not have autocorrelation")

  adf <- adf.test(x, k = l)
  adf.s <- adf$statistic
  adf.p <- adf$p.value
  adf.null <- "Series has a unit root process"
  adf.interp <- ifelse(lb$statistic < 0.05, "Series may not have unit root", "Series may have unit root")

  kpss <- kpss.test(x, null="Trend")
  kpss.s <- kpss$statistic
  kpss.p <- kpss$p.value
  kpss.null <- "Series is trend-stationary"
  kpss.interp <- ifelse(lb$statistic < 0.05, "Series may not be trend-stationary", "Series may be trend-stationary")

  stationary.table <- tibble(
    "Test" = c("Box-Ljung", "Box-Pierce", "ADF", "KPSS"),
    "Statistic" = c(lb.s, bp.s, adf.s, kpss.s),
    "P Value" = c(lb.p, bp.p, adf.p, kpss.p),
    "H0" = c(lb.null, bp.null, adf.null, kpss.null),
    "Interpretation" = c(lb.interp, bp.interp, adf.interp, kpss.interp)
  )

  print(stationary.table)
}

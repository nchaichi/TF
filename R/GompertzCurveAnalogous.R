Gompertz_Curve_Base <- function(b, k, l, t){

  Cumulative.Adoption <- l*exp(-b*exp(-k*t))

  return(Cumulative.Adoption)
}
#' @title Analogous Gompertz Curve
#'
#' @description Analogous Gompertz Curve uses analogy to determine curve coefficients (b & k) .
#' @param b is a number represents curve coefficients.
#' @param k is a number represents curve coefficients.
#' @param l is a number represents maximum growth can be reached.
#' @param t is an integer shows the growth periods.
#' @return data frame (period, cumulative adoption)
#' @export
#' @keywords
#' @seealso
#' @aliases
#' @examples \dontrun{
#' Gompertz_AC(0.8, 0.2, 10000, 20)
#' }


Gompertz_AC <- function(b, k, l, t){

  period <- seq(1, t, by = 1)
  growth <- sapply(period, function(x) Gompertz_Curve_Base(b, k, l, x))
  table <- cbind.data.frame(period,growth)

  return(table)
}

#' @title Analogous Gompertz Curve Plot
#'
#' @description Analogous Gompertz Curve Plot function plots Analogous Gompertz Curve.
#' @param b is a number represents curve coefficients.
#' @param k is a number represents curve coefficients.
#' @param l is a number represents maximum growth can be reached.
#' @param t is an integer shows the growth periods.
#' @return plot of cumulative adoption over certain period of time.
#' @export
#' @keywords
#' @seealso
#' @aliases
#' @examples \dontrun{
#' Gompertz_AC_Plot(0.8, 0.2, 10000, 20)
#' }

Gompertz_AC_Plot <- function(b, k, l, t){

  plot <- plot(Gompertz_AC(b, k, l, t), xlab = "Period", ylab = "Growth")

  return(plot)

}

Gompertz_AC(0.8, 0.02, 10000, 20)

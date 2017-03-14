Pearl_Curve_Base <- function(a, b, l, t){

  Cumulative.Adoption <- l / (1 + (a*exp(-b*t)))

  return(Cumulative.Adoption)
}

#' @title Analogous pearl Curve
#'
#' @description Analogous Pearl Curve uses analogy to determine curve coefficients (a & b) .
#' @param a is a number represents curve coefficients.
#' @param b is a number represents curve coefficients.
#' @param l is a number represents maximum growth can be reached.
#' @param t is an integer shows the growth periods.
#' @return data frame (period, cumulative adoption)
#' @export
#' @keywords
#' @seealso
#' @aliases
#' @examples \dontrun{
#' Pearl_AC(1.6, 0.8, 10000, 20)
#' }

Pearl_AC <- function(a, b, l, t){

  period <- seq(1, t, by = 1)
  growth <- sapply(period, function(x) Pearl_Curve_Base(a, b, l, x))
  table <- cbind.data.frame(period,growth)

  return(table)

}

#' @title Analogous pearl Curve Plot
#'
#' @description Analogous Pearl Curve Plot function plots Analogous Pearl Curve.
#' @param a is a number represents curve coefficients.
#' @param b is a number represents curve coefficients.
#' @param l is a number represents maximum growth can be reached.
#' @param t is an integer shows the growth periods.
#' @return plot of cumulative adoption over certain period of time.
#' @export
#' @keywords
#' @seealso
#' @aliases
#' @examples \dontrun{
#' Pearl_AC_Plot(1.6, 0.8, 10000, 20)
#' }

Pearl_AC_Plot <- function(a, b, l, t){

  plot <- plot(Pearl_AC(a, b, l, t), xlab = "Period", ylab = "Growth")

  return(plot)

}



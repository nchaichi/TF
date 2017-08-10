# Pearl_Curve_Coeff function estimates the a and b coefficients based on historical data.
Pearl_Curve_Coeff <- function(x, l){

  yp = (x[,2]/l)
  Y <- log(yp/(1-yp), base = exp(1))
  fit <- lm(Y ~ x[,1])
  coeff <- fit$coef
  b <- coeff[2]
  a <- exp(-coeff[1])

  return(list(a,b))
}

# Pearl_Curve_Base calculates the growth for a given time.
Pearl_Curve_Base <- function(a, b, l, t){

  Cumulative.Adoption <- l / (1 + (a*exp(-b*t)))

  return(Cumulative.Adoption)
}

#' @title Historical Pearl Curve
#'
#' @description Historical Pearl Curve function estimates the curve parameters (a & b) based on the historical data.
#' @param x is data frame with two columns to estimates b & k parameters. First column contains the period and second column contains
#' the associated cumulative adoption.
#' @param l is a number represents maximum growth can be reached.
#' @param t is an integer shows the growth periods.
#' @return data frame (period, best fitted cumulative adoption)
#' @export
#' @keywords
#' @seealso
#' @aliases
#' @examples \dontrun{
#' data('CATV')
#' Pearl_HC(CATV,89024390,20)
#' }

Pearl_HC <- function(x, l, t){

  coeff <- unlist(Pearl_Curve_Coeff(x,l))
  a <- as.numeric(coeff[1])
  b <- as.numeric(coeff[2])
  period <- seq(x[1,1], (x[nrow(x),1]+t), by = 1)
  growth <- sapply(period,function(x) Pearl_Curve_Base(a, b, l, x))

  table <- cbind.data.frame(period,growth)

  return(table)

}

#' @title Historical Pearl Curve Plot
#'
#' @description Historical Pearl Curve Plot function plots Historical Pearl Curve growth function results.
#' @param x is data frame with two columns to estimates b & k parameters. First column contains the period and second column contains
#' the associated cumulative adoption.
#' @param l is a number represents maximum growth can be reached.
#' @param t is an integer shows the growth periods.
#' @return plot of best fitted cumulative adoption.
#' @export
#' @keywords
#' @seealso
#' @aliases
#' @examples \dontrun{
#' data('CATV')
#' Pearl_HC_plot(CATV,89024390,20)
#' }

Pearl_HC_plot <- function(x, l, t){

  plot(Pearl_HC(x,l,t), xlab = "Period", ylab = "Growth")

}


# Gompertz_Curve_Coeff function estimates the a and b coefficients based on historical data.
Gompertz_Curve_Coeff <- function(x, l){

  yp <- (l/x[,2])
  Y <-log(log(yp, base = exp(1)) , base = exp(1))
  fit <- lm(Y ~ x[,1])
  coeff <- fit$coef
  k <- -coeff[2]
  b <- exp(coeff[1])

  return(list(k, b))
}

# Gompertz_Curve_Base calculates the growth for a given time.
Gompertz_Curve_Base <- function(b, k, l, t){

  Cumulative.Adoption <- l*exp(-b*exp(-k*t))

  return(Cumulative.Adoption)
}
#' @title Historical Gompertz Curve
#'
#' @description Historical Gomperz Curve function estimates the curve parameters (b & k) based on the historical data.
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
#' Gompertz_HC(PE, 2.81, 20)
#' }


Gompertz_HC <- function(x, l, t){

  coeff <- unlist(Gompertz_Curve_Coeff(x,l))
  k <- as.numeric(coeff[1])
  b <- as.numeric(coeff[2])
  period <- seq(x[1,1], (x[nrow(x),1]+t), by = 1)
  growth <- sapply(period,function(x) Gompertz_Curve_Base(b, k, l, x))

  table <- cbind.data.frame(period,growth)

  return(table)
}

#' @title Historical Gompertz Curve Plot
#'
#' @description Historical Gomperz Curve Plot function plots Historical Gomperz Curve function results.
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
#' Gompertz_HC_plot(PE, 2.81, 250)
#' }

Gompertz_HC_plot <- function(x, l, t){

  plot(Gompertz_HC(x,l,t), xlab = "Period", ylab = "Growth")

}


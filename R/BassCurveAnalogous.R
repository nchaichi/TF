Bass_Curve_Base <- function(p, q, m, t){

  N <- rep(NA,t+1)
  n <- rep(NA,t)
  N[1] <- 0
  for (i in 1:t){
    n[i] <- (m-N[i])*(p+(q/m)*N[i])
    N[i+1] <- n[i]+N[i]
  }

  result <- list(Adoption.Rate = n, Cumulative.Adoption = N[2:(t+1)])
  return (result)
}
#' @title Analogous Bass Curve
#'
#' @description Analogous Bass Curve uses analogy to estimate Bass Curve parameters. In case that there is no
#' data available for technology, parameters of similar cases can be used to forecast growth curve.
#' @param m is a number represents maximum market size can be reached.
#' @param p is a number represents coefficient of innovation.
#' @param q ia a number represents coefficient of immitation.
#' @param t is an integer shows the growth periods.
#' @return data frame (period, adoption rate, cumulative adoption)
#' @export
#' @keywords
#' @seealso
#' @aliases
#' @examples \dontrun{' 
#' Bass_AC(0.016, 0.304, 100000, 20) 
#' }

## Bass_Curve1 function, use analogy to estimate Bass Curve parameters. (Use parameters of similar cases)
## m: market size; p: coefficient of innovation, q: coefficient of imitation, t: forecasting period.
## This helps cases that have no past data.

Bass_AC <- function(p, q, m, t){

  growth <- Bass_Curve_Base(p, q, m, t)
  period <- seq(1,t, by=1)
  table <- cbind.data.frame(period,growth)
  return(table)

}

#' @title Analogous Bass Curve Plot
#'
#' @description Analogous Bass Curve Plot function plots Historical Bass Curve function results.
#' @param m is a number represents maximum market size can be reached.
#' @param p is a number represents coefficient of innovation.
#' @param q ia a number represents coefficient of immitation.
#' @param t is an integer shows the growth periods.
#' @return plot of adoption rate and cumulative adoption
#' @export
#' @keywords
#' @seealso
#' @aliases
#' @examples \dontrun{
#' Bass_AC_Plot(0.016, 0.304, 100000, 20)
#' }

Bass_AC_Plot <- function(p, q, m, t){

  growth <- Bass_AC(p, q, m, t)
  plot(growth[,1], growth[,2], xlab = "Period", ylab = "Adoption Rate")
  plot(growth[,1], growth[,3], xlab = "Period", ylab = "Cumulative Adoption")

}




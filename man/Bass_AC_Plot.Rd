% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BassCurveAnalogous.R
\name{Bass_AC_Plot}
\alias{Bass_AC_Plot}
\title{Analogous Bass Curve Plot}
\usage{
Bass_AC_Plot(p, q, m, t)
}
\arguments{
\item{p}{is a number represents coefficient of innovation.}

\item{q}{ia a number represents coefficient of immitation.}

\item{m}{is a number represents maximum market size can be reached.}

\item{t}{is an integer shows the growth periods.}
}
\value{
plot of adoption rate and cumulative adoption
}
\description{
Analogous Bass Curve Plot function plots Historical Bass Curve function results.
}
\examples{
\dontrun{
Bass_AC_Plot(0.016, 0.304, 100000, 20)
}
}


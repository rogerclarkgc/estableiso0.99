% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iso.densityplot2.R
\name{iso.densityplot2}
\alias{iso.densityplot2}
\title{Draw a densityplot to show the result of the model}
\usage{
iso.densityplot2(mcm.result, numbers, cycle = NULL)
}
\arguments{
\item{mcm.result}{the output of function iso.mcm}

\item{numbers}{the numbers of sources in your model}

\item{cycle}{the core paramater of whole package, do not use if your don't a advanced user}
}
\description{
This function is can generate a densityplot by using the output of function iso.mcm
}
\examples{
source.matrix <- data.frame(d15NPl = c(6.50, 4.42, 11.19, 9.82), d13CPl = c(-11.17, -30.88, -10.19, -15.01), sd.N = c(1.4594632, 2.2680709, 1.1124385, 0.8271039), mean.N = c(6.488984, 4.432160, 11.192613, 9.816280), sd.C = c(1.2149562, 0.6413182, 1.9593306, 1.1724677), mean.C = c(-11.17023, -30.87984, -11.17090, -14.05701))
mixture.matrix <- data.frame(d15NPl = 10.30, d13CPl = -11.58)
sample.list <- iso.euclidean2(source.matrix, mixture.matrix)
mcm.result <- iso.mcm(sample.list, numbers = 4)
iso.densityplot2(mcm.result, numbers = 4)
}


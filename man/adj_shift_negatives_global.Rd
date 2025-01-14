% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adjust-array.R
\name{adj_shift_negatives_global}
\alias{adj_shift_negatives_global}
\title{Shift all values such that the minimum of the array is 0}
\usage{
adj_shift_negatives_global(arr)
}
\arguments{
\item{arr}{array or matrix}
}
\value{
adjusted array
}
\description{
Shift all values such that the minimum of the array is 0
}
\examples{
arr <- array(c(-5, 1:23), c(4, 3, 2))
arr
adj_shift_negatives_global(arr)
}
\seealso{
Other array adjustment functions: 
\code{\link{adj_clamp}()},
\code{\link{adj_gamma}()},
\code{\link{adj_infinite}()},
\code{\link{adj_rescale}()},
\code{\link{adj_shift_negatives_local}()}
}
\concept{array adjustment functions}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pfm.R
\name{write_pfm}
\alias{write_pfm}
\title{Write a numeric array as PFM}
\usage{
write_pfm(arr, filename, endian = "little")
}
\arguments{
\item{arr}{numeric matrix or array (with 3 planes)}

\item{filename}{filename or connection object. If filename ends with ".xz", '.bz2' or
'.gz', then it will be automatically compressed.}

\item{endian}{One of 'little' or 'big'. Default: 'little'}
}
\value{
None.
}
\description{
Write a numeric array as PFM
}
\examples{
arr <- array(runif(10 * 30 * 3), dim = c(10, 30, 3))
write_pfm(arr, tempfile())
}
\seealso{
Other PFM functions: 
\code{\link{read_pfm}()}
}
\concept{PFM functions}

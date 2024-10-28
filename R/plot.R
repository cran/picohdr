
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Plot method for matrices and arrays
#' 
#' @param x matrix or array
#' @param interpolate Default: TRUE
#' @param ... other arguments passed to plot()
#' 
#' @return None.
#' @importFrom grDevices as.raster
#' @examplesIf interactive()
#' filename <- system.file("image/rstats.pfm.bz2", package = "picohdr")
#' image <- read_pfm(filename)
#' image <- adj_gamma(image)
#' plot(image)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot.array <- function(x, interpolate = TRUE, ...) {
  
  # print(dim(x))
  # print(summary(as.vector(x)))
  
  # Adjust down to 3 planes if more provided
  if (!is.matrix(x)) { x <- adj_array3(x)}
  
  if (!is.matrix(x) && dim(x)[3] == 1) {
    dim(x) <- dim(x)[1:2]
  }
  
  if (any(is.infinite(x))) {
    warning("Array has infinite values. Removing them.")
    x <- adj_infinite(x)
  }
  
  if (any(x < 0)) {
    warning("Array contains negative values. Clamping to 0.")
    x <- adj_clamp(x, lo = 0)
  }
  
  if (any(x > 1)) {
    warning("Array contains values above 1. Clamping to 1.")
    x <- adj_clamp(x, hi = 1)
  }
  
  oldpar <- graphics::par(mai = c(0, 0, 0, 0))
  on.exit(graphics::par(oldpar))
  plot(as.raster(x), interpolate = interpolate, ...)
  
  invisible()
}


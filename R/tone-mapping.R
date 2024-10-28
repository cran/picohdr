

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate luminance of an array
# @return matrix
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
calc_luminance <- function(arr) {
    0.2126 * arr[,,1] + 
    0.7152 * arr[,,2] + 
    0.0722 * arr[,,3]
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Reinhard's global tone mapping
#' 
#' Tone mapping is a method for adapting an HDR image for display on a
#' low dynamic range device.  There are three included variants of Reinhard's global
#' tone mapping operator.
#' 
#' @details
#' \describe{
#'   \item{\code{tm_reinhard()}}{[RGB] Reinhard's operator with a correction
#'         for the maximum luminance}
#'   \item{\code{tm_reinhard_basic()}}{[RGB images] Reinhard's operator applied
#'         equally to all colour channels}
#'   \item{\code{tm_reinhard_variant()}}{[RGB or Gray images] A combination of
#'         the above two methods}
#' }
#' These functions are based on Reinhard (2002) 
#' "Photographic tone reproduction for digital images" 
#' 
#' @inheritParams adj_clamp
#' @examples
#' filename <- system.file("image", "rstats.pfm.bz2", package = "picohdr")
#' image <- read_pfm(filename)
#' image <- tm_reinhard_basic(image)
#' image <- adj_gamma(image)
#' plot(image)
#' @return New array with adjusted color values
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tm_reinhard <- function(arr) {
  
  arr <- adj_array3(arr)
  nms <- dimnames(arr)[[3]]
  
  arr  <- adj_infinite(arr)
  
  if (any(arr < 0)) {
    arr <- adj_shift_negatives_local(arr)
  }
  
  im <- arr
  
  Lold <- calc_luminance(im)
  max_white <- max(as.vector(Lold))
  numerator = Lold * (1.0 + (Lold / (max_white * max_white)))
  Lnew <- numerator / (1 + Lold)
  
  fac <- Lnew/Lold
  fac[is.na(fac) | is.nan(fac) | is.infinite(fac)] <- 0
  
  im <- c(
    im[,,1] * fac,
    im[,,2] * fac,
    im[,,3] * fac
  )
  dim(im) <- dim(arr)
  
  
  res <- adj_clamp(im, 0, 1)
  dimnames(res)[[3]] <- nms
  res
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname tm_reinhard
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tm_reinhard_basic <- function(arr) {
  
  arr <- adj_array3(arr)
  nms <- dimnames(arr)[[3]]
  
  arr  <- adj_infinite(arr)
  
  if (any(arr < 0)) {
    arr <- adj_shift_negatives_local(arr)
  }
  
  L <- calc_luminance(arr)
  
  im <- arr
  im <- c(
    im[,,1] / (1 + L),
    im[,,2] / (1 + L),
    im[,,3] / (1 + L)
  )
  dim(im) <- dim(arr)
  
  
  res <- adj_clamp(im, 0, 1)
  dimnames(res)[[3]] <- nms
  res
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname tm_reinhard
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tm_reinhard_variant <- function(arr) {
  
  arr <- adj_array3(arr)
  nms <- dimnames(arr)[[3]]
  arr <- adj_infinite(arr)
  
  if (any(arr < 0)) {
    arr <- adj_shift_negatives_local(arr)
  }
  
  im <- arr
  
  L <- calc_luminance(im)
  
  tv <- im / (1 + im)
  
  im <- as.vector(im) / as.vector((1 + L))
  dim(im) <- dim(arr)  
  
  im <- im + (tv - im) * tv
  
  res <- adj_clamp(im, 0, 1)
  dimnames(res)[[3]] <- nms
  res
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# minimum finite values in the array
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
finite_min <- function(arr) {
  vec <- as.vector(arr)
  vec <- vec[is.finite(vec)]
  fm  <- min(vec, na.rm = TRUE)
  
  if (is.infinite(fm)) {
    fm <- 0
  }
  
  fm
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# maximum finite values in the array
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
finite_max <- function(arr) {
  vec <- as.vector(arr)
  vec <- vec[is.finite(vec)]
  fm  <- max(vec, na.rm = TRUE)
  
  if (is.infinite(fm)) {
    fm <- 1
  }
  
  fm
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Adjust the matrix or array to be an array with three planes
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
adj_array3 <- function(arr) {
  
  if (is.matrix(arr)) {
    d <- dim(arr)
    new_arr <- c(arr, arr, arr)
    dim(new_arr) <- c(d, 3)
    return(new_arr)
  } else if (is.array(arr)) {
    d <- dim(arr)
    if (length(d) != 3) {
      stop("Array must have 3 dimensions")
    } else if (d[2] == 1) {
      return(arr[,,c(1, 1, 1)])
    } else if (d[3] == 3) {
      return(arr)
    } else if (d[3] > 3) {
      return(arr[,,1:3])
    } else if (d[3] == 2) {
      message("Promoting 2-plane array to 3-planes. This won't look great")
      new_arr <- c(arr, arr[,,1])
      d[3] <- 3
      dim(new_arr) <- d
      return(new_arr)
    }
  }
  
  arr
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Clamp values outside the specified range
#' 
#' @param arr array or matrix
#' @param lo low value.  Values lower than this will be replaced with this value.
#'        Default: -Inf
#' @param hi Values higher than this will be replaced with this value. Default: Inf
#' 
#' @return adjusted array
#' @examples
#' arr <- array(1:12, c(4, 3, 1))
#' arr
#' adj_clamp(arr, 10, 20)
#' @family array adjustment functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
adj_clamp <- function(arr, lo = -Inf, hi = Inf) {
  arr[arr < lo] <- lo
  arr[arr > hi] <- hi
  arr
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Replace infinite values with the minimum/maximum of the finite values
#' 
#' @inheritParams adj_clamp
#' 
#' @return adjusted array
#' @examples
#' arr <- array(c(-Inf, Inf, 1:10), c(4, 3, 1))
#' arr
#' adj_infinite(arr)
#' @family array adjustment functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
adj_infinite <- function(arr) {
  
  if (any(is.infinite(arr))) {
    arr[arr == -Inf] <- finite_min(arr)
    arr[arr ==  Inf] <- finite_max(arr)
  }
  
  arr
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Shift all values in a plane such that the minimum in every plane is 0
#' 
#' @inheritParams adj_clamp
#' 
#' @return adjusted array
#' @examples
#' arr <- array(c(-5, 1:23), c(4, 3, 2))
#' arr
#' adj_shift_negatives_local(arr)
#' @family array adjustment functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
adj_shift_negatives_local <- function(arr) {
  
  if (all(arr > 0)) return(arr)
  
  if (any(is.infinite(arr))) {
    warning("Array contains infinite values. Try 'adj_infinite()'")
  }
  
  if (is.matrix(arr)) {
    return( arr - finite_min(arr))
  } else {
    nplanes <- dim(arr)[3]
    for (i in seq_len(nplanes)) {
      arr[,,i] <- arr[,,i] - finite_min(arr[,,i])
    }
    return(arr)
  }
  
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Shift all values such that the minimum of the array is 0
#' 
#' @inheritParams adj_clamp
#' 
#' @return adjusted array
#' @examples
#' arr <- array(c(-5, 1:23), c(4, 3, 2))
#' arr
#' adj_shift_negatives_global(arr)
#' @family array adjustment functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
adj_shift_negatives_global <- function(arr) {
  if (all(arr > 0)) return(arr)
  
  if (any(is.infinite(arr))) {
    warning("Array contains infinite values. Try 'adj_infinite()'")
  }
  
  return(arr - finite_min(arr))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Linearly rescale values to lie between the given limits
#' 
#' Infinite values will be clamped to the limits
#' 
#' @inheritParams adj_clamp
#' @param lo,hi limits
#' 
#' @return adjusted array
#' @examples
#' arr <- array(1:24, c(4, 3, 2))
#' arr
#' adj_rescale(arr, 0, 1)
#' @family array adjustment functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
adj_rescale <- function(arr, lo, hi) {
  
  arr <- adj_infinite(arr)
  fmin <- finite_min(arr)
  fmax <- finite_max(arr)
  
  arr <- (arr - fmin) / (fmax - fmin) * (hi - lo) + lo
  arr
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Adjust gamma
#' 
#' @inheritParams adj_clamp
#' @param gamma gamma correction factor. Default: 1/2.2
#' 
#' @return adjusted array
#' @examples
#' arr <- array(1:12, c(4, 3, 1))
#' arr
#' adj_gamma(arr)
#' @family array adjustment functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
adj_gamma <- function(arr, gamma = 1/2.2) {
  arr ^ gamma
}


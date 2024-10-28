

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NULL operator
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"%||%" <- function (x, y) {
  if (is.null(x)) 
    y 
  else 
    x
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' \code{rbind()} for arrays
#' 
#' Bind multiple arrays by row. Arrays must have the same number of columns
#' and plames
#' 
#' @param ... Arrays.  It is also valid for the first item to be a list
#'        of arrays
#' @return new array
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rbind_arrays <- function(...) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # List of arrays
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  arrs <- list(...)
  if (is.list(arrs[[1]])) {
    arrs <- arrs[[1]]
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # promote to array (from matrix)
  # this will limit the required logic in the remainder of this 
  # function where we'll always assume the objects are arrays
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  arrs <- lapply(arrs, function(arr) {
    if (is.matrix(arr)) {
      dim(arr) <- c(dim(arr), 1)
    }
    
    stopifnot(is.array(arr))
    arr
  })
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # sanity check 
  #  - same number of planes
  #  - same number of columns
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  nplanes <- vapply(arrs, function(arr) dim(arr)[3], integer(1)) |> unique() |> sort()
  ncols   <- vapply(arrs, function(arr) dim(arr)[2], integer(1)) |> unique() |> sort()
  
  if (length(nplanes) != 1) {
    stop("All arrays must have the same 'nplanes', not: ", deparse1(nplanes))
  }
  if (length(ncols) != 1) {
    stop("All arrays must have the same 'ncols', not: ", deparse1(ncols))
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Rbind the individual planes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  planes <- lapply(seq_len(nplanes), function(i) {
    # Take a slice out of each array => matrix represting a single plane
    mats <- lapply(arrs, function(arr) {
      arr <- arr[, , i, drop = FALSE]
      dim(arr) <- dim(arr)[1:2]
      arr
    })
    do.call(rbind, mats)
  })
  
  d  <- dim(planes[[1]])
  d3 <- length(planes)
  
  res <- unlist(planes, use.names = FALSE)
  dim(res) <- c(d, d3)
  res
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Print 'exr_type' objects
#' 
#' @param x exr_type object
#' @param ... other arguments passed on to NextMethod
#' 
#' @return None
#' @examples
#' bbox <- exr_type$box2i(0, 0, 1, 1)
#' print(bbox)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.exr_type <- function(x, ...)  {
  class(x) <- setdiff(class(x), 'exr_type') 
  prefix <- sprintf("EXR %s [%i]", attr(x, 'exr_type'), attr(x, 'exr_size'))
  attr(x, 'exr_type') <- NULL
  attr(x, 'exr_size') <- NULL
  cat(prefix, "\n")
  NextMethod()
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert array to a linear data.frame. Preserves array names if present.
#' 
#' This conversion is useful when preparing the data to summarise with
#' \code{ggplot}.
#' 
#' @param arr array
#' @return data.frame with 'x', 'y', 'z', 'channel' and 'value.'  'channel' will 
#'         be the channel name if found, otherwise it is equivalent to 'z'.
#' @examples
#' arr <- array(1:24, dim = c(4, 3, 2))
#' array_to_df(arr)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
array_to_df <- function(arr) {
  
  # If it's just a matrix, promote to array
  if (is.matrix(arr)) {
    dim(arr) <- c(dim(arr), 1)
  }
  
  # sanity check that we have a 3 plane array
  stopifnot(exprs = {
    is.array(arr)
    length(dim(arr)) == 3
  })
  
  # Get names on array
  n   <- dim(arr)[3]
  nms <- dimnames(arr)[[3]] %||% as.character(seq_len(n))
  
  # How many pixels per plane in the array?
  pixels_in_plane <- prod(dim(arr)[1:2])
  
  # unwrap the array data into a data.frame
  df <- data.frame(
    x       = rep(rep(seq_len(ncol(arr)), each = nrow(arr)), n),
    y       = rep(seq_len(nrow(arr)), ncol(arr) * n),
    z       = rep(seq_len(n), each = pixels_in_plane),
    channel = rep(nms, each = pixels_in_plane),
    value   = as.vector(arr)
  )
  
  df
}


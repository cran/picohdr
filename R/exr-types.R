

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Assertion helpers
# Ensure EXR type constructions process valid values
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
assert_is_float <- function(x, n = 1) {
  stopifnot(exprs = {
    is.atomic(x) 
    length(x)== n 
    is.numeric(x) 
    !anyNA(x)
    all(is.finite(x)) 
  })
}


assert_is_int <- function(x, n = 1) {
  stopifnot(exprs = {
    is.atomic(x) 
    length(x)== n 
    is.numeric(x) 
    !anyNA(x) 
    all(is.finite(x)) 
    isTRUE(all.equal(x, as.integer(x), check.names = FALSE))
  })
}


assert_is_str <- function(x) {
  stopifnot(exprs = {
    is.atomic(x) 
    is.character(x) 
    !anyNA(x) 
  })
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Functions for creating valid EXR type objects
#' 
#' This is a list of functions for creating EXR objects of a particular
#' EXR type.  Each function does checks for argument validity and calculates
#' size information required for EXR output.
#' 
#' @details
#' Refer to official OpenEXR documentation
#' 
#' 
#' @export
#' @examples
#' # Create a v2f type
#' exr_type$v2f(c(12.1, 2.3))
#' 
#' # Create an attribute
#' exr_attrs(copyright = exr_type$string("mike"))
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
exr_type <- list(
  
  box2i = function(xmin, ymin, xmax, ymax) {
    x <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
    assert_is_int(x, 4)
    x <- as.integer(x)
    attr(x, "exr_type") <- "box2i"
    attr(x, "exr_size") <- 16
    class(x) <- union(class(x), "exr_type")
    x
  },
  
  box2f = function(xmin, ymin, xmax, ymax) {
    x <- c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
    assert_is_float(x, 4)
    attr(x, "exr_type") <- "box2f"
    attr(x, "exr_size") <- 16
    class(x) <- union(class(x), "exr_type")
    x
  },
  
  chlist = function(df) {
    stopifnot(exprs = {
      is.data.frame(df)
      nrow(df) > 0
      identical(colnames(df), c('name', 'type', 'pLinear', 'xSampling', 'ySampling'))
    })
    assert_is_str(df$name)
    stopifnot(all(df$type %in% c('uint', 'half', 'float')))
    stopifnot(all(df$pLinear %in% c(0, 1)))
    
    # Each channel takes up size: 
    #  name = nchar(name) + 1
    #  type     = 4
    #  plinear  = 1
    # reserved  = 3
    # xample    = 4
    # ysampling = 4
    #
    # Plus add '1' for NULL byte at end of chlist
    nch <- nrow(df)
    size <- sum(nchar(df$name)) + nch + 
      nch * 16 + 
      1 # nul-terminator
    
    attr(df, "exr_type") <- "chlist"
    attr(df, "exr_size") <- size
    class(df) <- union(class(df), "exr_type")
    df
  },
  
  chromaticities = function(redx, redy, greenx, greeny, bluex, bluey, whitex, whitey) {
    x <- c(redx, redy, greenx, greeny, bluex, bluey, whitex, whitey)
    assert_is_float(x, 8)
    attr(x, "exr_type") <- "chromaticities"
    attr(x, "exr_size") <- 32
    class(x) <- union(class(x), "exr_type")
    x
  },
  
  compression = function(x = c("NONE", "ZIP")) {
    x <- match.arg(x)
    x <- match(x, compression_types) - 1L
    assert_is_int(x)
    attr(x, "exr_type") <- "compression"
    attr(x, "exr_size") <- 1
    class(x) <- union(class(x), "exr_type")
    x
  },
  
  double = function(x) {
    assert_is_float(x)
    attr(x, "exr_type") <- "double"
    attr(x, "exr_size") <- 8
    class(x) <- union(class(x), "exr_type")
    x
  },
  
  envmap = function(x = c('latlong', 'cube')) {
    x <- match.arg(x)
    x <- match(x, c('latlong', 'cube')) - 1L
    
    attr(x, "exr_type") <- "envmap"
    attr(x, "exr_size") <- 1
    class(x) <- union(class(x), "exr_type")
    x
  },
  
  float = function(x) {
    assert_is_float(x)
    attr(x, "exr_type") <- "float"
    attr(x, "exr_size") <- 4
    class(x) <- union(class(x), "exr_type")
    x
  },
  
  int = function(x) {
    assert_is_int(x)
    attr(x, "exr_type") <- "int"
    attr(x, "exr_size") <- 4
    class(x) <- union(class(x), "exr_type")
    x
  },
  
  keycode = function(filmMfcCode, filmType, prefix, count, perfOffset, 
                     perfsPerFrame, perfsPerCount) {
    x <- c(filmMfcCode, filmType, prefix, count, perfOffset, 
           perfsPerFrame, perfsPerCount)
    assert_is_int(x, 7)
    attr(x, "exr_type") <- "keycode"
    attr(x, "exr_size") <- 28
    class(x) <- union(class(x), "exr_type")
    x
  },
  
  lineOrder = function(x = c('increasing', 'decreasing', 'random')) {
    x <- match.arg(x)
    x <- match(x, c('increasing', 'decreasing', 'random')) - 1L
    attr(x, "exr_type") <- "lineOrder"
    attr(x, "exr_size") <- 1
    class(x) <- union(class(x), "exr_type")
    x
  },
  
  m33f = function(x) {
    assert_is_float(x, 9)
    x <- as.vector(t(x))
    attr(x, "exr_type") <- "m33f"
    attr(x, "exr_size") <- 64
    class(x) <- union(class(x), "exr_type")
    x
  },
  
  m44f = function(x) {
    assert_is_float(x, 16)
    x <- as.vector(t(x))
    attr(x, "exr_type") <- "m44f"
    attr(x, "exr_size") <- 64
    class(x) <- union(class(x), "exr_type")
    x
  },
  
  preview = function(x) {stop("'preview' not done")},
  
  rational = function(numerator, denominator) {
    x <- c(numerator, denominator)
    assert_is_int(x, 2)
    attr(x, "exr_type") <- "string"
    attr(x, "exr_size") <- 8
    class(x) <- union(class(x), "exr_type")
    x
  },
  
  string = function(x) {
    assert_is_str(x)
    stopifnot(length(x) == 1)
    attr(x, "exr_type") <- "string"
    attr(x, "exr_size") <- nchar(x) + 1L
    class(x) <- union(class(x), "exr_type")
    x
  },
  
  stringvector = function(x) {
    assert_is_str(x)
    attr(x, "exr_type") <- "stringvector"
    attr(x, "exr_size") <- sum(nchar(x)) + length(x) # remember to add null bytes
    class(x) <- union(class(x), "exr_type")
    x
  },
  
  tiledesc = function(x) {stop("'tiledesc' not done")},
  
  timecode = function(timeAndFlags, userData) {
    x <- c(timeAndFlags, userData)
    assert_is_int(x, 2)
    attr(x, "exr_type") <- "timecode"
    attr(x, "exr_size") <- 8
    class(x) <- union(class(x), "exr_type")
    x
  },
  
  
  v2i = function(x) {
    assert_is_int(x, 2)
    attr(x, "exr_type") <- "v2i"
    attr(x, "exr_size") <- 8
    class(x) <- union(class(x), "exr_type")
    x
  },
  
  v2f = function(x) {
    assert_is_float(x, 2)
    attr(x, "exr_type") <- "v2f"
    attr(x, "exr_size") <- 8
    class(x) <- union(class(x), "exr_type")
    x
  },
  
  v3i = function(x) {
    assert_is_int(x, 3)
    attr(x, "exr_type") <- "v3i"
    attr(x, "exr_size") <- 12
    class(x) <- union(class(x), "exr_type")
    x
  },
  
  v3f = function(x) {
    assert_is_float(x, 3)
    attr(x, "exr_type") <- "v3f"
    attr(x, "exr_size") <- 12
    class(x) <- union(class(x), "exr_type")
    x
  }
)
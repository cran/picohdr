
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Read PFM image
#' 
#' @param filename PFM filename or connection object. If filename ends with 
#'        'xz', 'bz2' or 'gz' suffix then it will be uncompressed automatically.
#'
#' @examples
#' file <- system.file("image/rstats.pfm.bz2", package = "picohdr")
#' arr <- read_pfm(file)
#' arr[1:5, 1:5, ]
#' 
#' # Tone-map the image, gamma correct and plot
#' arr <- tm_reinhard_basic(arr)
#' arr <- adj_gamma(arr)
#' plot(arr)
#' 
#' @return If input PFM file is grayscale, a 2D numeric array is returned.
#'         If PFM file represents RGB color values, a 3D numeric array is returned.
#' @family PFM functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_pfm <- function(filename) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Open the PFM file or connection for reading
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.character(filename)) {
    if (endsWith(tolower(filename), ".xz")) {
      con <- xzfile(filename, "rb")
    } else if (endsWith(tolower(filename), ".bz2")) {
      con <- bzfile(filename, "rb")
    } else if (endsWith(tolower(filename), ".gz")) {
      con <- gzfile(filename, "rb")
    } else {
      con <- file(filename, "rb")
    }
    on.exit(close(con))
  } else if (inherits(filename, "connection")) {
    con <- filename
    if (!isOpen(con)) {
      open(con, "rb")
    }
    on.exit(close(con))
  } else {
    stop("Can't read from the given filename: ", filename)
  }
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # First line must be 'PF' (for RGB) or 'Pf' (for greyscale)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pfm_type  <- ctypesio::scan_str(con, 1)
  if (!pfm_type %in% c('Pf', "PF")) {
    stop("Only RGB/Gray supported currently. Please submit issue on github with a link to the image which generated this error.")
  }
  nchannels <- ifelse(pfm_type == "PF", 3, 1)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Second line in file indicates image dimensions: width height
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  w <- ctypesio::scan_int(con)
  h <- ctypesio::scan_int(con)
  npixels   <- h * w
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Third line is a numeric scaling factor. The sign of this scaling
  # factor indicates endianness. i.e. negative value means little endian data
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pfm_order <- ctypesio::scan_dbl(con)
  endian <- ifelse(pfm_order < 0, "little", "big")
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Read the 32bit floating point values as double.  Data is in form:
  # Row 1:  RGB RGB RGB ... RGB
  # Row 2:  RGB ...
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  arr <- ctypesio::read_f32(con, n = npixels * nchannels, endian = endian)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Reshape linear data to an array
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  arr <- ctypesio::aperm_vector_to_array(
    arr, 
    src = c(planes = nchannels, cols = w, rows = h), 
    flipy = TRUE
  )
  
  
  arr
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Write a numeric array as PFM
#' 
#' @param arr numeric matrix or array (with 3 planes)
#' @param filename filename or connection object. If filename ends with ".xz", '.bz2' or
#'        '.gz', then it will be automatically compressed.
#' @param endian One of 'little' or 'big'. Default: 'little'
#' 
#' @return None.
#' @examples
#' arr <- array(runif(10 * 30 * 3), dim = c(10, 30, 3))
#' write_pfm(arr, tempfile())
#' @family PFM functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_pfm <- function(arr, filename, endian = "little") {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Only 1 and 3 channel arrays are supported
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (length(dim(arr)) == 2 || dim(arr)[3] == 1) {
    nchannels <- 1L
  } else if (dim(arr)[3] == 3) {
    nchannels <- 3L
  } else {
    stop("Unknown format. Must be matrix or array with 3 planes")
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Open the file. With compression if requested.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.character(filename)) {
    if (endsWith(tolower(filename), ".xz")) {
      con <- xzfile(filename, "wb")
    } else if (endsWith(tolower(filename), ".bz2")) {
      con <- bzfile(filename, "wb")
    } else if (endsWith(tolower(filename), ".gz")) {
      con <- gzfile(filename, "wb")
    } else {
      con <- file(filename, "wb")
    }
    on.exit(close(con))
  } else if (inherits(filename, "connection")) {
    con <- filename
    if (!isOpen(con)) {
      open(con, "wb")
    }
    on.exit(close(con))
  } else {
    stop("Can't read from the given 'filename'")
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Write the PFM image header
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (nchannels == 1) {
    ctypesio::write_utf8_raw(con, "Pf\n")
  } else {
    ctypesio::write_utf8_raw(con, "PF\n")
  }
  
  w <- dim(arr)[2]
  h <- dim(arr)[1]
  
  ctypesio::fprintf_raw(con, "%i %i\n", w, h)
  
  if (endian == 'little') {
    ctypesio::write_utf8_raw(con, "-1.000000\n")
  } else if (endian == 'big') {
    ctypesio::write_utf8_raw(con, "1.000000\n")
  } else {
    stop("Unknown 'endian': ", endian)
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Reshape the data to packed row-major order
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dat <- ctypesio::aperm_array_to_vector(
    arr, 
    dst = c('planes', 'cols', 'rows'), 
    flipy = TRUE
  )
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Write the pixel data
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ctypesio::write_f32(con, dat, endian = endian)
  
  invisible(con)
}


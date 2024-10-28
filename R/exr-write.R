
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Calculate bytes-per-pixel for the given pixel type
#' @param pixel_type 'half', 'float' or 'uint'
#' @return integer
#' @examples
#' calc_bytes_per_pixel('half')
#' 
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
calc_bytes_per_pixel <- function(pixel_type) {
  switch(
    pixel_type,
    half  = 2L, 
    float = 4L,
    uint  = 4L,
    {
      # Default
      stop("Unknown pixel_type: ", pixel_type)
    }
  )
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Reorder a scanline block in preparation for compression. Base R version
#' 
#' @param raw_vec all the raw bytes for a single scanline block
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
zip_compress_reorder_r <- function(raw_vec) {
  stopifnot(length(raw_vec) %% 2 == 0)
  v <- as.integer(raw_vec)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # de-interleave lo/hi bites
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  v <- matrix(v, ncol = 2, byrow = TRUE) 
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Apply Predictor
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  p <- v[1]
  for (j in 2:length(v)) {
    d <- v[j] - p + (128  + 256)
    p <- v[j]
    v[j] = d %% 256
  }
  
  as.raw(v)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Slice an array into uncompressed scanline blocks
#' 
#' When compression == 'NONE' these blocks are then written as-is to the file
#' 
#' @param arr an R array
#' @param info the exr metadata
#' @param verbosity verbosity
#'
#' @return list of scanline blocks
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
slice_array_into_uncompressed_scanline_blocks <- function(arr, info, verbosity = 0) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prep
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  h       <- nrow(arr)
  w       <- ncol(arr)
  nplanes <- dim(arr)[3]
  
  scanlines_per_block <- compression_scanlines[[info$compression + 1L]]
  if (verbosity >= 1) message("scanlines per block: ", scanlines_per_block)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # How many scanline blocks are needed?
  # Note: If the height of the image isn't an exact multiple of the 
  # 'scanlines_per_block', then the last block will only be 
  # partially filled
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  n_scanline_blocks <- ceiling(h / scanlines_per_block)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # The last block may or may not be full
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  last_scanlines_per_block <- h %% scanlines_per_block
  if (last_scanlines_per_block == 0) {
    last_scanlines_per_block = scanlines_per_block
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # When writing, only a single pixel type is allowed.
  # i.e. all channels must be the same: 'half', 'float', or 'uint'
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pixel_type <- info$channels$type[[1]]
  bytes_per_pixel <- calc_bytes_per_pixel(pixel_type)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Increasing Y is always used.  This ignores the 'lineOrder' attribute
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ys <- seq(0, h - 1, scanlines_per_block)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Storage for the uncompressed blocks
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  scanline_blocks <- list()
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (i in seq_len(n_scanline_blocks)) {
    
    if (verbosity >= 1) message("Scanline block: ", i, ": ", seek(con))
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # The last block may use a truncated number of scanlines
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (i == n_scanline_blocks) {
      nscanlines <- last_scanlines_per_block
    } else {
      nscanlines <- scanlines_per_block
    }
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Extract the values from the array
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    y     <- ys[i]
    idxs  <- seq(nscanlines)
    block <- arr[idxs + y, , , drop = FALSE]
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Reshape into linear scanlines
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    block <- ctypesio::aperm_array_to_vector(block, c('cols', 'planes', 'rows'))
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Ensure we don't have mixed pixel types
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (length(unique(info$channels$type)) > 1) {
      stop("Mixed types not implented here yet")
    } 
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Transform the slice into the linear sequence expected by EXR
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    block_nbytes <- w * nscanlines * nplanes * bytes_per_pixel
    if (verbosity >= 1) message("block_nbytes: ", block_nbytes, "    array size = ", length(block) * bytes_per_pixel)

    con <- rawConnection(raw(), "wb")
    switch(
      pixel_type,
      half  = write_f16   (con, block),
      float = write_f32   (con, block),
      uint  = write_uint32(con, block),
      {
        # default:
        stop("Writing block data. Unknown pixel type: ", pixel_type)
      }
    )
    bytes <- rawConnectionValue(con)
    close(con)
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Scanline block = 
    #   - y position
    #   - length of the data
    #   - the pixel data
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    scanline_blocks[[length(scanline_blocks) + 1]] <- list(
      y     = y,
      size  = length(bytes),
      bytes = bytes
    )
  }
  
  
  scanline_blocks
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Write a numeric array as an EXR image
#' 
#' @param arr array representing image
#' @param filename filename
#' @param channel_names character vector. names of each plane in the array. 
#'        If \code{NULL} then channel names are extracted from the array 
#'        with \code{dimnames(arr)[[3]]}.  If no names are set on the array, 
#'        then channel names defaults to "Y", "RGB" and 
#'        "RGBA" for 1, 3, and 4 plane arrays respectively.  For 
#'        all other array sizes, channel names allocated alphabetically from
#'        'A' to 'Z'
#' @param pixel_type one of 'half', 'float' or 'double'.  Default: 'half'
#' @param verbosity verbosity. default: 0
#' @param attrs EXR attributes for image. Use \code{\link{exr_attrs}()}
#' 
#' @return None
#' @examples
#' orig_file <- system.file("image", "rstats.pfm.bz2", package = "picohdr")
#' arr <- read_pfm(orig_file)
#' exr_file <- tempfile(fileext = ".exr")
#' write_exr(arr, exr_file)
#' @import ctypesio
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_exr <- function(arr, filename, pixel_type = c('half', 'float', 'uint'), 
                      channel_names = NULL, attrs = exr_attrs(), verbosity = 0) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Ensure the image is an array
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.matrix(arr)) {
    dim(arr) <- c(dim(arr), 1)
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check array
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(exprs = {
    is.array(arr)
    length(dim(arr)) == 3
    all(dim(arr) > 0)
    is.numeric(arr)
    !anyNA(arr)
    all(is.finite(arr))
  })
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # pixel type
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pixel_type <- match.arg(pixel_type)
  bytes_per_pixel <- calc_bytes_per_pixel(pixel_type)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Channel names
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  nplanes <- dim(arr)[3]
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Get the channel names from the array if not specified by the user
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  channel_names <- channel_names %||% dimnames(arr)[[3]]
  
  if (is.null(channel_names)) {
    if (nplanes == 1) {
      channel_names <- 'Y'
    } else if (nplanes == 3) {
      channel_names <- c('R', 'G', 'B')
    } else if (nplanes == 4) {
      channel_names <- c('R', 'G', 'B', 'A')
    } else {
      alpha <- c(LETTERS, paste0(LETTERS, 1), paste0(LETTERS, 2))
      if (nplanes > 52) stop("Too many planes. Max 78. Found: ", nplanes)
      channel_names <- alpha[seq_len(nplanes)]
    }
  }
  stopifnot(length(channel_names) == nplanes)
  channel_order <- order(channel_names)
  arr <- arr[,,channel_order, drop=FALSE]
  channel_names <- sort(channel_names)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check attributes are all named
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  info <- attrs
  nms <- names(info)
  if (length(info) > 0 && (is.null(nms) || anyNA(nms) || any(nms == ""))) {
    stop("All attributes must be named")  
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  info$compression <- info$compression %||% exr_type$compression('ZIP')
  compression_name <- compression_types[info$compression + 1]
  if (!compression_name %in% c('NONE', 'ZIP')) {
    stop("Can only write with 'NONE' or 'ZIP' compression, not: ", compression_name)
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Populate required attributes it not already specified
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  info$displayWindow      <- info$displayWindow      %||% exr_type$box2i(0, 0, ncol(arr) - 1, nrow(arr) - 1)
  info$dataWindow         <- info$dataWindow         %||% info$displayWindow
  info$pixelAspectRatio   <- info$pixelAspectRatio   %||% exr_type$float(1)
  info$screenWindowCenter <- info$screenWindowCenter %||% exr_type$v2f(c(0, 0))
  info$screenWindowWidth  <- info$screenWindowWidth  %||% exr_type$float(1)
  info$lineOrder          <- info$lineOrder          %||% exr_type$lineOrder("increasing")
  
  if (!"channels" %in% nms) {
    chlist <- data.frame(
      name      = channel_names,
      type      = pixel_type, 
      pLinear   = 0,
      xSampling = 1,
      ySampling = 1
    )
    info$channels <- exr_type$chlist(chlist)
  }
  
  nms <- names(info)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Channel types
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(all(info$channels$type %in% c('half', 'float', 'uint')))

  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Set up the connection
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.character(filename)) {
    con <- file(filename, open = "wb")
    on.exit(close(con))
  } else if (inherits(filename, 'connection')) {
    con <- filename
    if (!isOpen(con)) open(con, "wb")
    on.exit(close(con))
  } else {
    stop("Can't write to: ", filename)  
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Start wrting the file !!!!!!!!!!!!!!!!!!!
  # Magic
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  write_int32(con, 20000630)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Version
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  single_tile <- 0L
  long_name   <- 0L
  non_image   <- 0L
  multipart   <- 0L
  version <- 2L %|%
    (single_tile %<<%  9) %|%
    (long_name   %<<% 10) %|%
    (non_image   %<<% 11) %|%
    (multipart   %<<% 12) 
  write_int32(con, version)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Attributes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (i in seq_along(info)) {
    nm  <- nms[[i]]
    if (is.null(nm) || is.na(nm) || nm == "") {
      stop("Bad attribute: ", deparse1(info[i]))
    }
    
    att  <- info[[i]]
    type <- attr(att, "exr_type", exact = TRUE)
    size <- attr(att, "exr_size", exact = TRUE)
    
    if (is.null(type) || is.null(size) || is.na(type) || is.na(size) || size == 0) {
      stop("bad type of size: ", deparse1(info[i]))
    }
    
    # Write: name, type, size
    write_utf8(con, nm)
    write_utf8(con, type)
    write_int32(con, size)
  
    # write value
    switch(
      type,
      box2i    =,
      keycode  =,
      rational =, 
      int      = write_int32(con, att),
      
      chromaticities =,
      v2f   =,
      v3f   =,
      box2f =,
      m33f  =,
      m44f  =,
      float = write_f32(con, att),
      
      lineOrder   =,
      envmap      =,
      compression = write_uint8(con, att),
      
      double = write_f64   (con, att),
      string = write_utf8  (con, att),
      chlist = write_chlist(con, att),
      
      # Default:
      {
        msg <- sprintf("Unhandled attribute: [%s] %s", type, nm)
        stop(msg)
      }
    )
  }  # end of attributes
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Write nul byte to signify end of attributes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  write_uint8(con, 0)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate all scanline offsets. 
  # We are only going to allow "ZIP" compression, but this compression sceheme
  # allows for any individual scanline block to be uncompressed.
  # Thus we can already calculate all the scanline offsets 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (verbosity >= 1) message("Scanline offset table: ", seek(con))
  
  scanlines_per_block <- compression_scanlines[[info$compression + 1L]]
  if (verbosity >= 1) message("scanlines per block: ", scanlines_per_block)
  h <- nrow(arr)
  w <- ncol(arr)
  
  # How many scanline blocks are needed?
  # Note: If the height of the image isn't an exact multiple of the 
  # 'scanlines_per_block', then the last block will only be 
  # partially filled
  n_scanline_blocks <- ceiling(h / scanlines_per_block)
  
  # The last block may or may not be full
  last_scanlines_per_block <- h %% scanlines_per_block
  if (last_scanlines_per_block == 0) {
    last_scanlines_per_block = scanlines_per_block
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Compress Scanline data
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  uncompressed_scanline_blocks <- slice_array_into_uncompressed_scanline_blocks(arr, info, verbosity = verbosity)
  scanline_blocks <- NULL
  
  if (compression_name == 'ZIP') {
    scanline_blocks <- lapply(uncompressed_scanline_blocks, function(block) {
       # reordered_bytes <- zip_compress_reorder_r(block$bytes)
       reordered_bytes <- .Call(zip_compress_reorder_, block$bytes)
       compressed_bytes <- memCompress(reordered_bytes)
       
       if (length(compressed_bytes) >= length(block$bytes)) {
         # Do nothing. we'll stored the uncompressed bytes 
       } else {
         block$bytes <- compressed_bytes
         block$size  <- length(compressed_bytes)
       }
       block
    })
  } else if (compression_name == 'NONE') {
    scanline_blocks <- uncompressed_scanline_blocks
  } else {
    stop("Compression error: Must be either 'ZIP' or 'NONE'")
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Offset start is 
  #   where we are now
  #   PLUS the scanline offset table = n_scanline_blocks * sizeof(uint64)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  offset_start <- seek(con) + (n_scanline_blocks * 8)
  if (verbosity >= 1) message("predicted offset start: ", offset_start)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Write Scanline offset
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sizes <- vapply(scanline_blocks, \(x) x$size, integer(1))
  sizes <- sizes + 4 + 4 
  sizes <- sizes[-length(sizes)]
  chunk_offsets <- offset_start + cumsum(c(0, sizes))
  for (offset in chunk_offsets) {
    if (verbosity >= 1) message("Writing offset location: ", offset)
    write_uint64(con, offset)
  }

  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Write scanline data
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (scanline_block in scanline_blocks) {
    write_int32(con, scanline_block$y)
    write_int32(con, scanline_block$size)
    write_raw  (con, scanline_block$bytes)
  }

  
  if (verbosity >= 1) message("EOF: ", seek(con))
  invisible()
}


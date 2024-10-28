
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Calc the uncompressed scanline block size
#'
#' @param w image width
#' @param nscanlines how many scanlines
#' @param info exr attribute info
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
calc_uncompressed_block_size <- function(w, nscanlines, info) {
  
  # Old naive method (assuming uniform pixeltype i.e. all bytes_per_pixel are the same)
  # size <- w * nscanlines * nchannels * bytes_per_pixel
  
  channels  <- info$channels
  nchannels <- nrow(info$channels)

  size <- ifelse(channels$type == 'half', 2, 4)
  size <- w * nscanlines * sum(size)

  size
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Unpack a scanline block (raw vector) into values.
#' 
#' @param raw_vec raw vector of scanlines
#' @param info EXR metadata
#' 
#' @return unpacked values
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
unpack_raw_to_pixel_values <- function(raw_vec, nscanlines, info) {
  
  con <- rawConnection(raw_vec, "rb")
  on.exit(close(con))

  
  if (all(info$channels$type == 'half')) {
    bytes_per_pixel <- 2
    vals <- ctypesio::read_f16(con, n = length(raw_vec)/bytes_per_pixel, endian = 'little')
  } else if (all(info$channels$type == 'float')) {
    bytes_per_pixel <- 4
    vals <- ctypesio::read_f32(con, n = length(raw_vec)/bytes_per_pixel, endian = 'little')
  } else if (all(info$channels$type == 'uint')) {
    bytes_per_pixel <- 4
    vals <- ctypesio::read_uint32(con, n = length(raw_vec)/bytes_per_pixel, endian = 'little')
  } else {
    # Mixed pixel types
    # stop("unpack_raw_to_pixel_values(): Cannot handle mixed types: ", deparse1(info$channels$type))
    w         <- info$w
    nchannels <- info$nchannels
    
    vals <- lapply(seq_len(nscanlines), function(x) {
      scanline_vals <- lapply(seq_len(nchannels), function(i) {
        ch <- info$channels[i, ]
        
        this_val <- switch(
          ch$type,
          half  = ctypesio::read_f16   (con, n = w, endian = 'little'),
          float = ctypesio::read_f32   (con, n = w, endian = 'little'),
          uint  = ctypesio::read_uint32(con, n = w, endian = 'little'),
          {
            # BAD
            stop("Unknown channel type: ", ch$type)
          }
        )
        
        this_val
      })
      unlist(scanline_vals, recursive = FALSE, use.names = FALSE)
    })
    vals <- unlist(vals, recursive = FALSE, use.names = FALSE)
  }
  
  vals
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Decompress EXR scanline block with compression = ZIP
#' 
#' After decompression, need to 
#' \enumerate{
#'   \item{Undo the predictor}
#'   \item{Interleave the low and high bytes}
#' }
#' @param scaneline_block_raw raw vector of compressed scanline data
#' @param expected_uncompressed_size as the name suggests
#' @param use_c logical. Use the C interface (faster)
#' 
#' @return numeric vector
#' @importFrom ctypesio read_f16
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
decompress_zip_scanline_block <- function(scanline_block_raw, 
                                          expected_uncompressed_size,
                                          nscanlines, 
                                          info,
                                          use_c = TRUE) {
  
  if (length(scanline_block_raw) == expected_uncompressed_size) {
    vals <- unpack_raw_to_pixel_values(scanline_block_raw, nscanlines, info)
    return(vals)
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Decompress the data
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dat <- memDecompress(scanline_block_raw, type = 'gzip')
  
  if (isTRUE(use_c)) {
    # Undo predictor and shuffle
    dat <- .Call(zip_decompress_reorder_, dat)
    
    # Read half-floats from the data    
    vals <- unpack_raw_to_pixel_values(dat, nscanlines, info)
  }  else {
    # R version of undoing predictor and shuffle
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Apply the predictor at the byte level
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    idat <- as.integer(dat)
    for (j in 2:length(dat)) {
      new = idat[j - 1] + idat[j]  - 128
      idat[j] = ifelse(new < 0, new + 256, new %% 256)
    }
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # interleave  16-bit floats
    # before:  lo,lo,lo,lo  ...  hi,hi,hi,hi
    # after :  lo,hi,lo,hi  ...  lo,hi,lo,hi
    #
    # I am unsure on how this de-interleave would happen for f32 or uint pixel types
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    idat <- matrix(idat, nrow = 2, byrow = TRUE) |> as.vector()
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Convert 16-bit floats to 64-bit doubles
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    vals <- unpack_raw_to_pixel_values(idat, nscanlines, info)
  }
  
  
  vals
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Read an EXR image
#' 
#' Currently only single-part scanline images are supported (where the compression
#' is one of NONE, ZIP or ZIPS).
#' 
#' @param filename EXR filename or connection
#' @param verbosity Level of debugging output. Default: 0 (no debugging output)
#' 
#' @return Numeric array with names along the third dimension.  Each plane in the
#'         array corresponds to a channel in the EXR.
#' @examples
#' filename <- system.file("image/rstats.exr", package = "picohdr")
#' images <- read_exr(filename)
#' dimnames(images)[[3]]
#' 
#' # Naively adjust one of the images for display
#' im <- adj_rescale(images[, , 'dzdy'], lo = 0, hi = 1)
#' plot(im)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_exr <- function(filename, verbosity = 0) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Read the metainformation
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  info <- exr_info(filename)
  
  
  if (info$version$desc != "single-part scanline") {
    stop("Image type not handled: ", info$version$desc)
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Open the file connection and set it to close automatically on exit
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.character(filename)) {
    con <- file(filename, open = "rb")
    on.exit(close(con))
  } else if (inherits(filename, 'connection')) {
    con <- filename
    if (!isOpen(con)) open(con, "rb")
    seek(con, 0) # rewind to start of file
    on.exit(close(con))
  } else {
    stop("Can't read from: ", filename)  
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prepare storage for data
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  scanlines_per_block <- compression_scanlines[[info$compression]]
  
  # height + width of the image
  info$h <- h <- info$dataWindow[4] - info$dataWindow[2] + 1
  info$w <- w <- info$dataWindow[3] - info$dataWindow[1] + 1
  
  info$nchannels <- nchannels <- nrow(info$channels)
  
  # The last block may or may not be full
  last_scanlines_per_block <- h %% scanlines_per_block
  if (last_scanlines_per_block == 0) {
    last_scanlines_per_block = scanlines_per_block
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Extract the data for all the scanline blocks
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  scanline_blocks_raw <- list()
  
  if (verbosity > 0) message("Scanline read start")
  for (i in seq_along(info$chunk_offsets)) {
    offset <- info$chunk_offsets[[i]]
    seek(con, offset)
    y    <- ctypesio::read_int32(con)
    size <- ctypesio::read_int32(con)
    if (verbosity > 0) {
      cat(sprintf("Scanline Block [%4i] %6i\n", y, size))
    }  
    scanline_blocks_raw[[i]] <- ctypesio::read_raw(con, size)
  }
  if (verbosity > 0) message("Scanline read done")
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Decompress the scanline blocks
  #
  # This data is row-major, and we'll transpose it later
  # when we insert into the matrices
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  scanline_blocks <- list()
  arrs <- list()
  
  for (i in seq_along(scanline_blocks_raw)) {
    scanline_block_raw <- scanline_blocks_raw[[i]]
    
    if (i == length(scanline_blocks_raw)) {
      nscanlines <- last_scanlines_per_block
    } else {
      nscanlines <- scanlines_per_block
    }
    
    # w * nscanlines * nchannels * bytes_per_pixel
    expected_uncompressed_size <- calc_uncompressed_block_size(w, nscanlines, info)
    
    if (verbosity > 0) {
      message("w         : ", w)
      message("nscanlines: ", nscanlines)
      message("nchannels : ", nchannels)
      message("Data length vs Expected length: ", 
              length(scanline_block_raw), " : ",
              expected_uncompressed_size)
    }
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Decompress raw data
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (info$compression == 'NONE') {
      scanline_blocks[[i]] <- unpack_raw_to_pixel_values(scanline_block_raw, nscanlines, info)
    } else if (info$compression %in% c('ZIP', 'ZIPS')) {
      scanline_blocks[[i]] <- decompress_zip_scanline_block(
        scanline_block_raw, 
        expected_uncompressed_size,
        nscanlines,
        info,
        use_c = TRUE
      )
    } else {
      stop("Decompression not implemented for compression = ", info$compression)
    }
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Permute the linear data into array dimensions
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    arrs[[i]] <- aperm_vector_to_array(
      scanline_blocks[[i]], 
      src = c(cols = w, planes =  nchannels, rows =  nscanlines)
    )
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # rbind all the scanline arrays and add some meta info
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  im <- rbind_arrays(arrs)
  class(im) <- union("exr_image", class(im))
  attr(im, 'channels') <- info$channels$name
  dimnames(im)[[3]] <- info$channels$name
  
  
  im
}


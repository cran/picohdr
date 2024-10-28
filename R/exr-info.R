
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Parse the EXR header information from a connection
#' 
#' @param con connection
#' @return named list of header attributes
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_header <- function(con, verbosity) {
  
  header <- list()
  
  while (1) {
    name <- ctypesio::read_str(con) # attr
    if (name == "") {
      if (verbosity > 0) message("Finished attrs"); 
      break;
    }
    
    type <- ctypesio::read_str  (con) # type
    size <- ctypesio::read_int32(con) # size
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Parse the attribute's value
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    known <- TRUE
    res <- switch(
      type,
      int            = ctypesio::read_int32(con),
      float          = ctypesio::read_f32  (con),
      v2i            = ctypesio::read_int32(con, n = 2),
      v2f            = ctypesio::read_f32  (con, n = 2),
      v3i            = ctypesio::read_int32(con, n = 3),
      v3f            = ctypesio::read_f32  (con, n = 3),
      box2i          = ctypesio::read_int32(con, n = 4),
      box2f          = ctypesio::read_f32  (con, n = 4),
      rational       = read_rational(con),
      m44f           = read_m44f(con),
      m33f           = read_m33f(con),
      stringvector   = read_stringvector(con, size),
      chromaticities = read_chromaticities(con),
      compression    = read_compression(con),
      lineOrder      = read_line_order(con),
      envmap         = read_envmap(con),
      tiledesc       = read_tiledesc(con),
      preview        = read_preview(con),
      chlist         = read_chlist(con, size, verbosity),
      string         = ctypesio::read_str_raw(con, size),  # Read a fixed length string
      {
        message("unhandled attribute: ", type, " size = ", size)
        known <- FALSE
        ctypesio::read_raw(con, size)
      }
    )
    
    if (verbosity > 0) {
      cat(sprintf("%20s (%12s) [%4i]\n", name, type, size))
    }
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Either:
    #  - just the attribute value, or
    #  - A small list with raw information for debugging if attribute type is unknown
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (known) {
      this_attr <- res
    } else {
      this_attr <- list(
        name  = name,
        type  = type,
        size  = size,
        value = res
      )
    }
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Add this attribute to the list of all attributes
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    header <- append(header, setNames(list(this_attr), name))
  }
  
  return(header)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# For a single-part scanline image
#  * the compression type indicates the number of scanlines in each block
#  * Calculate the number of scanline blocks based upon the height of the image
#  * Read in an offset for each scanline block
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_offset_table_for_single_part_scanline <- function(con, info, verbosity) {
  
  # Scan line offset table for single-part
  # The number of scanlines per block depends on the compression scheme
  scanlines_per_block <- compression_scanlines[[info$compression]]
  
  # height + width of the image
  h <- info$dataWindow[4] - info$dataWindow[2] + 1
  w <- info$dataWindow[3] - info$dataWindow[1] + 1
  
  # How many scanline blocks are needed?
  # Note: If the height of the image isn't an exact multiple of the 
  # 'scanlines_per_block', then the last block will only be 
  # partially filled
  n_scanline_blocks <- ceiling(h / scanlines_per_block)
  
  # Read uint64 offsets where each scanline block starts
  info$chunk_offsets <- vapply(seq_len(n_scanline_blocks), function(i) {
    ctypesio::read_uint64(con)
  }, numeric(1))
  
  info
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# For a single-part tiled
#  * the offset table is a sequence of tile offsets (one offset per tile)
#  * have to calculate number of tiles from "info$tiles$xsize" and
#    "info$tiles$ysize"
#  * ???  Currently unsure of:
#     * how different levels are handled e.g. MIPMAP and RIPMAP
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_offset_table_for_single_part_tile <- function(con, info, verbosity) {
  
  h <- info$dataWindow[4] - info$dataWindow[2] + 1
  w <- info$dataWindow[3] - info$dataWindow[1] + 1
  
  # Doing something 
  if (identical(info$tiles$level_mode, "one")) {
    nx <- ceiling(w / info$tiles$xsize)
    ny <- ceiling(h / info$tiles$ysize)
    ntiles <- nx * ny
  } else {
    if (verbosity >= 1) message("Single Part Tile: Offset table parsing not implemented for level_mode = ", 
                                info$tiles$level_mode) 
    ntiles <- 0
  }
  
  info$chunk_offsets <- vapply(seq_len(ntiles), function(i) {
    ctypesio::read_uint64(con)
  }, numeric(1))
  
  info
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# For a multipart image
#  * each part has a separate 'header' in info$headers
#  * each header has a 'chunkCount'
#  * read offsets (N = header$chunkCount) and assign into the corresponding header 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_offset_table_for_multi_part <- function(con, info, verbosity) {
  for (idx in seq_along(info$headers)) {
    header <- info$headers[[idx]]
    if (verbosity >= 1) message("chunkCount: ", header$chunkCount, " ", header$compression)
    
    # Read uint64 offsets where each scanline block starts
    chunk_offsets <- vapply(seq_len(header$chunkCount), function(i) {
      ctypesio::read_uint64(con)
    }, numeric(1))
    
    info$headers[[idx]]$chunk_offsets <- chunk_offsets
  }
  
  if (verbosity >= 1) message("Multipart finished reading at pos: ", seek(con))
  
  return(info)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Extract the metadata from an EXR file
#' 
#' This will extract attributes from any of EXR file.
#' 
#' @param filename EXR filename or connection
#' @param verbosity verbosity. Default: 0
#' 
#' @return Named list of image attributes
#' @examples
#' filename <- system.file("image/rstats.exr", package = "picohdr")
#' exr_info(filename)
#' @importFrom stats setNames
#' @import ctypesio
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
exr_info <- function(filename, verbosity = 0) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Initialise the list() to return
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  info <- list()
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Open the connection in binary mode, and automatically close it 
  # when we exit
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.character(filename)) {
    con <- file(filename, open = "rb")
    on.exit(close(con))
  } else if (inherits(filename, 'connection')) {
    con <- filename
    if (!isOpen(con)) open(con, "rb")
    on.exit(close(con))
  } else {
    stop("Can't read from: ", filename)  
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # EXR must start with proper magic
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  magic <- ctypesio::read_int32(con) # magic
  if (magic != 20000630) {
    stop("Not an EXR file. Expecting magic value '20000630' but got: ", magic)
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Read version information
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  version_int <- ctypesio::read_int32(con) # version
  
  version <- list()
  version$number      <- version_int %&% (2^8 - 1)
  version$single_tile <- as.logical(version_int %&% 0x0200) # Bit  9
  version$long_name   <- as.logical(version_int %&% 0x0400) # Bit 10
  version$non_image   <- as.logical(version_int %&% 0x0800) # Bit 11
  version$multipart   <- as.logical(version_int %&% 0x1000) # Bit 12
  
  version$desc <- with(
    version, 
    if (!single_tile && !non_image && !multipart) {
      "single-part scanline"
    } else if ( single_tile && !non_image && !multipart) {
      "single-part tile"
    } else if (!single_tile && !non_image &&  multipart) {
      "multi-part"
    } else if (!single_tile &&  non_image && !multipart) {
      "single-part deep"
    } else if (!single_tile &&  non_image &&  multipart) {
      "multi-part deep"
    } else {
      "Unknown"
    }
  )
  
  if (verbosity > 0) {
    message("Description: ", version$desc)
  }
  
  info$version <- version

  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # "Multi-part" files have multiple headers
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (isTRUE(info$version$multipart)) {
    
    headers <- list()
    while (TRUE) {
      header <- parse_header(con, verbosity = verbosity)
      if (length(header) == 0) {
        info$headers <- headers
        if (verbosity >= 1) message("exr_info() Multi-part file: Skipping offset table")
        break
      }
      header_list <- list(header)
      if (!is.null(header$name)) {
        names(header_list) <- header$name
      }
      headers <- append(headers, header_list)
      # message("multi-part nheaders: ", length(headers))
    }
    
  } else {
    header <- parse_header(con, verbosity = verbosity)
    info <- append(info, header)
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Parse the chunk offset table
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (info$version$desc == "single-part scanline") {
    info <- parse_offset_table_for_single_part_scanline(con, info, verbosity)
  } else if (info$version$desc == "single-part tile") {
    info <- parse_offset_table_for_single_part_tile(con, info, verbosity)
  } else if (info$version$desc == "multi-part") {
    info <- parse_offset_table_for_multi_part(con, info, verbosity)
  } else {
    if (verbosity >= 0) message("Offset table parsing not implemented for: ", 
                                info$version$desc)
  }
  
  
  info
}


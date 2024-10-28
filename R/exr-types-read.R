
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The functions in this file are for reading compound attributes
# in the EXR file.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read the channel list
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_chlist <- function(con, size, verbosity) {
  # Keep track of how many bytes of channel information we've read
  channel_bytes <- 0
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Read channel data
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  channels   <- c()
  types      <- c()
  pLinears   <- c()
  xSamplings <- c()
  ySamplings <- c()
  while (1) {
    name      <- ctypesio::read_str(con) # channel name
    type      <- ctypesio::read_int32(con) # type
    pLinear   <- ctypesio::read_int32(con) # p linear
    xSampling <- ctypesio::read_int32(con) # x sampling
    ySampling <- ctypesio::read_int32(con) # y sampling
    
    if (verbosity > 0) {
      cat("Channel", length(channels) + 1, ":", name, "\n")
    }
    
    channels   <- c(channels, name)
    types      <- c(types   , pixel_types[type + 1L])
    pLinears   <- c(pLinears, pLinear)
    xSamplings <- c(xSamplings, xSampling)
    ySamplings <- c(ySamplings, ySampling)

    # Keep track of how many bytes we've read
    # Each name has a NULL terminator, so don't forget to add 1 for that
    channel_bytes <- channel_bytes + (nchar(name) + 1) + 16
    
    if (channel_bytes >= size - 1L) break;
  }
  
  ctypesio::read_uint8(con) # NULL terminator for chlist
  data.frame(name = channels, type = types, pLinear = pLinears, xSampling = xSamplings,
             ySampling = ySamplings)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read chromaticities
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_chromaticities <- function(con) {
  
  vals <- ctypesio::read_f32(con, 8)  
  names(vals) <- c('redX', 'redY', 'greenX', 'greenY', 
                   'blueX', 'blueY', 'whiteX', 'whiteY')
  vals
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compression Type object
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_compression <- function(con) {
  type <- ctypesio::read_uint8(con, 1)
  if (type > 9) return("Unknown")
  compression_types[type + 1]
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read envmap
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_envmap <- function(con) {
  type <- ctypesio::read_uint8(con)
  c('LATLONG', 'CUBE')[type + 1]
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# line order type
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_line_order <- function(con) {
  type <- ctypesio::read_uint8(con, 1)
  line_order_types[type + 1]
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3x3 matrix
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_m33f  <- function(con) {
  ctypesio::read_f32(con, n = 9) |>
    matrix(nrow = 3, ncol = 3, byrow = TRUE)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4x4 matrix
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_m44f  <- function(con) {
  ctypesio::read_f32(con, n = 16) |>
    matrix(nrow = 4, ncol = 4, byrow = TRUE)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# read preview as RGBA array
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_preview <- function(con) {
  w    <- ctypesio::read_int32(con)
  h    <- ctypesio::read_int32(con)
  data <- ctypesio::read_uint8(con, 4 * w * h)
  
  
  im <- data / 255
  dim(im) <- c(4, w, h)
  im <- aperm(im, c(3, 2, 1))  
  
  im
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pair: int32/uint32 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_rational <- function(con) {
  numerator   <- ctypesio::read_int32(con)
  denominator <- ctypesio::read_uint32(con)
  
  c(numerator = numerator, denominator = denominator)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Keep reading strings up to a total size
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_stringvector <- function(con, size) {
  
  total_len <- 0  
  strs <- c()
  while(TRUE) {
    len  <- ctypesio::read_int32(con)
    str  <- ctypesio::read_str_raw(con, len)
    # message(len, ": ", str)
    strs <- c(strs, str)
    total_len <- total_len + len + 4 # 4 bytes for 'len' itself
    # print(strs)
    # message(total_len, " / ", size)
    if (total_len >= size) {
      return(strs)
    }
  }
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Tile Desc
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_tiledesc <- function(con) {
  xsize <- ctypesio::read_int32(con)
  ysize <- ctypesio::read_int32(con)
  mode  <- ctypesio::read_uint8(con)
  
  level_mode <- mode %&% 0x01
  level_mode <- c('one', 'mipmap', 'ripmap')[level_mode + 1]
  
  rounding_mode <- mode %>>% 4
  rounding_mode <- c('down', 'up')[rounding_mode + 1]
  
  list(
    xsize         = xsize,
    ysize         = ysize,
    mode          = mode,
    level_mode    = level_mode,
    rounding_mode = rounding_mode
  )
}


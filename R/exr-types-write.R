
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' write 'channels' attribute of type 'chlist'
#' @param con connection
#' @param df data.frame of chlist information. Must have columns: 'type' (half, 
#'        float, uint), 'name', pLinear, 'xSampling', 'ySampling'
#' @return None
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_chlist <- function(con, df) {
  
  for (i in seq_len(nrow(df))) {
    ch <- df[i, ]
    
    type_int <- match(ch$type, pixel_types) - 1L
    
    write_utf8 (con, ch$name)
    write_int32(con, type_int)
    write_int32(con, ch$pLinear)
    write_int32(con, ch$xSampling)
    write_int32(con, ch$ySampling)
  }
  
  # Channel list is nul-terminated
  write_uint8(con, 0)
  invisible(con)
}


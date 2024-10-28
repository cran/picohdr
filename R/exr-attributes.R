
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Number of scanlines processed within each scanline block for
# each of the different compression types
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
compression_scanlines <- list(
  NONE =    1,
  RLE  =    1,
  ZIPS =    1,
  ZIP  =   16,
  PIZ  =   32,
  PXR24 =  16,
  B44   =  32,
  B44A  =  32,
  DWAA  =  32,
  DWAB  = 256
)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Enum for compression types
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
COMPRESSION_NONE  =  0
COMPRESSION_RLE   =  1
COMPRESSION_ZIPS  =  3
COMPRESSION_ZIP   =  4
COMPRESSION_PIZ   =  5
COMPRESSION_PXR24 =  6 
COMPRESSION_B44   =  7
COMPRESSION_B44A  =  8
COMPRESSION_DWAA  =  9
COMPRESSION_DWAB  = 10



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compression type names (in order)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
compression_types <- c(
  'NONE' , 'RLE', 'ZIPS',  'ZIP', 'PIZ',
  'PXR24', 'B44', 'B44A', 'DWAA', 'DWAB'
)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# types of line order
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
line_order_types <- c('increasing', 'decreasing', 'random')
pixel_types      <- c('uint', 'half', 'float')



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# All EXR images must contain:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
required_attributes <- c(
  'channels'          ,
  'compression'       ,
  'dataWindow'        ,
  'displayWindow'     ,
  'lineOrder'         ,
  'pixelAspectRatio'  ,
  'screenWindowCenter',
  'screenWindowWidth' 
)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The types of a bunch of standard attributes
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
exr_attr_type <- list(
  channels           = 'chlist',
  compression        = 'compression', 
  dataWindow         = 'box2i',
  displayWindow      = 'box2i',
  lineOrder          = 'lineOrder',
  pixelAspectRatio   = 'float',
  screenWindowCenter = 'v2f',
  screenWindowWidth  = 'float',
  
  tiles              = 'tileDesc',
  view               = 'text',
  name               = 'string',
  type               = 'string',
  version            = 'int',
  chunkCount         = 'int',
  maxSamplesPerPixel = 'int'
)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Helper function to create attributes for \code{\link{write_exr}()}
#' 
#' The EXR file specification requires particular types to define the 
#' metadata for the image.  This function helps define these metadata
#' attributes.
#' 
#' @details
#' In the majority of cases for basic image output, there is no need to specify
#' anything with this function.  \code{\link{write_exr}()} will create mandatory 
#' attributes required for image output.
#' 
#' @details
#' Note that all values must be an object with class \code{exr_type}.  To 
#' create these types, use \code{exr_type$<TYPE>(...)}.
#' 
#' @param channels            \code{[exr_type$chlist()]} data.frame of channel
#'        information with columns name [string], type ['half', 'float', 'uint'],
#'        pLinear [0, 1], xSampling [0, 1], ySampling [0, 1]
#' @param compression         \code{[exr_type$compression()]} 'NONE' or 'ZIP'
#' @param dataWindow          \code{[exr_type$box2i()]} xmin, ymin, xmax, ymax
#'        of data. Default: image size c(0, 0, w-1, h-1)
#' @param displayWindow       \code{[exr_type$box2i()]} xmin, ymin, xmax, ymax
#'        of display. Default: image size c(0, 0, w-1, h-1)
#' @param lineOrder           \code{[exr_type$lineOrder()]} Line ordering. One of
#'        'increasing', 'decreasing' or 'random'.  Default: 'increasing'
#' @param pixelAspectRatio    \code{[exr_type$float()]}. Default: 1.0
#' @param screenWindowCenter  \code{[exr_type$v2f()]}. Default: c(0.0, 0.0)
#' @param screenWindowWidth   \code{[exr_type$float()]}. Default: 1.0
#' @param ... Other named parameters. value must be of class \code{exr_type}
#'        e.g. \code{myLabel = exr_type$string("potpourri")}.
#'        
#' @return named list of attributes for writing EXR
#' @examples
#' exr_attrs(compression = exr_type$compression("ZIP"), 
#'           name        = exr_type$string("Render 032"))
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
exr_attrs <- function(channels           = NULL,
                      compression        = NULL,
                      dataWindow         = NULL,
                      displayWindow      = NULL,
                      lineOrder          = NULL,
                      pixelAspectRatio   = NULL,
                      screenWindowCenter = NULL,
                      screenWindowWidth  = NULL,
                      ...) {
  
  attrs <- list(...)
  attrs$channels            <- channels           
  attrs$compression         <- compression        
  attrs$dataWindow          <- dataWindow         
  attrs$displayWindow       <- displayWindow      
  attrs$lineOrder           <- lineOrder          
  attrs$pixelAspectRatio    <- pixelAspectRatio   
  attrs$screenWindowCenter  <- screenWindowCenter 
  attrs$screenWindowWidth   <- screenWindowWidth  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check types of all known arguments
  # e.g. compression should be of type 'compression'
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (att_name in names(exr_attr_type)) {
    
    type <- exr_attr_type[[att_name]]
    
    # Check if attribute is present
    if (!is.null(attrs[[att_name]])) {
      
      # Retrieve the attribute value
      val <- attrs[[att_name]]
      
      # It should have an exr_type attribute
      val_type <- attr(val, 'exr_type', exact = TRUE)
      if (is.null(val_type) && (type %in% c('int', 'float', 'double', 'string', 'compression', 'lineOrder'))) {
        # If the attribute is a simple type, then let's try to promote it to an exr_type
        val <- attrs[[att_name]] <- exr_type[[type]](attrs[[att_name]])
      } else if (!identical(val_type, type)) {
        stop("Expected attribute '", att_name, "' to be of type '", type, "' but found: ", val_type)
      }
      
      # thevalues should be of class 'exr_type'
      if (!inherits(val, 'exr_type')) {
        stop("Supplied attribute is not of class 'exr_type': ", att_name)
      }
    }
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Should all be of correct type
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (i in seq_along(attrs)) {
    attr <- attrs[[i]]
    if (!inherits(attr, "exr_type")) {
      stop("All attribute arguments must be of class 'exr_type' (usually created with 'exr_type$<TYPE>()'")
    }
  }
  
  
  attrs
}


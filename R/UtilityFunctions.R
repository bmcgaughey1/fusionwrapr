# Utility functions that are useful for reading and writing FUSION specific formats.
#
# *****************************************************************************
# Function to read FUSION's LDA point format files and create a data frame or
# LAS object (from the lidR package)
# *****************************************************************************
# ---------- readLDA
#
#' FUSION R command line interface -- Read LDA format files and return data frame or LAS-class object
#'
#' \code{readLDA} reads point data stored in FUSION's LDA format and returns either a data frame or
#' a LAS-class object. To create the LAS-class object, you must provide an existing LAS or LAZ
#' file to provide a template for the LAS header. Typically, this would be a file used to clip the
#' LDA file using \code{TreeSeg} (this is the only FUSION program that can't output LAS files). The
#' template file also provides coordinate system information for the LAS-class object.
#'
#' When \code{type = "DF"} (default), the return is a data frame containing the following columns:
#' \enumerate{
#'   \item x
#'   \item y
#'   \item x
#'   \item pulse: pseudo-pulse number. This is generated using the GPS time when the LDA file is created
#'   \item return: return number
#'   \item angle: scan angle rank
#'   \item intensity: intensity
#' }
#'
#' When \code{type = "LAS"}, the LAS-class object returned by \code{readLDA} is somewhat imperfect.
#' Many of the attributes for individual points are not populated with valid values because the LDA
#' format does not have all of the information required to fully populate the point records. In
#' particular, GPS time,number of returns in the pulse, and classification are not available.
#' NumberOfReturns for all point records is set to 1 for all point records. This may cause tools
#' designed to evaluate the validity of LAS data to throw warning since there may be points labeled
#' as return 2, 3, ... yet the NumberOfReturns will still be 1.
#'
#' @param fileName character (\strong{required}): Name of the LDA file containing point data.
#' @param type character: Desired return type: "DF" for a data frame and "LAS" for LAS-class object.
#' @param epsg numeric: EPSG code defining the projection for the point data. This is assigned to the LAS-class
#'   object when \code{type = "LAS"}.
#' @param LASTemplate character: File name (including path) for the LAS/LAZ file that will provide a template
#' for the LAS header. Used only when \code{type = "LAS"}.
#' @return Return value depends on \code{type}. If \code{type = "DF"}, return value is
#'   a (invisible) data frame with the columns listed above. If \code{type = "LAS"}, return value is a
#'   (invisible) LAS-class object compatible with the \strong{lidR} package.
#' @examples
#' \dontrun{
#' pts <- readLDA("points.lda")
#' las <- readLDA("points.lda", type = "LAS", LASTemplate = "source.las")
#' }
#' @family helpers
#' @export
readLDA <- function(
    fileName = NULL,
    type = "DF",
    epsg = NULL,
    LASTemplate = NULL
) {
  # validate parameters
  if (!isOpt(fileName))
    stop("You must provide a fileName!!")

  if (type != "DF" && type != "LAS")
    stop(paste0("Invalid value for type: ", type, " Valid values are \"DF\" or \"LAS\""))

  if (type == "LAS" && is.null(LASTemplate))
    stop("You must provide a file to serve as a template for the LAS header!!")

  con = file(fileName, open = "rb")
  Signaturebytes <- readBin(con, "raw", n = 8, size = 1, endian = "little")

  Signature <- readBin(Signaturebytes, "character", size = 8, endian = "little")
  if (Signature == "LIDARBIN") {
    readBin(con, "raw", 4) # skip bytes...version major
    readBin(con, "raw", 4) # skip bytes...version minor

    # get size of file
    sz <- file.size(fileName)

    # read point records...read "rest" of file (actually specifying total length of file)
    pts <- readBin(con, "raw", n = sz, size = 1, endian = "little", signed = FALSE)

    try(close(con))

    if (length(pts) > 0) {
      ptCount <- length(pts) / 36

      mat <- matrix(pts, ncol = 36, nrow = ptCount, byrow = TRUE)

      # parse...stole this method from Jacob: https://github.com/jstrunk001/RSForTools/blob/main/R/read_las.R
      pulse <- readBin(t(mat[, 1:4]), "integer", n = ptCount, size = 4, signed = TRUE, endian = "little")
      ret <- readBin(t(mat[, 5:8]), "integer", n = ptCount, size = 4, signed = TRUE, endian = "little")
      x <- readBin(t(mat[, 9:16]), "numeric", n = ptCount, size = 8, signed = TRUE, endian = "little")
      y <- readBin(t(mat[, 17:24]), "numeric", n = ptCount, size = 8, signed = TRUE, endian = "little")
      z <- readBin(t(mat[, 25:28]), "numeric", n = ptCount, size = 4, signed = TRUE, endian = "little")
      angle <- readBin(t(mat[, 29:32]), "numeric", n = ptCount, size = 4, signed = TRUE, endian = "little")
      intensity <- readBin(t(mat[, 33:36]), "numeric", n = ptCount, size = 4, signed = TRUE, endian = "little")

      if (type == "LAS") {
        # read header
        header <- lidR::readLASheader(LASTemplate)
        data <- data.frame(   X = x
                              , Y = y
                              , Z = z
                              , ReturnNumber = ret
                              , Intensity = as.integer(intensity)
                              , ScanAngleRank = as.integer(angle)
        )

        # build the LAS object
        las <- lidR::LAS(data, header, check = FALSE)
        las <- lidR::las_quantize(las, TRUE)
        las <- lidR::las_update(las)

        # populate data for points...we don't have good values for these attributes
        las@data$gpstime = 0
        las@data$NumberOfReturns = 1L
        las@data$ScanDirectionFlag = 0L
        las@data$EdgeOfFlightline = 0L
        las@data$Classification = 0L
        las@data$Synthetic_flag = FALSE
        las@data$Keypoint_flag = FALSE
        las@data$Withheld_flag = FALSE
        las@data$UserData = 0L
        las@data$PointSourceID = 0L

        if (!is.null(epsg)) {
          lidR::epsg(las) <- epsg
        }

        invisible(las)
      } else {
        invisible(data.frame(  x = x
                               , y = y
                               , z = z
                               , pulse = pulse
                               , return = ret
                               , angle = angle
                               , intensity = intensity))
      }
    }
  } else {
    try(close(con))

    cat(fileName, " is not a valid LDA file!!")
    invisible(NULL)
  }
}

# *****************************************************************************
# Function to read FUSION's DTM format files and create a data frame or
# raster data object
# *****************************************************************************
# ---------- readDTM
#
#' FUSION R command line interface -- Read DTM format files and return a matrix, RasterLayer, or SpatRaster object
#'
#' \code{readDTM} reads surface and raster data stored in FUSION's DTM format and returns information related
#' to the DTM.
#'
#' When \code{type = "matrix"} (default), the return is a matrix containing the values. Matrix element
#' [1,1] is the value in upper left corner.
#'
#' When \code{type = "terra"}, the return is a SpatRaster object compatible with the terra package.
#'
#' When \code{type = "raster"}, the return is a RasterLayer object compatible with the raster package.
#'
#' @param fileName character (\strong{required}): Name of the DTM format file containing data to be read.
#' @param type character: Desired return type: "matrix" for data values in a matrix, "terra" for SpatRaster object
#'   compatible with the terra package, "raster" for a RasterLayer object compatible with the
#'   raster package, and "header" to return the DTM file header as a data frame.
#' @param epsg numeric: EPSG code defining the projection for the data. This is assigned to the
#'   SpatRaster object when \code{type = "terra"} or the RasterLayer object when \code{type =
#'   "raster"}. You can only specify one of \code{epsg} or \code{crs}, not both.
#' @param crs character: PROJ.4 type description of a Coordinate Reference System (map projection).
#'   You can only specify one of \code{epsg} or \code{crs}, not both.
#' @param negativeToNA boolean: Replace negative values with NA (TRUE) or leave negative values as
#'   is (FALSE). Setting this
#'   to FALSE to preserve negative values can lead to erroneous values for the minimum value when writing the
#'   data to a new DTM using \code{writeDTM}.
#' @return Return value depends on \code{type}. If \code{type = "matrix"}, return value is a
#'   (invisible) matrix containing the values. If \code{type = "terra"}, return value is a
#'   (invisible) SpatRaster object compatible with the \strong{terra} package. If \code{type =
#'   "raster"}, return value is a (invisible) RasterLayer object compatible with the \strong{raster}
#'   package. If \code{type = "header"}, return type is a data frame with the header parameters.
#' @examples
#' \dontrun{
#' df <- readDTM("surface.dtm")
#' dtm <- readLDA("surface.dtm", type = "raster", epsg = 26910)
#' }
#' @family helpers
#' @export
readDTM <- function(
    fileName = NULL,
    type = "matrix",
    epsg = NULL,
    crs = NULL,
    negativeToNA = TRUE
) {
  # validate parameters
  if (!isOpt(fileName))
    stop("You must provide a fileName!!")

  types <- c("terra", "raster", "header", "matrix")

  if (!(tolower(type) %in% types))
    stop(paste0("Invalid value for type: ", type, " Valid values are ", toString(types)))

  if (!is.null(epsg) && !is.null(crs))
    stop("You can only specify one of epsg and crs, not both")

  # open file and read header
  con = file(fileName, open = "rb")
  Signaturebytes <- readBin(con, "raw", n = 21, size = 1, endian = "little")

  Signature <- readBin(Signaturebytes, "character", size = 20, endian = "little")
  if (Signature == "PLANS-PC BINARY .DTM") {
    Namebytes <- readBin(con, "raw", n = 61, size = 1, endian = "little")
    Name <- readBin(Namebytes, "character", size = 60, endian = "little")

    version <- readBin(con, "numeric", 1, 4, endian = "little")
    originX <- readBin(con, "double", 1, 8, endian = "little")
    originY <- readBin(con, "double", 1, 8, endian = "little")
    minZ <- readBin(con, "double", 1, 8, endian = "little")
    maxZ <- readBin(con, "double", 1, 8, endian = "little")
    rotation <- readBin(con, "double", 1, 8, endian = "little")
    columnSpacing <- readBin(con, "double", 1, 8, endian = "little")
    rowSpacing <- readBin(con, "double", 1, 8, endian = "little")
    columns <- readBin(con, "integer", 1, 4, endian = "little")
    rows <- readBin(con, "integer", 1, 4, endian = "little")
    horizontalUnits <- readBin(con, "integer", 1, 2, endian = "little")
    verticalUnits <- readBin(con, "integer", 1, 2, endian = "little")
    storageFormat <- readBin(con, "integer", 1, 2, endian = "little")

    if (version >= 2.0) {
      coordSystem <- readBin(con, "integer", 1, 2, endian = "little")
      coordZone <- readBin(con, "integer", 1, 2, endian = "little")
    } else {
      coordSystem <- 0
      coordZone <- 0
    }

    if (version >= 3.0) {
      horizontalDatum <- readBin(con, "integer", 1, 2, endian = "little")
      verticalDatum <- readBin(con, "integer", 1, 2, endian = "little")
    } else {
      horizontalDatum <- 0
      verticalDatum <- 0
    }

    if (version > 3.0) {
      bias <- readBin(con, "double", 1, 8, endian = "little")
    } else {
      bias <- 0.0
    }

    # if just returning the header, close the file and form the return data frame
    if (type == "header") {
      try(close(con))

      return(invisible(data.frame(Name
                                  , version
                                  , originX
                                  , originY
                                  , minZ
                                  , maxZ
                                  , rotation
                                  , columnSpacing
                                  , rowSpacing
                                  , columns
                                  , rows
                                  , horizontalUnits
                                  , verticalUnits
                                  , storageFormat
                                  , coordSystem
                                  , coordZone
                                  , horizontalDatum
                                  , verticalDatum
                                  , bias
      )
      )
      )
    }

    # jump to start of data values
    seek(con, 200, "start")

    # storage format:
    #   0 = 2-byte integer
    #   1 = 4-byte integer
    #   2 = 4-byte real number
    #   3 = 8-byte real number

    # read values...try reading entire array of values
    if (storageFormat == 0) {
      values <- readBin(con, "integer", n = columns * rows, size = 2, endian = "little", signed = TRUE)
    } else if (storageFormat == 1) {
      values <- readBin(con, "integer", n = columns * rows, size = 4, endian = "little", signed = TRUE)
    } else if (storageFormat == 2) {
      values <- readBin(con, "numeric", n = columns * rows, size = 4, endian = "little", signed = TRUE)
    } else if (storageFormat == 3) {
      values <- readBin(con, "double", n = columns * rows, size = 8, endian = "little", signed = TRUE)
    }

    # close the file
    try(close(con))

    if (length(values) > 0) {
      # we really want to parse this efficiently and then rotate the array so [1,1] is upper left corner
      # and values go across rows
      # found this: https://stackoverflow.com/questions/16496210/rotate-a-matrix-in-r-by-90-degrees-clockwise
      mat <- matrix(values, ncol = rows, nrow = columns, byrow = TRUE)

      # rotate 90 degrees CCW
      mat <- apply(t(mat),2,rev)

      # replace negative values with NA
      if (negativeToNA) mat[mat < 0] <- NA

      if (type == "terra") {
        crs_string <- crs
        if (is.null(crs) && !is.null(epsg)) crs_string <- paste0("EPSG:", epsg)

        if (is.null(crs_string)) {
          sr <- terra::rast(mat
                            , extent = terra::ext(originX, originX + columnSpacing * columns, originY, originY + rowSpacing * rows)
          )
        } else {
          sr <- terra::rast(mat
                            , crs = crs_string
                            , extent = terra::ext(originX, originX + columnSpacing * columns, originY, originY + rowSpacing * rows)
          )
        }

        return(invisible(sr))
      } else if (type == "raster") {
        crs_string <- crs
        if (is.null(crs) && !is.null(epsg)) crs_string <- paste0("+init=epsg:", epsg)

        if (is.null(crs_string)) {
          sr <- raster::raster(mat
                               , xmn = originX
                               , xmx = originX + columnSpacing * columns
                               , ymn = originY
                               , ymx = originY + rowSpacing * rows
          )
        } else {
          sr <- raster::raster(mat
                               , crs = crs_string
                               , xmn = originX
                               , xmx = originX + columnSpacing * columns
                               , ymn = originY
                               , ymx = originY + rowSpacing * rows
          )
        }

        return(invisible(sr))
      } else {
        return(invisible(mat))
      }
    }
  } else {
    try(close(con))

    stop(paste0(fileName, " is not a valid DTM file!!"))
  }
  return(invisible(NULL))
}

# function to write a DTM format file
# writeDTM
# ---------- writeDTM
#
#' FUSION R command line interface -- Write DTM format files from a matrix, RasterLayer, or SpatRaster object
#'
#' \code{writeDTM} writes FUSION's DTM format files from data contained in a matrix, RasterLayer, or SpatRaster object. Any NA values
#' in the data are converted to a value of -1 indicating areas with invalid data. This means that values must all be
#' greater than 0 for use with any of the FUSION tools. While it is possible to store data with negative values, there
#' are much better formats for data than FUSION's DTM format.
#'
#' FUSION DTM format files do not carry any projection information that is useful to other tools. FUSION tools use the minimal
#' projection information stored in DTM files to prevent merging of files with different projections or mixing analyses
#' using files with different projections. The enforcement for projection-related information is fairly lax in FUSION's
#' tools and, while I would like to enhance this information, it is not likely to change.
#'
#' This function is particularly useful for converting surfaces into the DTM format. A simple test using the raster package
#' to read a surface in TIF format and write to the DTM format was over 10 times faster that using gdal_translate to
#' convert the TIF file to ASCII raster and then using FUSION's ASCII2DTM program to convert to DTM format.
#'
#' @param x (\strong{required}): data object containing the values to be written to the DTM file. This can be a simple
#'   matrix, a SpatRaster object, or a SpatialLayer object. Values are assumed to be in rows with the first row and column
#'   in the upper left corner. Use \code{rotate = FALSE} if values are in columns with the first column and row in the
#'   lower left corner (same arrangement as FUSION's DTM files).
#' @param fileName character (\strong{required}): Name for the DTM format file.
#' @param description character: Descriptive name for the DTM. Default is "DTM written by fusionwrapr R package". Length
#'   will be truncated to 60 characters.
#' @template CoordInfo
#' @param originX numeric (\strong{required when \code{x} is matrix}): X value for origin (lower left point in \code{x}).
#' @param originY numeric (\strong{required when \code{x} is matrix}): Y value for origin (lower left point in \code{x}).
#' @param columnSpacing numeric (\strong{required when \code{x} is matrix}): Spacing between columns.
#' @param rowSpacing numeric (\strong{required when \code{x} is matrix}): Spacing between rows.
#' @param rotate boolean: Flag indicating grid of values needs to be rotated before being written to \code{fileName}. See the
#'   description of \code{x} for details regarding the expected arrangement of values in the grid.
#' @param storageFormat integer: Integer value indicating the numeric type for
#'   values stored in the DTM file. The default value (-1) indicates that the
#'   actual data type of the data object is used to dictate the appropriate
#'   format. Storage options when \code{storageFormat = -1} are 2-byte signed
#'   integers for integer values and 4-byte floating point numbers for
#'   non-integer values. Possible values are: 0: 2-byte signed integers, 1:
#'   4-byte signed integers, 2: 4-byte floating point numbers, and 3: 8-byte
#'   floating point numbers. Storing floating point values as integers will
#'   force truncation of the values.
#' @return Returns an invisible boolean value indicating success (TRUE) or failure (FALSE).
#' @examples
#' \dontrun{
#' writeDTM(rast, "test.dtm")
#' }
#' @family helpers
#' @export
writeDTM <- function(
    x,
    fileName = NULL,
    description = "DTM written by fusionwrapr R package",
    xyunits = NULL,
    zunits = NULL,
    coordsys = NULL,
    zone = NULL,
    horizdatum = NULL,
    vertdatum = NULL,
    originX = NULL,
    originY = NULL,
    columnSpacing = NULL,
    rowSpacing = NULL,
    rotate = TRUE,
    storageFormat = -1
) {
  # validate parameters
  if (missing(x))
    stop("You must provide data to write!!")

  if (!isOpt(fileName))
    stop("You must provide a fileName!!")

  # check storageFormat: valid range is -1 to 3
  if (storageFormat < -1 || storageFormat > 3) {
    stop(paste("Invalid value for storageFormat:", storageFormat, "valid values are -1, 0, 1, 2, and 3"))
  }

  # deal with units
  xyunitsInt <- 1
  if (toupper(xyunits) == "F") xyunitsInt <- 0
  if (toupper(xyunits) == "O") xyunitsInt <- 3

  zunitsInt <- 1
  if (toupper(zunits) == "F") zunitsInt <- 0
  if (toupper(zunits) == "O") zunitsInt <- 3

  # check the data type
  if (is.matrix(x)) {
    type <- "matrix"

    columns <- ncol(x)
    rows <- nrow(x)

    if (!isOpt(originX)
        || !isOpt(originY)
        || !isOpt(columnSpacing)
        || !isOpt(rowSpacing)
        || !isOpt(xyunits)
        || !isOpt(zunits)
        || !isOpt(coordsys)
        || !isOpt(zone)
        || !isOpt(horizdatum)
        || !isOpt(vertdatum)
    ) {
      stop("Missing required parameters for matrix: originX, originY, columnSpacing, rowSpacing, xyunits, zunits, coordsys, zone, horizdatum, vertdatum")
    }

    valsInt <- TRUE
    if (typeof(x) == "double")
      valsInt <- FALSE
  } else if (class(x)[[1]] == "SpatRaster") {
    type <- "terra"

    originX <- terra::xmin(x)
    originY <- terra::ymin(x)
    columnSpacing <- terra::xres(x)
    rowSpacing <- terra::yres(x)
    columns <- ncol(x)
    rows <- nrow(x)

    if (!isOpt(xyunits)
        || !isOpt(zunits)
        || !isOpt(coordsys)
        || !isOpt(zone)
        || !isOpt(horizdatum)
        || !isOpt(vertdatum)
    ) {
      stop("Missing required parameters for SpatRaster: xyunits, zunits, coordsys, zone, horizdatum, vertdatum")
    }

    valsInt <- terra::is.int(x)
  } else if (class(x)[[1]] == "RasterLayer") {
    type <- "raster"

    originX <- raster::extent(x)[1]
    originY <- raster::extent(x)[3]
    columnSpacing <- raster::xres(x)
    rowSpacing <- raster::yres(x)
    columns <- x@ncols
    rows <- x@nrows

    if (!isOpt(xyunits)
        || !isOpt(zunits)
        || !isOpt(coordsys)
        || !isOpt(zone)
        || !isOpt(horizdatum)
        || !isOpt(vertdatum)
    ) {
      stop("Missing required parameters for RasterLayer: xyunits, zunits, coordsys, zone, horizdatum, vertdatum")
    }

    valsInt <- TRUE
    if (substr(raster::dataType(x), 1, 1) == "F")
      valsInt <- FALSE

    # check for top to bottom arrangement
    # if (!x@toptobottom)
    #   stop("RasterLayer must be arranged top to bottom!!")
  }

  if (type == "matrix") {
    mat <- x
  } else if (type == "terra") {
    mat <- terra::as.matrix(x, wide = TRUE)
  } else if (type == "raster") {
    mat <- raster::as.matrix(x)
  }

  # get min value...special care needed to omit NA
  minZ <- min(mat[!is.na(mat)])

  # convert NA to -1...anything below 0 is considered NODATA in DTM format
  mat[is.na(mat)] <- -1

  if (rotate) {
    # rotate matrix
    mat <- t(apply(mat, 2, rev))
  }

  # truncate description
  description <- substr(description, 1, 60)

  # check for folder for output file
  verifyFolder(dirname(fileName), runCmd = FALSE, saveCmd = FALSE)

  # open file and write header
  con = file(fileName, open = "wb")
  if (!isOpen(con, rw = "write"))
    stop(paste0("Could not open DTM file:", fileName))

  writeBin("PLANS-PC BINARY .DTM", con, endian = "little")
  writeBin(description, con, endian = "little")
  seek(con, 82, "start")

  writeBin(3.1, con, size = 4, endian = "little")
  writeBin(originX, con, size = 8, endian = "little")
  writeBin(originY, con, size = 8, endian = "little")
  writeBin(minZ, con, size = 8, endian = "little")
  writeBin(max(mat), con, size = 8, endian = "little")
  writeBin(0.0, con, size = 8, endian = "little")
  writeBin(columnSpacing, con, size = 8, endian = "little")
  writeBin(rowSpacing, con, size = 8, endian = "little")
  writeBin(as.integer(columns), con, size = 4, endian = "little")
  writeBin(as.integer(rows), con, size = 4, endian = "little")
  writeBin(as.integer(xyunitsInt), con, size = 2, endian = "little")
  writeBin(as.integer(zunitsInt), con, size = 2, endian = "little")
  if (storageFormat == -1) {
    if (valsInt) {
      writeBin(0L, con, size = 2, endian = "little")
    } else {
      writeBin(2L, con, size = 2, endian = "little")
    }
  } else {
    writeBin(as.integer(storageFormat), con, size = 2, endian = "little")
  }
  writeBin(as.integer(coordsys), con, size = 2, endian = "little")
  writeBin(as.integer(zone), con, size = 2, endian = "little")
  writeBin(as.integer(horizdatum), con, size = 2, endian = "little")
  writeBin(as.integer(vertdatum), con, size = 2, endian = "little")

  # jump to start of data and write values
  seek(con, 200, "start")

  # write values
  if (storageFormat == -1) {
    if (valsInt) {
      writeBin(c(t(mat)), con, size = 2, endian = "little")
    } else {
      writeBin(c(t(mat)), con, size = 4, endian = "little")
    }
  } else {
    if (storageFormat == 0) {
      writeBin(as.integer(c(t(mat))), con, size = 2, endian = "little")
    } else if (storageFormat == 1) {
      writeBin(as.integer(c(t(mat))), con, size = 4, endian = "little")
    } else if (storageFormat == 2) {
      writeBin(as.numeric(c(t(mat))), con, size = 4, endian = "little")
    } else if (storageFormat == 3) {
      writeBin(as.numeric(c(t(mat))), con, size = 8, endian = "little")
    }
  }

  # close file
  try(close(con))

  # return
  return(invisible(TRUE))
}


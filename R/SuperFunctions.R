# *****************************************************************************
# Super-functions that simplify specific tasks using FUSION command line tools.
#
# These functions could be considered as functions that should have been added
# to FUSION but were not. Adding them to the package made sense rather than
# adding new programs.
# *****************************************************************************
# ---------- ClipPlot
#
#' FUSION R command line interface -- Super-function to clip data for plots using the ClipData program.
#'
#' \code{ClipPlot} creates command lines for the FUSION ClipData program and optionally executes them.
#' Command lines are designed to clip a circular or square area centered in the \code{(x,y)}.
#'
#' @template MultipleCommands
#'
#' @param inputspecifier character (\strong{required}): LIDAR data file template, name of a text file containing
#'   a list of file names (must have .txt extension), or a
#'   FUSION catalog file.
#' @param samplefile character (\strong{required}): Name for plot clip file (extension will be added).
#'   \code{samplefile} cannot contain spaces. To save compressed LAS files, specify the .laz extension.
#'   If the folder for the output file does not exist, it will be created
#'   when the function is called even when saving commands to a batch file.
#' @param x numeric (\strong{required}): Easting value for plot location.
#' @param y numeric (\strong{required}): Northing value for plot location.
#' @param radius numeric (\strong{required}): Radius for round (circular) plots or half width for square plots.
#' @param shape numeric: default = 1: Shape of the sample area (0 = rectangle, 1 = circle).
#' @param ... Additional parameters that will be passed to \code{ClipData}.
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' ClipPlot("*.las", "plot_0001.las", 435895.9, 5669341.4, 0, ground = "small.dtm", height = TRUE)
#' }
#' @export
ClipPlot <- function(
    inputspecifier = NULL,
    samplefile = NULL,
    x = NULL,
    y = NULL,
    radius = NULL,
    shape = 1,
    ...
) {
  # check for required options
  if (!isOpt(inputspecifier)
      || !isOpt(samplefile)
      || !isOpt(x)
      || !isOpt(y)
      || !isOpt(radius)
  ) {
    stop("Missing required parameters: inputspecifier, samplefile, x, y, radius")
  }

  # call ClipData returning its return value
  invisible(
    ClipData(inputspecifier,
             samplefile,
             x - radius,
             y - radius,
             x + radius,
             y + radius,
             shape = shape, ...)
  )
}

# ---------- GetSurfaceValues
#
#' FUSION R command line interface -- Super-function to run SurfaceSample to get interpolated values from surface for (X,Y) locations.
#'
#' \code{GetSurfaceValues} builds an input file with the identifier (optional), X, and Y and runs SurfaceSample
#' to interpolate a value from the specified surface. Output is a CSV file with the value.
#'
#' @template MultipleCommands
#'
#' @param df data frame containing columns for the identifier (optional), X, and Y.
#' @param xLabel character: Label for the column containing the X value.
#' @param yLabel character: Label for the column containing the Y value.
#' @param idLabel character: Label for the new column in \code{df} containing the surface value.
#' @param surfaceFile character: Name for the input surface files (PLANS DTM format). \code{surfacefile} may
#'   be a wildcard or text list file (extension .txt).
#' @param ... Additional parameters that will be passed to \code{SurfaceSample}.
#' @return Returns a dataframe with an additional column containing the sampled surface values. If the
#'   \code{surfacefile} does not cover the location or contains invalid data, values for locations will be -1.0.
#' @examples
#' \dontrun{
#' GetSurfaceValues(df, "X", "Y", "ground.dtm")
#' }
#' @export
GetSurfaceValues <- function(
    df,
    xLabel = "X",
    yLabel = "Y",
    idLabel = "Value",
    surfaceFile = NULL,
    ...
) {
  # check parameters
  err <- FALSE
  if (!is.data.frame(df)) {
    message("df is not a data frame")
    err <- TRUE
  } else {
    # check column labels
    if (!(xLabel %in% colnames(df))) {
      message(paste("No column named", xLabel, "in data frame"))
      err <- TRUE
    }
    if (!(yLabel %in% colnames(df))) {
      message(paste("No column named", yLabel, "in data frame"))
      err <- TRUE
    }
  }
  if (err) stop()

  # build new data frame for input and write to temp file
  tdf <- data.frame(X = df[, xLabel], Y = df[, yLabel])
  tFile <- tempfile()
  utils::write.csv(tdf, tFile, row.names = FALSE)

  # call SurfaceSample to get surface values
  outFile <- tempfile()
  SurfaceSample(surfaceFile, tFile, outFile, ...)

  # read output file and merge into original data frame
  tdf <- utils::read.csv(outFile)
  df[[idLabel]] <- tdf[, 3]

  # delete files
  #unlink(tFile)
  #unlink(outFile)

  # return original data frame with new column
  return(df)
}



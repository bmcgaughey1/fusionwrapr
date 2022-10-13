# IntensityImage
# ---------- IntensityImage
#
#' FUSION R command line interface -- Creates images using the intensity values from a point cloud.
#'
#' \code{IntensityImage} creates command lines for the FUSION IntensityImage program and optionally executes them.
#'
#' @template MultipleCommands
#'
#' @param cellsize numeric (\strong{required}): Pixel size for the intensity image (same units as LIDAR data).
#' @param imagefile  character (\strong{required}): Name for the image file.
#' @param datafile character (\strong{required}): Name(s) of lidar data files.
#' @template StandardOptions
#' @template SkipFileCheck
#' @param minint numeric: Minimum intensity percentile used for the image (default: 2.0).
#' @param maxint numeric: Maximum intensity percentile used for the image (default: 98.0).
#' @param intrange character: "min,max": Force the scaling of intensity values using the specified
#'  \code{min} and \code{max} values. Setting the min value to -1 should force the output range to start with
#'  1 instead of 0. Doing this in combination with \code{void="0,0,0"} will allow you to identify areas
#'  with no data in the output image as they will have a value of 0 for all color bands.
#' @param intcell numeric: Cell size multiplier for intermediate images.
#' @param void character: "R,G,B": Color for areas with no data (default is red (255,0,0)).
#' @param allreturns boolean: Use all returns to create the intensity image.
#' @param lowest boolean: Use lowest return in pixel to assign intensity value...should be
#'  used with /allreturns for best effect.
#' @param lowall boolean: Combines \code{lowest=TRUE} and \code{allreturns=TRUE} switches...will have no effect if
#'  used with either \code{lowest=TRUE} or \code{allreturns=TRUE}.
#' @param saveint boolean: Save the intermediate image files.
#' @param rasterorigin boolean: Force alignment to match other raster products generated from point
#'  data (offsets the origin of the image by 1/2 pixel.
#' @param diskonly boolean: Do not attempt to read all returns into memory for processing.
#' @param hist boolean: Produce the intensity histogram data files.
#' @param jpg boolean: Save the intensity image using the JPEG format (default is BMP).
#' @param projection character: "filename": Associate the specified projection \code{filename} with image
#'  products.
#' @param class character: "#,#,#,...": LAS files only: Specifies that only points with classification
#'   values listed are to be included in the subsample.
#'   Classification values should be separated by a comma.
#'   e.g. (2,3,4,5) and can range from 0 to 31.
#'   If the first character in string is ~, the list is interpreted
#'   as the classes you DO NOT want included in the subsample.
#'   e.g. /class:~2,3 would include all class values EXCEPT 2 and 3.
#' @param ignoreoverlap boolean: Ignore points with the overlap flag set (LAS V1.4+ format).
#' @template Use64bit
#' @template RunSaveOptions
#' @template Comment
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' IntensityImage(2.0, "IntImage.jpg", "*.las", jpg = TRUE)
#' }
#' @family LTKFunctions
#' @export
IntensityImage <- function(
    cellsize = NULL,
    imagefile = NULL,
    datafile = NULL,
    quiet = FALSE,
    verbose = FALSE,
    version = FALSE,
    newlog = FALSE,
    log = NULL,
    locale = FALSE,
    nolaszipdll = FALSE,
    skipfilecheck = FALSE,
    minint = NULL,
    maxint = NULL,
    intrange = NULL,
    intcell = NULL,
    void = NULL,
    allreturns = FALSE,
    lowest = FALSE,
    lowall = FALSE,
    saveint = FALSE,
    rasterorigin = FALSE,
    diskonly = FALSE,
    hist =FALSE,
    jpg = FALSE,
    projection = NULL,
    class = NULL,
    ignoreoverlap = FALSE,
    use64bit = TRUE,
    runCmd = TRUE,
    saveCmd = TRUE,
    cmdFile = NULL,
    cmdClear = FALSE,
    echoCmd = FALSE,
    comment = NULL
) {
  # check for required options
  if (!isOpt(cellsize)
      || !isOpt(imagefile)
      || !isOpt(datafile)
  ) {
    stop("Missing required parameters: cellsize, imagefile, datafile")
  }

  # use the global variables to set command dispatch options...global options
  # are only used if the corresponding option was not passed to the function
  if (fusionrEnv$areSet) {
    if (missing(use64bit)) use64bit <- fusionrEnv$use64bit
    if (missing(runCmd)) runCmd <- fusionrEnv$runCmd
    if (missing(saveCmd)) saveCmd <- fusionrEnv$saveCmd
    if (missing(cmdFile)) cmdFile <- fusionrEnv$cmdFile
    if (missing(echoCmd)) echoCmd <- fusionrEnv$echoCmd
  }

  # check for option to run command...if FALSE, check for command file name
  checkRunSaveFile(runCmd, saveCmd, cmdFile)

  # check for folder included in output...will create if it doesn't exist
  verifyFolder(dirname(imagefile), runCmd, saveCmd, cmdFile, cmdClear)

  # if we are saving commands to a file, cmdClear will have done its job in the call to verifyFolder
  cmdClear <- FALSE

  # build command line
  cmd <- programName("IntensityImage", use64bit)

  options <- ""
  required <- ""

  # deal with switches true/false...
  # "standard" options
  options <- addSwitch(options, quiet)
  options <- addSwitch(options, verbose)
  options <- addSwitch(options, version)
  options <- addSwitch(options, newlog)
  options <- addSwitch(options, locale)
  options <- addSwitch(options, nolaszipdll)
  options <- addOption(options, log, TRUE)
  options <- addSwitch(options, skipfilecheck)

  # program-specific options
  options <- addSwitch(options, allreturns)
  options <- addSwitch(options, lowest)
  options <- addSwitch(options, lowall)
  options <- addSwitch(options, saveint)
  options <- addSwitch(options, rasterorigin)
  options <- addSwitch(options, diskonly)
  options <- addSwitch(options, hist)
  options <- addSwitch(options, jpg)
  options <- addSwitch(options, ignoreoverlap)

  # deal with options...
  # program-specific options
  options <- addOption(options, minint)
  options <- addOption(options, maxint)
  options <- addOption(options, intrange)
  options <- addOption(options, intcell)
  options <- addOption(options, void)
  options <- addOption(options, projection)
  options <- addOption(options, class)

  # deal with required parameters...some may have defaults
  required <- addRequired(required, cellsize)
  required <- addRequired(required, imagefile, TRUE)
  required <- addRequired(required, datafile, TRUE)

  echoCommand(cmd, options, required, echoCmd)

  ret <- dispatchCommand(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile, comment)

  if (runCmd) {
    invisible(ret)
  } else {
    invisible(buildCommand(cmd, options, required))
  }
}

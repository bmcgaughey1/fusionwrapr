# ReturnDensity
# ---------- ReturnDensity
#
#' FUSION R command line interface -- Builds a raster data layer containing the number of returns in each cell.
#'
#' \code{ReturnDensity} creates command lines for the FUSION ReturnDensity program and optionally executes them.
#'
#' @template MultipleCommands
#'
#' @param outputfile character (\strong{required}): Name for the output raster file (stored in PLANS DTM format
#'  with .dtm extension). When \code{ascii = TRUE}, the output is stored in ASCII raster format
#' @param cellsize numeric (\strong{required}): Desired grid cell size in the same units as LIDAR data.
#' @param datafile character (\strong{required}): Name(s) of lidar data files.
#' @template StandardOptions
#' @template SkipFileCheck
#' @param first boolean: Use first returns when computing return counts (default is all returns).
#'  Cannot be combined with /last or /only.
#' @param last boolean: Use last returns when computing return counts (default is all returns). Only valid for
#'  LAS or LAZ format files. Cannot be combined with /first or /only.
#' @param only boolean: Use last returns from pulses with only 1 return when computing return counts (default
#'  is allreturns). Only valid for LAS or LAZ format files. Cannot be combined with /first or /last.
#' @param ascii boolean: Output raster data in ASCII raster format instead of PLANS DTM format. Output file extension will be .asc.
#' @param nointpercentile boolean: Do not compute percentile values associated with first return intensity and
#'  output to CSV file. These values are useful to determine the range of values to scale intensity products.
#' @param projection character: "filename": Associate the specified projection file with ASCII raster products.
#' @param class character: "#,#,#,...": LAS files only: Specifies that only points with classification
#'   values listed are to be included in the subsample.
#'   Classification values should be separated by a comma.
#'   e.g. (2,3,4,5) and can range from 0 to 31.
#'   If the first character in string is ~, the list is interpreted
#'   as the classes you DO NOT want included in the subsample.
#'   e.g. /class:~2,3 would include all class values EXCEPT 2 and 3.
#' @param ignoreoverlap boolean: Ignore points with the overlap flag set (LAS V1.4+ format).
#' @param grid character: "X1,X2,Y1,Y2": Force the origin of the output grid to be (X,Y) instead of
#'   computing an origin from the data extents and force the grid to
#'   be W units wide and H units high...W and H will be rounded up to
#'   a multiple of cellsize.
#' @param gridxy character: "X1,X2,Y1,Y2": Force the origin of the output grid to be (X1,Y1) instead
#'   of computing an origin from the data extents and force the grid
#'   to use (X2,Y2) as the upper right corner of the coverage area.
#'   The actual upper right corner will be adjusted to be a multiple
#'   of cellsize.
#' @param align character: Force the origin and extent of the output grid to match the
#'   lower left corner and extent of the specified PLANS format DTM file.
#' @param extent character: Force the origin and extent of the output grid to match the
#'   lower left corner and extent of the specified PLANS format DTM
#'   file but adjust the origin to be an even multiple of the cell
#'   size and the width and height to be multiples of the cell size.
#' @param datacoverage boolean: Check for points within each cell in the output grid. Output a value of 1 if
#'  there are points covering the cell and a value of 0 if not.
#' @template Use64bit
#' @template RunSaveOptions
#' @template Comment
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' ReturnDensity("PointDensity.dtm", 10.0, "*.las")
#' }
#' @family LTKFunctions
#' @export
ReturnDensity <- function(
    outputfile = NULL,
    cellsize = NULL,
    datafile = NULL,
    quiet = FALSE,
    verbose = FALSE,
    version = FALSE,
    newlog = FALSE,
    log = NULL,
    locale = FALSE,
    nolaszipdll = FALSE,
    skipfilecheck = FALSE,
    first = FALSE,
    last = FALSE,
    only = FALSE,
    ascii = FALSE,
    nointpercentile = FALSE,
    projection = NULL,
    class = NULL,
    ignoreoverlap = FALSE,
    grid = NULL,
    gridxy = NULL,
    align = NULL,
    extent = NULL,
    datacoverage = FALSE,
    use64bit = TRUE,
    runCmd = TRUE,
    saveCmd = TRUE,
    cmdFile = NULL,
    cmdClear = FALSE,
    echoCmd = FALSE,
    comment = NULL
) {
  # check for required options
  if (!isOpt(outputfile)
      || !isOpt(cellsize)
      || !isOpt(datafile)
  ) {
    stop("Missing required parameters: outputfile, cellsize, datafile")
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
  verifyFolder(dirname(outputfile), runCmd, saveCmd, cmdFile, cmdClear)

  # if we are saving commands to a file, cmdClear will have done its job in the call to verifyFolder
  cmdClear <- FALSE

  # build command line
  cmd <- programName("ReturnDensity", use64bit)

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
  options <- addSwitch(options, first)
  options <- addSwitch(options, last)
  options <- addSwitch(options, only)
  options <- addSwitch(options, ascii)
  options <- addSwitch(options, nointpercentile)
  options <- addSwitch(options, ignoreoverlap)
  options <- addSwitch(options, datacoverage)

  # deal with options...
  # program-specific options
  options <- addOption(options, projection)
  options <- addOption(options, class)
  options <- addOption(options, grid)
  options <- addOption(options, gridxy)
  options <- addOption(options, align, TRUE)
  options <- addOption(options, extent, TRUE)

  # deal with required parameters...some may have defaults
  required <- addRequired(required, outputfile, TRUE)
  required <- addRequired(required, cellsize, TRUE)
  required <- addRequired(required, datafile, TRUE)

  echoCommand(cmd, options, required, echoCmd)

  ret <- dispatchCommand(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile, comment)

  if (runCmd) {
    invisible(ret)
  } else {
    invisible(buildCommand(cmd, options, required))
  }
}

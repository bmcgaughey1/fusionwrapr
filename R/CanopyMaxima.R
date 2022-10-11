# CanopyMaxima
# ---------- CanopyMaxima
#
#' FUSION R command line interface -- Finds and reports surface maxima using a variable-size window based on surface height.
#'
#' \code{CanopyMaxima} creates command lines for the FUSION CanopyMaxima program and optionally executes them.
#'
#' The default equation used to compute the window size was developed using metric LIDAR return data (XYZ in meters).
#' When using other measurement units, different equation coefficients are needed.
#'
#' The default behavior is to append new trees to an existing file. If you mix outputs from runs with different
#' command line options, column labels may not make sense. This is especially true with the /crad and /minmax options.
#'
#' @template MultipleCommands
#'
#' @param inputfile character (\strong{required}):  Name of the canopy height model file (PLANS DTM with .dtm extension).
#' @param outputfile character (\strong{required}): Name for the output ASCII CSV file that will contain the maxima information.
#' @template StandardOptionsNoPts
#' @param ground character: Use the specified bare-earth surface model to normalize the LIDAR data
#'   file may be wildcard or text list file (extension .txt only).
#' @param threshold numeric: Limit analysis to areas above a height of # units (default: 10.0).
#' @param wse character: "A,B,C,D[,E,F]": Constant and coefficients for the variable window size
#'  equation used to compute the window size given the canopy surface height window:
#'  width = A + B*ht + C*ht^2 + D*ht^3
#'  (defaults for metric: A = 2.51503, B = 0, C = 0.00901, D = 0)
#'  Use A = 8.251, B = 0, C = 0.00274, D = 0 for imperial units .
#'  E and F parameters are for 4th and 5th order polynomial terms.
#' @param mult numeric: Window size multiplier (default: 1.0). Applied after the window size is computed using
#'  \code{wse} options.
#' @param res numeric: Resolution multiplier for intermediate grids (default: 2.0). A value of 2 results in intermediate
#'  grids with twice the number of rows and columns.
#' @param outxy character: "minx,miny,maxx,maxy": Restrict output of tree located outside of the extent defined by
#'  (minx,miny) and (maxx,maxy). Tree on the left and bottom edges will be output, those on the top and right edges will not.
#' @param crad boolean: Output 16 individual crown radii for each tree. Radii start at 3 o'clock and are in counter-clockwise
#'  order at 22.5 degree intervals.
#' @param shape boolean: Create shapefile outputs for the canopy maxima points and the perimeter of the area associated with each maxima.
#' @param img8 boolean: Create an 8-bit image showing local maxima and minima (use when 24 bit image fails due to large canopy model).
#' @param img24 boolean: Create an 24-bit image showing local maxima and minima.
#' @param new boolean: Create a new output file (erase output file if one exists).
#' @param summary boolean: Produce a summary file containing tree height summary statistics.
#' @param projection character: "filename": Associate the specified projection file with shapefile and raster data products.
#' @param minmax numeric: Change the calculation method for the min/max crown width.
#'  Options: 0 = report the max and min crown radii * 2
#'  1 = report the max and min diameters
#'  2 = report N-S diameter and the E-W diameter
#'  3 = report max diameter and the diameter perpendicular to the max diameter line and the rotation to the max line.
#' @template Use64bit
#' @template RunSaveOptions
#' @template Comment
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' CanopyMaxima("chm.dtm", "out.csv")
#' }
#' @family LTKFunctions
#' @export
CanopyMaxima <- function(
    inputfile = NULL,
    outputfile = NULL,
    quiet = FALSE,
    verbose = FALSE,
    version = FALSE,
    newlog = FALSE,
    log = NULL,
    locale = FALSE,
    ground = NULL,
    threshold = NULL,
    wse = NULL,
    mult = NULL,
    res = NULL,
    outxy = NULL,
    crad = FALSE,
    shape = FALSE,
    img8 = FALSE,
    img24 = FALSE,
    new = FALSE,
    summary = FALSE,
    projection = NULL,
    minmax = NULL,
    use64bit = TRUE,
    runCmd = TRUE,
    saveCmd = TRUE,
    cmdFile = NULL,
    cmdClear = FALSE,
    echoCmd = FALSE,
    comment = NULL
) {
  # check for required options
  if (!isOpt(inputfile)
      || !isOpt(outputfile)
  ) {
    stop("Missing required parameters: inputfile, outputfile")
  }

  # use the global variables to set command dispatch options...global options
  # are only used if the corresponding option was not passed to the function
  if (fusionrEnv$areSet) {
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
  cmd <- programName("CanopyMaxima", FALSE)

  options <- ""
  required <- ""

  # deal with switches true/false...
  # "standard" options
  options <- addSwitch(options, quiet)
  options <- addSwitch(options, verbose)
  options <- addSwitch(options, version)
  options <- addSwitch(options, newlog)
  options <- addSwitch(options, locale)
  options <- addOption(options, log, TRUE)

  # program-specific options
  options <- addSwitch(options, crad)
  options <- addSwitch(options, shape)
  options <- addSwitch(options, img8)
  options <- addSwitch(options, img24)
  options <- addSwitch(options, new)
  options <- addSwitch(options, summary)

  # deal with options...
  options <- addOption(options, ground)
  options <- addOption(options, threshold)
  options <- addOption(options, wse)
  options <- addOption(options, mult)
  options <- addOption(options, res)
  options <- addOption(options, outxy)
  options <- addOption(options, projection)
  options <- addOption(options, minmax)

  # deal with required parameters...some may have defaults
  required <- addRequired(required, inputfile, TRUE)
  required <- addRequired(required, outputfile, TRUE)

  echoCommand(cmd, options, required, echoCmd)

  ret <- dispatchCommand(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile, comment)

  if (runCmd) {
    invisible(ret)
  } else {
    invisible(buildCommand(cmd, options, required))
  }
}

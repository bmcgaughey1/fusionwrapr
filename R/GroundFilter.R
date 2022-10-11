# ---------- GroundFilter
#
#' FUSION R command line interface -- Filters a point cloud to identify bare-earth points.
#'
#' \code{GroundFilter} creates command lines for the FUSION GroundFilter program and optionally executes them.
#'
#' @template MultipleCommands
#'
#' @param outputfile character (\strong{required}):  Name for the output point data file (stored in LAS format
#'  when input data are LAS. When \code{lda = TRUE}, the output is stored in LDA format.
#' @param cellsize numeric (\strong{required}): Desired grid cell size in the same units as LIDAR data. This
#'  cell size is used for intermediate surfaces and is not the cell size for the final ground surface model
#'  created using GridSurfaceCreate. This is also the cell size used when \code{surface = TRUE}.
#' @param datafile character (\strong{required}): Name(s) of lidar data files.
#' @template StandardOptions
#' @template SkipFileCheck
#' @param lda boolean: Store output data file in FUSION's LDA format. You cannot use this option with
#'  \code{reclass = TRUE}.
#' @param surface boolean: Create a surface model using the final ground points.
#' @param median numeric: Apply median filter to intermediate surface model using # by # window
#'  (default is no median filter).
#' @param smooth numeric: Apply mean filter to intermediate surface model using # by # window
#'  (default is no smoothing).
#' @param finalsmooth boolean: Apply smoothing after the final iteration before selecting bare-earth points.
#' This option is only used when either \code{median = TRUE} or \code{smooth = TRUE}.
#' @param outlier character: "low,high": Omit points with elevations below \code{low} and above \code{high}.
#' @param gparam numeric: Value of g parameter (default is -2.0).
#' @param wparam numeric: Value of w parameter (default is 2.5).
#' @param aparam numeric: Value of a parameter (default is 1.0).
#' @param bparam numeric: Value of b parameter (default is 4.0).
#' @param tolerance numeric: Tolerance value for final filtering of ground points. If not specified,
#'  weight values are used to determine final ground points. When specified, points with elevations
#'  less than or equal to the final surface elevation plus the tolerance are included in the ground point data set.
#' @param iterations numeric: Number of iterations for the filtering logic (default is 5).
#'  output to CSV file. These values are useful to determine the range of values to scale intensity products.
#' @param class character: "#,#,#,...": LAS files only: Specifies that only points with classification
#'   values listed are to be included in the subsample to be considered as potential ground points.
#'   Classification values should be separated by a comma. For example, class = "2,3,4,5" and can range from 0 to 31.
#'   If the first character in string is ~, the list is interpreted as the classes you DO NOT want included
#'   in the subsample. For example class = "~2,3" would include all class values EXCEPT 2 and 3.
#' @param ignoreoverlap boolean: Ignore points with the overlap flag set (LAS V1.4+ format).
#' @param reclass numeric: Reclassify the identified ground points using class #. Default value is 2.
#'  When the /reclass option is used, all points in the input file(s) are written to the output file.
#'  For ground points, the classification value is changed to #. For all other points, the classification
#'  value remains the same unless the value is the same as the reclass # and the point is not identified
#'  as a ground point. For these points, the class for the point is set to 1. You cannot use the reclass option
#'  with the /lda option.
#' @param extent character: "X1,Y1,X2,Y2": Consider only points within the extent defined by (X1,Y1) and
#'  (X2, Y2) for filtering.
#' @param trim character: "X1,Y1,X2,Y2": Output only points within the extent defined by (X1,Y1) and
#'  (X2, Y2). The \code{trim} option is used along with the \code{extent} option to allow filtering using
#'  points within a larger extent but only output points within the smaller extent. This minimizes edge
#'  artifacts in the final point set and surface created using the points.
#' @param diagnostics boolean: Display diagnostic information and produce diagnostic files.
#' @param precision character: "scaleX,scaleY,scaleZ": Control the scale factor used for X, Y, and Z values
#'  in output LAS files. These values will override the values in the source LAS files. There is rarely
#'  any need for the scale parameters to be smaller than 0.001.
#' @template Use64bit
#' @template RunSaveOptions
#' @template Comment
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' GroundFilter("grnd_pts.las", 15.0, "*.las", iterations = 8)
#' }
#' @family LTKFunctions
#' @export
GroundFilter <- function(
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
    lda = FALSE,
    surface = FALSE,
    median = NULL,
    smooth = NULL,
    finalsmooth = FALSE,
    outlier = NULL,
    gparam = NULL,
    wparam = NULL,
    aparam = NULL,
    bparam = NULL,
    tolerance = NULL,
    iterations = NULL,
    class = NULL,
    ignoreoverlap = FALSE,
    reclass = 2,
    extent = NULL,
    trim = NULL,
    diagnostics = FALSE,
    precision = NULL,
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
  cmd <- programName("GroundFilter", use64bit)

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
  options <- addSwitch(options, lda)
  options <- addSwitch(options, surface)
  options <- addSwitch(options, finalsmooth)
  options <- addSwitch(options, ignoreoverlap)
  options <- addSwitch(options, diagnostics)

  # deal with options...
  # program-specific options
  options <- addOption(options, median)
  options <- addOption(options, smooth)
  options <- addOption(options, outlier)
  options <- addOption(options, gparam)
  options <- addOption(options, wparam)
  options <- addOption(options, aparam)
  options <- addOption(options, bparam)
  options <- addOption(options, tolerance)
  options <- addOption(options, iterations)
  options <- addOption(options, class)
  options <- addOption(options, reclass)
  options <- addOption(options, extent, TRUE)
  options <- addOption(options, trim)
  options <- addOption(options, precision)

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


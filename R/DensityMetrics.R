# DensityMetrics
# ---------- DensityMetrics
#
#' FUSION R command line interface -- Computes point density metrics using elevation-based slices.
#'
#' \code{DensityMetrics} creates command lines for the FUSION DensityMetrics program and optionally executes them.
#'
#' @template MultipleCommands
#'
#' @param groundfile  character (\strong{required}): Name for ground surface model (PLANS DTM with .dtm extension).
#'   May be wildcard or text list file (extension .txt only).
#' @param cellsize numeric (\strong{required}): Desired grid cell size in the same units as LIDAR data.
#' @param slicethickness numeric (\strong{required}): Slice thickness for density metrics in the same units
#'  as LIDAR data.
#' @param outputfile character (\strong{required}): Base name for output file. Metrics are stored in CSV format with
#'   .csv extension unless the /nocsv switch is used. Other outputs are stored in files named using the
#'   base name and additional descriptive information. If the folder for the output file does not exist,
#'   it will be created  when the function is called even when saving commands to a batch file.
#' @param datafile character (\strong{required}): Name(s) of lidar data files.
#' @template StandardOptions
#' @template SkipFileCheck
#' @param outlier character: "low,high": Omit points with elevations below low and above high.
#'   When used with data that has been normalized using a ground
#'   surface, low and high are interpreted as heights above ground.
#'   You should use care when using /outlier:low,high with /minht and
#'   /maxht options. If the low value specified with /outlier is above
#'   the value specified with /minht, the value for /outlier will
#'   override the value specified for /minht. Similarly, if the high
#'   value specified with /outlier is less than the value specified
#'   for /maxht, the /outlier value will override the value for
#'   /maxht.
#' @param maxsliceht numeric: "high": Limit the range of height slices from 0 to \code{high}.
#' @param ignoreoverlap boolean: Ignore points with the overlap flag set (LAS V1.4+ format).
#' @param nocsv boolean: Do not create an output file for cell metrics.
#' @param class character: "#,#,#,...": LAS files only: Specifies that only points with classification
#'   values listed are to be included in the subsample.
#'   Classification values should be separated by a comma.
#'   e.g. (2,3,4,5) and can range from 0 to 31.
#'   If the first character in string is ~, the list is interpreted
#'   as the classes you DO NOT want included in the subsample.
#'   e.g. /class:~2,3 would include all class values EXCEPT 2 and 3.
#' @param first boolean: Use only first returns to compute all metrics (default is to
#'   use all returns for metrics).
#' @param slices character: "#,#,#,...": Use specific slice height breaks rather than evenly spaced
#'  breaks based on the range of heights in the data (max of 64 slice heights). The first slice
#'  always starts at 0.0.
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
# @param extent character: Force the origin and extent of the output grid to match the
#   lower left corner and extent of the specified PLANS format DTM
#   file but adjust the origin to be an even multiple of the cell
#   size and the width and height to be multiples of the cell size.
#' @param buffer numeric: Add a buffer to the data extent specified by /grid or /gridxy
#'   when computing metrics but only output data for the specified
#'   extent. The buffer width is first converted to a cellbuffer and
#'   then added all around the extent. The actual buffer width may be
#'   slightly larger than specified by width.
#' @param cellbuffer numeric: Add a buffer to the data extent specified by /grid or /gridxy
#'   when computing metrics but only output data for the specified
#'   extent. The buffer (number of cells) is added around the extent.
#' @template Use64bit
#' @template RunSaveOptions
#' @template Comment
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' DensityMetrics("ground.dtm", 30.0, 4.0, "density.csv", "*.las")
#' }
#' @family LTKFunctions
#' @export
DensityMetrics <- function(
    groundfile = NULL,
    cellsize = NULL,
    slicethickness = NULL,
    outputfile = NULL,
    datafile = NULL,
    quiet = FALSE,
    verbose = FALSE,
    version = FALSE,
    newlog = FALSE,
    log = NULL,
    locale = FALSE,
    nolaszipdll = FALSE,
    skipfilecheck = FALSE,
    outlier = NULL,
    maxsliceht = NULL,
    ignoreoverlap = FALSE,
    nocsv = FALSE,
    class = NULL,
    first = FALSE,
    slices = NULL,
    grid = NULL,
    gridxy = NULL,
#    extent = NULL,
    align = NULL,
    buffer = NULL,
    cellbuffer = NULL,
    use64bit = TRUE,
    runCmd = TRUE,
    saveCmd = TRUE,
    cmdFile = NULL,
    cmdClear = FALSE,
    echoCmd = FALSE,
    comment = NULL
) {
  # check for required options
  if (!isOpt(groundfile)
      || !isOpt(cellsize)
      || !isOpt(slicethickness)
      || !isOpt(outputfile)
      || !isOpt(datafile)
  ) {
    stop("Missing required parameters: groundfile, cellsize, slicethickness, outputfile, datafile")
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
  cmd <- programName("DensityMetrics", use64bit)

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
  options <- addSwitch(options, ignoreoverlap)
  options <- addSwitch(options, nocsv)
  options <- addSwitch(options, first)

  # deal with options...
  # program-specific options
  options <- addOption(options, outlier)
  options <- addOption(options, maxsliceht)
  options <- addOption(options, class)
  options <- addOption(options, slices)
  options <- addOption(options, grid)
  options <- addOption(options, gridxy)
  options <- addOption(options, align, TRUE)
#  options <- addOption(options, extent, TRUE)
  options <- addOption(options, buffer)
  options <- addOption(options, cellbuffer)

  # deal with required parameters...some may have defaults
  required <- addRequired(required, groundfile, TRUE)
  required <- addRequired(required, cellsize, TRUE)
  required <- addRequired(required, slicethickness, TRUE)
  required <- addRequired(required, outputfile, TRUE)
  required <- addRequired(required, datafile, TRUE)

  echoCommand(cmd, options, required, echoCmd)

  ret <- dispatchCommand(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile, comment)

  if (runCmd) {
    invisible(ret)
  } else {
    invisible(buildCommand(cmd, options, required))
  }
}

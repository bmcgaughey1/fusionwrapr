# TopoMetrics
# ---------- TopoMetrics
#
#' FUSION R command line interface -- Function to interpolate surface values for XY locations.
#'
#' \code{TopoMetrics} creates command lines for the FUSION TopoMetrics program and optionally executes them.
#'
#' @template MultipleCommands
#'
#' @param surfacefile character (\strong{required}): Name for the input surface file (PLANS DTM format)
#'  surfacefile may be wildcard or text list file (extension .txt).
#' @param cellsize numeric (\strong{required}): Size of the cell used to report topographic metrics.
#' @param topopointspacing numeric (\strong{required}): The spacing between points in the 3 by 3 array used
#'  to compute the basic topographic metrics.
#' @param latitude numeric (\strong{required}): Latitude for the center of the data area. North latitude is
#'  positive, South is negative. Latitude is used to compute the solar radiation index.
#' @param tpiwindowsize numeric (\strong{required}): The size of the window used to compute the topographic
#'  position index. When  \code{square=TRUE} is used, the TPI window will be \code{tpiwindowsize} by \code{tpiwindowsize}.
#'  For round windows, the diameter will be \code{tpiwindowsize}.
#' @param outputfile character (\strong{required}): Base name for the output metrics (CSV format). "_topo_metrics"
#'  will be appended to the name provided on the command line.
#' @template StandardOptionsNoPts
#' @param grid character: "X,Y,W,H": Force the origin of the output grid to be (X,Y) instead of computing
#'  an origin from the data extents and force the grid to be W units wide and H units high...W and H will
#'  be rounded up to a multiple of cellsize.
#' @param gridxy character: "X1,Y1,X2,Y2": Force the origin of the output grid to be (X1,Y1) instead
#'  of computing an origin from the data extents and force the grid to use (X2,Y2) as the upper right
#'  corner of the coverage area. The actual upper right corner will be adjusted to be a multiple of \code{cellsize}.
#' @param align character: Force the origin and extent of the output grid to match the lower left corner
#'   and extent of the specified PLANS format DTM file.
#' @param extent character: Force the origin and extent of the output grid to match the lower left corner
#'  and extent of the specified PLANS format DTM file but adjust the origin to be an even multiple of the
#'  cell size and the width and height to be multiples of the cell size.
#' @param square boolean: Use a square-shaped mask when computing the topographic position index.
#'  The default mask shape is a circle.
#' @param annulusinnerdia numeric: Use a doughnut-shaped mask when computing the topographic position index.
#'  The outer diameter is \code{topopointspacing} and the inner diameter is te value specified. When using
#'  this option to define a narrow ring, use the \code{verbose=TRUE} to display the computed mask to ensure
#'  it meets your expectations.
#' @param annuluswidth numeric: Use a donut-shaped mask when computing the topographic position index.
#'  The outer diameter is \code{tpiwindowsize} and the inner diameter is \code{TPIWindowSize-(annuluswidth*2)}. When using
#'  this option to define a narrow ring, use \code{verbose=TRUE} to display the computed mask to ensure
#'  it meets your expectations.
#' @param diskground boolean: Do not load ground surface models into memory or create a temporary surface
#'  in memory. When this option is specified, larger areas can be processed but processing will be very
#'  much slower.
#' @param nointernalground boolean: Do not create a temporary surface in memory. When this option
#'  is specified, larger areas can be processed but processing will be much slower. This option has no
#'  effect when used with \code{diskground=TRUE}.
#' @param lockdtmcellsize boolean: Force the cell size used when creating an internal ground model
#'  to be the same as the highest resolution input ground surface model. The default behavior will
#'  increase the cell size until the internal model will fit in the available memory.
#' @template Use64bit
#' @template RunSaveOptions
#' @template Comment
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' TopoMetrics("grnd.dtm", 30.0, 10.0, 43.5, 90.0, "topometrics.csv")
#' }
#' @family LTKFunctions
#' @export
TopoMetrics <- function(
    surfacefile = NULL,
    cellsize = NULL,
    topopointspacing = NULL,
    latitude = NULL,
    tpiwindowsize = NULL,
    outputfile = NULL,
    quiet = FALSE,
    verbose = FALSE,
    version = FALSE,
    newlog = FALSE,
    log = NULL,
    locale = FALSE,
    grid = NULL,
    gridxy = NULL,
    align = NULL,
    extent = NULL,
    square = FALSE,
    annulusinnerdia = NULL,
    annuluswidth = NULL,
    diskground = FALSE,
    nointernalground = FALSE,
    lockdtmcellsize = FALSE,
    use64bit = TRUE,
    runCmd = TRUE,
    saveCmd = TRUE,
    cmdFile = NULL,
    cmdClear = FALSE,
    echoCmd = FALSE,
    comment = NULL
) {
  # check for required options
  if (!isOpt(surfacefile)
      || !isOpt(cellsize)
      || !isOpt(topopointspacing)
      || !isOpt(latitude)
      || !isOpt(tpiwindowsize)
      || !isOpt(outputfile)
  ) {
    stop("Missing required parameters: surfacefile, cellsize, topopointspacing, latitude, tpiwindowsize, outputfile")
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
  cmd <- programName("TopoMetrics", FALSE)

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
  options <- addSwitch(options, square)
  options <- addSwitch(options, diskground)
  options <- addSwitch(options, nointernalground)
  options <- addSwitch(options, lockdtmcellsize)

  # deal with options...
  options <- addOption(options, gridxy)
  options <- addOption(options, gridxy)
  options <- addOption(options, align)
  options <- addOption(options, extent)
  options <- addOption(options, annulusinnerdia)
  options <- addOption(options, annuluswidth)

  # deal with required parameters...some may have defaults
  required <- addRequired(required, surfacefile, TRUE)
  required <- addRequired(required, cellsize)
  required <- addRequired(required, topopointspacing)
  required <- addRequired(required, latitude)
  required <- addRequired(required, tpiwindowsize)
  required <- addRequired(required, outputfile, TRUE)

  echoCommand(cmd, options, required, echoCmd)

  ret <- dispatchCommand(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile, comment)

  if (runCmd) {
    invisible(ret)
  } else {
    invisible(buildCommand(cmd, options, required))
  }
}

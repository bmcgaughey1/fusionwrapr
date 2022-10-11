# CanopyModel
# ---------- CanopyModel
#
#' FUSION R command line interface -- Function to create command lines for the CanopyModel program.
#'
#' \code{CanopyModel} creates command lines for the FUSION CanopyModel program and optionally executes them.
#'
#' @template MultipleCommands
#'
#' @param surfacefile character (\strong{required}): Name for output surface file (stored in PLANS DTM format with .dtm extension). If the
#'   folder for the output file does not exist, it will be created  when the function is called even when saving
#'   commands to a batch file.
#' @param cellsize numeric (\strong{required}): Desired grid cell size in the same units as LIDAR data.
#' @template CoordInfo
#' @param datafile character (\strong{required}): Name(s) of lidar data files.
#' @template StandardOptions
#' @template SkipFileCheck
#' @param median numeric: Apply median filter to model using # by # neighbor window.
#' @param smooth numeric: Apply mean filter to model using # by # neighbor window.
#' @param texture numeric: Calculate the surface texture metric using # by # neighbor window.
#' @param slope boolean: Calculate surface slope.
#' @param aspect boolean: Calculate surface aspect.
#' @param outlier character: "low,high": Omit points with elevations below low and above high.
#'   If used with a bare-earth surface this option will omit points
#'   with heights below low or above high.
#' @param multiplier numeric: Multiply the output values by the constant.
#' @param return character: "ccc...": Specifies the returns to be included in the sample (can
#'   include A,1,2,3,4,5,6,7,8,9,F,L,O) Options are specified without
#'   commas (e.g. /return:123) For LAS files only: F indicates first
#'   and only returns, L indicates last of many returns.
#' @param class character: "#,#,#,...": LAS files only: Specifies that only points with classification
#'   values listed are to be included in the subsample.
#'   Classification values should be separated by a comma.
#'   e.g. (2,3,4,5) and can range from 0 to 31.
#'   If the first character in string is ~, the list is interpreted
#'   as the classes you DO NOT want included in the subsample.
#'   e.g. /class:~2,3 would include all class values EXCEPT 2 and 3.
#' @param ignoreoverlap boolean: Ignore points with the overlap flag set (LAS V1.4+ format).
#' @param ground character: Use the specified bare-earth surface model to normalize the LIDAR data
#'   file may be wildcard or text list file (extension .txt only).
#' @param hole numeric: Defines the threshold for the hole filling logic. Any areas
#'   at or below the height will be filled unless the /nofill option is used.
#' @param ascii boolean: Write output surface in ASCII raster format as well as DTM
#'   format. Extension will be .asc.
#' @param surface boolean: Use the bare-earth surface model in conjunction with values
#'   specified in /outlier to omit points but create a surface
#'   that is not normalized relative to the bare-earth surface.
#' @param peaks boolean: Preserve localized peaks in the final surface.
#' @param pointcount boolean: Output the number of data points in each cell in addition to the
#'   canopy surface/height values. Counts are output in .DTM format.
#'   If there are no points for a cell, the elevation/height value
#'   for the cell is set to 999.0.
#' @param nofill boolean: Don't fill holes in the canopy surface model. The default
#'   logic fills holes prior to doing any smoothing.
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
#' @param rasterorigin boolean: Offsets the origin and adjusts the extent to correspond with
#'   raster data layers covering the same extent and using the same
#'   resolution. Only used with the /grid and /gridxy options.
#' @param smoothfirst boolean: indicating smoothing should occur before median
#'   filtering. The default is for median filtering to happen before smoothing.
#' @template Use64bit
#' @template RunSaveOptions
#' @template Comment
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' CanopyModel("test.dtm", 2.0, "M", "M", 1, 10, 2, 2, "Test/pts.las")
#' }
#' @family LTKFunctions
#' @export
CanopyModel <- function(
    surfacefile = NULL,
    cellsize = NULL,
    xyunits = NULL,
    zunits = NULL,
    coordsys = NULL,
    zone = NULL,
    horizdatum = NULL,
    vertdatum = NULL,
    datafile = NULL,
    quiet = FALSE,
    verbose = FALSE,
    version = FALSE,
    newlog = FALSE,
    log = NULL,
    locale = FALSE,
    nolaszipdll = FALSE,
    skipfilecheck = FALSE,
    median = NULL,
    smooth = NULL,
    texture = NULL,
    slope = FALSE,
    aspect = FALSE,
    outlier = NULL,
    multiplier = NULL,
    return = NULL,
    class = NULL,
    ignoreoverlap = FALSE,
    ground = NULL,
    hole = NULL,
    ascii = FALSE,
    surface = FALSE,
    peaks = FALSE,
    pointcount = FALSE,
    nofill = FALSE,
    grid = NULL,
    gridxy = NULL,
    align = NULL,
    extent = NULL,
    rasterorigin = FALSE,
    smoothfirst = FALSE,
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
      || !isOpt(xyunits)
      || !isOpt(zunits)
      || !isOpt(coordsys)
      || !isOpt(zone)
      || !isOpt(horizdatum)
      || !isOpt(vertdatum)
      || !isOpt(datafile)
  ) {
    stop("Missing required parameters: surfacefile, cellsize, xyunits, zunits, coordsys, zone, horizdatum, vertdatum, datafile")
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
  verifyFolder(dirname(surfacefile), runCmd, saveCmd, cmdFile, cmdClear)

  # if we are saving commands to a file, cmdClear will have done its job in the call to verifyFolder
  cmdClear <- FALSE

  # build command line
  cmd <- programName("CanopyModel", use64bit)

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
  options <- addSwitch(options, slope)
  options <- addSwitch(options, aspect)
  options <- addSwitch(options, ignoreoverlap)
  options <- addSwitch(options, ascii)
  options <- addSwitch(options, surface)
  options <- addSwitch(options, peaks)
  options <- addSwitch(options, pointcount)
  options <- addSwitch(options, nofill)
  options <- addSwitch(options, rasterorigin)

  # deal with options...
  # program-specific options
  if (smoothfirst)
    options <- addOption(options, smooth)

  options <- addOption(options, median)
  if (!smoothfirst)
    options <- addOption(options, smooth)
  options <- addOption(options, texture)
  options <- addOption(options, outlier)
  options <- addOption(options, multiplier)
  options <- addOption(options, return)
  options <- addOption(options, class)
  options <- addOption(options, ground, TRUE)
  options <- addOption(options, hole)
  options <- addOption(options, grid)
  options <- addOption(options, gridxy)
  options <- addOption(options, align, TRUE)
  options <- addOption(options, extent)

  # deal with required parameters...some may have defaults
  required <- addRequired(required, surfacefile, TRUE)
  required <- addRequired(required, cellsize)
  required <- addRequired(required, xyunits)
  required <- addRequired(required, zunits)
  required <- addRequired(required, coordsys)
  required <- addRequired(required, zone)
  required <- addRequired(required, horizdatum)
  required <- addRequired(required, vertdatum)
  required <- addRequired(required, datafile, TRUE)

  echoCommand(cmd, options, required, echoCmd)

  ret <- dispatchCommand(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile, comment)

  if (runCmd) {
    invisible(ret)
  } else {
    invisible(buildCommand(cmd, options, required))
  }
}

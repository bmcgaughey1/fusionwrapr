# TINSurfaceCreate
# ---------- TINSurfaceCreate
#
#' FUSION R command line interface -- Creates a DTM surface model using points in LIDAR data files (uses TIN then
#'  grids to final cell size).
#'
#' \code{TINSurfaceCreate} creates command lines for the FUSION TINSurfaceCreate program and optionally executes them.
#'
#' @template MultipleCommands
#'
#' @param surfacefile character (\strong{required}): Name for output surface file (stored in PLANS DTM format with .dtm extension).
#'   If the folder for the output file does not exist, it will be created
#'   when the function is called even when saving commands to a batch file.
#' @param cellsize numeric (\strong{required}): Desired grid cell size in the same units as LIDAR data.
#' @template CoordInfo
#' @param datafile character (\strong{required}): Name(s) of lidar data files.
#' @template StandardOptions
#' @template SkipFileCheck
#' @param return character: "ccc...": Specifies the returns to be included in the sample (can
#'  include A,1,2,3,4,5,6,7,8,9,F,L,O). Options are specified without commas (e.g. \code{return="123"}).
#'  For LAS files only: F indicates first and only returns, L indicates last of many returns.
#' @param class character: "#,#,#,...": LAS files only: Specifies that only points with classification
#'   values listed are to be included in the subsample.
#'   Classification values should be separated by a comma.
#'   e.g. (2,3,4,5) and can range from 0 to 31.
#'   If the first character in string is ~, the list is interpreted
#'   as the classes you DO NOT want included in the subsample.
#'   e.g. /class:~2,3 would include all class values EXCEPT 2 and 3.
#' @param ignoreoverlap boolean: Ignore points with the overlap flag set (LAS V1.4+ format).
#' @template RunSaveOptions
#' @template Comment
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' TINSurfaceCreate("test.dtm", 2.0, "M", "M", 1, 10, 2, 2, "Test/pts.las", class = "2")
#' }
#' @family LTKFunctions
#' @export
TINSurfaceCreate <- function(
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
    return = NULL,
    class = NULL,
    ignoreoverlap = FALSE,
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
  cmd <- programName("TINSurfaceCreate", FALSE)

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

  # deal with options...
  # program-specific options
  options <- addOption(options, return)
  options <- addOption(options, class)

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

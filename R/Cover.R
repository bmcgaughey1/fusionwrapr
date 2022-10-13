# Cover
# ---------- Cover
#
#' FUSION R command line interface -- Computes cover estimates using a bare-earth surface model and point cloud.
#'
#' \code{Cover} creates command lines for the FUSION Cover program and optionally executes them.
#'
#' @template MultipleCommands
#'
#' @param groundfile character (\strong{required}): Name of the bare-earth surface model used to normalize
#'  LIDAR data groundfile may be wildcard or text list file (extension .txt).
#' @param coverfile character (\strong{required}): Name for output canopy cover file (stored in PLANS DTM format with .dtm extension).
#'  If the folder for the output file does not exist, it will be created when the function is called even
#'  when saving commands to a batch file.
#' @param heightbreak numeric (\strong{required}): Height break for cover layer...base of desired layer.
#' @param cellsize numeric (\strong{required}): Desired grid cell size in the same units as LIDAR data.
#' @template CoordInfo
#' @param datafile character (\strong{required}): Name(s) of lidar data files.
#' @template StandardOptions
#' @template SkipFileCheck
#' @param all boolean: Use all returns when computing percent cover (default is first returns only).
#' @param class character: "#,#,#,...": LAS files only: Specifies that only points with classification
#'   values listed are to be included in the subsample.
#'   Classification values should be separated by a comma.
#'   e.g. (2,3,4,5) and can range from 0 to 31.
#'   If the first character in string is ~, the list is interpreted
#'   as the classes you DO NOT want included in the subsample.
#'   e.g. /class:~2,3 would include all class values EXCEPT 2 and 3.
#' @param ignoreoverlap boolean: Ignore points with the overlap flag set (LAS V1.4+ format).
#' @param penetration boolean: Compute proportion of returns close to ground surface (returns
#'  within \code{+-heightbreak} units).
#' @param upper numeric: Use an upper limit when computing the cover value. This allows for
#'  calculation of the proportion of returns between a lower and upper height/elevation range.
#' @template RunSaveOptions
#' @template Comment
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' Cover("ground.dtm", "cancover.dtm", 6.0, 2.0, "M", "M", 1, 10, 2, 2, "pts.las", class = "~7")
#' }
#' @family LTKFunctions
#' @export
Cover <- function(
    groundfile = NULL,
    coverfile = NULL,
    heightbreak = NULL,
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
    all = FALSE,
    class = NULL,
    ignoreoverlap = FALSE,
    penetration = FALSE,
    upper = NULL,
    runCmd = TRUE,
    saveCmd = TRUE,
    cmdFile = NULL,
    cmdClear = FALSE,
    echoCmd = FALSE,
    comment = NULL
) {
  # check for required options
  if (!isOpt(groundfile)
      || !isOpt(coverfile)
      || !isOpt(heightbreak)
      || !isOpt(cellsize)
      || !isOpt(xyunits)
      || !isOpt(zunits)
      || !isOpt(coordsys)
      || !isOpt(zone)
      || !isOpt(horizdatum)
      || !isOpt(vertdatum)
      || !isOpt(datafile)
  ) {
    stop("Missing required parameters: groundfile, coverfile, heightbreak, cellsize, xyunits, zunits, coordsys, zone, horizdatum, vertdatum, datafile")
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
  verifyFolder(dirname(coverfile), runCmd, saveCmd, cmdFile, cmdClear)

  # if we are saving commands to a file, cmdClear will have done its job in the call to verifyFolder
  cmdClear <- FALSE

  # build command line
  cmd <- programName("Cover", FALSE)

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
  options <- addSwitch(options, all)
  options <- addSwitch(options, ignoreoverlap)
  options <- addSwitch(options, penetration)

  # deal with options...
  # program-specific options
  options <- addOption(options, class)
  options <- addOption(options, upper)

  # deal with required parameters...some may have defaults
  required <- addRequired(required, groundfile, TRUE)
  required <- addRequired(required, coverfile, TRUE)
  required <- addRequired(required, heightbreak, TRUE)
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

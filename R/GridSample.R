# GridSample
# ---------- GridSample
#
#' FUSION R command line interface -- Function to interpolate surface values for XY locations.
#'
#' \code{GridSample} creates command lines for the FUSION GridSample program and optionally executes them.
#'
#' @template MultipleCommands
#'
#' @param gridfile character (\strong{required}): Name for input surface file (PLANS DTM with .dtm extension).
#' @param inputfile character (\strong{required}):  Name of file containing XY locations (space or comma delimited).
#' @param outputfile character (\strong{required}): Name for the output ASCII CSV file.
#' @param windowsize numeric: Size of the sample window in grid cells (must be an odd number).
#' @template StandardOptionsNoPts
#' @param center boolean: Include the location of the center of the cell that contains the sample point in the output CSV file.
#' @template Use64bit
#' @template RunSaveOptions
#' @template Comment
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' GridSample("ground.dtm", "in.csv", "out.csv")
#' }
#' @family LTKFunctions
#' @export
GridSample <- function(
    gridfile = NULL,
    inputfile = NULL,
    outputfile = NULL,
    windowsize = NULL,
    quiet = FALSE,
    verbose = FALSE,
    version = FALSE,
    newlog = FALSE,
    log = NULL,
    locale = FALSE,
    center = FALSE,
    use64bit = TRUE,
    runCmd = TRUE,
    saveCmd = TRUE,
    cmdFile = NULL,
    cmdClear = FALSE,
    echoCmd = FALSE,
    comment = NULL
) {
  # check for required options
  if (!isOpt(gridfile)
      || !isOpt(inputfile)
      || !isOpt(outputfile)
      || !isOpt(windowsize)
  ) {
    stop("Missing required parameters: gridfile, inputfile, outputfile, windowsize")
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
  cmd <- programName("GridSample", use64bit)

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
  options <- addSwitch(options, center)

  # deal with options...

  # deal with required parameters...some may have defaults
  required <- addRequired(required, gridfile, TRUE)
  required <- addRequired(required, inputfile, TRUE)
  required <- addRequired(required, outputfile, TRUE)
  required <- addRequired(required, windowsize)

  echoCommand(cmd, options, required, echoCmd)

  ret <- dispatchCommand(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile, comment)

  if (runCmd) {
    invisible(ret)
  } else {
    invisible(buildCommand(cmd, options, required))
  }
}

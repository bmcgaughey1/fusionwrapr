# ClipDTM
# ---------- ClipDTM
#
#' FUSION R command line interface -- Clips a portion of a DTM using user-specified extents.
#'
#' \code{ClipDTM} creates command lines for the FUSION ClipDTM program and optionally executes them.
#'
#' @template MultipleCommands
#'
#' @param inputdtm character (\strong{required}): Name of the existing PLANS format DTM file to be subdivided.
#' @param outputdtm character (\strong{required}):  Base name for the new PLANS format DTM tiles. The actual tile
#'  name will have the column and row numbers appended to it.
#' @param minx numeric (\strong{required}): Lower left corner X value.
#' @param miny numeric (\strong{required}): Lower left corner Y value.
#' @param maxx numeric (\strong{required}): Upper right corner X value.
#' @param maxy numeric (\strong{required}): Upper right corner Y value.
#' @template StandardOptionsNoPts
#' @param shrink boolean: Shrink the extent of the input model by the amounts specified. \code{minx} is
#'  removed from left side, \code{miny} is removed from bottom, \code{maxx} is removed from right side,
#'  \code{maxy} is removed from top.
#' @param multiplier numeric: Multiply the output values by the constant (#).
#' @template Use64bit
#' @template RunSaveOptions
#' @template Comment
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' ClipDTM("grnd.dtm", "clipped_grnd.dtm", 45700, 5100400, 49700, 5105400)
#' }
#' @family LTKFunctions
#' @export
ClipDTM <- function(
    inputdtm = NULL,
    outputdtm = NULL,
    minx = NULL,
    miny = NULL,
    maxx = NULL,
    maxy = NULL,
    quiet = FALSE,
    verbose = FALSE,
    version = FALSE,
    newlog = FALSE,
    log = NULL,
    locale = FALSE,
    shrink = FALSE,
    multiplier = NULL,
    use64bit = TRUE,
    runCmd = TRUE,
    saveCmd = TRUE,
    cmdFile = NULL,
    cmdClear = FALSE,
    echoCmd = FALSE,
    comment = NULL
) {
  # check for required options
  if (!isOpt(inputdtm)
      || !isOpt(outputdtm)
      || !isOpt(minx)
      || !isOpt(miny)
      || !isOpt(maxx)
      || !isOpt(maxy)
  ) {
    stop("Missing required parameters: inputdtm, outputdtm, minx, miny, maxx, maxy")
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
  verifyFolder(dirname(outputdtm), runCmd, saveCmd, cmdFile, cmdClear)

  # if we are saving commands to a file, cmdClear will have done its job in the call to verifyFolder
  cmdClear <- FALSE

  # build command line
  cmd <- programName("ClipDTM", use64bit)

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
  options <- addSwitch(options, shrink)

  # deal with options...
  options <- addOption(options, multiplier)

  # deal with required parameters...some may have defaults
  required <- addRequired(required, inputdtm, TRUE)
  required <- addRequired(required, outputdtm, TRUE)
  required <- addRequired(required, minx)
  required <- addRequired(required, miny)
  required <- addRequired(required, maxx)
  required <- addRequired(required, maxy)

  echoCommand(cmd, options, required, echoCmd)

  ret <- dispatchCommand(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile, comment)

  if (runCmd) {
    invisible(ret)
  } else {
    invisible(buildCommand(cmd, options, required))
  }
}

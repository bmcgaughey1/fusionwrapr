# RepairGridDTM
# ---------- RepairGridDTM
#
#' FUSION R command line interface -- Expands DTM tiles created from ArcInfo GRID files.
#'
#' \code{RepairGridDTM} creates command lines for the FUSION RepairGridDTM program and optionally executes them.
#'
#' @template MultipleCommands
#'
#' @param groundfiles character (\strong{required}): File specifier for ground surface models in PLANS DTM format.
#'  May be wildcard or text list file (extension .txt only). Normally this will specify all DTM files for a project area.
#' @param extraspace numericr (\strong{required}): Distance added around all sides of a DTM tile.
#' @template StandardOptionsNoPts
#' @template RunSaveOptions
#' @template Comment
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' RepairGridDTM("*.dtm", 5.0)
#' }
#' @family LTKFunctions
#' @export
RepairGridDTM <- function(
    groundfiles = NULL,
    extraspace = NULL,
    quiet = FALSE,
    verbose = FALSE,
    version = FALSE,
    newlog = FALSE,
    log = NULL,
    locale = FALSE,
    runCmd = TRUE,
    saveCmd = TRUE,
    cmdFile = NULL,
    cmdClear = FALSE,
    echoCmd = FALSE,
    comment = NULL
) {
  # check for required options
  if (!isOpt(groundfiles)
      || !isOpt(extraspace)
  ) {
    stop("Missing required parameters: groundfiles, extraspace")
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

  # if we are saving commands to a file, cmdClear will have done its job in the call to verifyFolder
  cmdClear <- FALSE

  # build command line
  cmd <- programName("RepairGridDTM", FALSE)

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

  # deal with options...

  # deal with required parameters...some may have defaults
  required <- addRequired(required, groundfiles, TRUE)
  required <- addRequired(required, extraspace)

  echoCommand(cmd, options, required, echoCmd)

  ret <- dispatchCommand(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile, comment)

  if (runCmd) {
    invisible(ret)
  } else {
    invisible(buildCommand(cmd, options, required))
  }
}

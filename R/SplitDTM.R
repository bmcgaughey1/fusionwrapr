# SplitDTM
# ---------- SplitDTM
#
#' FUSION R command line interface -- Splits a DTM into separate tiles.
#'
#' \code{SplitDTM} creates command lines for the FUSION SplitDTM program and optionally executes them.
#'
#' @template MultipleCommands
#'
#' @param inputdtm character (\strong{required}): Name of the existing PLANS format DTM file to be subdivided.
#' @param outputdtm character (\strong{required}):  Base name for the new PLANS format DTM tiles. The actual tile
#'  name will have the column and row numbers appended to it.
#' @param columns numeric (\strong{required}): Number of columns of tiles.
#' @param rows numeric (\strong{required}): Number of rows of tiles.
#' @template StandardOptionsNoPts
#' @param maxcells numeric: Maximum number of cells in the output tiles. Columns and Rows are
#'  ignored (but values are needed on the command line). New values for Columns and Rows will
#'  be calculated. The default maximum number of cells is 25,000,000.
#' @template Use64bit
#' @template RunSaveOptions
#' @template Comment
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' SplitDTM("merged_grnd.dtm", "*.dtm")
#' }
#' @family LTKFunctions
#' @export
SplitDTM <- function(
    inputdtm = NULL,
    outputdtm = NULL,
    columns = NULL,
    rows = NULL,
    quiet = FALSE,
    verbose = FALSE,
    version = FALSE,
    newlog = FALSE,
    log = NULL,
    locale = FALSE,
    maxcells = NULL,
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
      || !isOpt(columns)
      || !isOpt(rows)
  ) {
    stop("Missing required parameters: inputdtm, outputdtm, columns, rows")
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
  cmd <- programName("SplitDTM", use64bit)

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
  options <- addOption(options, maxcells)

  # deal with required parameters...some may have defaults
  required <- addRequired(required, inputdtm, TRUE)
  required <- addRequired(required, outputdtm, TRUE)
  required <- addRequired(required, columns)
  required <- addRequired(required, rows)

  echoCommand(cmd, options, required, echoCmd)

  ret <- dispatchCommand(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile, comment)

  if (runCmd) {
    invisible(ret)
  } else {
    invisible(buildCommand(cmd, options, required))
  }
}

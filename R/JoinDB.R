# JoinDB
# ---------- JoinDB
#
#' FUSION R command line interface -- Combines columns from two data files into a single data file.
#'
#' \code{JoinDB} creates command lines for the FUSION JoinDB program and optionally executes them.
#'
#' @template MultipleCommands
#'
#' @param basefile character (\strong{required}): Primary data file in CSV format.
#' @param basefield numeric: Field in \code{basefile} that will be matched to records in \code{addfile}.
#' @param addfile character: Secondary data file in CSV format.
#' @param addfield numeric: Field in \code{addfile} that will be matched to records in \code{basefile}.
#' @param startfield numeric: Starting field in \code{addfile}. All fields (columns) starting with
#'  the \code{startfield} will be added to records in the \code{outputfile}.
#' @param outputfile character: Name for the new data file. The extension specifies the desired format (.csv).
#'  \code{outputfile} can be the same as \code{basefile}. \code{outputfile} cannot be the same as \code{addfile}.
#' @template StandardOptionsNoPts
#' @param noheader boolean: Treat the first line of the \code{basefile} and \code{addfile} as data. Default behavior assumes
#'  the first line of each file contains column names. Valid for CSV format files only.
#' @template RunSaveOptions
#' @template Comment
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' JoinDB("attribs.csv", 4, "base.csv", 1, 2, "new.csv")
#' }
#' @family LTKFunctions
#' @export
JoinDB <- function(
    basefile = NULL,
    basefield = NULL,
    addfile = NULL,
    addfield = NULL,
    startfield = NULL,
    outputfile = NULL,
    quiet = FALSE,
    verbose = FALSE,
    version = FALSE,
    newlog = FALSE,
    log = NULL,
    locale = FALSE,
    noheader = FALSE,
    runCmd = TRUE,
    saveCmd = TRUE,
    cmdFile = NULL,
    cmdClear = FALSE,
    echoCmd = FALSE,
    comment = NULL
) {
  # check for required options
  if (!isOpt(basefile)
      || !isOpt(basefield)
      || !isOpt(addfile)
      || !isOpt(addfield)
      || !isOpt(startfield)
      || !isOpt(outputfile)
  ) {
    stop("Missing required parameters: basefile, basefield, addfile, addfield, startfield, outputfile")
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
  cmd <- programName("JoinDB", FALSE)

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
  options <- addSwitch(options, noheader)

  # deal with options...

  # deal with required parameters...some may have defaults
  required <- addRequired(required, basefile, TRUE)
  required <- addRequired(required, basefield)
  required <- addRequired(required, addfile, TRUE)
  required <- addRequired(required, addfield)
  required <- addRequired(required, startfield)
  required <- addRequired(required, outputfile, TRUE)

  echoCommand(cmd, options, required, echoCmd)

  ret <- dispatchCommand(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile, comment)

  if (runCmd) {
    invisible(ret)
  } else {
    invisible(buildCommand(cmd, options, required))
  }
}

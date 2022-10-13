# DTMDescribe
# ---------- DTMDescribe
#
#' FUSION R command line interface -- Outputs information from PLANS dtm file headers to CSV file.
#'
#' \code{DTMDescribe} creates command lines for the FUSION DTMDescribe program and optionally executes them.
#'
#' @template MultipleCommands
#'
#' @param inputfile character (\strong{required}): DTM file name, DTM file template, or name of a text file
#'  containing a list of file names (must have .txt extension).
#' @param outputfile character (\strong{required}):  Name for the output ASCII CSV file. If no extension is
#'  provided, an extension (.csv) will be added.
#' @template StandardOptionsNoPts
#' @param stats boolean: Compute descriptive statistics for the data values in the DTM.
#' @template RunSaveOptions
#' @template Comment
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' DTMDescribe("grnd.dtm", "DTMinfo.csv")
#' }
#' @family LTKFunctions
#' @export
DTMDescribe <- function(
    inputfile = NULL,
    outputfile = NULL,
    quiet = FALSE,
    verbose = FALSE,
    version = FALSE,
    newlog = FALSE,
    log = NULL,
    locale = FALSE,
    stats = FALSE,
    runCmd = TRUE,
    saveCmd = TRUE,
    cmdFile = NULL,
    cmdClear = FALSE,
    echoCmd = FALSE,
    comment = NULL
) {
  # check for required options
  if (!isOpt(inputfile)
      || !isOpt(outputfile)
  ) {
    stop("Missing required parameters: inputfile, outputfile")
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
  cmd <- programName("DTMDescribe", FALSE)

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
  options <- addSwitch(options, stats)

  # deal with options...

  # deal with required parameters...some may have defaults
  required <- addRequired(required, inputfile, TRUE)
  required <- addRequired(required, outputfile, TRUE)

  echoCommand(cmd, options, required, echoCmd)

  ret <- dispatchCommand(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile, comment)

  if (runCmd) {
    invisible(ret)
  } else {
    invisible(buildCommand(cmd, options, required))
  }
}

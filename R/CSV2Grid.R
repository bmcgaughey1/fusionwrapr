# CSV2Grid
# ---------- CSV2Grid
#
#' FUSION R command line interface -- Converts data stored in comma separated value (CSV) format into ASCII raster format.
#'
#' \code{CSV2Grid} creates command lines for the FUSION CSV2Grid program and optionally executes them.
#'
#' CSV2Grid looks for a header file with the name formed by appending "_ascii_header" to \code{inputfile}
#'  and adding an extension of "txt".
#'
#' @template MultipleCommands
#'
#' @param inputfile character (\strong{required}): Name for the input CSV file (normally output by GridMetrics).
#' @param column numeric: Column number for values to populate grid (1 is first column).
#' @param outputfile character: Name for the output ASCII grid file.
#' @template StandardOptionsNoPts
#' @param multiplier numeric: Multiply all data values by the constant value.
#' @param ndzero numeric: If the value in the target column is NODATA, look at the value in \code{column}
#'  and if it is a valid value (not NODATA), change the value for the target column to 0 for output to the ASCII grid
#'  file. This is useful when the ASCII grid files are being used for further analysis in GIS or statistical packages.
#' @template RunSaveOptions
#' @template Comment
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' CSV2Grid("metrics.csv", 4, "ave_elev.asc")
#' }
#' @family LTKFunctions
#' @export
CSV2Grid <- function(
    inputfile = NULL,
    column = NULL,
    outputfile = NULL,
    quiet = FALSE,
    verbose = FALSE,
    version = FALSE,
    newlog = FALSE,
    log = NULL,
    locale = FALSE,
    multiplier = NULL,
    ndzero = NULL,
    runCmd = TRUE,
    saveCmd = TRUE,
    cmdFile = NULL,
    cmdClear = FALSE,
    echoCmd = FALSE,
    comment = NULL
) {
  # check for required options
  if (!isOpt(inputfile)
      || !isOpt(column)
      || !isOpt(outputfile)
  ) {
    stop("Missing required parameters: inputfile, column, outputfile")
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
  cmd <- programName("CSV2Grid", FALSE)

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
  options <- addOption(options, multiplier)
  options <- addOption(options, ndzero)

  # deal with required parameters...some may have defaults
  required <- addRequired(required, inputfile, TRUE)
  required <- addRequired(required, column)
  required <- addRequired(required, outputfile, TRUE)

  echoCommand(cmd, options, required, echoCmd)

  ret <- dispatchCommand(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile, comment)

  if (runCmd) {
    invisible(ret)
  } else {
    invisible(buildCommand(cmd, options, required))
  }
}

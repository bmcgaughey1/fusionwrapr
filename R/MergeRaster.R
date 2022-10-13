# MergeRaster
# ---------- MergeRaster
#
#' FUSION R command line interface -- Merge ASCII raster files into a single ASCII raster file.
#'
#' \code{MergeRaster} creates command lines for the FUSION MergeRaster program and optionally executes them.
#'
#' @template MultipleCommands
#'
#' @param outputfile character (\strong{required}): Name for the output ASCII raster file containing the merged data.
#' @param inputfile character (\strong{required}):  File name template for ASCII raster files to be merged, a list of
#'  ASCII raster file names, or text file containing a list of ASCII raster file names with the
#'  ".txt" extension. If you are specifying a single input file on the command line, the file
#'  extension cannot be ".txt".
#' @template StandardOptionsNoPts
#' @param overlap character: Specify how overlap areas should be treated. Operators are: "average", "min",
#'  "max", "add", "new." The \code{new} operator populates a pixel using the value from the last .DTM file containing
#'  ASCII raster file containing valid data for the pixel.
#' @param compare boolean: Reports if values in cells common to two or more input files are different
#'  by more than 0.001.
#' @param precision numeric: Output value with # digits to the right of the decimal point.
#'  Default precision is 4 digits.
#' @param nodata numeric: Use value (#) to indicate no data in the output file. Default NODATA value is -9999.
#' @template Use64bit
#' @template RunSaveOptions
#' @template Comment
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' MergeRaster("merged_grnd.asc", "*.asc")
#' }
#' @family LTKFunctions
#' @export
MergeRaster <- function(
    outputfile = NULL,
    inputfile = NULL,
    quiet = FALSE,
    verbose = FALSE,
    version = FALSE,
    newlog = FALSE,
    log = NULL,
    locale = FALSE,
    overlap = NULL,
    compare = FALSE,
    precision = NULL,
    nodata = NULL,
    use64bit = TRUE,
    runCmd = TRUE,
    saveCmd = TRUE,
    cmdFile = NULL,
    cmdClear = FALSE,
    echoCmd = FALSE,
    comment = NULL
) {
  # check for required options
  if (!isOpt(outputfile)
      || !isOpt(inputfile)
  ) {
    stop("Missing required parameters: outputfile, inputfile")
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
  cmd <- programName("MergeRaster", use64bit)

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
  options <- addSwitch(options, compare)

  # deal with options...
  options <- addOption(options, overlap)
  options <- addOption(options, precision)
  options <- addOption(options, nodata)

  # deal with required parameters...some may have defaults
  required <- addRequired(required, outputfile, TRUE)
  required <- addRequired(required, inputfile, TRUE)

  echoCommand(cmd, options, required, echoCmd)

  ret <- dispatchCommand(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile, comment)

  if (runCmd) {
    invisible(ret)
  } else {
    invisible(buildCommand(cmd, options, required))
  }
}

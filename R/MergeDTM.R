# MergeDTM
# ---------- MergeDTM
#
#' FUSION R command line interface -- Function to interpolate surface values for XY locations.
#'
#' \code{MergeDTM} creates command lines for the FUSION MergeDTM program and optionally executes them.
#'
#' @template MultipleCommands
#'
#' @param outputfile character (\strong{required}): Name for the output .DTM file containing the merged data.
#' @param inputfile character (\strong{required}):  File name template for .DTM files to be merged or the
#'  name of a text file containing a list of .DTM files.
#' @template StandardOptionsNoPts
#' @param cellsize numeric: Resample the data using a # by # pixel.
#' @param overlap character: Specify how overlap areas should be treated. Operators are: "average", "min",
#'  "max", "add", "new." The \code{new} operator populates a pixel using the value from the last .DTM file containing
#'  valid data for the pixel.
#' @param disk boolean: Merge the .DTM files to a disk file. The default behavior is to try to hold the
#'  merged model into memory but there can be problems when there is not quite enough memory for the model.
#' @param precision numeric: Override the default precision for the merged output file. The default behavior
#'  is to use the highest precision of the input models to establish the precision of the output model.
#'  Valid values for precision are:
#'  0     2-byte integer
#'  1     4-byte integer
#'  2     4-byte floating point (C type: float)
#'  3     8-byte floating point (C type: double)
#' @param exactextent boolean: Preserve the exact extent of the input models in the output model. The
#'  default behavior is to expand the extent to the nearest multiple of the output cell size.
#' @param halfcell boolean: Offset the origin and expand the width and height by 1/2 of the output cell size.
#' @param nofill boolean: Do not fill holes in the merged DTM.
#' @template Use64bit
#' @template RunSaveOptions
#' @template Comment
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' MergeDTM("merged_grnd.dtm", "*.dtm")
#' }
#' @family LTKFunctions
#' @export
MergeDTM <- function(
    outputfile = NULL,
    inputfile = NULL,
    quiet = FALSE,
    verbose = FALSE,
    version = FALSE,
    newlog = FALSE,
    log = NULL,
    locale = FALSE,
    cellsize = NULL,
    overlap = NULL,
    disk = FALSE,
    precision = NULL,
    exactextent = FALSE,
    halfcell = FALSE,
    nofill = FALSE,
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
  cmd <- programName("MergeDTM", FALSE)

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
  options <- addSwitch(options, disk)
  options <- addSwitch(options, exactextent)
  options <- addSwitch(options, halfcell)
  options <- addSwitch(options, nofill)

  # deal with options...
  options <- addOption(options, cellsize)
  options <- addOption(options, overlap)
  options <- addOption(options, precision)

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

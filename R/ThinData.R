# ---------- ThinData
#
#' FUSION R command line interface -- Thins point data to specific pulse densities.
#'
#' \code{ThinData} creates command lines for the FUSION ThinData program and optionally executes them.
#'
#' @template MultipleCommands
#'
#' @param outputfile character (\strong{required}): Name for output data file...will contain new dataset
#'  thinned to the desired density.
#' @param density numeric (\strong{required}): Desired data density in pulses per unit area.
#' @param cellsize numeric (\strong{required}): Area of the cell used to compute data density specified in SQUARE UNITS
#' @param datafile character (\strong{required}): LIDAR data file template or the  name of a text file containing
#'   a list of file names (must have .txt extension),
#' @template StandardOptions
#' @template SkipFileCheck
#' @param rseed numeric: Random number stream to use for stochastic processes (100 streams are available 0-99).
#' @param index boolean: Create FUSION index files for the \code{outputfile}.
#' @param class character: "#,#,#,...": LAS files only: Specifies that only points with classification
#'   values listed are to be included in the subsample to be considered as potential ground points.
#'   Classification values should be separated by a comma. For example, class = "2,3,4,5" and can range from 0 to 31.
#'   If the first character in string is ~, the list is interpreted as the classes you DO NOT want included
#'   in the subsample. For example class = "~2,3" would include all class values EXCEPT 2 and 3.
#' @param ignoreoverlap boolean: Ignore points with the overlap flag set (LAS V1.4+ format).
#' @param lda boolean: Write sample files using FUSION's LDA format when using LAS input
#'   files. The default behavior of ClipData (after version 2.35) is
#'   to write data in LAS format when the input data are in LAS
#'   format. When using input data in a format other than LAS, sample
#'   files are written in LDA format.
#' @param precision character: "#,#,#": Control the scale factor used for X, Y, and Z
#'   values in output LAS files. These values will override the values
#'   in the source LAS files. There is rarely any need for the scale
#'   parameters to be smaller than 0.001.
#' @template Use64bit
#' @template RunSaveOptions
#' @template Comment
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' ThinData("thinned_pts.las", 10, 100.0, "*.las")
#' }
#' @family LTKFunctions
#' @export
ThinData <- function(
    outputfile = NULL,
    density = NULL,
    cellsize = NULL,
    datafile = NULL,
    quiet = FALSE,
    verbose = FALSE,
    version = FALSE,
    newlog = FALSE,
    log = NULL,
    locale = FALSE,
    nolaszipdll = FALSE,
    skipfilecheck = FALSE,
    rseed = NULL,
    index = FALSE,
    class = NULL,
    ignoreoverlap = FALSE,
    lda = FALSE,
    precision = NULL,
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
      || !isOpt(density)
      || !isOpt(cellsize)
      || !isOpt(datafile)
  ) {
    stop("Missing required parameters: outputfile, density, cellsize, datafile")
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
  verifyFolder(dirname(outputfile), runCmd, saveCmd, cmdFile, cmdClear)

  # if we are saving commands to a file, cmdClear will have done its job in the call to verifyFolder
  cmdClear <- FALSE

  # build command line
  cmd <- programName("ThinData", use64bit)

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
  options <- addSwitch(options, index)
  options <- addSwitch(options, ignoreoverlap)
  options <- addSwitch(options, lda)

  # deal with options...
  # program-specific options
  options <- addOption(options, rseed)
  options <- addOption(options, class)
  options <- addOption(options, precision)

  # deal with required parameters...some may have defaults
  required <- addRequired(required, outputfile, TRUE)
  required <- addRequired(required, density)
  required <- addRequired(required, cellsize)
  required <- addRequired(required, datafile)

  echoCommand(cmd, options, required, echoCmd)

  ret <- dispatchCommand(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile, comment)

  if (runCmd) {
    invisible(ret)
  } else {
    invisible(buildCommand(cmd, options, required))
  }
}

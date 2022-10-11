# ---------- MergeData
#
#' FUSION R command line interface -- Merges several point cloud files into a single file.
#'
#' \code{MergeData} creates command lines for the FUSION MergeData program and optionally executes them.
#'
#' @template MultipleCommands
#'
#' @param datafile character (\strong{required}): LIDAR data file template or name of a text file containing
#'   a list of file names (must have .txt extension).
#' @param outputfile character (\strong{required}): Name for the output data file with extension.
#' @template StandardOptions
#' @template SkipFileCheck
#' @param lda boolean: Write sample files using FUSION's LDA format when using LAS input
#'   files. The default behavior of ClipData (after version 2.35) is
#'   to write data in LAS format when the input data are in LAS
#'   format. When using input data in a format other than LAS, sample
#'   files are written in LDA format.
#' @param class character: "c,c,c,...": LAS files only: Specifies that only points with classification
#'   values listed are to be included in the merged points. Classification values should be separated
#'   by a comma. For example, \code{"2,3,4,5"}. Class values can range from 0 to 31. If the first
#'   character in string is ~, the list is interpreted as the classes you DO NOT want included in the subsample.
#'   For example, \code{"~2,3"} would include all class values EXCEPT 2 and 3.
#' @param ignoreoverlap boolean: Ignore points with the overlap flag set (LAS V1.4+ format).
#' @param index boolean: Create FUSION index files for the \code{outputfile}.
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
#' MergeData("*.las", "merged.las", class = "~7")
#' }
#' @family LTKFunctions
#' @export
MergeData <- function(
    datafile = NULL,
    outputfile = NULL,
    quiet = FALSE,
    verbose = FALSE,
    version = FALSE,
    newlog = FALSE,
    log = NULL,
    locale = FALSE,
    nolaszipdll = FALSE,
    skipfilecheck = FALSE,
    lda = FALSE,
    class = NULL,
    ignoreoverlap = FALSE,
    index = FALSE,
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
  if (!isOpt(datafile)
      || !isOpt(outputfile)
  ) {
    stop("Missing required parameters: datafile, outputfile")
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
  verifyFolder(dirname(samplefile), runCmd, saveCmd, cmdFile, cmdClear)

  # if we are saving commands to a file, cmdClear will have done its job in the call to verifyFolder
  cmdClear <- FALSE

  # build command line
  cmd <- programName("MergeData", use64bit)

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
  options <- addSwitch(options, lda)
  options <- addSwitch(options, ignoreoverlap)
  options <- addSwitch(options, index)

  # deal with options...
  # program-specific options
  options <- addOption(options, class)
  options <- addOption(options, precision)

  # deal with required parameters...some may have defaults
  required <- addRequired(required, datafile, TRUE)
  required <- addRequired(required, outputfile, TRUE)

  echoCommand(cmd, options, required, echoCmd)

  ret <- dispatchCommand(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile, comment)

  if (runCmd) {
    invisible(ret)
  } else {
    invisible(buildCommand(cmd, options, required))
  }
}

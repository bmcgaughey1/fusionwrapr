# ---------- PolyClipData
#
#' FUSION R command line interface -- Merges several point cloud files into a single file.
#'
#' \code{PolyClipData} creates command lines for the FUSION PolyClipData program and optionally executes them.
#'
#' @template MultipleCommands
#'
#' @param polyfile character (\strong{required}): Name of the polygon file used for clipping. Format
#'  should be Arc shapefile containing polygons.
#' @param outputfile character (\strong{required}): Base name for output data files. Default behavior is to
#'  create one output file for all polygons in \code{polyfile}. See below for use of ]code{outside}, \code{shape},
#'  and /code{multifile} switches to modify this behavior.
#' @param datafile character (\strong{required}): LIDAR data file template, list of file names or name of
#'  a text file containing a list of file names (must have .txt extension).
#' @template StandardOptions
#' @template SkipFileCheck
#' @param lda boolean: Write sample files using FUSION's LDA format when using LAS input files.  When
#'  using input data in a format other than LAS, output files are written in LDA format.
#' @param index boolean: Create FUSION index files for output data file.
#' @param outside boolean: Create output file containing all points outside of all polygons in \code{polyfile}.
#'  When used with the \code{shape} option, output file will contain all points outside the specified polygon.
#' @param multifile boolean: Create separate output data files for each polygon in \code{polyFile}.
#'  NOTE: The \code{multifile} switch can result in thousands of files for large polygon coverages.
#' @param shape character: "field,value": Use the feature in \code{polyfile} identified by value in \code{field}.
#'  Output file will contain all points within the specified polygon. \code{ield} is a 1-based index that
#'  refers to fields in the DBASE file associated with the shapefile. The \code{shape} option is ignored for
#'  other formats. If polygon identifier contains a space, enclose the identifier in quotes.
#'  Use a * for value to force indexing of the polygon file and parse polygon identifiers from \code{field}.
#'  If you do not use the \code{shape} option in conjunction with the \code{multifile} switch, output files
#'  will be identified using the sequential number associated with the polygon rather than a value from
#'  a database field.
#' @param class character: "c,c,c,...": LAS files only: Specifies that only points with classification
#'   values listed are to be included in the clipped points. Classification values should be separated
#'   by a comma. For example, \code{"2,3,4,5"}. Class values can range from 0 to 31. If the first
#'   character in string is ~, the list is interpreted as the classes you DO NOT want included in the subsample.
#'   For example, \code{"~2,3"} would include all class values EXCEPT 2 and 3.
#' @param ignoreoverlap boolean: Ignore points with the overlap flag set (LAS V1.4+ format).
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
#' PolyClipData("stands.shp", "stand_457.las", "*.las", shape = "5,457")
#' }
#' @family LTKFunctions
#' @export
MergeData <- function(
    polyfile = NULL,
    outputfile = NULL,
    datafile = NULL,
    quiet = FALSE,
    verbose = FALSE,
    version = FALSE,
    newlog = FALSE,
    log = NULL,
    locale = FALSE,
    nolaszipdll = FALSE,
    skipfilecheck = FALSE,
    lda = FALSE,
    index = FALSE,
    outside = FALSE,
    multifile = FALSE,
    shape = NULL,
    class = NULL,
    ignoreoverlap = FALSE,
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
  if (!isOpt(polyfile)
      || !isOpt(outputfile)
      || !isOpt(datafile)
  ) {
    stop("Missing required parameters: polyfile, outputfile, datafile")
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
  options <- addSwitch(options, index)
  options <- addSwitch(options, outside)
  options <- addSwitch(options, multifile)
  options <- addSwitch(options, ignoreoverlap)

  # deal with options...
  # program-specific options
  options <- addOption(options, shape)
  options <- addOption(options, class)
  options <- addOption(options, precision)

  # deal with required parameters...some may have defaults
  required <- addRequired(required, polyfile, TRUE)
  required <- addRequired(required, outputfile, TRUE)
  required <- addRequired(required, datafile, TRUE)

  echoCommand(cmd, options, required, echoCmd)

  ret <- dispatchCommand(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile, comment)

  if (runCmd) {
    invisible(ret)
  } else {
    invisible(buildCommand(cmd, options, required))
  }
}

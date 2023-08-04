# ---------- FilterData
#
#' FUSION R command line interface -- Applies various filters to LIDAR return data.
#'
#' \code{FilterData} creates command lines for the FUSION FilterData program and optionally executes them.
#'
#' @template MultipleCommands
#'
#' @param filtertype character: Filtering algorithm used to remove returns from the DataFile(s)
#'  outlier removes returns above or below the mean elevation plus or minus FilterParms * standard
#'  deviation of the elevations
#'  outlier2  More robust outlier removal...experimental
#'  minimum   removes all returns except the return with the minimum elevation
#'  maximum   removes all returns except the return with the maximum elevation
#' @param filterparams numeric: Parameters specific to the filtering method. For outlier this is the
#'  multiplier applied to the standard deviation. For minimum and maximum, FilterParms is ignored
#'  (but a value must be included...use 0).
#' @param windowsize numeric: Size of the window used to compute the standard deviation of elevations
#'  or the minimum/maximum return.
#' @param outputfile character (\strong{required}): Name of the output file.
#' @param datafile character (\strong{required}): LIDAR data file template or the  name of a text file containing
#'   a list of file names (must have .txt extension),
#' @template StandardOptions
#' @template SkipFileCheck
#' @param lda boolean: Write sample files using FUSION's LDA format when using LAS input
#'   files. The default behavior is to write data in LAS format when the input data are in LAS
#'   format. When using input data in a format other than LAS, sample files are written in LDA format.
#' @param layers boolean: Output intermediate raster data layers.
#' @param index boolean: Create FUSION index files for the \code{outputfile}.
#' @param invert boolean: Inverts the elevations for points so the logic will work for points below ground. Use with
#' \code{filtertype="outlier2"}.
#' @param minsd numeric: Minimum standard deviation for points within a window for filtering to take place.
#'  Default is 1.0 elevation units Used only with \code{filtertype="outlier"}.
#' @param minpts numeric: Minimum number of points in a window for filtering to take place. Can be used with
#'  all filters. Must be at least 3 when used with  \code{filtertype="outlier"}. The default when used with \code{filtertype="outlier"}
#'  is 3. The default for  \code{filtertype="minimum"} or  \code{filtertype="maximum"} is 1.
#' @param minrange numeric: Minimum range in elevations within a window for outlier filtering to take place.
#'  Default is 150.0 elevation units. Used only with \code{filtertype="outlier2"}.
#' @param mingap numeric: Minimum vertical distance that define a gap. Used to isolate points above
#'  the majority of points in the filter window. Used only with \code{filtertype="outlier2"}.
#' @param gapratio numeric: Proportion of points in window that can be above a vertical gap.
#'  Ranges from 0.0 to 1.0. Used only with \code{filtertype="outlier2"}.
#' @param class character: "#,#,#,...": LAS files only: Specifies that only points with classification
#'   values listed are to be included in the subsample to be considered as potential ground points.
#'   Classification values should be separated by a comma. For example, class = "2,3,4,5" and can range from 0 to 31.
#'   If the first character in string is ~, the list is interpreted as the classes you DO NOT want included
#'   in the subsample. For example class = "~2,3" would include all class values EXCEPT 2 and 3.
#' @param ignoreoverlap boolean: Ignore points with the overlap flag set (LAS V1.4+ format).
#' @param precision character: "#,#,#": Control the scale factor used for X, Y, and Z
#'   values in output LAS files. These values will override the values
#'   in the source LAS files. There is rarely any need for the scale
#'   parameters to be smaller than 0.001.
#' @param reclass numeric: Change the classification code for points identified as outliers and write them
#'  to the output file. The value is the classification code assigned to the points. Only valid
#'  when used with \code{filtertype="outlier"} or \code{filtertype="outlier2"}.
#' @template Use64bit
#' @template RunSaveOptions
#' @template Comment
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' FilterData("thinned_pts.las", 10, 100.0, "*.las")
#' }
#' @family LTKFunctions
#' @export
FilterData <- function(
    filtertype = NULL,
    filterparams = NULL,
    windowsize = NULL,
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
    layers = FALSE,
    index = FALSE,
    invert = FALSE,
    minsd = NULL,
    minpts = NULL,
    minrange = NULL,
    mingap = NULL,
    gapratio = NULL,
    class = NULL,
    ignoreoverlap = FALSE,
    precision = NULL,
    reclass = NULL,
    use64bit = TRUE,
    runCmd = TRUE,
    saveCmd = TRUE,
    cmdFile = NULL,
    cmdClear = FALSE,
    echoCmd = FALSE,
    comment = NULL
) {
  # check for required options
  if (!isOpt(filtertype)
      || !isOpt(filterparams)
      || !isOpt(windowsize)
      || !isOpt(outputfile)
      || !isOpt(datafile)
  ) {
    stop("Missing required parameters: filtertype, filterparams, windowsize, outputfile, datafile")
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
  cmd <- programName("FilterData", use64bit)

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
  options <- addSwitch(options, layers)
  options <- addSwitch(options, index)
  options <- addSwitch(options, ignoreoverlap)

  # deal with options...
  # program-specific options
  options <- addOption(options, minsd)
  options <- addOption(options, minpts)
  options <- addOption(options, minrange)
  options <- addOption(options, mingap)
  options <- addOption(options, gapratio)
  options <- addOption(options, class)
  options <- addOption(options, precision)
  options <- addOption(options, reclass)

  # deal with required parameters...some may have defaults
  required <- addRequired(required, filtertype)
  required <- addRequired(required, filterparams)
  required <- addRequired(required, windowsize)
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

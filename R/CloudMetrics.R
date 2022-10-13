# ---------- CloudMetrics
#
#' FUSION R command line interface -- Computes metrics describing an entire point cloud.
#'
#' \code{CloudMetrics} creates command lines for the FUSION CloudMetrics program and optionally executes them.
#'
#' @template MultipleCommands
#'
#' @param inputspecifier  character (\strong{required}): LIDAR data file template, name of text file containing a
#'   list of file names (must have .txt extension), or a catalog file.
#' @param outputfile character (\strong{required}): Name for output file to contain cloud metrics (usually .csv extension).
#'   If the folder for the output file does not exist, it will be created
#'   when the function is called even when saving commands to a batch file.
#' @template StandardOptions
#' @template SkipFileCheck
#' @param above numeric: Compute proportion of first returns above # (canopy cover).
#'   Also compute the proportion of all returns above # and the
#'   (number of returns above #) / (total number of 1st returns).
#'   The same metrics are also computed using the mean and mode
#'   point elevation (or height) values. Starting with version
#'   2.0 of CloudMetrics, the /relcover and /alldensity options were
#'   removed. All cover metrics are computed when the /above:#
#'   switch is used.
#' @param new boolean: Start new output file...delete existing output file.
#' @param firstinpulse boolean: Use only the first return for the pulse to compute metrics.
#' @param firstreturn boolean: Use only first returns to compute metrics.
#' @param first boolean: Use only first returns to compute metrics (same as \code{firstreturn}).
#' @param highpoint boolean: Produce a limited set of metrics ([ID],#pts,highX,highY,highZ).
#' @param subset boolean: Produce a limited set of metrics([ID],#pts,Mean ht,Std dev ht,
#'   75th percentile,cover)...must be used with /above:#.
#' @param id boolean: Parse an identifier from the beginning of the data file name...
#'   output as the first column of data.
#' @param rid  boolean: Parse an identifier from the end of the data file name...
#'   output as the first column of data.
#' @param pa boolean: Output detailed percentile data used to compute the canopy
#'   profile area. Output file name uses the base output name with
#'   _percentile appended.
#' @param minht numeric: Use only returns above # (use when data is normalized to ground)
#'   Prior to version 2.20 this switch was /htmin. /htmin can still
#'   be used but /minht is preferred. The minht is not used when
#'   computing metrics related to the /strata and /intstrata options.
#' @param maxht numeric: Use only returns below # (use when data is normalized to ground)
#'   to compute metrics. The maxht is not used when computing metrics
#'   related to the /strata or /intstrata options.
#' @param outlier character: "low,high": Omit points with elevations below low and above high.
#'   When used with data that has been normalized using a ground
#'   surface, low and high are interpreted as heights above ground.
#'   You should use care when using /outlier:low,high with /minht and
#'   /maxht options. If the low value specified with /outlier is above
#'   the value specified with /minht, the value for /outlier will
#'   override the value specified for /minht. Similarly, if the high
#'   value specified with /outlier is less than the value specified
#'   for /maxht, the /outlier value will override the value for
#'   /maxht.
#' @param ignoreoverlap boolean: Ignore points with the overlap flag set (LAS V1.4+ format).
#' @param strata character: "#,#,#,...": Count returns in various height strata. # gives the upper
#'   limit for each strata. Returns are counted if their height
#'   is >= the lower limit and < the upper limit. The first strata
#'   contains points < the first limit. The last strata contains
#'   points >= the last limit. Default strata: 0.15, 1.37, 5, 10,
#'   20, 30.
#' @param intstrata  character: "#, #, #,...": Compute metrics using the intensity values in various
#'   height strata. Strata for intensity metrics are defined in
#'   the same way as the /strata option. Default strata: 0.25, 1.37.
#' @param kde character: "window,mult": Compute the number of modes and minimum and maximum node
#'   using a kernal density estimator. Window is the width of a
#'   moving average smoothing window in data units and mult is a
#'   multiplier for the bandwidth parameter of the KDE. Default
#'   window is 2.5 and the multiplier is 1.0.
#' @param rgb  character: "color": Compute intensity metrics using the color value from the RGB
#'   color for the returns. Valid with LAS version 1.2 and newer
#'   data files that contain RGB information for each return (point
#'   record types 2 and 3). Valid color values are R, G, or B.
#' @template Use64bit
#' @template RunSaveOptions
#' @template Comment
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' CloudMetrics("points/*.las", "test.csv", minht = 2.0)
#' }
#' @family LTKFunctions
#' @export
CloudMetrics <- function(
    inputspecifier = NULL,
    outputfile = NULL,
    quiet = FALSE,
    verbose = FALSE,
    version = FALSE,
    newlog = FALSE,
    log = NULL,
    locale = FALSE,
    nolaszipdll = FALSE,
    skipfilecheck = FALSE,
    above = NULL,
    new = FALSE,
    firstinpulse = FALSE,
    firstreturn = FALSE,
    first = FALSE,
    highpoint = FALSE,
    subset = FALSE,
    id = FALSE,
    rid = FALSE,
    pa = FALSE,
    minht = NULL,
    maxht = NULL,
    outlier = NULL,
    ignoreoverlap = FALSE,
    strata = NULL,
    intstrata = NULL,
    kde = NULL,
    rgb = NULL,
    use64bit = TRUE,
    runCmd = TRUE,
    saveCmd = TRUE,
    cmdFile = NULL,
    cmdClear = FALSE,
    echoCmd = FALSE,
    comment = NULL
) {
  # check for required options
  if (!isOpt(inputspecifier)
      || !isOpt(outputfile)
  ) {
    stop("Missing required parameters: inputspecifier, outputfile")
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

  # if inputspecifier and outputfile are different lengths, there are problems
  if (length(inputspecifier) != length(outputfile)) {
    stop("Lengths (number of items) of inputspecifier and outputfile are incompatible")
  }

  # build command line
  cmd <- programName("CloudMetrics", use64bit)

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
  options <- addSwitch(options, new)
  options <- addSwitch(options, firstinpulse)
  options <- addSwitch(options, firstreturn)
  options <- addSwitch(options, first)
  options <- addSwitch(options, highpoint)
  options <- addSwitch(options, subset)
  options <- addSwitch(options, id)
  options <- addSwitch(options, rid)
  options <- addSwitch(options, pa)
  options <- addSwitch(options, ignoreoverlap)

  # deal with options...
  # program-specific options
  options <- addOption(options, above)
  options <- addOption(options, minht)
  options <- addOption(options, maxht)
  options <- addOption(options, outlier)
  options <- addOption(options, strata)
  options <- addOption(options, intstrata)
  options <- addOption(options, kde)
  options <- addOption(options, rgb)

  # deal with required parameters...some may have defaults
  required <- addRequired(required, inputspecifier, TRUE)
  required <- addRequired(required, outputfile, TRUE)

  echoCommand(cmd, options, required, echoCmd)

  ret <- dispatchCommand(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile, comment)

  if (runCmd) {
    invisible(ret)
  } else {
    invisible(buildCommand(cmd, options, required))
  }
}

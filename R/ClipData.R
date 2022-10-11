# ---------- ClipData
#
#' FUSION R command line interface -- Function to create command lines for the ClipData program.
#'
#' \code{ClipData} creates command lines for the FUSION ClipData program and optionally executes them.
#'
#' @template MultipleCommands
#'
#' @param inputspecifier character (\strong{required}): LIDAR data file template, name of a text file containing
#'   a list of file names (must have .txt extension), or a
#'   FUSION catalog file.
#' @param samplefile character (\strong{required}): Name for subsample file (extension will be added) or a
#'   text file containing sample information for 1 or more samples. Each line in the text file should
#'   have the output filename and the MinX MinY MaxX MaxY values for the sample area separated
#'   by spaces or commas. The output filename cannot contain spaces. To save compressed LAS files,
#'   specify the .laz extension. If the folder for the output file does not exist, it will be created
#'   when the function is called even when saving commands to a batch file.
#' @param minx numeric: X for lower left corner of the sample area bounding box.
#' @param miny numeric: Y for lower left corner of the sample area bounding box.
#' @param maxx numeric: X for upper right corner of the sample area bounding box.
#' @param maxy numeric: Y for upper right corner of the sample area bounding box.
#' @template StandardOptions
#' @template SkipFileCheck
#' @param shape numeric: Shape of the sample area (0 = rectangle, 1 = circle).
#' @param decimate numeric: Skip # points between included points (must be > 0).
#' @param ground character: Use a surface file with /zmin to include points above zmin
#'   or with /zmax to include points below zmax. file may be
#'   wildcard or text list file (extension .txt only). file
#'   must be in FUSION/PLANS format.
#' @param zmin numeric: Include points above # elevation..use with /dtm to include points
#'   above # height. Use with /height option to store heights in
#'   output file.
#' @param zmax numeric: Include points below # elevation..use with /dtm to include points
#'   below # height. Use with /height option to store heights in
#'   output file.
#' @param zpercent numeric: Include only the upper # percent of the points..if # is (-) only
#'   the lower # percent of the points..# = 0-100.
#' @param height boolean: Convert point elevations into heights above ground using the
#'   specified DTM file..use with /dtm.
#' @param timemin numeric: Include points with GPS times greater than # (LAS only).
#' @param timemax numeric: Include points with GPS times less than or equal to # (LAS only).
#'   Interpretation of # depends on the GPS time recorded in the LAS
#'   point records.
#' @param anglemin numeric: Include points with scan angles greater than # (LAS only).
#' @param anglemax numeric: Include points with scan angles less than or equal to # (LAS only).
#' @param zero boolean: Save subsample files that contain no data points.
#' @param biaselev numeric: Add an elevation offset to every LIDAR point..# can be + or -.
#' @param return character: "c,c,c,...": Specifies the returns to be included in the sample (can
#'   include A,1,2,3,4,5,6,7,8,9,F,L,O) Options are specified without
#'   commas (e.g. /return:123) For LAS files only: F indicates first
#'   and only returns, L indicates last of many returns.
#' @param class character: "c,c,c,...": LAS files only: Specifies that only points with classification
#'   values listed are to be included in the subsample.
#'   Classification values should be separated by a comma.
#'   e.g. (2,3,4,5) and can range from 0 to 31.
#'   If the first character in string is ~, the list is interpreted
#'   as the classes you DO NOT want included in the subsample.
#'   e.g. /class:~2,3 would include all class values EXCEPT 2 and 3.
#' @param ignoreoverlap boolean: Ignore points with the overlap flag set (LAS V1.4+ format).
#' @param line numeric: LAS files only: Only include returns from the specified flight
#'   line. Line numbering varies by acquisition so you need to know
#'   your data to specify values for the flight line number.
#' @param noindex boolean: Do not use the data index files to access the data files.
#' @param index boolean: Create FUSION index files for the SampleFile.
#' @param lda boolean: Write sample files using FUSION's LDA format when using LAS input
#'   files. The default behavior of ClipData (after version 2.35) is
#'   to write data in LAS format when the input data are in LAS
#'   format. When using input data in a format other than LAS, sample
#'   files are written in LDA format.
#' @param nooffset boolean: Removes the offset value in the output LAS file making it
#'   difficult to pinpoint the location of the point cloud. This is
#'   typically used when the location of a sample cannot be known.
#' @param cleanlas boolean: Only output points that adhere to the LAS format specification
#'   (valid GPS time, return # from 1 to 5, within header extent,
#'   points not flagged as withheld. Valid for LAS format input.
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
#' ClipData("*.las", "clip1.las", ground = "small.dtm", height = TRUE)
#' }
#' @family LTKFunctions
#' @export
ClipData <- function(
    inputspecifier = NULL,
    samplefile = NULL,
    minx = "-1000000000",
    miny = "-1000000000",
    maxx = "1000000000",
    maxy = "1000000000",
    quiet = FALSE,
    verbose = FALSE,
    version = FALSE,
    newlog = FALSE,
    log = NULL,
    locale = FALSE,
    nolaszipdll = FALSE,
    skipfilecheck = FALSE,
    shape = 0,
    decimate = NULL,
    ground = NULL,
    zmin = NULL,
    zmax = NULL,
    zpercent = NULL,
    height = FALSE,
    timemin = NULL,
    timemax = NULL,
    anglemin = NULL,
    anglemax = NULL,
    zero = FALSE,
    biaselev = NULL,
    return = NULL,
    class = NULL,
    ignoreoverlap = FALSE,
    line = NULL,
    noindex = FALSE,
    index = FALSE,
    lda = FALSE,
    nooffset = FALSE,
    cleanlas = FALSE,
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
  if (!isOpt(inputspecifier)
      || !isOpt(samplefile)
  ) {
    stop("Missing required parameters: inputspecifier, samplefile")
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

  # if inputspecifier and samplefile are different lengths, there is some work to do...
  # if length(inputspecifier) > 1 and length(samplefile) == 1, create a list file and use this for a single command line
  # if length(inputspecifier) > 1 and length(samplefile) > 1, they must be the same length
  # length(inputspecifier) == 1 and length(samplefile) > 1, doesn't make sense...might if length(min/max ) > 1 but not going to handle this case
  if (length(inputspecifier) != length(samplefile)) {
    if (length(inputspecifier) > 1 && length(samplefile) == 1) {
      # create temporary list file and use this to create a single command line
      tmpFile <- tempfile("list", fileext = ".txt")
      unlink(tmpFile)
      lapply(inputspecifier, function(x) {cat(paste0(x, "\n"), file = tmpFile, append = TRUE)})
      inputspecifier <- tmpFile
      message("Created list file for inputspecifier: ", tmpFile)
    } else {
      # problem
      stop("Lengths (number of items) of inputspecifier and samplefile are incompatible")
    }
  }

  # build command line
  cmd <- programName("ClipData", use64bit)

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
  options <- addSwitch(options, height)
  options <- addSwitch(options, zero)
  options <- addSwitch(options, ignoreoverlap)
  options <- addSwitch(options, noindex)
  options <- addSwitch(options, index)
  options <- addSwitch(options, lda)
  options <- addSwitch(options, nooffset)
  options <- addSwitch(options, cleanlas)

  # deal with options...
  # program-specific options
  options <- addOption(options, decimate)
  options <- addOption(options, ground, TRUE)
  options <- addOption(options, zmin)
  options <- addOption(options, zmax)
  options <- addOption(options, zpercent)
  options <- addOption(options, timemin)
  options <- addOption(options, timemax)
  options <- addOption(options, anglemin)
  options <- addOption(options, anglemax)
  options <- addOption(options, biaselev)
  options <- addOption(options, return)
  options <- addOption(options, class)
  options <- addOption(options, line)
  options <- addOption(options, precision)
  options <- addOption(options, shape)

  # deal with required parameters...some may have defaults
  required <- addRequired(required, inputspecifier, TRUE)
  required <- addRequired(required, samplefile, TRUE)
  required <- addRequired(required, minx)
  required <- addRequired(required, miny)
  required <- addRequired(required, maxx)
  required <- addRequired(required, maxy)

  echoCommand(cmd, options, required, echoCmd)

  ret <- dispatchCommand(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile, comment)

  if (runCmd) {
    invisible(ret)
  } else {
    invisible(buildCommand(cmd, options, required))
  }
}

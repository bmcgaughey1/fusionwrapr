# TreeSeg
# ---------- TreeSeg
#
#' FUSION R command line interface -- Function to create command lines for the TreeSeg program.
#'
#' \code{TreeSeg} creates command lines for the FUSION TreeSeg program and optionally executes them.
#'
#' @template MultipleCommands
#'
#' @param CHM character (\strong{required}): Name for canopy height model (PLANS DTM with .dtm extension).
#'   May be wildcard or text list file (extension .txt only). This can be a canopy surface
#'   model if the /ground option is used to specify a ground surface for normalization.
#' @param ht_threshold numeric (\strong{required}): Minimum height for object segmentation. Portions of the CHM
#'   below this height are not considered in the segmentation.
#' @param outputfile character (\strong{required}): Base name for output file. Metrics are stored in CSV format with
#'   .csv extension. Other outputs are stored in files named using the base name and additional
#'   descriptive information. If the folder for the output file does not exist, it will be created
#'   when the function is called even when saving commands to a batch file.
#' @template StandardOptions
#' @template SkipFileCheck
#' @param height boolean: Normalize canopy surface model(s) using ground model(s).
#' @param ptheight numeric: Normalize point heights using ground model(s).
#' @param maxht numeric: Force the maximum height for the segmentation. This will override
#'   the actual maximum value in the CHM. Use this option to force equal vertical resolution
#'   across areas with varying maximum canopy heights.
#' @param grid character: "X1,X2,Y1,Y2": Force the origin of the analysis grid to be (X,Y) instead of
#'   computing an origin from the data extents and force the grid to be W units wide and H units
#'   high...W and H will be rounded up to a multiple of cellsize.
#' @param gridxy character: "X1,X2,Y1,Y2": Force the origin of the analysis grid to be (X1,Y1) instead
#'   of computing an origin from the data extents and force the grid to use (X2,Y2) as the upper
#'   right corner of the coverage area. The actual upper right corner will be adjusted to be a
#'   multiple of cellsize.
#' @param align character: Force the origin and extent of the analysis grid to match the
#'   lower left corner and extent of the specified PLANS format DTM file.
#' @param buffer numeric: Add a buffer to the data extent specified by /grid, /gridxy or /align
#'   when segmenting but only output data for the segments located within the extent.
#' @param ground character: Use the specified bare-earth surface model to normalize the LIDAR data
#'   file may be wildcard or text list file (extension .txt only).
#' @param points character: LIDAR point data file(s) in LDA or LAS format. May be wildcard or text
#'   list file (extension .txt only). Points are assigned to individual basins or crown polygons
#'   and a separate file (in LDA format) is output for each basin or polygon.
#' @param class character: "c,c,c,...": LAS files only: Specifies that only points with classification
#'   values listed are to be included in the subsample.
#'   Classification values should be separated by a comma.
#'   e.g. (2,3,4,5) and can range from 0 to 31.
#'   If the first character in string is ~, the list is interpretted
#'   as the classes you DO NOT want included in the subsample.
#'   e.g. /class:~2,3 would include all class values EXCEPT 2 and 3.
#' @param segmentpts boolean: Output points for the raster segments. Default is to output points
#'   for crown polygons when the /shape option is used and for raster segments when /shape is
#'   not used. Used only with the /points option.
#' @param clipfolder character: folder name where point files for individual clips are stored. Used
#'   only with the /points option. If not specified, point files are stored in the same folder with
#'   other outputs. If the folder does not exist, it will be created when the function is called even
#'   when saving commands to a batch file.
#' @param shape boolean: Create a shapefile containing the high points and basin metrics.
#' @param cleantile boolean: Output an ASCII raster map that only includes basins within the
#'   reporting extent defined by the /grid, /gridxy, and /align options.
#' @param htmultiplier numeric: Multiply the high point and surface heights by this value for
#'   output products. Also multiply individual point heights by # before writing point files
#'   (see /points option).
#' @param projection character: Associate the specified projection file with shapefile and raster
#'   data products.
#' @template Use64bit
#' @template RunSaveOptions
#' @template Comment
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' TreeSeg("CHM.dtm", 2.0, "trees.csv")
#' }
#' @family LTKFunctions
#' @export
TreeSeg <- function(
    CHM = NULL,
    ht_threshold = NULL,
    outputfile = NULL,
    quiet = FALSE,
    verbose = FALSE,
    version = FALSE,
    newlog = FALSE,
    log = NULL,
    locale = FALSE,
    nolaszipdll = FALSE,
    skipfilecheck = FALSE,
    height = FALSE,
    ptheight = FALSE,
    maxht = NULL,
    grid = NULL,
    gridxy = NULL,
    align = NULL,
    buffer = NULL,
    ground = NULL,
    points = NULL,
    class = NULL,
    segmentpts = FALSE,
    clipfolder = NULL,
    shape = FALSE,
    cleantile = FALSE,
    htmultiplier = NULL,
    projection = NULL,
    use64bit = TRUE,
    runCmd = TRUE,
    saveCmd = TRUE,
    cmdFile = NULL,
    cmdClear = FALSE,
    echoCmd = FALSE,
    comment = NULL
) {
  # check for required options
  if (!isOpt(CHM)
      || !isOpt(ht_threshold)
      || !isOpt(outputfile)
  ) {
    stop("Missing required parameters: CHM, ht_threshold, outputfile")
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

  # check for folder for tree clips...will create if it doesn't exist
  # don't call dirname() because it will strip off the last folder in the path
  if (isOpt(clipfolder)) {
    # make sure there is a trailing slash
    if (!endsWith(clipfolder, "/"))
      clipfolder <- paste0(clipfolder, "/")

    verifyFolder(clipfolder, runCmd, saveCmd, cmdFile, cmdClear)
  }

  # build command line
  cmd <- programName("TreeSeg", use64bit)

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
  options <- addSwitch(options, ptheight)
  options <- addSwitch(options, segmentpts)
  options <- addSwitch(options, shape)
  options <- addSwitch(options, cleantile)

  # deal with options...
  options <- addOption(options, maxht)
  options <- addOption(options, grid)
  options <- addOption(options, gridxy)
  options <- addOption(options, align, TRUE)
  options <- addOption(options, buffer)
  options <- addOption(options, ground, TRUE)
  options <- addOption(options, points, TRUE)
  options <- addOption(options, class)
  options <- addOption(options, clipfolder, TRUE)
  options <- addOption(options, htmultiplier)
  options <- addOption(options, projection)

  # deal with required parameters...some may have defaults
  required <- addRequired(required, CHM, TRUE)
  required <- addRequired(required, ht_threshold)
  required <- addRequired(required, outputfile, TRUE)

  echoCommand(cmd, options, required, echoCmd)

  ret <- dispatchCommand(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile, comment)

  if (runCmd) {
    invisible(ret)
  } else {
    invisible(buildCommand(cmd, options, required))
  }
}

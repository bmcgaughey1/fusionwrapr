# GridMetrics
# ---------- GridMetrics
#
#' FUSION R command line interface -- Function to create command lines for the GridMetrics program.
#'
#' \code{GridMetrics} creates command lines for the FUSION GridMetrics program and optionally executes them.
#'
#' @template MultipleCommands
#'
#' @param groundfile  character (\strong{required}): Name for ground surface model (PLANS DTM with .dtm extension).
#'   May be wildcard or text list file (extension .txt only).
#' @param heightbreak numeric (\strong{required}): Height break for cover calculation.
#' @param cellsize numeric (\strong{required}): Desired grid cell size in the same units as LIDAR data.
#' @param outputfile character (\strong{required}): Base name for output file. Metrics are stored in CSV format with
#'   .csv extension unless the /nocsv switch is used. Other outputs are stored in files named using the
#'   base name and additional descriptive information. If the folder for the output file does not exist,
#'   it will be created  when the function is called even when saving commands to a batch file.
#' @param datafile character (\strong{required}): Name(s) of lidar data files.
#' @template StandardOptions
#' @template SkipFileCheck
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
#' @param class character: "#,#,#,...": LAS files only: Specifies that only points with classification
#'   values listed are to be included in the subsample.
#'   Classification values should be separated by a comma.
#'   e.g. (2,3,4,5) and can range from 0 to 31.
#'   If the first character in string is ~, the list is interpreted
#'   as the classes you DO NOT want included in the subsample.
#'   e.g. /class:~2,3 would include all class values EXCEPT 2 and 3.
#' @param ignoreoverlap boolean: Ignore points with the overlap flag set (LAS V1.4+ format).
#' @param id character: "identifier": Include the identifier string as the last column in every
#'   record in the outputfile. The identifier will be included in
#'   all files containing metrics (elevation, intensity, and topo).
#'   The identifier cannot include spaces.
#' @param minpts numeric: Minimum number of points in a cell required to compute metrics
#'   default is 4 points.
#' @param minht numeric: Minimum height for points used to compute metrics. Density always
#'   uses all points including those with heights below the minimum.
#' @param nocsv boolean: Do not create an output file for cell metrics.
#' @param noground boolean: Do not use a ground surface model. When this option is specified,
#'   omit the groundfile parameter from the command line. Cover
#'   estimates, densitytotal, densityabove, and densitycell metrics
#'   are meaningless when no ground surface model is used unless the
#'   LIDAR data have been normalized to the ground surface using some
#'   other process.
#' @param diskground boolean: Do not load ground surface models into memory. When this option
#'   is specified, larger areas can be processed but processing will
#'   be 4 to 5 times slower. Ignored when /noground option is used.
#' @param nointdtm boolean: Do not create an internal ground model that corresponds to the
#'   data extent. This option is most often used to prevent edge
#'   artifacts when computing metrics for small areas using a
#'   relatively larger cell size.
#' @param failnoz boolean: Verify that all ground surface models can be opened and read
#'   and exit immediately if there are any problems. The default
#'   behavior is to go ahead and compute metrics using point
#'   elevations (no normalization to create point heights).
#' @param first boolean: Use only first returns to compute all metrics (default is to
#'   use all returns for metrics).
#' @param nointensity boolean: Do not compute metrics using intensity values (default is
#'   to compute metrics using both intensity and elevation values).
#' @param rgb character: "color": Compute intensity metrics using the color value from the RGB
#'   color for the returns. Valid with LAS version 1.2 and newer
#'   data files that contain RGB information for each return (point
#'   record types 2 and 3). Valid color values are R, G, or B.
#' @param fuel boolean: Apply fuel parameter models (cannot be used with /intensity,
#'   /alldensity, or /first switches.
#' @param grid character: "X1,X2,Y1,Y2": Force the origin of the output grid to be (X,Y) instead of
#'   computing an origin from the data extents and force the grid to
#'   be W units wide and H units high...W and H will be rounded up to
#'   a multiple of cellsize.
#' @param gridxy character: "X1,X2,Y1,Y2": Force the origin of the output grid to be (X1,Y1) instead
#'   of computing an origin from the data extents and force the grid
#'   to use (X2,Y2) as the upper right corner of the coverage area.
#'   The actual upper right corner will be adjusted to be a multiple
#'   of cellsize.
#' @param align character: Force the origin and extent of the output grid to match the
#'   lower left corner and extent of the specified PLANS format DTM file.
#' @param extent character: Force the origin and extent of the output grid to match the
#'   lower left corner and extent of the specified PLANS format DTM
#'   file but adjust the origin to be an even multiple of the cell
#'   size and the width and height to be multiples of the cell size.
#' @param buffer numeric: Add a buffer to the data extent specified by /grid or /gridxy
#'   when computing metrics but only output data for the specified
#'   extent. The buffer width is first converted to a cellbuffer and
#'   then added all around the extent. The actual buffer width may be
#'   slightly larger than specified by width.
#' @param cellbuffer numeric: Add a buffer to the data extent specified by /grid or /gridxy
#'   when computing metrics but only output data for the specified
#'   extent. The buffer (number of cells) is added around the extent.
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
#' @param topo character: "dist,lat": Compute topographic metrics using the groundfile(s) and output
#'   them in a separate file. Distance is the cell size for the 3 by
#'   3 cell analysis area and lat is the latitude (+north, -south).
#' @template Use64bit
#' @template RunSaveOptions
#' @template Comment
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' GridMetrics("points/*.las", "test.csv", minht = 2.0)
#' }
#' @family LTKFunctions
#' @export
GridMetrics <- function(
    groundfile = NULL,
    heightbreak = NULL,
    cellsize = NULL,
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
    outlier = NULL,
    class = NULL,
    ignoreoverlap = FALSE,
    id = NULL,
    minpts = NULL,
    minht = NULL,
    nocsv = FALSE,
    noground = FALSE,
    diskground = FALSE,
    nointdtm = FALSE,
    failnoz = FALSE,
    first = FALSE,
    nointensity = FALSE,
    rgb = NULL,
    fuel = FALSE,
    grid = NULL,
    gridxy = NULL,
    extent = NULL,
    align = NULL,
    buffer = NULL,
    cellbuffer = NULL,
    strata = NULL,
    intstrata = NULL,
    kde = NULL,
    topo = NULL,
    use64bit = TRUE,
    runCmd = TRUE,
    saveCmd = TRUE,
    cmdFile = NULL,
    cmdClear = FALSE,
    echoCmd = FALSE,
    comment = NULL
) {
  # check for required options
  if (!isOpt(groundfile)
      || !isOpt(heightbreak)
      || !isOpt(cellsize)
      || !isOpt(outputfile)
      || !isOpt(datafile)
  ) {
    stop("Missing required parameters: groundfile, heightbreak, cellsize, outputfile, datafile")
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
  cmd <- programName("GridMetrics", use64bit)

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
  options <- addSwitch(options, ignoreoverlap)
  options <- addSwitch(options, nocsv)
  options <- addSwitch(options, noground)
  options <- addSwitch(options, diskground)
  options <- addSwitch(options, nointdtm)
  options <- addSwitch(options, failnoz)
  options <- addSwitch(options, first)
  options <- addSwitch(options, nointensity)
  options <- addSwitch(options, fuel)

  # deal with options...
  # program-specific options
  options <- addOption(options, outlier)
  options <- addOption(options, class)
  options <- addOption(options, id)
  options <- addOption(options, minpts)
  options <- addOption(options, minht)
  options <- addOption(options, rgb)
  options <- addOption(options, grid)
  options <- addOption(options, gridxy)
  options <- addOption(options, align, TRUE)
  options <- addOption(options, extent, TRUE)
  options <- addOption(options, buffer)
  options <- addOption(options, cellbuffer)
  options <- addOption(options, strata)
  options <- addOption(options, intstrata)
  options <- addOption(options, kde)
  options <- addOption(options, topo)

  # deal with required parameters...some may have defaults
  required <- addRequired(required, groundfile, TRUE)
  required <- addRequired(required, heightbreak, TRUE)
  required <- addRequired(required, cellsize, TRUE)
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

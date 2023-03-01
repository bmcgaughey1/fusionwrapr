# ---------- Catalog
#
#' FUSION R command line interface -- Prepares a report describing a LIDAR dataset and optionally
#'  indexes all data file for use in FUSION
#'
#' \code{Catalog} creates command lines for the FUSION Catalog program and optionally executes them.
#'
#' @template MultipleCommands
#'
#' @param datafile  character (\strong{required}): LIDAR data file template or the name of text file containing a
#'   list of file names (must have .txt extension).
#' @param catalogfile character (\strong{required}): Base name for the output catalog file (extensions will be added).
#' @template StandardOptions
#' @template SkipFileCheck
#' @param image boolean: Create image files showing the coverage area for each LIDAR file.
#' @param index boolean: Create LIDAR data file indexes if they don't already exist.
#' @param newindex boolean: Create new LIDAR data file indexes for all files (even if they already exist).
#' @param drawtiles boolean: Draw data file extents and names on the intensity image.
#' @param coverage boolean: Create one image that shows the nominal coverage area for all data files
#'  included in the catalog. Also creates a FUSION hotspot file that provides details for each file in the catalog.
#' @param countreturns boolean: Produce a count of returns by return number and include in the CSV and
#'  HTML output reports.
#' @param uselascounts boolean: Use the return counts from the header of LAS files instead of scanning
#'  the entire data file to count the returns. Many LAS files produced by TerraScan do not contain
#'   valid data for the return counts so make sure your data has good numbers before using this switch
#' @param rawcounts boolean: Outputs the number of returns (or first returns) in each cell. Used in
#'  conjunction with the \code{density} and \code{firstdensity} options. The output is in PLANS DTM format.
#' @param density character: "area,min,max": Creates an image for all data files that shows the return density
#'  for the area represented by each pixel. area is the pixel area, min is the minimum acceptable point
#'  density, and max is the upper limit for the acceptable density range. Cells with point densities
#'  falling within the min-max range are colored green, cells with point densities below the minimum
#'  are colored red, and cells with densities above the maximum are colored blue.
#' @param firstdensity character: "area,min,max": Creates an image for all data files that shows the density of
#'  first returns for the area represented by each pixel. area is the pixel area, min is the minimum
#'  acceptable point density, and max is the upper limit for the acceptable density range. Cells with
#'  first return densities falling within the min-max range are colored green, cells with point densities
#'  below the minimum are colored red, and cells with densities above the maximum are colored blue.
#' @param intensity character: "area,min,max": Creates an intensity image for all data files using the average
#'  intensity for all first returns within each pixel. area is the pixel area, min is the minimum
#'  intensity value, and max is the maximum intensity value. A black to white color ramp is mapped
#'  to the range of intensity values defined by min and max. Ideally, min and max correspond to
#'  the range of intensity values present in the data. However, you may not always know the range of
#'  values for a given data set.
#' @param imageextent character: "minx,miny,maxx,maxy": Limit the area covered by image products to the
#'  specified extent.
#' @param bmp boolean: Save second copy of intensity, return density, and pulse density images in
#'  BMP format with associated world file.
#' @param outlier character: "multiplier": Performs a simple analysis to identify data tiles that might
#'  contain elevation outliers. The analysis marks tiles where the minimum, maximum, or range of
#'  elevations are outside the range defined by:
#'  mean value +- multiplier * std dev
#'  The default multiplier is 2.0.
#' @param class character: "#,#,#,...": LAS files only: Specifies that only points with classification
#'   values listed are to be included in the subsample to be considered as potential ground points.
#'   Classification values should be separated by a comma. For example, class = "2,3,4,5" and can range from 0 to 31.
#'   If the first character in string is ~, the list is interpreted as the classes you DO NOT want included
#'   in the subsample. For example class = "~2,3" would include all class values EXCEPT 2 and 3.
#' @param ignoreoverlap boolean: Ignore points with the overlap flag set (LAS V1.4+ format).
#' @param noclasssummary boolean: Do not create a summary table showing the number of points by
#'  LAS classification values. Only valid for LAS format files.
#' @param validate character: "maxreturn": Produce report describing potential errors in point data
#'  files. Report will contain files with errors the might cause problems for other FUSION programs.
#'  \code{maxreturn} is the maximum number of returns expected for a pulse.
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
#' Catalog("points/*.las", "test")
#' }
#' @family LTKFunctions
#' @export
Catalog <- function(
    datafile = NULL,
    catalogfile = NULL,
    quiet = FALSE,
    verbose = FALSE,
    version = FALSE,
    newlog = FALSE,
    log = NULL,
    locale = FALSE,
    nolaszipdll = FALSE,
    skipfilecheck = FALSE,
    image = FALSE,
    index = FALSE,
    newindex = FALSE,
    drawtiles = FALSE,
    coverage = FALSE,
    countreturns = FALSE,
    uselascounts = FALSE,
    rawcounts = FALSE,
    density = NULL,
    firstdensity = NULL,
    intensity = NULL,
    imageextent = NULL,
    bmp = FALSE,
    outlier = NULL,
    class = NULL,
    ignoreoverlap = FALSE,
    noclasssummary = FALSE,
    validate = NULL,
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
  if (!isOpt(datafile)
      || !isOpt(catalogfile)
  ) {
    stop("Missing required parameters: datafile, catalogfile")
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
  verifyFolder(dirname(catalogfile), runCmd, saveCmd, cmdFile, cmdClear)

  # if we are saving commands to a file, cmdClear will have done its job in the call to verifyFolder
  cmdClear <- FALSE

  # build command line
  cmd <- programName("Catalog", use64bit)

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
  options <- addSwitch(options, image)
  options <- addSwitch(options, index)
  options <- addSwitch(options, newindex)
  options <- addSwitch(options, drawtiles)
  options <- addSwitch(options, coverage)
  options <- addSwitch(options, countreturns)
  options <- addSwitch(options, uselascounts)
  options <- addSwitch(options, rawcounts)
  options <- addSwitch(options, bmp)
  options <- addSwitch(options, ignoreoverlap)
  options <- addSwitch(options, noclasssummary)

  # deal with options...
  # program-specific options
  options <- addOption(options, density)
  options <- addOption(options, firstdensity)
  options <- addOption(options, intensity)
  options <- addOption(options, imageextent)
  options <- addOption(options, outlier)
  options <- addOption(options, class)
  options <- addOption(options, validate)
  options <- addOption(options, projection, TRUE)

  # deal with required parameters...some may have defaults
  required <- addRequired(required, datafile, TRUE)
  required <- addRequired(required, catalogfile, TRUE)

  echoCommand(cmd, options, required, echoCmd)

  ret <- dispatchCommand(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile, comment)

  if (runCmd) {
    invisible(ret)
  } else {
    invisible(buildCommand(cmd, options, required))
  }
}

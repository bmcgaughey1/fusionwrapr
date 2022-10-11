# SurfaceSample
# ---------- SurfaceSample
#
#' FUSION R command line interface -- Function to interpolate surface values for XY locations.
#'
#' \code{SurfaceSample} creates command lines for the FUSION SurfaceSample program and optionally executes them.
#'
#' @template MultipleCommands
#'
#' @param surfacefile character (\strong{required}): Name for input surface file (PLANS DTM with .dtm extension).
#'   May be wildcard or text list file (extension .txt only).
#' @param inputfile character (\strong{required}):  Name of file containing XY locations (space or
#'   comma delimited). If \code{pattern} is used with \code{type 3}, the \code{inputfile}
#'   should contain two coordinate pairs that specify the endpoints of the line. If \code{id = TRUE}, the
#'   \code{inputfile} should contain a point identifier in the first column.
#' @param outputfile character (\strong{required}): Name for the output ASCII CSV file.
#' @template StandardOptionsNoPts
#' @param pattern character: "type,p1,p2,p3": Generate a test pattern of sample points centered on the XY
#'   location from the \code{inputfile}. Pattern type 1 is a radial network of \code{p1} lines that are at
#'   least \code{p2} long with sample points every \code{p3} units. The first radial is at 3 o'clock. Radials
#'   are generated in counter-clockwise order. Pattern type 2 is a radial network of \code{p1} lines that are at
#'   least \code{p2} long with sample points every \code{p3} units ON AVERAGE. The sample point spacing decreases
#'   as the distance from the XY location increases to provide sample point locations that represent a uniform
#'   area The first radial is at 3 o'clock. Radials are generated in counter-clockwise order. Pattern type 3
#'   expects two coordinate pairs in the input data. The pairs specify the endpoints of a line. Lines with points
#'   spaced \code{p1} units apart are created and written to the output.
#' @param topo character: "dist,lat": Compute solar radiation index (SRI) as defined in: Keating, K.
#'   A.,P. J. P. Gogan, J. M. Vore, and L. R. Irby. 2007. A simple solar radiation index for
#'   wildlife habitat studies. Journal of Wildlife Management 71(4):1344-1348. Algorithm uses a 3-
#'   by 3-cell grid with cells that are \code{dist} by \code{dist} units to compute topographic
#'   attributes. Latitude values (\code{lat}) are in degrees with north positive and south negative.
#'   Output from this option includes slope, aspect, SRI, and other topographic indices. The range
#'   for SRI is 0.0 to 2.0 (differs from the -1.0 to 1.0 range in Keating et al. 2007).
#' @param noheader boolean: Suppress the header line in the output file. This option is useful when you want
#'   to use PDQ to view the outputfile.
#' @param novoid boolean: Suppress output for XY sample points outside the surface extent or for points with
#'   invalid surface elevations.
#' @param id boolean: Read a point identifier from the first field of each line from the
#'   \code{inputfile} and output it as the first field of the \code{outputfile}. If \code{id} is
#'   used with pattern, a separate output file is created for each point in the \code{inputfile}.
#'   Output files are named using \code{outputfile} as the base name with the point identifier
#'   appended to the filename. Even when \code{inputfile} contains a single point, the
#'   \code{outputfile} name is modified to reflect the point identifier.
#' @template RunSaveOptions
#' @template Comment
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' SurfaceSample("ground.dtm", "in.csv", "out.csv")
#' }
#' @family LTKFunctions
#' @export
SurfaceSample <- function(
    surfacefile = NULL,
    inputfile = NULL,
    outputfile = NULL,
    quiet = FALSE,
    verbose = FALSE,
    version = FALSE,
    newlog = FALSE,
    log = NULL,
    locale = FALSE,
    pattern = NULL,
    topo = NULL,
    noheader = FALSE,
    novoid = FALSE,
    id = NULL,
    runCmd = TRUE,
    saveCmd = TRUE,
    cmdFile = NULL,
    cmdClear = FALSE,
    echoCmd = FALSE,
    comment = NULL
) {
  # check for required options
  if (!isOpt(surfacefile)
      || !isOpt(inputfile)
      || !isOpt(outputfile)
  ) {
    stop("Missing required parameters: surfacefile, inputfile, outputfile")
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
  cmd <- programName("SurfaceSample", FALSE)

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
  options <- addSwitch(options, noheader)
  options <- addSwitch(options, novoid)

  # deal with options...
  options <- addOption(options, pattern)
  options <- addOption(options, topo)

  # deal with required parameters...some may have defaults
  required <- addRequired(required, surfacefile, TRUE)
  required <- addRequired(required, inputfile, TRUE)
  required <- addRequired(required, outputfile, TRUE)

  echoCommand(cmd, options, required, echoCmd)

  ret <- dispatchCommand(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile, comment)

  if (runCmd) {
    invisible(ret)
  } else {
    invisible(buildCommand(cmd, options, required))
  }
}

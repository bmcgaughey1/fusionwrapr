# GridSurfaceStats
# ---------- GridSurfaceStats
#
#' FUSION R command line interface -- Computes surface area and volume for the surface. Result is a raster layer.
#'
#' \code{GridSurfaceStats} creates command lines for the FUSION GridSurfaceStats program and optionally executes them.
#'
#' @template MultipleCommands
#'
#' @param inputfile  character (\strong{required}): File specifier for the input surface file. This can be a single
#'  file, a wildcard specifier, or a text list file (extension .txt only).
#' @param outputfile character (\strong{required}): Base name for the output files containing the surface statistics
#'  including the .dtm extension. If \code{ascii=TRUE}, the output files will all be in ASCII raster
#'  format with the .asc extension.
#' @param samplefactor numeric (\strong{required}): Multiplier for \code{outputfile} cell size. \code{outputfile} cells will
#'  represent \code{samplefactor * samplefactor} cells from the \code{inputfile}. When multiple input files are used,
#'  the cell size of the first file is used to compute the outfile cell size.
#' @template StandardOptionsNoPts
#' @param ground character: "file": Use the specified surface model to represent the ground surface
#'  file may be wildcard or text list file (extension .txt only).
#' @param ascii boolean: Output all files in ASCII raster format with the .asc extension.
#' @param area boolean: Compute the surface area of inputfile instead of the surface area
#'  divided by the flat cell area.
#' @param halfcell boolean: Force alignment of the output grid to match the grid used by the GridMetrics progam.
#'  This option cannot be used with \code{grid, gridxy, extent, or align} switches.
#' @param svonly boolean: Output only the surface volume metric layer.
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
#' @template Use64bit
#' @template RunSaveOptions
#' @template Comment
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' GridSurfaceStats("ground.dtm", 30.0, 4.0, "density.csv", "*.las")
#' }
#' @family LTKFunctions
#' @export
GridSurfaceStats <- function(
    inputfile = NULL,
    outputfile = NULL,
    samplefactor = NULL,
    quiet = FALSE,
    verbose = FALSE,
    version = FALSE,
    newlog = FALSE,
    log = NULL,
    locale = FALSE,
    ground = NULL,
    ascii = FALSE,
    area = FALSE,
    halfcell = NULL,
    svonly = FALSE,
    grid = NULL,
    gridxy = NULL,
    align = NULL,
    extent = NULL,
    use64bit = TRUE,
    runCmd = TRUE,
    saveCmd = TRUE,
    cmdFile = NULL,
    cmdClear = FALSE,
    echoCmd = FALSE,
    comment = NULL
) {
  # check for required options
  if (!isOpt(inputfile)
      || !isOpt(outputfile)
      || !isOpt(samplefactor)
  ) {
    stop("Missing required parameters: inputfile, outputfile, samplefactor")
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
  cmd <- programName("GridSurfaceStats", use64bit)

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
  options <- addSwitch(options, ascii)
  options <- addSwitch(options, area)
  options <- addSwitch(options, halfcell)
  options <- addSwitch(options, svonly)

  # deal with options...
  # program-specific options
  options <- addOption(options, ground, TRUE)
  options <- addOption(options, grid)
  options <- addOption(options, gridxy)
  options <- addOption(options, align, TRUE)
  options <- addOption(options, extent, TRUE)

  # deal with required parameters...some may have defaults
  required <- addRequired(required, inputfile, TRUE)
  required <- addRequired(required, outputfile, TRUE)
  required <- addRequired(required, samplefactor)

  echoCommand(cmd, options, required, echoCmd)

  ret <- dispatchCommand(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile, comment)

  if (runCmd) {
    invisible(ret)
  } else {
    invisible(buildCommand(cmd, options, required))
  }
}

# FUSION function wrappers -- fusionR
#
# found an rFUSION package https://cran.r-project.org/web/packages/rFUSION/index.html but not many functions
# and it doesn't look very robust
#
# thinking about this as a package but not sure I want to add all programs.
# as a package, there could be global options for FUSION install folder, runCmd, saveCmd, and cmdFile that would
# override options for specific calls to build and execute command lines.
# this would also require functions to write comments to command file (preceded by a blank line),
# clear the command  file, and (probably) run the command file


# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           Ctrl + Shift + B
#   Check Package:             Ctrl + Shift + E
#   Test Package:              Ctrl + Shift + T
#   Build documentation:       Ctrl + Shift + D...not working
#   Build vignette:            Ctrl + Shift + K
#
# setwd("G:/R_Stuff/fusionwrapr")
#
# To build documentation:
# devtools::document()

#
# set up local environment to hold global variables
fusionrEnv <- new.env(parent = emptyenv())

fusionrEnv$runCmd <- FALSE
fusionrEnv$saveCmd <- TRUE
fusionrEnv$cmdFile <- NULL
fusionrEnv$installPath <- ""

# accessory functions

# ---------- verifyFolder
#
#' FUSION R command line interface -- Verify existence of a folder
#'
#' This is a helper function used in the fusionwrapr package to verify
#' the existence of a folder and create the folder if it does not exist.
#' verifyFolder is normally called with a folder extracted using
#' \code{dirname()} when an output file is needed for a function.
#'
#' @param folder folder name
#' @return An (invisible) boolean TRUE, If the folder does not exist and cannot
#'   be created, execution stops.
#' @examples
#' \dontrun{
#' verifyFolder("/test")
#' }
#' @export
verifyFolder <- function(folder) {
  lapply(folder, function(x) {
      if (!base::dir.exists(file.path(x))) {
        if (!base::dir.create(file.path(x), recursive = TRUE)) {
          stop("could not create folder: ", x)
        } else {
          message("created folder: ", x)
        }
      }
    }
  )
  invisible(TRUE)
}

# ---------- isOpt
#
#' FUSION R command line interface -- test if object is NULL
#'
#' This is a helper function used in the fusionwrapr package to test the
#' value of an object: usually a function parameter.
#'
#' @param x object
#' @return A (invisible) boolean value, TRUE indicates the object value is not NULL,
#'   FALSE indicates the object value is NULL.
isOpt <- function(x) { invisible(!is.null(x)) }

# ---------- addSwitch
#
#' FUSION R command line interface -- Build text string for a command line option.
#'
#' This is a helper function used in the fusionwrapr package to build the text string
#' for command line switches. The resulting string is appended to \code{cl}.
#'
#' @param cl character string. Potentially contains other command line elements.
#' @param opt object containing the switch value. The name of the object is used
#'   when constructing the option text.
#' @return A (invisible) string containing the modified \code{cl} string.
#' @examples
#' \dontrun{
#' addOption(cl, zmin)
#' }
addSwitch <- function(cl, opt) {
  if (opt) {
    cl <- paste(cl, paste0("/", deparse(substitute(opt))))
  }
  invisible(cl)
}

# ---------- addOption
#
#' FUSION R command line interface -- Build text string for a command line option.
#'
#' This is a helper function used in the fusionwrapr package to build the text string
#' for command line options that require parameters. The resulting string is appended
#' to \code{cl}.
#'
#' @param cl character string. Potentially contains other command line elements.
#' @param opt object containing the option value. The name of the object and the
#'   value are used when constructing the option text.
#' @param quote boolean indicating the entire option should be enclosed in quotes.
#' @return A (invisible) string containing the modified \code{cl} string.
#' @examples
#' \dontrun{
#' addOption(cl, zmin)
#' }
addOption <- function(cl, opt, quote = FALSE) {
  if (isOpt(opt)) {
    if (quote) {
      cl <- paste(cl, shQuote(paste0("/", deparse(substitute(opt)), ":", opt)))
    } else {
      cl <- paste(cl, paste0("/", deparse(substitute(opt)), ":", opt))
    }
  }
  invisible(cl)
}

# ---------- addRequired
#
#' FUSION R command line interface -- Build text string for a required parameter.
#'
#' This is a helper function used in the fusionwrapr package to build the text string
#' for a required parameter. The resulting string is appended to \code{cl}.
#'
#' @param cl character string. Potentially contains other command line elements.
#' @param req object containing the value for the required parameter.
#' @param quote boolean indicating the entire parameter should be enclosed in quotes.
#' @return A (invisible) string containing the modified \code{cl} string.
#' @examples
#' \dontrun{
#' addRequired(cl, "*.las")
#' }
addRequired <- function(cl, req, quote = FALSE) {
  if (quote) {
    cl <- paste(cl, shQuote(req))
  } else {
    cl <- paste(cl, req)
  }
  invisible(cl)
}

# ---------- programName
#
#' FUSION R command line interface -- Build text string for the program name.
#'
#' This is a helper function used in the fusionwrapr package to build the text string
#' for the program name. It uses the \code{use64bit} parameter to control use of
#' 64-bit versions of command line programs and will include the FUSION install
#' path if it has been provided using \code{setFUSIONpath()}.
#'
#' @param name character string with the base program name.
#' @param use64bit boolean value indicating 64-bit version of the program
#'   specified in \code{name} should be used. If \code{setFUSIONpath} has
#'   been called,
#' @return A (invisible) string containing the program name.
#' @examples
#' \dontrun{
#' programName("ClipData", TRUE)
#' }
programName <- function(name, use64bit = FALSE) {
  # if there is a global variable for the FUSION install folder, use it here
  if (use64bit) {
    # see if we have a FUSION install folder
    if (fusionrEnv$installPath != "") {
      t <- paste0(fusionrEnv$installPath, name, "64")
      if (file.exists(t)) {
        invisible(paste0(fusionrEnv$installPath, name, "64"))
      } else {
        invisible(paste0(fusionrEnv$installPath, name))
      }
    } else {
      invisible(paste0(name, "64"))
    }
  } else {
    invisible(paste0(fusionrEnv$installPath, name))
  }
}

# ---------- checkRunSaveFile
#
#' FUSION R command line interface -- Verify compatibility of function options.
#'
#' This is a helper function used in the fusionwrapr package to verify the combination
#' of the runCmd, saveCmd, and cmdFile options for functions that build command line.
#'
#' @param runCmd boolean indicating command line should be executed.
#' @param saveCmd boolean indicating command line should be written to a file.
#' @param cmdFile character string containing the name of the file to which commands
#'   should be written.
#' @return An (invisible) boolean TRUE, If the options are inconsistent, execution stops.
#' @examples
#' \dontrun{
#' checkRunSaveFile(FALSE, TRUE, "test.bat")
#' }
checkRunSaveFile <- function(runCmd, saveCmd, cmdFile) {
  if (!runCmd && saveCmd) {
    if (!isOpt(cmdFile)) {
      stop("Missing command file with runCmd = FALSE and saveCmd = TRUE")
    }
  }
  invisible(TRUE)
}

# ---------- dispatchCommand
#
#' FUSION R command line interface -- Execute command lines or write them to a file.
#'
#' This is a helper function used in the fusionwrapr package to either execute command lines or
#' write them to a file.
#'
#' @param cmd character string containing the program name and possibly the path.
#' @param options character string containing command line options.
#' @param required character string(s) containing required parameters for the command line.
#' @param runCmd boolean indicating command line should be executed.
#' @param saveCmd boolean indicating command line should be written to a file.
#' @param cmdClear boolean indicating file for command should be deleted before the command
#'   line is written.
#' @param cmdFile character string containing the name of the file to which commands
#'   should be written.
#' @return A single integer value or vector of integers. If \code{required} is a vector of
#'   strings and \code{runCmd = TRUE}, the return is a vector of return codes from the
#'   operating system indicating the return values from the FUSION program. If \code{required}
#'   is a vector of values, \code{runCmd = FALSE} and \code{saveCmd = TRUE}, the return
#'   is a vector of zeros. if \code{required} is a single string and \code{runCmd = TRUE},
#'   the return is the return value from the FUSION program. If \code{required} is a single
#'   string and \code{runCmd = FALSE} and \code{saveaCmd = TRUE}, the return value is 0.
#' @examples
#' \dontrun{
#' dispatchCommand("ClipData", "/minht:2.0", "*.las clip1.las", runCmd = TRUE)
#' dispatchCommand("ClipData", "/minht:2.0", "*.las clip1.las",
#'                  runCmd = FALSE, cmdClear = TRUE, cmdFile = "test.bat")
#' }
dispatchCommand <- function(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile) {
  # when runCmd=TRUE, saveCmd is never referenced. when runCmd=FALSE, the default is to write cmd to file
  # with runCmd=FALSE and saveCmd=FALSE, command is returned only (invisible)
  ret <- lapply(required, function(x) {
      if (runCmd) {
        ret <- system2(cmd, paste(options, x))
      } else {
        if (saveCmd) {
          if (cmdClear) unlink(cmdFile)
          cat(paste0(buildCommand(cmd, options, x), "\n"), file = cmdFile, append = TRUE)
        }
        ret <- 0
      }
    }
  )
  invisible(ret)
}

# ---------- echoCommand
#
#' FUSION R command line interface -- Construct and echo the command line.
#'
#' This is a helper function used in the fusionwrapr package to construct and echo the command
#' line (depending of value of echoCmd parameter).
#'
#' @param cmd character string containing the program name and possibly the path.
#' @param options character string containing command line options.
#' @param required character string(s) containing required parameters for the command line.
#' @param echoCmd boolean indicating command line should be echoed using message().
#' @return boolean TRUE
#' @examples
#' \dontrun{
#' echoCommand("ClipData", "/minht:2.0", "*.las clip1.las", echoCmd = TRUE)
#' }
echoCommand <- function(cmd, options, required, echoCmd) {
  if (echoCmd) message(buildCommand(cmd, options, required))

  invisible(TRUE)
}

# ---------- buildCommand
#
#' FUSION R command line interface -- Construct the command line.
#'
#' This is a helper function used in the fusionwrapr package to construct the command line.
#'
#' @param cmd character string containing the program name and possibly the path.
#' @param options character string containing command line options.
#' @param required character string(s) containing required parameters for the command line.
#' @return invisible string containing complete command line
#' @examples
#' \dontrun{
#' buildCommand("ClipData", "/minht:2.0", "*.las clip1.las")
#' }
buildCommand <- function(cmd, options, required) {
  invisible(paste(trimws(cmd), trimws(options), trimws(required)))
}

# ---------- setFUSIONpath
#
#' FUSION R command line interface -- Set a package environment variable containing the FUSION install folder.
#'
#' /code{setFUSIONpath} sets an environment variable local to the \code{fusionwrapr} package that specifies
#' the install folder for FUSION. This is necessary when the FUSION install folder has not been added
#' to the PATH environment variable.
#'
#' @param installPath character string containing the FUSION install folder. A trailing forward
#'   slash will be added if not included.
#' @return character string containing the \code{installPath} passed to the function.
#' @examples
#' \dontrun{
#' setFUSIONpath("C:/FUSION/")
#' }
#' @export
setFUSIONpath <- function(installPath) {
  fusionrEnv$installPath <- installPath

  if (fusionrEnv$installPath != "") {
    # make sure path ends with slash
    if (!endsWith(fusionrEnv$installPath, "/"))
      fusionrEnv$installPath <- paste0(fusionrEnv$installPath, "/")
  }

  invisible(fusionrEnv$installPath)
}

# ---------- addToCommandFile
#
#' FUSION R command line interface -- Write comments or other lines to command file
#'
#' /code{addToCommandFile} writes lines to a command file (batch file). The normal behavior is to write comments
#' but with \code{comment=FALSE}, you can write commands.
#'
#' @param line character: line to be written to the command file.
#' @param cmdFile character: contains the name of the file to which commands
#'   should be written.
#' @param cmdClear boolean: indicates file for command should be deleted before the command
#'   line is written. Use this option with caution as it will wipe out the content of the
#'   command file.
#' @param comment boolean: \code{line} is preceeded by "REM" when TRUE, otherwise not.
#' @return invisible boolean indicating success or failure
#' @examples
#' \dontrun{
#' addToCommandFile("This is a comment!!", "Test/test.bat")
#' }
#' @export
addToCommandFile <- function(
  line = "",
  cmdFile = NULL,
  cmdClear = FALSE,
  comment = TRUE
) {
  if (isOpt(cmdFile)) {
    if (cmdClear) unlink(cmdFile)

    if (comment) {
      cat(paste0("REM ", line, "\n"), file = cmdFile, append = TRUE)
    } else {
      cat(paste0(line, "\n"), file = cmdFile, append = TRUE)
    }
    invisible(TRUE)
  }

  invisible(FALSE)
}

# ---------- runCommandFile
#
#' FUSION R command line interface -- Run a command file
#'
#' /code{runCommandFile} runs a command file.
#'
#' @param cmdFile character: contains the name of the file to be run.
#' @param ... additional parameter passed to system2().
#' @return invisible boolean indicating success or failure for the initiation of the run.
#' @examples
#' \dontrun{
#' runCommandFile("Test/test.bat")
#' }
#' @export
runCommandFile <- function(
  cmdFile = NULL,
  ...
) {
  if (isOpt(cmdFile)) {
    system2(cmdFile, ...)

    invisible(TRUE)
  }

  invisible(FALSE)
}

# *****************************************************************************
# Super-functions that simplify specific tasks using FUSION command line tools.
# *****************************************************************************
# ---------- ClipPlot
#
#' FUSION R command line interface -- Super-function to clip data for plots using the ClipData program.
#'
#' /code{ClipPlot} creates command lines for the FUSION ClipData program and optionally executes them.
#' Command lines are designed to clip a circular or square area centered in the \code{(x,y)}.
#'
#' @param inputspecifier character: LIDAR data file template, name of a text file containing
#'   a list of file names (must have .txt extension), or a
#'   FUSION catalog file.
#' @param samplefile character: Name for plot clip file (extension will be added).
#'   \code{samplefile} cannot contain spaces. To save compressed LAS files, specify the .laz extension.
#' @param x numeric: Easting value for plot location.
#' @param y numeric: Northing value for plot location.
#' @param radius numeric: Radius for round (circular) plots or half width for square plots.
#' @param shape numeric: default = 1: Shape of the sample area (0 = rectangle, 1 = circle).
#' @param ... Additional parameters that will be passed to \code{ClipData}.
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' ClipPlot("*.las", "plot_0001.las", 435895.9, 5669341.4, 0, ground = "small.dtm", height = TRUE)
#' }
#' @export
ClipPlot <- function(
  inputspecifier = NULL,
  samplefile = NULL,
  x = NULL,
  y = NULL,
  radius = NULL,
  shape = 1,
  ...
) {
  # check for required options
  if (!isOpt(inputspecifier)
      || !isOpt(samplefile)
      || !isOpt(x)
      || !isOpt(y)
      || !isOpt(radius)
  ) {
    stop("Missing required parameters: inputspecifier, samplefile, x, y, radius")
  }

  # call ClipData returning its return value
  invisible(
    ClipData(inputspecifier,
             samplefile,
             x - radius,
             y - radius,
             x + radius,
             y + radius,
             shape = shape, ...)
  )
}



# *****************************************************************************
# Interface functions for FUSION command line tools
# *****************************************************************************
# ---------- ClipData
#
#' FUSION R command line interface -- Function to create command lines for the ClipData program.
#'
#' /code{ClipData} creates command lines for the FUSION ClipData program and optionally executes them.
#'
#' @param inputspecifier character: LIDAR data file template, name of a text file containing
#'   a list of file names (must have .txt extension), or a
#'   FUSION catalog file.
#' @param samplefile character: Name for subsample file (extension will be added) or a
#'   text file containing sample information for 1 or more samples.
#'   Each line in the text file should have the output filename
#'   and the MinX MinY MaxX MaxY values for the sample area separated
#'   by spaces or commas. The output filename cannot contain spaces.
#'   To save compressed LAS files, specify the .laz extension.
#' @param minx numeric: X for lower left corner of the sample area bounding box.
#' @param miny numeric: Y for lower left corner of the sample area bounding box.
#' @param maxx numeric: X for upper right corner of the sample area bounding box.
#' @param maxy numeric: Y for upper right corner of the sample area bounding box.
#' @param quiet boolean: Suppress all output during the run.
#' @param verbose boolean: Display all status information during the run.
#' @param version boolean: Report version information and exit with no processing.
#' @param newlog boolean: Erase the existing log file and start a new log
#' @param log character: Use the name specified for the log file.
#' @param locale boolean: Adjust program logic to input and output locale-specific numeric
#'   formats (e.g. use a comma for the decimal separator).
#' @param nolaszipdll boolean: Suppress the use of the LASzip dll (c) Martin Isenburg...
#'   removes support for compressed LAS (LAZ) files. This option
#'   is only useful for programs that read or write point files.
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
#'   If the first character in string is ~, the list is interpretted
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
#' @param use64bit boolean: indicates 64-bit version of the program
#'   should be used.
#' @param runCmd boolean: indicates command line should be executed.
#' @param saveCmd boolean: indicates command line should be written to a file.
#' @param cmdFile character: contains the name of the file to which commands
#'   should be written.
#' @param cmdClear boolean: indicates file for command should be deleted before the command
#'   line is written.
#' @param echoCmd boolean: indicates command line should be displayed.
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' ClipData("*.las", "clip1.las", ground = "small.dtm", height = TRUE)
#' }
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
  echoCmd = FALSE
) {
  # check for required options
  if (!isOpt(inputspecifier)
      || !isOpt(samplefile)
  ) {
    stop("Missing required parameters: inputspecifier, samplefile")
  }

  # check for option to run command...if FALSE, check for command file name
  checkRunSaveFile(runCmd, saveCmd, cmdFile)

  # check for folder included in output...will create if it doesn't exist
  verifyFolder(dirname(samplefile))

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
  options <- addOption(options, log)
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

  ret <- dispatchCommand(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile)

  if (runCmd) {
    invisible(ret)
  } else {
    invisible(buildCommand(cmd, options, required))
  }
}

# CloudMetrics
# ---------- CloudMetrics
#
#' FUSION R command line interface -- Function to create command lines for the CloudMetrics program.
#'
#' /code{CloudMetrics} creates command lines for the FUSION CloudMetrics program and optionally executes them.
#'
#' @param inputspecifier  character: LIDAR data file template, name of text file containing a
#'   list of file names (must have .txt extension), or a catalog file.
#' @param outputfile character: Name for output file to contain cloud metrics (usually .csv extension).
#' @param quiet boolean: Suppress all output during the run.
#' @param verbose boolean: Display all status information during the run.
#' @param version boolean: Report version information and exit with no processing.
#' @param newlog boolean: Erase the existing log file and start a new log.
#' @param log character string: Use the name specified for the log file.
#' @param locale boolean: Adjust program logic to input and output locale-specific numeric
#'   formats (e.g. use a comma for the decimal separator).
#' @param nolaszipdll boolean: Suppress the use of the LASzip dll (c) Martin Isenburg...
#'   removes support for compressed LAS (LAZ) files. This option
#'   is only useful for programs that read or write point files.
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
#' @param use64bit boolean: indicates 64-bit version of the program
#'   should be used.
#' @param runCmd boolean: indicates command line should be executed.
#' @param saveCmd boolean: indicates command line should be written to a file.
#' @param cmdFile character: contains the name of the file to which commands
#'   should be written.
#' @param cmdClear boolean: indicates file for command should be deleted before the command
#'   line is written.
#' @param echoCmd boolean: indicates command line should be displayed.
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' CloudMetrics("points/*.las", "test.csv", minht = 2.0)
#' }
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
  echoCmd = FALSE
) {
  # check for required options
  if (!isOpt(inputspecifier)
      || !isOpt(outputfile)
  ) {
    stop("Missing required parameters: inputspecifier, outputfile")
  }

  # check for option to run command...if FALSE, check for command file name
  checkRunSaveFile(runCmd, saveCmd, cmdFile)

  # check for folder included in output...will create if it doesn't exist
  verifyFolder(dirname(outputfile))

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

  ret <- dispatchCommand(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile)

  if (runCmd) {
    invisible(ret)
  } else {
    invisible(buildCommand(cmd, options, required))
  }
}

# GridSurfaceCreate
# ---------- GridSurfaceCreate
#
#' FUSION R command line interface -- Function to create command lines for the GridSurfaceCreate program.
#'
#' /code{GridSurfaceCreate} creates command lines for the FUSION GridSurfaceCreate program and optionally executes them.
#'
#' @param surfacefile character: Name for output surface file (stored in PLANS DTM format with .dtm extension).
#' @param cellsize numeric: Desired grid cell size in the same units as LIDAR data.
#' @param xyunits character: Units for LIDAR data XY (M for meters or F for feet).
#' @param zunits character: Units for LIDAR data elevations (M for meters or F for feet).
#' @param coordsys numeric: Coordinate system for LIDAR data:
#'   0 for unknown
#'   1 for UTM
#'   2 for state plane)
#' @param zone numeric: Coordinate system zone for LIDAR data (0 for unknown).
#' @param horizdatum numeric: Horizontal datum:
#'   0 for unknown
#'   1 for NAD27
#'   2 for NAD83
#' @param vertdatum numeric: Vertical datum:
#'   0 for unknown
#'   1 for NGVD29
#'   2 for NAVD88
#'   3 for GRS80
#' @param datafile character: Name(s) of lidar data files.
#' @param quiet boolean: Suppress all output during the run.
#' @param verbose boolean: Display all status information during the run.
#' @param version boolean: Report version information and exit with no processing.
#' @param newlog boolean: Erase the existing log file and start a new log
#' @param log character: Use the name specified for the log file.
#' @param locale boolean: Adjust program logic to input and output locale-specific numeric
#'   formats (e.g. use a comma for the decimal separator).
#' @param nolaszipdll boolean: Suppress the use of the LASzip dll (c) Martin Isenburg...
#'   removes support for compressed LAS (LAZ) files. This option
#'   is only useful for programs that read or write point files.
#' @param median numeric: Apply median filter to model using # by # neighbor window.
#' @param smooth numeric: Apply mean filter to model using # by # neighbor window.
#' @param slope numeric: Filter areas from the surface with slope greater than # percent.
#'   Slope filtering takes place after all other smoothing operations.
#' @param spike numeric: Filter to remove spikes with slopes greater than # percent.
#'   Spike filtering takes place after slope filtering.
#' @param residuals boolean: Compute residual statistics for all points.
#' @param filldist numeric: Maximum search radius (in cells) used when filling holes in the
#'   surface. Default is 99 cells.
#' @param class character: "#,#,#,...": LAS files only: Specifies that only points with classification
#'   values listed are to be included in the subsample.
#'   Classification values should be separated by a comma.
#'   e.g. (2,3,4,5) and can range from 0 to 31.
#'   If the first character in string is ~, the list is interpreted
#'   as the classes you DO NOT want included in the subsample.
#'   e.g. /class:~2,3 would include all class values EXCEPT 2 and 3.
#' @param ignoreoverlap boolean: Ignore points with the overlap flag set (LAS V1.4+ format).
#' @param minimum boolean: Use the minimum elevation value in cells to create the surface.
#' @param maximum boolean: Use the maximum elevation value in cells to create the surface.
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
#' @param smoothfirst boolean: indicating smoothing should occur before median
#'   filtering. The default is for median filtering to happen before smoothing.
#' @param use64bit boolean: indicates 64-bit version of the program
#'   should be used.
#' @param runCmd boolean: indicates command line should be executed.
#' @param saveCmd boolean: indicates command line should be written to a file.
#' @param cmdFile character: contains the name of the file to which commands
#'   should be written.
#' @param cmdClear boolean: indicates file for command should be deleted before the command
#'   line is written.
#' @param echoCmd boolean: indicates command line should be displayed.
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' GridSurfaceCreate("test.dtm", 2.0, "M", "M", 1, 10, 2, 2, "Test/pts.las", class = "2")
#' }
#' @export
GridSurfaceCreate <- function(
  surfacefile = NULL,
  cellsize = NULL,
  xyunits = NULL,
  zunits = NULL,
  coordsys = NULL,
  zone = NULL,
  horizdatum = NULL,
  vertdatum = NULL,
  datafile = NULL,
  quiet = FALSE,
  verbose = FALSE,
  version = FALSE,
  newlog = FALSE,
  log = NULL,
  locale = FALSE,
  nolaszipdll = FALSE,
  median = NULL,
  smooth = NULL,
  slope = NULL,
  spike = NULL,
  residuals = FALSE,
  filldist = NULL,
  class = NULL,
  ignoreoverlap = FALSE,
  minimum = FALSE,
  maximum = FALSE,
  grid = NULL,
  gridxy = NULL,
  align = NULL,
  extent = NULL,
  smoothfirst = FALSE,
  use64bit = TRUE,
  runCmd = TRUE,
  saveCmd = TRUE,
  cmdFile = NULL,
  cmdClear = FALSE,
  echoCmd = FALSE
) {
  # check for required options
  if (!isOpt(surfacefile)
      || !isOpt(cellsize)
      || !isOpt(xyunits)
      || !isOpt(zunits)
      || !isOpt(coordsys)
      || !isOpt(zone)
      || !isOpt(horizdatum)
      || !isOpt(vertdatum)
      || !isOpt(datafile)
  ) {
    stop("Missing required parameters: surfacefile, cellsize, xyunits, zunits, coordsys, zone, horizdatum, vertdatum, datafile")
  }

  # check for option to run command...if FALSE, check for command file name
  checkRunSaveFile(runCmd, saveCmd, cmdFile)

  # check for folder included in output...will create if it doesn't exist
  verifyFolder(dirname(surfacefile))

  # build command line
  cmd <- programName("GridSurfaceCreate", use64bit)

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

  # program-specific options
  options <- addSwitch(options, residuals)
  options <- addSwitch(options, ignoreoverlap)
  options <- addSwitch(options, minimum)
  options <- addSwitch(options, maximum)

  # deal with options...
  # program-specific options
  if (smoothfirst)
    options <- addOption(options, smooth)

  options <- addOption(options, median)
  if (!smoothfirst)
    options <- addOption(options, smooth)
  options <- addOption(options, slope)
  options <- addOption(options, spike)
  options <- addOption(options, filldist)
  options <- addOption(options, class)
  options <- addOption(options, grid)
  options <- addOption(options, gridxy)
  options <- addOption(options, align, TRUE)
  options <- addOption(options, extent, TRUE)

  # deal with required parameters...some may have defaults
  required <- addRequired(required, surfacefile, TRUE)
  required <- addRequired(required, cellsize)
  required <- addRequired(required, xyunits)
  required <- addRequired(required, zunits)
  required <- addRequired(required, coordsys)
  required <- addRequired(required, zone)
  required <- addRequired(required, horizdatum)
  required <- addRequired(required, vertdatum)
  required <- addRequired(required, datafile, TRUE)

  echoCommand(cmd, options, required, echoCmd)

  ret <- dispatchCommand(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile)

  if (runCmd) {
    invisible(ret)
  } else {
    invisible(buildCommand(cmd, options, required))
  }
}

# GridMetrics
# ---------- GridMetrics
#
#' FUSION R command line interface -- Function to create command lines for the GridMetrics program.
#'
#' /code{GridMetrics} creates command lines for the FUSION GridMetrics program and optionally executes them.
#'
#' @param groundfile  character: Name for ground surface model (PLANS DTM with .dtm extension).
#'   May be wildcard or text list file (extension .txt only).
#' @param heightbreak numeric: Height break for cover calculation.
#' @param cellsize numeric: Desired grid cell size in the same units as LIDAR data.
#' @param outputfile character: Base name for output file. Metrics are stored in CSV format with
#'   .csv extension unless the /nocsv switch is used. Other outputs
#'   are stored in files named using the base name and additional
#'   descriptive information.
#' @param datafile character: Name(s) of lidar data files.
#' @param quiet boolean: Suppress all output during the run.
#' @param verbose boolean: Display all status information during the run.
#' @param version boolean: Report version information and exit with no processing.
#' @param newlog boolean: Erase the existing log file and start a new log.
#' @param log character string: Use the name specified for the log file.
#' @param locale boolean: Adjust program logic to input and output locale-specific numeric
#'   formats (e.g. use a comma for the decimal separator).
#' @param nolaszipdll boolean: Suppress the use of the LASzip dll (c) Martin Isenburg...
#'   removes support for compressed LAS (LAZ) files. This option
#'   is only useful for programs that read or write point files.
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
#' @param use64bit boolean: indicates 64-bit version of the program
#'   should be used.
#' @param runCmd boolean: indicates command line should be executed.
#' @param saveCmd boolean: indicates command line should be written to a file.
#' @param cmdFile character: contains the name of the file to which commands
#'   should be written.
#' @param cmdClear boolean: indicates file for command should be deleted before the command
#'   line is written.
#' @param echoCmd boolean: indicates command line should be displayed.
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' GridMetrics("points/*.las", "test.csv", minht = 2.0)
#' }
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
  echoCmd = FALSE
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

  # check for option to run command...if FALSE, check for command file name
  checkRunSaveFile(runCmd, saveCmd, cmdFile)

  # check for folder included in output...will create if it doesn't exist
  verifyFolder(dirname(outputfile))

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

  ret <- dispatchCommand(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile)

  if (runCmd) {
    invisible(ret)
  } else {
    invisible(buildCommand(cmd, options, required))
  }
}

# CanopyModel
# ---------- CanopyModel
#
#' FUSION R command line interface -- Function to create command lines for the CanopyModel program.
#'
#' /code{CanopyModel} creates command lines for the FUSION CanopyModel program and optionally executes them.
#'
#' @param surfacefile character: Name for output surface file (stored in PLANS DTM format with .dtm extension).
#' @param cellsize numeric: Desired grid cell size in the same units as LIDAR data.
#' @param xyunits character: Units for LIDAR data XY (M for meters or F for feet).
#' @param zunits character: Units for LIDAR data elevations (M for meters or F for feet).
#' @param coordsys numeric: Coordinate system for LIDAR data:
#'   0 for unknown
#'   1 for UTM
#'   2 for state plane)
#' @param zone numeric: Coordinate system zone for LIDAR data (0 for unknown).
#' @param horizdatum numeric: Horizontal datum:
#'   0 for unknown
#'   1 for NAD27
#'   2 for NAD83
#' @param vertdatum numeric: Vertical datum:
#'   0 for unknown
#'   1 for NGVD29
#'   2 for NAVD88
#'   3 for GRS80
#' @param datafile character: Name(s) of lidar data files.
#' @param quiet boolean: Suppress all output during the run.
#' @param verbose boolean: Display all status information during the run.
#' @param version boolean: Report version information and exit with no processing.
#' @param newlog boolean: Erase the existing log file and start a new log
#' @param log character: Use the name specified for the log file.
#' @param locale boolean: Adjust program logic to input and output locale-specific numeric
#'   formats (e.g. use a comma for the decimal separator).
#' @param nolaszipdll boolean: Suppress the use of the LASzip dll (c) Martin Isenburg...
#'   removes support for compressed LAS (LAZ) files. This option
#'   is only useful for programs that read or write point files.
#' @param median numeric: Apply median filter to model using # by # neighbor window.
#' @param smooth numeric: Apply mean filter to model using # by # neighbor window.
#' @param texture numeric: Calculate the surface texture metric using # by # neighbor window.
#' @param slope boolean: Calculate surface slope.
#' @param aspect boolean: Calculate surface aspect.
#' @param outlier character: "low,high": Omit points with elevations below low and above high.
#'   If used with a bare-earth surface this option will omit points
#'   with heights below low or above high.
#' @param multiplier numeric: Multiply the output values by the constant.
#' @param return character: "ccc...": Specifies the returns to be included in the sample (can
#'   include A,1,2,3,4,5,6,7,8,9,F,L,O) Options are specified without
#'   commas (e.g. /return:123) For LAS files only: F indicates first
#'   and only returns, L indicates last of many returns.
#' @param class character: "#,#,#,...": LAS files only: Specifies that only points with classification
#'   values listed are to be included in the subsample.
#'   Classification values should be separated by a comma.
#'   e.g. (2,3,4,5) and can range from 0 to 31.
#'   If the first character in string is ~, the list is interpreted
#'   as the classes you DO NOT want included in the subsample.
#'   e.g. /class:~2,3 would include all class values EXCEPT 2 and 3.
#' @param ignoreoverlap boolean: Ignore points with the overlap flag set (LAS V1.4+ format).
#' @param ground character: Use the specified bare-earth surface model to normalize the LIDAR data
#'   file may be wildcard or text list file (extension .txt only).
#' @param hole numeric: Defines the threshold for the hole filling logic. Any areas
#'   at or below the height will be filled unless the /nofill option is used.
#' @param ascii boolean: Write output surface in ASCII raster format as well as DTM
#'   format. Extension will be .asc.
#' @param surface boolean: Use the bare-earth surface model in conjunction with values
#'   specified in /outlier to omit points but create a surface
#'   that is not normalized relative to the bare-earth surface.
#' @param peaks boolean: Preserve localized peaks in the final surface.
#' @param pointcount boolean: Output the number of data points in each cell in addition to the
#'   canopy surface/height values. Counts are output in .DTM format.
#'   If there are no points for a cell, the elevation/height value
#'   for the cell is set to 999.0.
#' @param nofill boolean: Don't fill holes in the canopy surface model. The default
#'   logic fills holes prior to doing any smoothing.
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
#' @param rasterorigin boolean: Offsets the origin and adjusts the extent to correspond with
#'   raster data layers covering the same extent and using the same
#'   resolution. Only used with the /grid and /gridxy options.
#' @param smoothfirst boolean: indicating smoothing should occur before median
#'   filtering. The default is for median filtering to happen before smoothing.
#' @param use64bit boolean: indicates 64-bit version of the program
#'   should be used.
#' @param runCmd boolean: indicates command line should be executed.
#' @param saveCmd boolean: indicates command line should be written to a file.
#' @param cmdFile character: contains the name of the file to which commands
#'   should be written.
#' @param cmdClear boolean: indicates file for command should be deleted before the command
#'   line is written.
#' @param echoCmd boolean: indicates command line should be displayed.
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' CanopyModel("test.dtm", 2.0, "M", "M", 1, 10, 2, 2, "Test/pts.las")
#' }
#' @export
CanopyModel <- function(
  surfacefile = NULL,
  cellsize = NULL,
  xyunits = NULL,
  zunits = NULL,
  coordsys = NULL,
  zone = NULL,
  horizdatum = NULL,
  vertdatum = NULL,
  datafile = NULL,
  quiet = FALSE,
  verbose = FALSE,
  version = FALSE,
  newlog = FALSE,
  log = NULL,
  locale = FALSE,
  nolaszipdll = FALSE,
  median = NULL,
  smooth = NULL,
  texture = NULL,
  slope = FALSE,
  aspect = FALSE,
  outlier = NULL,
  multiplier = NULL,
  return = NULL,
  class = NULL,
  ignoreoverlap = FALSE,
  ground = NULL,
  hole = NULL,
  ascii = FALSE,
  surface = FALSE,
  peaks = FALSE,
  pointcount = FALSE,
  nofill = FALSE,
  grid = NULL,
  gridxy = NULL,
  align = NULL,
  extent = NULL,
  rasterorigin = FALSE,
  smoothfirst = FALSE,
  use64bit = TRUE,
  runCmd = TRUE,
  saveCmd = TRUE,
  cmdFile = NULL,
  cmdClear = FALSE,
  echoCmd = FALSE
) {
  # check for required options
  if (!isOpt(surfacefile)
      || !isOpt(cellsize)
      || !isOpt(xyunits)
      || !isOpt(zunits)
      || !isOpt(coordsys)
      || !isOpt(zone)
      || !isOpt(horizdatum)
      || !isOpt(vertdatum)
      || !isOpt(datafile)
  ) {
    stop("Missing required parameters: surfacefile, cellsize, xyunits, zunits, coordsys, zone, horizdatum, vertdatum, datafile")
  }

  # check for option to run command...if FALSE, check for command file name
  checkRunSaveFile(runCmd, saveCmd, cmdFile)

  # check for folder included in output...will create if it doesn't exist
  verifyFolder(dirname(surfacefile))

  # build command line
  cmd <- programName("CanopyModel", use64bit)

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

  # program-specific options
  options <- addSwitch(options, slope)
  options <- addSwitch(options, aspect)
  options <- addSwitch(options, ignoreoverlap)
  options <- addSwitch(options, ascii)
  options <- addSwitch(options, surface)
  options <- addSwitch(options, peaks)
  options <- addSwitch(options, pointcount)
  options <- addSwitch(options, nofill)
  options <- addSwitch(options, rasterorigin)

  # deal with options...
  # program-specific options
  if (smoothfirst)
    options <- addOption(options, smooth)

  options <- addOption(options, median)
  if (!smoothfirst)
    options <- addOption(options, smooth)
  options <- addOption(options, texture)
  options <- addOption(options, outlier)
  options <- addOption(options, multiplier)
  options <- addOption(options, return)
  options <- addOption(options, class)
  options <- addOption(options, ground, TRUE)
  options <- addOption(options, hole)
  options <- addOption(options, grid)
  options <- addOption(options, gridxy)
  options <- addOption(options, align, TRUE)
  options <- addOption(options, extent)

  # deal with required parameters...some may have defaults
  required <- addRequired(required, surfacefile, TRUE)
  required <- addRequired(required, cellsize)
  required <- addRequired(required, xyunits)
  required <- addRequired(required, zunits)
  required <- addRequired(required, coordsys)
  required <- addRequired(required, zone)
  required <- addRequired(required, horizdatum)
  required <- addRequired(required, vertdatum)
  required <- addRequired(required, datafile, TRUE)

  echoCommand(cmd, options, required, echoCmd)

  ret <- dispatchCommand(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile)

  if (runCmd) {
    invisible(ret)
  } else {
    invisible(buildCommand(cmd, options, required))
  }
}

# TreeSeg
# ---------- TreeSeg
#
#' FUSION R command line interface -- Function to create command lines for the TreeSeg program.
#'
#' /code{CanopyModel} creates command lines for the FUSION TreeSeg program and optionally executes them.
#'
#' @param CHM character: Name for canopy height model (PLANS DTM with .dtm extension).
#'   May be wildcard or text list file (extension .txt only). This can be a canopy surface
#'   model if the /ground option is used to specify a ground surface for normalization.
#' @param ht_threshold numeric: Minimum height for object segmentation. Portions of the CHM
#'   below this height are not considered in the segmentation.
#' @param outputfile character: Base name for output file. Metrics are stored in CSV format with
#'   .csv extension. Other outputs are stored in files named using the base name and additional
#'   descriptive information.
#' @param quiet boolean: Suppress all output during the run.
#' @param verbose boolean: Display all status information during the run.
#' @param version boolean: Report version information and exit with no processing.
#' @param newlog boolean: Erase the existing log file and start a new log
#' @param log character: Use the name specified for the log file.
#' @param locale boolean: Adjust program logic to input and output locale-specific numeric
#'   formats (e.g. use a comma for the decimal separator).
#' @param nolaszipdll boolean: Suppress the use of the LASzip dll (c) Martin Isenburg...
#'   removes support for compressed LAS (LAZ) files. This option
#'   is only useful for programs that read or write point files.
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
#' @param segmentpts boolean: Output points for the raster segments. Default is to output points
#'   for crown polygons when the /shape option is used and for raster segments when /shape is
#'   not used. Used only with the /points option.
#' @param aspect boolean: Calculate surface aspect.
#' @param clipfolder character: folder name where point files for individual clips are stored. Used
#'   only with the /points option. If not specified, point files are stored in the same folder with
#'   other outputs. The folder name must end with a trailing backslash and must already exist.
#' @param shape boolean: Create a shapefile containing the high points and basin metrics.
#' @param cleantile boolean: Output an ASCII raster map that only includes basins within the
#'   reporting extent defined by the /grid, /gridxy, and /align options.
#' @param htmultiplier numeric: Multiply the high point and surface heights by this value for
#'   output products. Also multiply individual point heights by # before writing point files
#'   (see /points option).
#' @param projection character: Associate the specified projection file with shapefile and raster
#'   data products.
#' @param use64bit boolean: indicates 64-bit version of the program
#'   should be used.
#' @param runCmd boolean: indicates command line should be executed.
#' @param saveCmd boolean: indicates command line should be written to a file.
#' @param cmdFile character: contains the name of the file to which commands
#'   should be written.
#' @param cmdClear boolean: indicates file for command should be deleted before the command
#'   line is written.
#' @param echoCmd boolean: indicates command line should be displayed.
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' TreeSeg("CHM.dtm", 2.0, "trees.csv")
#' }
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
  height = FALSE,
  ptheight = FALSE,
  maxht = NULL,
  grid = NULL,
  gridxy = NULL,
  align = NULL,
  buffer = NULL,
  ground = NULL,
  points = NULL,
  segmentpts = FALSE,
  clipfolder = NULL,
  shape = FALSE,
  cleantile = FALSE,
  htmultiplier = NULL,
  projection = FALSE,
  use64bit = TRUE,
  runCmd = TRUE,
  saveCmd = TRUE,
  cmdFile = NULL,
  cmdClear = FALSE,
  echoCmd = FALSE
) {
  # check for required options
  if (!isOpt(CHM)
      || !isOpt(ht_threshold)
      || !isOpt(outputfile)
  ) {
    stop("Missing required parameters: CHM, ht_threshold, outputfile")
  }

  # check for option to run command...if FALSE, check for command file name
  checkRunSaveFile(runCmd, saveCmd, cmdFile)

  # check for folder included in output...will create if it doesn't exist
  verifyFolder(dirname(outputfile))

  # check for folder for tree clips...will create if it doesn't exist
  verifyFolder(dirname(clipfolder))

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
  options <- addOption(options, clipfolder, TRUE)
  options <- addOption(options, htmultiplier)
  options <- addOption(options, projection)

  # deal with required parameters...some may have defaults
  required <- addRequired(required, CHM, TRUE)
  required <- addRequired(required, ht_threshold)
  required <- addRequired(required, outputfile)

  echoCommand(cmd, options, required, echoCmd)

  ret <- dispatchCommand(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile)

  if (runCmd) {
    invisible(ret)
  } else {
    invisible(buildCommand(cmd, options, required))
  }
}



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
# setwd("G:/R_Stuff/fusionr")
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
#' This is a helper function used in the fusionr package to verify
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
#' This is a helper function used in the fusionr package to test the
#' value of an object: usually a function parameter.
#'
#' @param x object
#' @return A (invisible) boolean value, TRUE indicates the object value is not NULL,
#'   FALSE indicates the object value is NULL.
isOpt <- function(x) { invisible(!is.null(x)) }

# build text for true/false switch
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
#' This is a helper function used in the fusionr package to build the text string
#' for command line options that require parameters. The resulting string is appended
#' to \code{cl}.
#'
#' @param cl character string. Potentially contains other command line elements.
#' @param opt object containing the option value. The name of the object and the
#'   value are used when construction of the option text.
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
#' This is a helper function used in the fusionr package to build the text string
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
#' This is a helper function used in the fusionr package to build the text string
#' for the program name. It uses the \code{use64bit} parameter to control use of
#' 64-bit versions of command line programs and will include the FUSION install
#' path if it has been provided using \code{setFUSIONpath()}.
#'
#' @param name character string with the base program name.
#' @param use64bit boolean value indicating 64-bit version of the program
#'   specified in \code{name} should be used.
#' @return A (invisible) string containing the program name.
#' @examples
#' \dontrun{
#' programName("ClipData", TRUE)
#' }
programName <- function(name, use64bit = FALSE) {
  # if there is a global variable for the FUSION install folder, use it here
  if (use64bit) {
    invisible(paste0(fusionrEnv$installPath, name, "64"))
  } else {
    invisible(paste0(fusionrEnv$installPath, name))
  }
}

# ---------- checkRunSaveFile
#
#' FUSION R command line interface -- Verify compatibility of function options.
#'
#' This is a helper function used in the fusionr package to verify the combination
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

# ---------- dispatchCmd
#
#' FUSION R command line interface -- Execute command lines or write them to a file.
#'
#' This is a helper function used in the fusionr package to either execute command lines or
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
#' dispatchCmd("ClipData", "/minht:2.0", "*.las clip1.las", runCmd = TRUE)
#' dispatchCmd("ClipData", "/minht:2.0", "*.las clip1.las", runCmd = FALSE, cmdClear = TRUE, cmdFile = "test.bat")
#' }
dispatchCmd <- function(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile) {
  # when runCmd=TRUE, saveCmd is never referenced. when runCmd=FALSE, the default is to write cmd to file
  # with runCmd=FALSE and saveCmd=FALSE, command is returned only (invisible)
  ret <- lapply(required, function(x) {
      if (runCmd) {
        ret <- system2(cmd, paste(options, x))
      } else {
        if (saveCmd) {
          if (cmdClear) unlink(cmdFile)
          cat(paste0(paste(cmd, options, x), "\n"), file = cmdFile, append = TRUE)
        }
        ret <- 0
      }
    }
  )
  invisible(ret)
}

# ---------- setFUSIONpath
#
#' FUSION R command line interface -- Set a package environment variable containing the FUSION install folder.
#'
#' /code{setFUSIONpath} sets an environment variable local to the \code{fusionr} package that specifys
#' the install folder for FUSION. This is necessary when the FUSION install folder has not been added
#' to the PATH environment variable.
#'
#' @param installPath character string containing the FUSION install folder. A trailing forward
#'   slash will be added if not included.
#' @return character string containing the \code{installPath} passed to the function.
#' @examples
#' \dontrun{
#' dispatchCmd("ClipData", "/minht:2.0", "*.las clip1.las", runCmd = TRUE)
#' dispatchCmd("ClipData", "/minht:2.0", "*.las clip1.las", runCmd = FALSE, cmdClear = TRUE, cmdFile = "test.bat")
#' }
#' @export
setFUSIONpath <- function(installPath) {
  fusionrEnv$installPath <- installPath

  # make sure path ends with slash
  if (!endsWith(fusionrEnv$installPath, "/"))
    fusionrEnv$installPath <- paste0(fusionrEnv$installPath, "/")

  invisible(fusionrEnv$installPath)
}

# ---------- ClipData
#
#' FUSION R command line interface -- Function to create command lines for the ClipData program.
#'
#' /code{ClipData} creates command lines for the FUSION ClipData program and optionally executes them.
#'
#' @param InputSpecifier a
#' @param SampleFile a
#' @param MinX a
#' @param MinY a
#' @param MaxX a
#' @param MaxY a
#' @param quiet a
#' @param verbose a
#' @param version a
#' @param newlog a
#' @param log a
#' @param locale a
#' @param nolaszipdll a
#' @param shape a
#' @param decimate a
#' @param ground a
#' @param zmin a
#' @param zmax a
#' @param zpercent a
#' @param height a
#' @param timemin a
#' @param timemmax a
#' @param anglemin a
#' @param anglemax a
#' @param zero a
#' @param biaselev a
#' @param return a
#' @param class a
#' @param ignoreoverlap a
#' @param line a
#' @param noindex a
#' @param index a
#' @param lda a
#' @param nooffset a
#' @param cleanlas a
#' @param precision a
#' @param use64bit boolean value indicating 64-bit version of the program
#'   specified in \code{name} should be used.
#' @param runCmd boolean indicating command line should be executed.
#' @param saveCmd boolean indicating command line should be written to a file.
#' @param cmdFile character string containing the name of the file to which commands
#'   should be written.
#' @param cmdClear boolean indicating file for command should be deleted before the command
#'   line is written.
#' @param echoCmd boolean indicating commadn line should be diasplayed.
#' @return character string containing the \code{installPath} passed to the function.
#' @examples
#' \dontrun{
#' ClipData("*.las", "clip1.las", ground = "small.dtm", height = TRUE)
#' }
#' @export
ClipData <- function(
  InputSpecifier = NULL,
  SampleFile = NULL,
  MinX = "-1000000000",
  MinY = "-1000000000",
  MaxX = "1000000000",
  MaxY = "1000000000",
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
  if (!isOpt(InputSpecifier)
      || !isOpt(SampleFile)
  ) {
    stop("Missing required parameters: InputSpecifier, SampleFile")
  }

  # check for option to run command...if FALSE, check for command file name
  checkRunSaveFile(runCmd, saveCmd, cmdFile)

  # check for folder included in SampleFile...will create if it doesn't exist
  verifyFolder(dirname(SampleFile))

  # if InputSpecifier and SampleFile are different lengths, there is some work to do...
  # if length(InputSpecifier) > 1 and length(SampleFile) == 1, create a list file and use this for a single command line
  # if length(InputSpecifier) > 1 and length(SampleFile) > 1, they must be the same length
  # length(InputSpecifier) == 1 and length(SampleFile) > 1, doesn't make sense...might if length(min/max ) > 1 but not going to handle this case
  if (length(InputSpecifier) != length(SampleFile)) {
    if (length(InputSpecifier) > 1 && length(SampleFile) == 1) {
      # create temporary list file and use this to create a single command line
      tmpFile <- tempfile("list", fileext = ".txt")
      unlink(tmpFile)
      lapply(InputSpecifier, function(x) {cat(paste0(x, "\n"), file = tmpFile, append = TRUE)})
      InputSpecifier <- tmpFile
      message("Created list file for InputSpecifier: ", tmpFile)
    } else {
      # problem
      stop("Lengths (number of items) of InputSpecifier and SampleFile are incompatible")
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
  required <- addRequired(required, InputSpecifier, TRUE)
  required <- addRequired(required, SampleFile, TRUE)
  required <- addRequired(required, MinX)
  required <- addRequired(required, MinY)
  required <- addRequired(required, MaxX)
  required <- addRequired(required, MaxY)

  if (echoCmd) message(paste(cmd, options, required, "\n"))

  dispatchCmd(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile)

  invisible(paste(cmd, options, required))
}

# CloudMetrics
# ---------- CloudMetrics
#
#' FUSION R command line interface -- Function to create command lines for the CloudMetrics program.
#'
#' /code{CloudMetrics} creates command lines for the FUSION CloudMetrics program and optionally executes them.
#'
#' @param InputSpecifier A
#' @param OutputFile A
#' @param quiet A
#' @param verbose A
#' @param version a
#' @param newlog a
#' @param log a
#' @param locale a
#' @param nolaszipdll a
#' @param above a
#' @param new a
#' @param firstinpulse a
#' @param firstreturn a
#' @param highpoint a
#' @param subset a
#' @param id a
#' @param rid a
#' @param pa a
#' @param minht a
#' @param maxht a
#' @param outlier a
#' @param ignoreoverlap a
#' @param strata a
#' @param intstrata a
#' @param kde a
#' @param rgb a
#' @param use64bit boolean value indicating 64-bit version of the program
#'   specified in \code{name} should be used.
#' @param runCmd boolean indicating command line should be executed.
#' @param saveCmd boolean indicating command line should be written to a file.
#' @param cmdFile character string containing the name of the file to which commands
#'   should be written.
#' @param cmdClear boolean indicating file for command should be deleted before the command
#'   line is written.
#' @param echoCmd boolean indicating commadn line should be diasplayed.
#' @return character string containing the \code{installPath} passed to the function.
#' @examples
#' \dontrun{
#' CloudMetrics("points/*.las", "test.csv", minht = 2.0)
#' }
#' @export
CloudMetrics <- function(
  InputSpecifier = NULL,
  OutputFile = NULL,
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
  if (!isOpt(InputSpecifier)
      || !isOpt(OutputFile)
  ) {
    stop("Missing required parameters: InputSpecifier, OutputFile")
  }

  # check for option to run command...if FALSE, check for command file name
  checkRunSaveFile(runCmd, saveCmd, cmdFile)

  # check for folder included in SampleFile...will create if it doesn't exist
  verifyFolder(dirname(OutputFile))

  # if InputSpecifier and OutputFile are different lengths, there are problems
  if (length(InputSpecifier) != length(OutputFile)) {
    stop("Lengths (number of items) of InputSpecifier and OutputFile are incompatible")
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
  required <- addRequired(required, InputSpecifier, TRUE)
  required <- addRequired(required, OutputFile, TRUE)

  if (echoCmd) message(paste(cmd, options, required, "\n"))

  dispatchCmd(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile)

  invisible(paste(cmd, options, required))
}

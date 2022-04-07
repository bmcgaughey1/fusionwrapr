# FUSION function wrappers -- fusionR
#
# found an rFUSION package https://cran.r-project.org/web/packages/rFUSION/index.html but not many functions
# and it doesn't look very robust
#
# I'm not sure I want to add all programs but I am starting with things I use most often and that I think will be
# most useful. It takes about 30 minutes to add a new function (representing a single program). There are about 45
# programs...do the math!
#
# It might be really helpful if I could write functions to read/write DTM format files
#
#
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

fusionrEnv$use64bit <- TRUE
fusionrEnv$runCmd <- TRUE
fusionrEnv$saveCmd <- TRUE
fusionrEnv$cmdFile <- NULL
fusionrEnv$echoCmd <- FALSE
fusionrEnv$installPath <- ""

fusionrEnv$areSet <- FALSE

# accessory functions

# ---------- verifyFolder
#
#' FUSION R command line interface -- Verify existence of a folder
#'
#' This is a helper function used in the fusionwrapr package to verify
#' the existence of \code{folder} and create the folder(s) if it does not exist.
#' \code{verifyFolder} is normally called with a folder extracted using
#' \code{dirname()} when an output file is provided to a function. When saving commands
#' to a file, a command will be written to create the folder in addition to creating
#' the folder. This can make it easier to delete the results from a set of processing
#' steps and then re-run them using the command file. When saving to a command file,
#' there is checking to see if the folder already exists. This prevents multiple attempts
#' to create the same folder and any associated error or warning messages. In addition,
#' when saving to a command file, the path is enclosed in quotes to prevent problems
#' when the folder names contain spaces.
#'
#' @param folder folder name
#' @param runCmd boolean: indicates command line should be executed. If TRUE, no
#'   commands will be written to the batch file regardless of the value for \code{saveCmd}.
#' @param saveCmd boolean: indicates command line the create the folder(s) should be written to a file. This command will
#'   create the folder(s) when the command file is run from a command prompt.
#' @param cmdFile character: contains the name of the file to which commands
#'   should be written.
#' @param cmdClear boolean: indicates file for commands should be deleted (cleared) before the command
#'   line is written.
#' @return An (invisible) boolean TRUE, If the folder does not exist and cannot
#'   be created, execution stops.
#' @examples
#' \dontrun{
#' verifyFolder("/test")
#' }
#' @export
verifyFolder <- function(
  folder,
  runCmd = TRUE,
  saveCmd = TRUE,
  cmdFile = NULL,
  cmdClear = FALSE
) {
  lapply(folder, function(x) {
      if (!base::dir.exists(file.path(x))) {
        if (!base::dir.create(file.path(x), recursive = TRUE)) {
          stop("could not create folder: ", x)
        } else {
          message("created folder: ", x)
        }
      }
      if (!runCmd && saveCmd) {
        addToCommandFile(paste0("Creating folder: ", file.path(x)), cmdFile = cmdFile, cmdClear = cmdClear)
        t <- file.path(x)
        addToCommandFile(paste0("IF NOT EXIST "
                                , shQuote(gsub("/", "\\", t, fixed=TRUE))
                                , " mkdir "
                                , shQuote(gsub("/", "\\", t, fixed=TRUE))
                                , "\n"
                                )
                         , cmdFile = cmdFile
                         , cmdClear = cmdClear
                         , comment = FALSE
                         , addLine = FALSE
                         )
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
#' value of an object: usually a function parameter. While this simply
#' wraps \code{is.null()}, it makes for slightly more readable code in the
#' functions that build command lines.
#'
#' @param x object
#' @return A (invisible) boolean value, TRUE indicates the object value is not NULL,
#'   FALSE indicates the object value is NULL.
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#'   been called, the path is added to the return string.
#' @return A (invisible) string containing the program name.
#' @examples
#' \dontrun{
#' programName("ClipData", TRUE)
#' }
#' @keywords internal
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
#' @keywords internal
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
#' @param comment character string containing comment to be written to command file before writing
#'   the actual command. Only used when \code{runCmd = FALSE} and \code{saveCmd = TRUE}. When written,
#'   there is always a blank line before the comment line in the command file.
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
#' @keywords internal
dispatchCommand <- function(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile, comment = NULL) {
  # when runCmd=TRUE, saveCmd is never referenced. when runCmd=FALSE, the default is to write cmd to file
  # with runCmd=FALSE and saveCmd=FALSE, command is returned only (invisible)
  ret <- lapply(required, function(x) {
      if (runCmd) {
        ret <- system2(cmd, paste(options, x))
      } else {
        if (saveCmd) {
          if (is.null(cmdFile)) stop("Missing command file name!!")

          if (cmdClear) unlink(cmdFile)

          if (!is.null(comment)) cat(paste0("\nREM ", comment, "\n"), file = cmdFile, append = TRUE)

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
#' @keywords internal
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
#' @keywords internal
buildCommand <- function(cmd, options, required) {
  invisible(paste(trimws(cmd), trimws(options), trimws(required)))
}

# ---------- setFUSIONpath
#
#' FUSION R command line interface -- Set a package environment variable containing the FUSION install folder.
#'
#' \code{setFUSIONpath} sets an environment variable local to the \code{fusionwrapr} package that specifies
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

## ---------- setGlobalCommandOptions
#
#' FUSION R command line interface -- Set global options to control the dispatch of commands.
#'
#' \code{setGlobalCommandOptions} sets environment variables local to the \code{fusionwrapr} package that control
#' how individual commands are handled. Use of \code{setGlobalCommandOptions} can simplify testing of commands and
#' the creation of batch files.
#'
#' Default behavior for dispatching commands is use the 64-bit version of programs (if available) and run the
#' commands without echoing them to the console.
#'
#' @param use64bit boolean: indicates 64-bit version of the program should be used.
#' @param runCmd boolean: indicates command line should be executed.
#' @param saveCmd boolean: indicates command line should be written to a file. If this is \code{TRUE}, you
#'   must also set \code{cmdFile} either in the global options or in individual command function calls.
#' @param cmdFile character: contains the name of the file to which commands should be written.
#' @param echoCmd boolean: indicates command line should be displayed.
#' @return boolean TRUE
#' @examples
#' setGlobalCommandOptions(runCmd = FALSE)
#' @export
setGlobalCommandOptions <- function(
  use64bit = NULL,
  runCmd = NULL,
  saveCmd = NULL,
  cmdFile = NULL,
  echoCmd = NULL
) {
  # only set variables that are passed to the function
  if (isOpt(use64bit)) fusionrEnv$use64bit <- use64bit
  if (isOpt(runCmd)) fusionrEnv$runCmd <- runCmd
  if (isOpt(saveCmd)) fusionrEnv$saveCmd <- saveCmd
  if (isOpt(cmdFile)) fusionrEnv$cmdFile <- cmdFile
  if (isOpt(echoCmd)) fusionrEnv$echoCmd <- echoCmd

  fusionrEnv$areSet <- TRUE

  invisible(TRUE)
}

## ---------- resetGlobalCommandOptions
#
#' FUSION R command line interface -- reset global options to control the dispatch of commands.
#'
#' \code{resetGlobalCommandOptions} sets environment variables local to the \code{fusionwrapr} package that control
#' how individual commands are handled to their default values.
#'
#' Default behavior for dispatching commands is use the 64-bit version of programs (if available) and run the
#' commands without echoing them to the console.
#'
#' @return boolean TRUE
#' @examples
#' resetGlobalCommandOptions()
#' @export
resetGlobalCommandOptions <- function() {
  fusionrEnv$use64bit <- TRUE
  fusionrEnv$runCmd <- TRUE
  fusionrEnv$saveCmd <- TRUE
  fusionrEnv$cmdFile <- NULL
  fusionrEnv$echoCmd <- FALSE

  fusionrEnv$areSet <- FALSE

  invisible(TRUE)
}

# ---------- addToCommandFile
#
#' FUSION R command line interface -- Write comments or other lines to command file
#'
#' \code{addToCommandFile} writes lines to a command file (batch file). The normal behavior is to write comments
#' but with \code{comment=FALSE}, you can write commands.
#'
#' @param line character: line to be written to the command file. You can start or end \code{line}
#'   with newline characters to add blank lines.
#' @param cmdFile character: contains the name of the file to which commands
#'   should be written.
#' @param cmdClear boolean: indicates file for command should be deleted before the command
#'   line is written. Use this option with caution as it will wipe out the contents of the
#'   command file.
#' @param comment boolean: \code{line} is preceded by "REM" when TRUE, otherwise not.
#' @param addLine boolean: if TRUE, a blank line is added to the command file before writing \code{line}.
#' @return invisible boolean indicating success or failure
#' @examples
#' \dontrun{
#' addToCommandFile("This is a comment!!", "Test/test.bat")
#'
#' # add a single blank line to the command file when setGlobalCommandOptions has
#' # been called to set the command file name
#' addToCommandFile(addLine = FALSE, comment = FALSE)
#' }
#' @export
addToCommandFile <- function(
  line = "",
  cmdFile = NULL,
  cmdClear = FALSE,
  comment = TRUE,
  addLine = TRUE
) {
  if (fusionrEnv$areSet) {
    if (is.null(cmdFile)) cmdFile <- fusionrEnv$cmdFile
  }

  if (isOpt(cmdFile)) {
    if (cmdClear) unlink(cmdFile)

    if (addLine) {
      cat("\n", file = cmdFile, append = TRUE)
    }

    if (comment) {
      cat(paste0("REM ", line, "\n"), file = cmdFile, append = TRUE)
    } else {
      cat(paste0(line, "\n"), file = cmdFile, append = TRUE)
    }
    invisible(TRUE)
  }

  invisible(FALSE)
}

# ---------- useLogFile
#
#' FUSION R command line interface -- Write comments or other lines to command file
#'
#' \code{useLogFile} writes code to a command file (batch file) to set the log
#' file for FUSION tools. The normal behavior is to write output to a log file in the
#' FUSION install folder (LTKmaster.log).
#'
#' Note that the \code{logFile} can contain spaces (the folder name, file name, or both).
#' However, using spaces is not recommended and may cause problems. If you include a folder
#' name, you can use the backslash or forwardslash characters as the path separator.
#'
#' To reset logging behavior to use the log file in the FUSION install folder, use \code{useLogFile("")}.
#'
#' @param logFile character: Name of the log file.
#' @param cmdFile character: contains the name of the file to which commands
#'   should be written.
#' @param logClear boolean: add a line to the command file to delete the \code{logFile} before
#'   setting the environment variable for the log file.
#' @param cmdClear boolean: indicates file for command should be deleted before the command
#'   line is written. Use this option with caution as it will wipe out the content of the
#'   command file.
#' @return invisible boolean indicating success or failure
#' @examples
#' \dontrun{
#' useLogFile("NewLog.log", "test.bat", logClear = TRUE)
#' }
#' @export
useLogFile <- function(
  logFile = NULL,
  cmdFile = NULL,
  logClear = FALSE,
  cmdClear = FALSE
) {
  # check for options
  if (!isOpt(logFile)) {
    invisible(FALSE)
  }

  # use the global variables to set command dispatch options...global options
  # are only used if the corresponding option was not passed to the function
  if (fusionrEnv$areSet) {
    if (missing(cmdFile)) cmdFile <- fusionrEnv$cmdFile
  }

  # add a comment...this will also handle clearing the command file if needed
  addToCommandFile(paste0("Redirecting log output to: ", logFile)
                   , cmdFile = cmdFile
                   , cmdClear = cmdClear
                   , comment = TRUE
  )

  if (logClear) {
    addToCommandFile(paste0("DEL ", shQuote(logFile)), cmdFile = cmdFile, comment = FALSE, addLine = FALSE)
  }

  # write command to set environment variable
  addToCommandFile(paste0("SET LTKLOG=", logFile), cmdFile = cmdFile, comment = FALSE, addLine = FALSE)

  invisible(TRUE)
}

# ---------- runCommandFile
#
#' FUSION R command line interface -- Run a command file
#'
#' \code{runCommandFile} runs a command file.
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
  if (fusionrEnv$areSet) {
    if (is.null(cmdFile)) cmdFile <- fusionrEnv$cmdFile
  }

  if (isOpt(cmdFile)) {
    system2(cmdFile, ...)

    invisible(TRUE)
  }

  invisible(FALSE)
}



# *****************************************************************************
# Function to read FUSION's LDA point format files and create a data frame or
# LAS object (from the lidR package)
# *****************************************************************************
# ---------- readLDA
#
#' FUSION R command line interface -- Read LDA format files and return data frame or LAS-class object
#'
#' \code{readLDA} reads point data stored in FUSION's LDA format and returns either a data frame or
#' a LAS-class object. To create the LAS-class object, you must provide an existing LAS or LAZ
#' file to provide a template for the LAS header. Typically, this would be a file used to clip the
#' LDA file using \code{TreeSeg} (this is the only FUSION program that can't output LAS files). The
#' template file also provides coordinate system information for the LAS-class object.
#'
#' When \code{type = "DF"} (default), the return is a data frame containing the following columns:
#' \enumerate{
#'   \item x
#'   \item y
#'   \item x
#'   \item pulse: pseudo-pulse number. This is generated using the GPS time when the LDA file is created
#'   \item return: return number
#'   \item angle: scan angle rank
#'   \item intensity: intensity
#' }
#'
#' When \code{type = "LAS"}, the LAS-class object returned by \code{readLDA} is somewhat imperfect.
#' Many of the attributes for individual points are not populated with valid values because the LDA
#' format does not have all of the information required to fully populate the point records. In
#' particular, GPS time,number of returns in the pulse, and classification are not available.
#' NumberOfReturns for all point records is set to 1 for all point records. This may cause tools
#' designed to evaluate the validity of LAS data to throw warning since there may be points labeled
#' as return 2, 3, ... yet the NumberOfReturns will still be 1.
#'
#' @param fileName character (\strong{required}): Name of the LDA file containing point data.
#' @param type character: Desired return type: "DF" for a data frame and "LAS" for LAS-class object.
#' @param epsg numeric: EPSG code defining the projection for the point data. This is assigned to the LAS-class
#'   object when \code{type = "LAS"}.
#' @param LASTemplate character: File name (including path) for the LAS/LAZ file that will provide a template
#' for the LAS header. Used only when \code{type = "LAS"}.
#' @return Return value depends on \code{type}. If \code{type = "DF"}, return value is
#'   a (invisible) data frame with the columns listed above. If \code{type = "LAS"}, return value is a
#'   (invisible) LAS-class object compatible with the \strong{lidR} package.
#' @examples
#' \dontrun{
#' pts <- readLDA("points.lda")
#' las <- readLDA("points.lda", type = "LAS", LASTemplate = "source.las")
#' }
#' @family helpers
#' @export
readLDA <- function(
  fileName = NULL,
  type = "DF",
  epsg = NULL,
  LASTemplate = NULL
) {
  # validate parameters
  if (!isOpt(fileName))
    stop("You must provide a fileName!!")

  if (type != "DF" && type != "LAS")
    stop(paste0("Invalid value for type: ", type, " Valid values are \"DF\" or \"LAS\""))

  if (type == "LAS" && is.null(LASTemplate))
    stop("You must provide a file to serve as a template for the LAS header!!")

  con = file(fileName, open = "rb")
  Signaturebytes <- readBin(con, "raw", n = 8, size = 1, endian = "little")

  Signature <- readBin(Signaturebytes, "character", size = 8, endian = "little")
  if (Signature == "LIDARBIN") {
    readBin(con, "raw", 4) # skip bytes...version major
    readBin(con, "raw", 4) # skip bytes...version minor

    # get size of file
    sz <- file.size(fileName)

    # read point records...read "rest" of file (actually specifying total length of file)
    pts <- readBin(con, "raw", n = sz, size = 1, endian = "little", signed = FALSE)

    try(close(con))

    if (length(pts) > 0) {
      ptCount <- length(pts) / 36

      mat <- matrix(pts, ncol = 36, nrow = ptCount, byrow = TRUE)

      # parse...stole this method from Jacob: https://github.com/jstrunk001/RSForTools/blob/main/R/read_las.R
      pulse <- readBin(t(mat[, 1:4]), "integer", n = ptCount, size = 4, signed = TRUE, endian = "little")
      ret <- readBin(t(mat[, 5:8]), "integer", n = ptCount, size = 4, signed = TRUE, endian = "little")
      x <- readBin(t(mat[, 9:16]), "numeric", n = ptCount, size = 8, signed = TRUE, endian = "little")
      y <- readBin(t(mat[, 17:24]), "numeric", n = ptCount, size = 8, signed = TRUE, endian = "little")
      z <- readBin(t(mat[, 25:28]), "numeric", n = ptCount, size = 4, signed = TRUE, endian = "little")
      angle <- readBin(t(mat[, 29:32]), "numeric", n = ptCount, size = 4, signed = TRUE, endian = "little")
      intensity <- readBin(t(mat[, 33:36]), "numeric", n = ptCount, size = 4, signed = TRUE, endian = "little")

      if (type == "LAS") {
        # read header
        header <- lidR::readLASheader(LASTemplate)
        data <- data.frame(   X = x
                              , Y = y
                              , Z = z
                              , ReturnNumber = ret
                              , Intensity = as.integer(intensity)
                              , ScanAngleRank = as.integer(angle)
        )

        # build the LAS object
        las <- lidR::LAS(data, header, check = FALSE)
        las <- lidR::las_quantize(las, TRUE)
        las <- lidR::las_update(las)

        # populate data for points...we don't have good values for these attributes
        las@data$gpstime = 0
        las@data$NumberOfReturns = 1L
        las@data$ScanDirectionFlag = 0L
        las@data$EdgeOfFlightline = 0L
        las@data$Classification = 0L
        las@data$Synthetic_flag = FALSE
        las@data$Keypoint_flag = FALSE
        las@data$Withheld_flag = FALSE
        las@data$UserData = 0L
        las@data$PointSourceID = 0L

        if (!is.null(epsg)) {
          lidR::epsg(las) <- epsg
        }

        invisible(las)
      } else {
        invisible(data.frame(  x = x
                            , y = y
                            , z = z
                            , pulse = pulse
                            , return = ret
                            , angle = angle
                            , intensity = intensity))
      }
    }
  } else {
    try(close(con))

    cat(fileName, " is not a valid LDA file!!")
    invisible(NULL)
  }
}

# *****************************************************************************
# Function to read FUSION's DTM format files and create a data frame or
# raster data object
# *****************************************************************************
# ---------- readDTM
#
#' FUSION R command line interface -- Read DTM format files and return a matrix, RasterLayer, or SpatRaster object
#'
#' \code{readDTM} reads surface and raster data stored in FUSION's DTM format and returns information related
#' to the DTM.
#'
#' When \code{type = "matrix"} (default), the return is a matrix containing the values. Matrix element
#' [1,1] is the value in upper left corner.
#'
#' When \code{type = "rast"}, the return is a SpatRaster object compatible with the terra package.
#'
#' @param fileName character (\strong{required}): Name of the DTM format file containing data to be read.
#' @param type character: Desired return type: "matrix" for data values in a matrix, "terra" for SpatRaster object
#'   compatible with the terra package, "raster" for a RasterLayer object compatible with the
#'   raster package, and "header" to return the DTM file header as a data frame.
#' @param epsg numeric: EPSG code defining the projection for the data. This is assigned to the SpatRaster
#'   object when \code{type = "rast"}. You can only specify one of \code{epsg} or \code{crs}, not both.
#' @param crs character: PROJ.4 type description of a Coordinate Reference System (map projection). You
#'   can only specify one of \code{epsg} or \code{crs}, not both.
#' @param negativeToNA boolean: Replace negative values with NA (TRUE) or leave negative values as is (FALSE). Setting this
#'   to FALSE to preserve negative values can lead to erroneous values for the minimum value when writing the
#'   data to a new DTM using \code{writeDTM}.
#' @return Return value depends on \code{type}. If \code{type = "matrix"}, return value is
#'   a (invisible) matrix containing the values. If \code{type = "rast"}, return value is a
#'   (invisible) SpatRaster object compatible with the \strong{terra} package. If \code{type = "header"},
#'   return type is a data frame with the header parameters.
#' @examples
#' \dontrun{
#' df <- readDTM("surface.dtm")
#' dtm <- readLDA("surface.dtm", type = "rast", epsg = 26910)
#' }
#' @family helpers
#' @export
readDTM <- function(
  fileName = NULL,
  type = "matrix",
  epsg = NULL,
  crs = NULL,
  negativeToNA = TRUE
) {
  # validate parameters
  if (!isOpt(fileName))
    stop("You must provide a fileName!!")

  types <- c("terra", "raster", "header", "matrix")

  if (!(tolower(type) %in% types))
    stop(paste0("Invalid value for type: ", type, " Valid values are ", toString(types)))

  if (!is.null(epsg) && !is.null(crs))
    stop("You can only specify one of epsg and crs, not both")

  # open file and read header
  con = file(fileName, open = "rb")
  Signaturebytes <- readBin(con, "raw", n = 21, size = 1, endian = "little")

  Signature <- readBin(Signaturebytes, "character", size = 20, endian = "little")
  if (Signature == "PLANS-PC BINARY .DTM") {
    Namebytes <- readBin(con, "raw", n = 61, size = 1, endian = "little")
    Name <- readBin(Namebytes, "character", size = 60, endian = "little")

    version <- readBin(con, "numeric", 1, 4, endian = "little")
    originX <- readBin(con, "double", 1, 8, endian = "little")
    originY <- readBin(con, "double", 1, 8, endian = "little")
    minZ <- readBin(con, "double", 1, 8, endian = "little")
    maxZ <- readBin(con, "double", 1, 8, endian = "little")
    rotation <- readBin(con, "double", 1, 8, endian = "little")
    columnSpacing <- readBin(con, "double", 1, 8, endian = "little")
    rowSpacing <- readBin(con, "double", 1, 8, endian = "little")
    columns <- readBin(con, "integer", 1, 4, endian = "little")
    rows <- readBin(con, "integer", 1, 4, endian = "little")
    horizontalUnits <- readBin(con, "integer", 1, 2, endian = "little")
    verticalUnits <- readBin(con, "integer", 1, 2, endian = "little")
    storageFormat <- readBin(con, "integer", 1, 2, endian = "little")

    if (version >= 2.0) {
      coordSystem <- readBin(con, "integer", 1, 2, endian = "little")
      coordZone <- readBin(con, "integer", 1, 2, endian = "little")
    } else {
      coordSystem <- 0
      coordZone <- 0
    }

    if (version >= 3.0) {
      horizontalDatum <- readBin(con, "integer", 1, 2, endian = "little")
      verticalDatum <- readBin(con, "integer", 1, 2, endian = "little")
    } else {
      horizontalDatum <- 0
      verticalDatum <- 0
    }

    if (version > 3.0) {
      bias <- readBin(con, "double", 1, 8, endian = "little")
    } else {
      bias <- 0.0
    }

    # if just returning the header, close the file and form the return data frame
    if (type == "header") {
      try(close(con))

      return(invisible(data.frame(Name
                           , version
                           , originX
                           , originY
                           , minZ
                           , maxZ
                           , rotation
                           , columnSpacing
                           , rowSpacing
                           , columns
                           , rows
                           , horizontalUnits
                           , verticalUnits
                           , storageFormat
                           , coordSystem
                           , coordZone
                           , horizontalDatum
                           , verticalDatum
                           , bias
                           )
                )
      )
    }

    # jump to start of data values
    seek(con, 200, "start")

    # storage format:
    #   0 = 2-byte integer
    #   1 = 4-byte integer
    #   2 = 4-byte real number
    #   3 = 8-byte real number

    # read values...try reading entire array of values
    if (storageFormat == 0) {
      values <- readBin(con, "integer", n = columns * rows, size = 2, endian = "little", signed = TRUE)
    } else if (storageFormat == 1) {
      values <- readBin(con, "integer", n = columns * rows, size = 4, endian = "little", signed = TRUE)
    } else if (storageFormat == 2) {
      values <- readBin(con, "numeric", n = columns * rows, size = 4, endian = "little", signed = TRUE)
    } else if (storageFormat == 3) {
      values <- readBin(con, "double", n = columns * rows, size = 8, endian = "little", signed = TRUE)
    }

    # close the file
    try(close(con))

    if (length(values) > 0) {
      # we really want to parse this efficiently and then rotate the array so [1,1] is upper left corner
      # and values go across rows
      # found this: https://stackoverflow.com/questions/16496210/rotate-a-matrix-in-r-by-90-degrees-clockwise
      mat <- matrix(values, ncol = rows, nrow = columns, byrow = TRUE)

      # rotate 90 degrees CCW
      mat <- apply(t(mat),2,rev)

      # replace negative values with NA
      if (negativeToNA) mat[mat < 0] <- NA

      if (type == "terra") {
        crs_string <- crs
        if (is.null(crs) && !is.null(epsg)) crs_sting <- paste0("EPSG:", epsg)

        if (is.null(crs_string)) {
          sr <- terra::rast(mat
                     , extent = terra::ext(originX, originX + columnSpacing * columns, originY, originY + rowSpacing * rows)
          )
        } else {
          sr <- terra::rast(mat
                     , crs = crs_string
                     , extent = terra::ext(originX, originX + columnSpacing * columns, originY, originY + rowSpacing * rows)
          )
        }

        return(invisible(sr))
      } else if (type == "raster") {
        crs_string <- crs
        if (is.null(crs) && !is.null(epsg)) crs_sting <- paste0("EPSG:", epsg)

        if (is.null(crs_string)) {
          sr <- raster::raster(mat
                     , xmn = originX
                     , xmx = originX + columnSpacing * columns
                     , ymn = originY
                     , ymx = originY + rowSpacing * rows
          )
        } else {
          sr <- raster::raster(mat
                     , crs = crs_string
                     , xmn = originX
                     , xmx = originX + columnSpacing * columns
                     , ymn = originY
                     , ymx = originY + rowSpacing * rows
          )
        }

        return(invisible(sr))
      } else {
        return(invisible(mat))
      }
    }
  } else {
    try(close(con))

    stop(paste0(fileName, " is not a valid DTM file!!"))
  }
  return(invisible(NULL))
}

# function to write a DTM format file
# writeDTM
# ---------- writeDTM
#
#' FUSION R command line interface -- Write DTM format files from a matrix, RasterLayer, or SpatRaster object
#'
#' \code{writeDTM} writes FUSION's DTM format files from data contained in a matrix, RasterLayer, or SpatRaster object. Any NA values
#' in the data are converted to a value of -1 indicating areas with invalid data. This means that values must all be
#' greater than 0 for use with any of the FUSION tools. While it is possible to store data with negative values, there
#' are much better formats for data than FUSION's DTM format.
#'
#' FUSION DTM format files do not carry any projection information that is useful to other tools. FUSION tools use the minimal
#' projection information stored in DTM files to prevent merging of files with different projections or mixing analyses
#' using files with different projections. The enforcement for projection-related information is fairly lax in FUSION's
#' tools and, while I would like to enhance this information, it is not likely to change.
#'
#' This function is particularly useful for converting surfaces into the DTM format. A simple test using the raster package
#' to read a surface in TIF format and write to the DTM format was over 10 times faster that using gdal_translate to
#' convert the TIF file to ASCII raster and then using FUSION's ASCII2DTM program to convert to DTM format.
#'
#' @param x (\strong{required}): data object containing the values to be written to the DTM file. This can be a simple
#'   matrix, a SpatRaster object, or a SpatialLayer object. Values are assumed to be in rows with the first row and column
#'   in the upper left corner. Use \code{rotate = FALSE} if values are in columns with the first column and row in the
#'   lower left corner (same arrangement as FUSION's DTM files).
#' @param fileName character (\strong{required}): Name for the DTM format file.
#' @param description character: Descriptive name for the DTM. Default is "DTM written by fusionwrapr R package". Length
#'   will be truncated to 60 characters.
#' @template CoordInfo
#' @param originX numeric (\strong{required when \code{x} is matrix}): X value for origin (lower left point in \code{x}).
#' @param originY numeric (\strong{required when \code{x} is matrix}): Y value for origin (lower left point in \code{x}).
#' @param columnSpacing numeric (\strong{required when \code{x} is matrix}): Spacing between columns.
#' @param rowSpacing numeric (\strong{required when \code{x} is matrix}): Spacing between rows.
#' @param rotate boolean: Flag indicating grid of values needs to be rotated before being written to \code{fileName}. See the
#'   description of \code{x} for details regarding the expected arrangement of values in the grid.
#' @param storageFormat integer: Integer value indicating the numeric type for values stored in the DTM file. The default
#'   value (-1) indicates that the actual data type is used to dictate the appropriate format. Storage options when \code{storageFormat = -1}
#'   are 2-byte signed integers for integer values and 4-byte floating point numbers for non-integer values. Possible values
#'   are: 0: 2-byte signed integers, 1: 4-byte signed integers, 2: 4-byte floating point numbers, and 3: 8-byte floating point numbers.
#'   Storing floating point values as integers will force truncation of the values.
#' @return Returns an invisible boolean value indicating success (TRUE) or failure (FALSE).
#' @examples
#' \dontrun{
#' writeDTM(rast, "test.dtm")
#' }
#' @family helpers
#' @export
writeDTM <- function(
  x,
  fileName = NULL,
  description = "DTM written by fusionwrapr R package",
  xyunits = NULL,
  zunits = NULL,
  coordsys = NULL,
  zone = NULL,
  horizdatum = NULL,
  vertdatum = NULL,
  originX = NULL,
  originY = NULL,
  columnSpacing = NULL,
  rowSpacing = NULL,
  rotate = TRUE,
  storageFormat = -1
) {
  # validate parameters
  if (missing(x))
    stop("You must provide data to write!!")

  if (!isOpt(fileName))
    stop("You must provide a fileName!!")

  # check storageFormat: valid range is -1 to 3
  if (storageFormat < -1 || storageFormat > 3) {
    stop(paste("Invalid value for storageFormat:", storageFormat, "valid values are -1, 0, 1, 2, and 3"))
  }

  # check the data type
  if (is.matrix(x)) {
    type <- "matrix"

    columns <- ncol(x)
    rows <- nrow(x)

        if (!isOpt(originX)
        || !isOpt(originY)
        || !isOpt(columnSpacing)
        || !isOpt(rowSpacing)
        || !isOpt(xyunits)
        || !isOpt(zunits)
        || !isOpt(coordsys)
        || !isOpt(zone)
        || !isOpt(horizdatum)
        || !isOpt(vertdatum)
    ) {
      stop("Missing required parameters for matrix: originX, originY, columnSpacing, rowSpacing, xyunits, zunits, coordsys, zone, horizdatum, vertdatum")
    }

    valsInt <- TRUE
    if (typeof(x) == "double")
      valsInt <- FALSE
  } else if (class(x)[[1]] == "SpatRaster") {
    type <- "terra"

    originX <- terra::xmin(x)
    originY <- terra::ymin(x)
    columnSpacing <- terra::xres(x)
    rowSpacing <- terra::yres(x)
    columns <- ncol(x)
    rows <- nrow(x)

    if (!isOpt(xyunits)
        || !isOpt(zunits)
        || !isOpt(coordsys)
        || !isOpt(zone)
        || !isOpt(horizdatum)
        || !isOpt(vertdatum)
    ) {
      stop("Missing required parameters for SpatRaster: xyunits, zunits, coordsys, zone, horizdatum, vertdatum")
    }

    valsInt <- terra::is.int(x)
  } else if (class(x)[[1]] == "RasterLayer") {
    type <- "raster"

    originX <- raster::extent(x)[1]
    originY <- raster::extent(x)[3]
    columnSpacing <- raster::xres(x)
    rowSpacing <- raster::yres(x)
    columns <- x@ncols
    rows <- x@nrows

    if (!isOpt(xyunits)
        || !isOpt(zunits)
        || !isOpt(coordsys)
        || !isOpt(zone)
        || !isOpt(horizdatum)
        || !isOpt(vertdatum)
    ) {
      stop("Missing required parameters for RasterLayer: xyunits, zunits, coordsys, zone, horizdatum, vertdatum")
    }

    valsInt <- TRUE
    if (substr(raster::dataType(x), 1, 1) == "F")
      valsInt <- FALSE

    # check for top to bottom arrangement
    # if (!x@toptobottom)
    #   stop("RasterLayer must be arranged top to bottom!!")
  }

  if (type == "matrix") {
    mat <- x
  } else if (type == "terra") {
    mat <- terra::as.matrix(x, wide = TRUE)
  } else if (type == "raster") {
    mat <- raster::as.matrix(x)
  }

  # get min value...special care needed to omit NA
  minZ <- min(mat[!is.na(mat)])

  # convert NA to -1...anything below 0 is considered NODATA in DTM format
  mat[is.na(mat)] <- -1

  if (rotate) {
    # rotate matrix
    mat <- t(apply(mat, 2, rev))
  }

  # truncate description
  description <- substr(description, 1, 60)

  # check for folder for output file
  verifyFolder(dirname(fileName), runCmd = FALSE, saveCmd = FALSE)

  # open file and write header
  con = file(fileName, open = "wb")
  writeBin("PLANS-PC BINARY .DTM", con, endian = "little")
  writeBin(description, con, endian = "little")
  seek(con, 82, "start")

  writeBin(3.0, con, size = 4, endian = "little")
  writeBin(originX, con, size = 8, endian = "little")
  writeBin(originY, con, size = 8, endian = "little")
  writeBin(minZ, con, size = 8, endian = "little")
  writeBin(max(mat), con, size = 8, endian = "little")
  writeBin(0.0, con, size = 8, endian = "little")
  writeBin(columnSpacing, con, size = 8, endian = "little")
  writeBin(rowSpacing, con, size = 8, endian = "little")
  writeBin(as.integer(columns), con, size = 4, endian = "little")
  writeBin(as.integer(rows), con, size = 4, endian = "little")
  writeBin(as.integer(xyunits), con, size = 2, endian = "little")
  writeBin(as.integer(zunits), con, size = 2, endian = "little")
  if (storageFormat == -1) {
    if (valsInt) {
      writeBin(0L, con, size = 2, endian = "little")
    } else {
      writeBin(2L, con, size = 2, endian = "little")
    }
  } else {
    writeBin(as.integer(storageFormat), con, size = 2, endian = "little")
  }
  writeBin(as.integer(coordsys), con, size = 2, endian = "little")
  writeBin(as.integer(zone), con, size = 2, endian = "little")
  writeBin(as.integer(horizdatum), con, size = 2, endian = "little")
  writeBin(as.integer(vertdatum), con, size = 2, endian = "little")

  # jump to start of data and write values
  seek(con, 200, "start")

  # write values
  if (storageFormat == -1) {
    if (valsInt) {
      writeBin(c(t(mat)), con, size = 2, endian = "little")
    } else {
      writeBin(c(t(mat)), con, size = 4, endian = "little")
    }
  } else {
    if (storageFormat == 0) {
      writeBin(as.integer(c(t(mat))), con, size = 2, endian = "little")
    } else if (storageFormat == 1) {
      writeBin(as.integer(c(t(mat))), con, size = 4, endian = "little")
    } else if (storageFormat == 2) {
      writeBin(as.numeric(c(t(mat))), con, size = 4, endian = "little")
    } else if (storageFormat == 3) {
      writeBin(as.numeric(c(t(mat))), con, size = 8, endian = "little")
    }
  }

  # close file
  try(close(con))

  # return
  return(invisible(TRUE))
}











# *****************************************************************************
# Super-functions that simplify specific tasks using FUSION command line tools.
# *****************************************************************************
# ---------- ClipPlot
#
#' FUSION R command line interface -- Super-function to clip data for plots using the ClipData program.
#'
#' \code{ClipPlot} creates command lines for the FUSION ClipData program and optionally executes them.
#' Command lines are designed to clip a circular or square area centered in the \code{(x,y)}.
#'
#' @template MultipleCommands
#'
#' @param inputspecifier character (\strong{required}): LIDAR data file template, name of a text file containing
#'   a list of file names (must have .txt extension), or a
#'   FUSION catalog file.
#' @param samplefile character (\strong{required}): Name for plot clip file (extension will be added).
#'   \code{samplefile} cannot contain spaces. To save compressed LAS files, specify the .laz extension.
#'   If the folder for the output file does not exist, it will be created
#'   when the function is called even when saving commands to a batch file.
#' @param x numeric (\strong{required}): Easting value for plot location.
#' @param y numeric (\strong{required}): Northing value for plot location.
#' @param radius numeric (\strong{required}): Radius for round (circular) plots or half width for square plots.
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

# ---------- GetSurfaceValues
#
#' FUSION R command line interface -- Super-function to run SurfaceSample to get interpolated values from surface for (X,Y) locations.
#'
#' \code{GetSurfaceValues} builds an input file with the identifier (optional), X, and Y and runs SurfaceSample
#' to interpolate a value from the specified surface. Output is a CSV file with the value.
#'
#' @template MultipleCommands
#'
#' @param df data frame containing columns for the identifier (optional), X, and Y.
#' @param xLabel character: Label for the column containing the X value.
#' @param yLabel character: Label for the column containing the Y value.
#' @param idLabel character: Label for the new column in \code{df} containing the surface value.
#' @param surfaceFile character: Name for the input surface files (PLANS DTM format). \code{surfacefile} may
#'   be a wildcard or text list file (extension .txt).
#' @return Returns a dataframe with an additional column containing the sampled surface values. If the
#'   \code{surfacefile} does not cover the location or contains invalid data, values for locations will be -1.0.
#' @examples
#' \dontrun{
#' GetSurfaceValues(df, "X", "Y", "ground.dtm")
#' }
#' @export
GetSurfaceValues <- function(
  df,
  xLabel = "X",
  yLabel = "Y",
  idLabel = "Value",
  surfaceFile = NULL
) {
  # check parameters
  err <- FALSE
  if (!is.data.frame(df)) {
    message("df is not a data frame")
    err <- TRUE
  } else {
    # check column labels
    if (!(xLabel %in% colnames(df))) {
      message(paste("No column named", xLabel, "in data frame"))
      err <- TRUE
    }
    if (!(yLabel %in% colnames(df))) {
      message(paste("No column named", yLabel, "in data frame"))
      err <- TRUE
    }
  }
  if (err) stop()

  # build new data frame for input and write to temp file
  tdf <- data.frame(X = df[, xLabel], Y = df[, yLabel])
  tFile <- tempfile()
  utils::write.csv(tdf, tFile, row.names = FALSE)

  # call SurfaceSample to get surface values
  outFile <- tempfile()
  SurfaceSample(surfaceFile, tFile, outFile, verbose = TRUE)

  # read output file and merge into original data frame
  tdf <- utils::read.csv(outFile)
  df[[idLabel]] <- tdf[, 3]

  # delete files
  #unlink(tFile)
  #unlink(outFile)

  # return original data frame with new column
  return(df)
}


# *****************************************************************************
# Interface functions for FUSION command line tools
# *****************************************************************************
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

  ret <- dispatchCommand(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile, comment)

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

# GridSurfaceCreate
# ---------- GridSurfaceCreate
#
#' FUSION R command line interface -- Function to create command lines for the GridSurfaceCreate program.
#'
#' \code{GridSurfaceCreate} creates command lines for the FUSION GridSurfaceCreate program and optionally executes them.
#'
#' @template MultipleCommands
#'
#' @param surfacefile character (\strong{required}): Name for output surface file (stored in PLANS DTM format with .dtm extension).
#'   If the folder for the output file does not exist, it will be created
#'   when the function is called even when saving commands to a batch file.
#' @param cellsize numeric (\strong{required}): Desired grid cell size in the same units as LIDAR data.
#' @template CoordInfo
#' @param datafile character (\strong{required}): Name(s) of lidar data files.
#' @template StandardOptions
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
#' @template Use64bit
#' @template RunSaveOptions
#' @template Comment
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' GridSurfaceCreate("test.dtm", 2.0, "M", "M", 1, 10, 2, 2, "Test/pts.las", class = "2")
#' }
#' @family LTKFunctions
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
  echoCmd = FALSE,
  comment = NULL
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
  verifyFolder(dirname(surfacefile), runCmd, saveCmd, cmdFile, cmdClear)

  # if we are saving commands to a file, cmdClear will have done its job in the call to verifyFolder
  cmdClear <- FALSE

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

  ret <- dispatchCommand(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile, comment)

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

# CanopyModel
# ---------- CanopyModel
#
#' FUSION R command line interface -- Function to create command lines for the CanopyModel program.
#'
#' \code{CanopyModel} creates command lines for the FUSION CanopyModel program and optionally executes them.
#'
#' @template MultipleCommands
#'
#' @param surfacefile character (\strong{required}): Name for output surface file (stored in PLANS DTM format with .dtm extension). If the
#'   folder for the output file does not exist, it will be created  when the function is called even when saving
#'   commands to a batch file.
#' @param cellsize numeric (\strong{required}): Desired grid cell size in the same units as LIDAR data.
#' @template CoordInfo
#' @param datafile character (\strong{required}): Name(s) of lidar data files.
#' @template StandardOptions
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
#' @template Use64bit
#' @template RunSaveOptions
#' @template Comment
#' @return Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
#'   the (invisible) integer value return from the operating system after running the command.
#'   if \code{runCmd = FALSE}, return value is the (invisible) command line.
#' @examples
#' \dontrun{
#' CanopyModel("test.dtm", 2.0, "M", "M", 1, 10, 2, 2, "Test/pts.las")
#' }
#' @family LTKFunctions
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
  echoCmd = FALSE,
  comment = NULL
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
  verifyFolder(dirname(surfacefile), runCmd, saveCmd, cmdFile, cmdClear)

  # if we are saving commands to a file, cmdClear will have done its job in the call to verifyFolder
  cmdClear <- FALSE

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

  ret <- dispatchCommand(cmd, options, required, runCmd, saveCmd, cmdClear, cmdFile, comment)

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
#' @template StandardOptions
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
  nolaszipdll = FALSE,
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
  options <- addSwitch(options, nolaszipdll)
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



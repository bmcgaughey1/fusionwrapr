# FUSION function wrappers -- fusionwrapr
#
# found an rFUSION package https://cran.r-project.org/web/packages/rFUSION/index.html but not many functions
# and it doesn't look very robust
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

# *****************************************************************************
# Core functions: called by other functions to manage command lines and program execution.
# *****************************************************************************
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
#' @param opt object containing the boolean switch value. The name of the object
#'   is used when constructing the option text. The option is only included when
#'   the value is TRUE.
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
      t <- (paste0(fusionrEnv$installPath, name, "64"))
      if (file.exists(t)) {
        invisible((paste0(fusionrEnv$installPath, name, "64")))
      } else {
        invisible((paste0(fusionrEnv$installPath, name)))
      }
    } else {
      invisible((paste0(name, "64")))
    }
  } else {
    invisible((paste0(fusionrEnv$installPath, name)))
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

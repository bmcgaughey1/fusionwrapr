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

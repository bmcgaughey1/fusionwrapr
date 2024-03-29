#' \strong{fusionwrapr} -- Create commands to use FUSION command line tools
#'
#' The \strong{fusionwrapr} package provides functions to construct command lines for some of the FUSION command line
#' tools. FUSION is a suite of programs that operate on LIDAR data. The FUSION/LDV software was originally
#' developed to help researchers understand, explore, and analyze LIDAR data. Since its initial release in 2006,
#' the software has become one of the most widely used tools for working with lidar data within the realm
#' of forestry.
#'
#' While FUSION includes tools to interact with and visualize LIDAR data, the most powerful part of
#' the suite is the set of command line tools for processing LIDR point cloud data. The
#' \strong{fusionwrapr} package allows you to build commands using the FUSION tools using R. This
#' greatly enhances the functionality of the FUSION tools in that you can combine point cloud
#' processing with additional analyses or data manipulation. The package can execute commands
#' directly or write commands to batch files for later execution.
#'
#' The \strong{fusionwrapr} package requires a working installation of FUSION. For installation
#' details and FUSION documentation, go \url{http://forsys.sefs.uw.edu/fusion/fusionlatest.html}.
#'
#' Because FUSION is only available for Windows and command files produced by the package utilize
#' DOS batch commands, \strong{fusionwrapr} is only useful on Windows.
#'
#' \strong{Working with command files}
#' The \strong{fusionwrapr} package can be used to create command files to automate processing tasks. When creating
#' command files, you should always start your code by adding a comment to the command file using \code{addToCommandFile} or
#' calling \code{useLogFile} with the \code{cmdClear} parameter set to TRUE and then set \code{cmdClear} to FALSE for all other
#' calls to package functions (this is the default). Most of the package functions that construct command lines for FUSION
#' programs check for the existence of folders used for output files and create the folders if they don't already exist. While
#' this behavior works as expected when running commands directly (\code{runCmd=TRUE}), it may be possible to call a function that
#' inserts commands (\code{runCmd=FALSE and saveCmd=TRUE}) to create a folder and then clear the command file and lose the commands.
#' This is especially true if you call \code{verifyFolder} directly in your code.
#' @family LTKFunctions
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

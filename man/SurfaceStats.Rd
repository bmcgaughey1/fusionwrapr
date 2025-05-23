% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SurfaceStats.R
\name{SurfaceStats}
\alias{SurfaceStats}
\title{FUSION R command line interface -- Computes surface area and volume for an entire surface.}
\usage{
SurfaceStats(
  inputfile = NULL,
  outputfile = NULL,
  quiet = FALSE,
  verbose = FALSE,
  version = FALSE,
  newlog = FALSE,
  log = NULL,
  locale = FALSE,
  ground = NULL,
  use64bit = TRUE,
  runCmd = TRUE,
  saveCmd = TRUE,
  cmdFile = NULL,
  cmdClear = FALSE,
  echoCmd = FALSE,
  comment = NULL
)
}
\arguments{
\item{inputfile}{character (\strong{required}): Name for the input DTM surface file.}

\item{outputfile}{character (\strong{required}): Name for the output CSV file containing the surface statistics.}

\item{quiet}{boolean: Suppress all output during the run.}

\item{verbose}{boolean: Display all status information during the run.}

\item{version}{boolean: Report version information and exit with no processing.}

\item{newlog}{boolean: Erase the existing log file and start a new log}

\item{log}{character: Use the name specified for the log file.}

\item{locale}{boolean: Adjust program logic to input and output locale-specific numeric
formats (e.g. use a comma for the decimal separator).}

\item{ground}{character: Use the specified surface model to represent the ground surface file may be
wildcard or text list file (extension .txt only).}

\item{use64bit}{boolean: indicates 64-bit version of the program
should be used.}

\item{runCmd}{boolean: indicates command line should be executed.}

\item{saveCmd}{boolean: indicates command line should be written to a file.}

\item{cmdFile}{character: contains the name of the file to which commands
should be written.}

\item{cmdClear}{boolean: indicates file for commands should be deleted (cleared) before the command
line is written.}

\item{echoCmd}{boolean: indicates command line should be displayed.}

\item{comment}{character string containing comment to be written to command file before writing
the actual command. Only used when \code{runCmd = FALSE} and \code{saveCmd = TRUE}. When written,
there is always a blank line before the comment line in the command file.}
}
\value{
Return value depends on \code{runCmd}. if \code{runCmd = TRUE}, return value is
  the (invisible) integer value return from the operating system after running the command.
  if \code{runCmd = FALSE}, return value is the (invisible) command line.
}
\description{
\code{SurfaceStats} creates command lines for the FUSION SurfaceStats program and optionally executes them.
}
\section{Multiple Commands}{

  LTKFunctions generate a separate command for each item when the required parameters are lists.
  This is useful when you want to use the same command and options but different input data and
  output files. However, this works only when the switches and options are the same for all
  commands (not lists). If you try to use a list of switches and/or options and a list of input
  files, you will produce commands using all sets of switches and/or options for every input
  file.
}

\examples{
\dontrun{
SurfaceStats("merged_grnd.dtm", "*.dtm")
}
}
\seealso{
Other LTKFunctions: 
\code{\link{CSV2Grid}()},
\code{\link{CanopyMaxima}()},
\code{\link{CanopyModel}()},
\code{\link{Catalog}()},
\code{\link{ClipDTM}()},
\code{\link{ClipData}()},
\code{\link{CloudMetrics}()},
\code{\link{Cover}()},
\code{\link{DTMDescribe}()},
\code{\link{DensityMetrics}()},
\code{\link{FilterData}()},
\code{\link{FirstLastReturn}()},
\code{\link{GridMetrics}()},
\code{\link{GridSample}()},
\code{\link{GridSurfaceCreate}()},
\code{\link{GridSurfaceStats}()},
\code{\link{GroundFilter}()},
\code{\link{IntensityImage}()},
\code{\link{JoinDB}()},
\code{\link{MergeDTM}()},
\code{\link{MergeData}()},
\code{\link{MergeRaster}()},
\code{\link{PolyClipData}()},
\code{\link{RepairGridDTM}()},
\code{\link{ReturnDensity}()},
\code{\link{SplitDTM}()},
\code{\link{SurfaceSample}()},
\code{\link{TINSurfaceCreate}()},
\code{\link{ThinData}()},
\code{\link{TopoMetrics}()},
\code{\link{TreeSeg}()},
\code{\link{fusionwrapr-package}}
}
\concept{LTKFunctions}

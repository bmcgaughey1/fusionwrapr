% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/JoinDB.R
\name{JoinDB}
\alias{JoinDB}
\title{FUSION R command line interface -- Combines columns from two data files into a single data file.}
\usage{
JoinDB(
  basefile = NULL,
  basefield = NULL,
  addfile = NULL,
  addfield = NULL,
  startfield = NULL,
  outputfile = NULL,
  quiet = FALSE,
  verbose = FALSE,
  version = FALSE,
  newlog = FALSE,
  log = NULL,
  locale = FALSE,
  noheader = FALSE,
  runCmd = TRUE,
  saveCmd = TRUE,
  cmdFile = NULL,
  cmdClear = FALSE,
  echoCmd = FALSE,
  comment = NULL
)
}
\arguments{
\item{basefile}{character (\strong{required}): Primary data file in CSV format.}

\item{basefield}{numeric: Field in \code{basefile} that will be matched to records in \code{addfile}.}

\item{addfile}{character: Secondary data file in CSV format.}

\item{addfield}{numeric: Field in \code{addfile} that will be matched to records in \code{basefile}.}

\item{startfield}{numeric: Starting field in \code{addfile}. All fields (columns) starting with
the \code{startfield} will be added to records in the \code{outputfile}.}

\item{outputfile}{character: Name for the new data file. The extension specifies the desired format (.csv).
\code{outputfile} can be the same as \code{basefile}. \code{outputfile} cannot be the same as \code{addfile}.}

\item{quiet}{boolean: Suppress all output during the run.}

\item{verbose}{boolean: Display all status information during the run.}

\item{version}{boolean: Report version information and exit with no processing.}

\item{newlog}{boolean: Erase the existing log file and start a new log}

\item{log}{character: Use the name specified for the log file.}

\item{locale}{boolean: Adjust program logic to input and output locale-specific numeric
formats (e.g. use a comma for the decimal separator).}

\item{noheader}{boolean: Treat the first line of the \code{basefile} and \code{addfile} as data. Default behavior assumes
the first line of each file contains column names. Valid for CSV format files only.}

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
\code{JoinDB} creates command lines for the FUSION JoinDB program and optionally executes them.
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
JoinDB("attribs.csv", 4, "base.csv", 1, 2, "new.csv")
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
\code{\link{MergeDTM}()},
\code{\link{MergeData}()},
\code{\link{MergeRaster}()},
\code{\link{PolyClipData}()},
\code{\link{RepairGridDTM}()},
\code{\link{ReturnDensity}()},
\code{\link{SplitDTM}()},
\code{\link{SurfaceSample}()},
\code{\link{SurfaceStats}()},
\code{\link{TINSurfaceCreate}()},
\code{\link{ThinData}()},
\code{\link{TopoMetrics}()},
\code{\link{TreeSeg}()},
\code{\link{fusionwrapr-package}}
}
\concept{LTKFunctions}

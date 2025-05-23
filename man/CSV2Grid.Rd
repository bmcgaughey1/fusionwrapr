% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CSV2Grid.R
\name{CSV2Grid}
\alias{CSV2Grid}
\title{FUSION R command line interface -- Converts data stored in comma separated value (CSV) format into ASCII raster format.}
\usage{
CSV2Grid(
  inputfile = NULL,
  column = NULL,
  outputfile = NULL,
  quiet = FALSE,
  verbose = FALSE,
  version = FALSE,
  newlog = FALSE,
  log = NULL,
  locale = FALSE,
  multiplier = NULL,
  ndzero = NULL,
  runCmd = TRUE,
  saveCmd = TRUE,
  cmdFile = NULL,
  cmdClear = FALSE,
  echoCmd = FALSE,
  comment = NULL
)
}
\arguments{
\item{inputfile}{character (\strong{required}): Name for the input CSV file (normally output by GridMetrics).}

\item{column}{numeric: Column number for values to populate grid (1 is first column).}

\item{outputfile}{character: Name for the output ASCII grid file.}

\item{quiet}{boolean: Suppress all output during the run.}

\item{verbose}{boolean: Display all status information during the run.}

\item{version}{boolean: Report version information and exit with no processing.}

\item{newlog}{boolean: Erase the existing log file and start a new log}

\item{log}{character: Use the name specified for the log file.}

\item{locale}{boolean: Adjust program logic to input and output locale-specific numeric
formats (e.g. use a comma for the decimal separator).}

\item{multiplier}{numeric: Multiply all data values by the constant value.}

\item{ndzero}{numeric: If the value in the target column is NODATA, look at the value in \code{column}
and if it is a valid value (not NODATA), change the value for the target column to 0 for output to the ASCII grid
file. This is useful when the ASCII grid files are being used for further analysis in GIS or statistical packages.}

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
\code{CSV2Grid} creates command lines for the FUSION CSV2Grid program and optionally executes them.
}
\details{
CSV2Grid looks for a header file with the name formed by appending "_ascii_header" to \code{inputfile}
 and adding an extension of "txt".
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
CSV2Grid("metrics.csv", 4, "ave_elev.asc")
}
}
\seealso{
Other LTKFunctions: 
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
\code{\link{SurfaceStats}()},
\code{\link{TINSurfaceCreate}()},
\code{\link{ThinData}()},
\code{\link{TopoMetrics}()},
\code{\link{TreeSeg}()},
\code{\link{fusionwrapr-package}}
}
\concept{LTKFunctions}

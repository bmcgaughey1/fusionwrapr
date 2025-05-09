% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IntensityImage.R
\name{IntensityImage}
\alias{IntensityImage}
\title{FUSION R command line interface -- Creates images using the intensity values from a point cloud.}
\usage{
IntensityImage(
  cellsize = NULL,
  imagefile = NULL,
  datafile = NULL,
  quiet = FALSE,
  verbose = FALSE,
  version = FALSE,
  newlog = FALSE,
  log = NULL,
  locale = FALSE,
  nolaszipdll = FALSE,
  skipfilecheck = FALSE,
  minint = NULL,
  maxint = NULL,
  intrange = NULL,
  intcell = NULL,
  void = NULL,
  allreturns = FALSE,
  lowest = FALSE,
  lowall = FALSE,
  saveint = FALSE,
  rasterorigin = FALSE,
  diskonly = FALSE,
  hist = FALSE,
  jpg = FALSE,
  projection = NULL,
  class = NULL,
  ignoreoverlap = FALSE,
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
\item{cellsize}{numeric (\strong{required}): Pixel size for the intensity image (same units as LIDAR data).}

\item{imagefile}{character (\strong{required}): Name for the image file.}

\item{datafile}{character (\strong{required}): Name(s) of lidar data files.}

\item{quiet}{boolean: Suppress all output during the run.}

\item{verbose}{boolean: Display all status information during the run.}

\item{version}{boolean: Report version information and exit with no processing.}

\item{newlog}{boolean: Erase the existing log file and start a new log}

\item{log}{character: Use the name specified for the log file.}

\item{locale}{boolean: Adjust program logic to input and output locale-specific numeric
formats (e.g. use a comma for the decimal separator).}

\item{nolaszipdll}{boolean: Suppress the use of the LASzip dll (c) Martin Isenburg...
removes support for compressed LAS (LAZ) files. This option
is only useful for programs that read or write point files.}

\item{skipfilecheck}{boolean: Skip logic that checks for valid point files and removes those that are invalid
prior to the start of point processing. This option is designed to overcome some limitations with windows
and wildcard processing. This option is only useful for programs that read point data.}

\item{minint}{numeric: Minimum intensity percentile used for the image (default: 2.0).}

\item{maxint}{numeric: Maximum intensity percentile used for the image (default: 98.0).}

\item{intrange}{character: "min,max": Force the scaling of intensity values using the specified
\code{min} and \code{max} values. Setting the min value to -1 should force the output range to start with
1 instead of 0. Doing this in combination with \code{void="0,0,0"} will allow you to identify areas
with no data in the output image as they will have a value of 0 for all color bands.}

\item{intcell}{numeric: Cell size multiplier for intermediate images.}

\item{void}{character: "R,G,B": Color for areas with no data (default is red (255,0,0)).}

\item{allreturns}{boolean: Use all returns to create the intensity image.}

\item{lowest}{boolean: Use lowest return in pixel to assign intensity value...should be
used with /allreturns for best effect.}

\item{lowall}{boolean: Combines \code{lowest=TRUE} and \code{allreturns=TRUE} switches...will have no effect if
used with either \code{lowest=TRUE} or \code{allreturns=TRUE}.}

\item{saveint}{boolean: Save the intermediate image files.}

\item{rasterorigin}{boolean: Force alignment to match other raster products generated from point
data (offsets the origin of the image by 1/2 pixel.}

\item{diskonly}{boolean: Do not attempt to read all returns into memory for processing.}

\item{hist}{boolean: Produce the intensity histogram data files.}

\item{jpg}{boolean: Save the intensity image using the JPEG format (default is BMP).}

\item{projection}{character: "filename": Associate the specified projection \code{filename} with image
products.}

\item{class}{character: "#,#,#,...": LAS files only: Specifies that only points with classification
values listed are to be included in the subsample.
Classification values should be separated by a comma.
e.g. (2,3,4,5) and can range from 0 to 31.
If the first character in string is ~, the list is interpreted
as the classes you DO NOT want included in the subsample.
e.g. /class:~2,3 would include all class values EXCEPT 2 and 3.}

\item{ignoreoverlap}{boolean: Ignore points with the overlap flag set (LAS V1.4+ format).}

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
\code{IntensityImage} creates command lines for the FUSION IntensityImage program and optionally executes them.
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
IntensityImage(2.0, "IntImage.jpg", "*.las", jpg = TRUE)
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

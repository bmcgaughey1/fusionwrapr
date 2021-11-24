
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fusionwrapr

<!-- badges: start -->

<!-- badges: end -->

The fusionwrapr package provides functions to construct command lines
for some of the FUSION command line tools. FUSION is a package of
programs that operate on LIDAR data. Use of the fusionwrapr package
requires a working installation of FUSION. For installation details and
FUSION documentation, go
[here](http://forsys.sefs.uw.edu/fusion/fusionlatest.html).

In addition, fusionwrapr provides some handy functions to simply tasks
common when working with LIDAR data in a forestry context.

## Installation

~~You can install the released version of fusionwrapr from
[CRAN](https://CRAN.R-project.org) with:~~

fusionwrapr is distributed as a development version and is not available
(yet) on CRAN.

``` r
#install.packages("fusionwrapr")
```

Install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bmcgaughey1/fusionwrapr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(fusionwrapr)

## basic example code
# set FUSION install path...not needed if the FUSION install folder has been added to PATH
#setFUSIONpath("C:/FUSION")

# create ground model using class 2 points
GridSurfaceCreate("Test/test.dtm", 1.0, "M", "M", 1, 10, 2, 2, "Test/pts.laz", class = 2)

# normalize point data...takes advantage of defaults for min/max to process entire file
ClipData("Test/pts.laz", "Test/pts_normalized.laz", height= TRUE, ground = "Test/test.dtm")

# compute metrics using points above 2m
CloudMetrics("Test/pts_normalized.laz", "Test/metrics.csv", new = TRUE, minht = 2.0, above = 2.0)

# compute gridded metrics
GridMetrics("Test/test.dtm", 2, 5, "Test/gridded_metrics.csv", "Test/pts.laz", minht = 2)

# clip data for a plot
ClipPlot("Test/pts.laz", "Test/Plot_001.laz", 523650, 4667650, 15, 1, height= TRUE, ground = "Test/test.dtm")
```

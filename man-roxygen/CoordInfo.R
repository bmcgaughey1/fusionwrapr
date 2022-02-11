#' @param xyunits character (\strong{required}): Units for LIDAR data XY (M for meters or F for feet).
#' @param zunits character (\strong{required}): Units for LIDAR data elevations (M for meters or F for feet).
#' @param coordsys numeric (\strong{required}): Coordinate system for LIDAR data:
#'   0 for unknown
#'   1 for UTM
#'   2 for state plane)
#' @param zone numeric (\strong{required}): Coordinate system zone for LIDAR data (0 for unknown).
#' @param horizdatum numeric (\strong{required}): Horizontal datum:
#'   0 for unknown
#'   1 for NAD27
#'   2 for NAD83
#' @param vertdatum numeric (\strong{required}): Vertical datum:
#'   0 for unknown
#'   1 for NGVD29
#'   2 for NAVD88
#'   3 for GRS80

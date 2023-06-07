#'
#' functions to get receiver and tagging metadata into session
#'
#' @name signalSeries
#' @import data.table
#' @import magrittr
#' @import dplyr
#' @import tidyr
#' @param data
#' @param positions
#' @param units
#' @param units.position
#' @param from
#' @export

if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

signalSeries<-function (data, positions., units, units.position, from = 1,
          by = 1)
{
  if (missing(positions.) && missing(data) && missing(units) &&
      missing(units.position) && missing(from) && missing(by))
    return(new("signalSeries"))
  if (!missing(positions.) && (length(positions.) != numRows(data)))
    stop("Positions and data lengths do not agree")
  ret <- new("signalSeries")
  ret@data <- asSeriesData(data)
  if (missing(positions.)) {
    len <- numRows(ret@data)
    ret@positions <- numericSequence(from = from, length. = len,
                                     by = by)
  }
  else {
    if (!is(positions., "positionsNumeric"))
      positions. <- as(positions., "numeric")
    ret@positions <- positions.
  }
  if (!missing(units))
    ret@units <- as(units, "character")
  if (!missing(units.position))
    ret@units.position <- as(units.position, "character")
  ret
}

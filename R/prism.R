##' Segment prism data
##' 
##' See the package summary (prisms) for some details.
##'
##' @format An object of class \code{data.table} (inherits from
##'   \code{data.frame}) with 6427 rows and 13 columns.
##' \itemize{
##'
##'     \item YEAR: year of census
##'     \item CONTNAM: Contour name
##'     \item STPACE: starting pace of segment
##'     \item ELEVCL: Elevation class
##'     \item ASPCL: aspect class
##'     \item SGLEN: segment length
##'     \item SPEC: species
##'     \item TYPE: Type of sample - ALIVE, DEAD, SNAG
##'     \item CTP[1-3]: prism sample points: start, middle, end of segments.
##'     \item CTP: Weighted average of prism data - 2*middle + 1*(end + start)
##'     \item SGDSP: segment displacement from contour (paces up or down)
##' }
"prism"

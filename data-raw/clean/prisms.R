##' This package contains the data for prism estimates of basal area for the contour segments.
##' 
##' For most years (1989, 1998-99) prism estimates were collected at three locations on
##' sampled segments: start, middle, end.  In 1988, only the start and end locations were
##' sampled.  Also, in 1988-1989 only data on PIRU were collected, whereas in 1998-1999 
##' prism data was collected for all species.
##' 
##' Separate prism values were collected for LIVE and DEAD trees, and for SNAG trees as well
##' in some years (all except 1988).
##' 
##' The prism collection points are labeled \code{CTP[1-3]}, 2 being the middle of the segment.  
##' The `CTP` column is the weighted average of the three, 2*middle + start + end.
##' 
##' Notes:
##' \itemize{
##'   \item 1988: Only PIRU was counted (LIVE and DEAD) at the beginning and end of each segment.
##'   \item 1989: Only PIRU - start, middle, end prism samples
##'   \item 1998-1999: All species - start, middle, end prism samples
##' }
##' @name prisms
##' @import data.table
NULL

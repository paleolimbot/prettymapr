#' Create a Bounding Box
#'
#' Convencience method to create a bounding box like that returned by \code{sp::bbox()}.
#' To generate a bounding box from lists of lat/lon values use \code{sp::bbox(cbind(lons, lats))}.
#'
#' @param n North bounding latitude
#' @param e East bounding longitude
#' @param s South bounding latitude
#' @param w West bounding longitude
#' @return A 2x2 matrix describing a bounding box like that returned by \code{sp::bbox()}
#' @seealso sp::bbox
#' @examples makebbox(45.125, -64.25, 44.875, -64.75)
#'
#' @export
makebbox <- function(n, e, s, w) {
  matrix(c(w, s, e, n), byrow=FALSE, ncol=2, dimnames=list(c("x", "y"), c("min", "max")))
}

#' Query The Interwebs For A Bounding Box
#'
#' Use the Data Science Toolkit (\url{http://www.datasciencetoolkit.org/about}) to
#' retreive a bounding box for the given query. Implemented
#' from the \code{ggmap:geocode} function from the \code{ggmap}
#' package (\url{https://cran.r-project.org/package=ggmap})
#' by David Kahle to remove dependencies
#' of \code{ggmap} that are not necessary for \code{prettymapr}.
#'
#' @param querystring The search query
#' @param source One of \code{dsk} or {google}
#' @return A 2x2 matrix describing a bounding box like that returned by \code{sp::bbox()}
#'
#' @examples
#' \donttest{
#' searchbbox("kings county, NS")
#' searchbbox("University Ave. Wolfville NS")
#' searchbbox("Wolfville ns")
#' }
#'
#' @export
#'
searchbbox <- function(querystring, source="dsk") {
  out <- geocode(querystring, output="all", source=source)
  if(out$status != "OK") stop("geocode returned with status ", out$status)
  if(length(out$results) == 0) stop("No results found for query ", querystring)
  if(length(out$results) > 1) warning("More than one result found, loading first result: ",
                              out$results[[1]]$formatted_address)

  makebbox(out$results[[1]]$geometry$viewport$northeast$lat,
            out$results[[1]]$geometry$viewport$northeast$lng,
            out$results[[1]]$geometry$viewport$southwest$lat,
            out$results[[1]]$geometry$viewport$southwest$lng)
}

#' Zoom the extents of a bounding box
#'
#' Manipulate the extents of a bounding box by zooming and moving an
#' existing bbox. This is helpful when manipulating the extents of a
#' plot created by \code{canvec.qplot()}
#'
#' @param bbox An existing bbox
#' @param factor A factor to zoom by. >1 will zoom in, <1 will zoom out.
#' If a vector is passed, the first element will zoom the X extent, the
#' second element will zoom the Y extent.
#' @param offset A vector describing the X and Y offset that should be applied.
#' @return A zoomed bounding box.
#'
#' @examples
#' \donttest{
#' alta <- searchbbox("alta lake bc")
#' zoombbox(alta, c(.2,.5))
#' }
#' @export
#'
zoombbox <- function(bbox, factor, offset=c(0,0)) {
  lons <- bbox[1,]
  lats <- bbox[2,]
  clon <- mean(lons) + offset[1]
  clat <- mean(lats) + offset[2]

  if(length(factor)>1) {
    factorx <- factor[1]
    factory <- factor[2]
  } else {
    factorx <- factor
    factory <- factor
  }

  newwidth <- (lons[2]-lons[1]) / factorx
  newheight <- (lats[2]-lats[1]) / factory

  makebbox(clat+newheight/2.0, clon+newwidth/2.0, clat-newheight/2.0, clon-newwidth/2.0)
}

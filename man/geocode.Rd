% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geocode.R
\name{geocode}
\alias{geocode}
\title{Geocode Locations}
\usage{
geocode(
  location,
  output = c("data.frame", "list"),
  source = "default",
  messaging = NULL,
  limit = 1,
  key = NULL,
  quiet = TRUE,
  cache = NA,
  progress = c("time", "text", "none"),
  ...
)
}
\arguments{
\item{location}{A character vector (or an object that can be coerced to one)
of locations to pass to the geocoding API.}

\item{output}{One of \code{data.frame} or \code{list}. If \code{data.frame},
the results are distilled into columns: query, source, status, rank,
lon, lat, address, bbox_n, bbox_e, bbox_s, and bbox_w. Other columns may also exist
for certain API types. The data frame will have the same number of rows as the
length of the input vector, and will always have the columns query, source, status,
lon and lat. If \code{output='list'},
the raw JSON output from the geocoding API is returned as a \code{list}
(containing lists). The list output of a failed geocode return varies by API type,
but the length of the output list is guaranteed to be the same as the input vector.}

\item{source}{One of "default", "google", "pickpoint", or "dsk". If "default", the
function calls \code{getOption("prettymapr.geosource")} or chooses
"pickpoint" if none is set. If using "pickpoint", please
sign up for your own (free) API key to avoid using the default excessively.}

\item{messaging}{\code{TRUE} if verbose messaging is desired (now deprecated,
use 'quiet = FALSE' instead.}

\item{limit}{The number of results to return per query. This refers to
individual locations, for which ambiguous queries may return multiple
results (e.g. Halifax, Nova Scotia; Halifax, United Kingdom, etc.). The
default is 1. Pass 0 if no limit on queries is desired.}

\item{key}{API key if \code{source="pickpoint"}.}

\item{quiet}{By default, error messages are suppressed, and are instead included in the
output as objects with a $status describing the error (list output) or the appropriate value in the
'status' column (data frame output).}

\item{cache}{The cache to use. Use NA for the internal cache (keeps first 1000 results),
or a directory name (e.g. 'geo.cache'), which keeps an unlimited number of results. Use
\link{clear_geocode_cache} to clear the cache.}

\item{progress}{A plyr status bar, one of "time", "text", or "none". Passing quiet = FALSE
will also disable the progress bar.}

\item{...}{A number of key/value pairs to append to the URL, specifying
further options specific to each API. Google users may wish to provide
\code{sensor}, \code{client} and \code{signature} arguments for use with the enterprise
version with the API, or to specify additional constraints on geocoding.}
}
\value{
A \code{list} or \code{data.frame}; see documentation for
  \code{output} argument.
}
\description{
Geocode locations using the Google Web API, the PickPoint.io API, or the
Data Science Toolkit API. For large
requests you should really use your own API key if you are using the default (pickpoint).
Note that the Google Terms seem to indicate that you cannot place locations obtained
from their API on non-google maps. Locations are all geocoded with erorrs kept quiet,
which may result in list output containing items with a $status element describing
the error message, or data frame
output containing a non-OK status in the status column.
}
\examples{
\dontshow{if (identical(Sys.getenv("R_PRETTYMAPR_HAS_API_KEY"), "true")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# don't test to speed up checking time
\donttest{
geocode("wolfville, ns")
geocode("wolfville, ns", output="list")
geocode("halifax", limit=0)
geocode("Paddy's Pub Wolfville NS", source="google")
geocode(c("Houston, TX", "San Antonio TX", "Cleavland OH"), source="google")

#fails quietly
geocode("don't even think about geocoding this")
geocode("don't even think about geocoding this", output="list")
}
\dontshow{\}) # examplesIf}
}

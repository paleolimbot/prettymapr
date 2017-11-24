#' Geocode Locations
#'
#' Geocode locations using the
#' \href{https://developers.google.com/maps/documentation/geocoding/intro}{Google
#' Web API}, the \href{https://pickpoint.io/}{PickPoint.io API}, or the
#' \href{http://www.datasciencetoolkit.org/}{Data Science Toolkit API}. For large
#' requests you should really use your own API key if you are using the default (pickpoint).
#' Note that the Google Terms seem to indicate that you cannot place locations obtained
#' from their API on non-google maps. Locations are all geocoded with erorrs kept quiet,
#' which may result in list output containing items with a $status element describing
#' the error message, or data frame
#' output containing a non-OK status in the status column.
#'
#' @param location A character vector (or an object that can be coerced to one)
#'   of locations to pass to the geocoding API.
#' @param output One of \code{data.frame} or \code{list}. If \code{data.frame},
#'   the results are distilled into columns: query, source, status, rank,
#'   lon, lat, address, bbox_n, bbox_e, bbox_s, and bbox_w. Other columns may also exist
#'   for certain API types. The data frame will have the same number of rows as the
#'   length of the input vector, and will always have the columns query, source, status,
#'   lon and lat. If \code{output='list'},
#'   the raw JSON output from the geocoding API is returned as a \code{list}
#'   (containing lists). The list output of a failed geocode return varies by API type,
#'   but the length of the output list is guaranteed to be the same as the input vector.
#' @param source One of "default", "google", "pickpoint", or "dsk". If "default", the
#'   function calls \code{getOption("prettymapr.geosource")} or chooses
#'   "pickpoint" if none is set. If using "pickpoint", please
#'   \href{https://app.pickpoint.io/sign-up}{sign up for your own (free) API
#'   key} to avoid using the default excessively.
#' @param messaging \code{TRUE} if verbose messaging is desired (now deprecated,
#'   use 'quiet = FALSE' instead.
#' @param limit The number of results to return per query. This refers to
#'   individual locations, for which ambiguous queries may return multiple
#'   results (e.g. Halifax, Nova Scotia; Halifax, United Kingdom, etc.). The
#'   default is 1. Pass 0 if no limit on queries is desired.
#' @param key API key if \code{source="pickpoint"}.
#' @param cache The cache to use. Use NA for the internal cache (keeps first 1000 results),
#'   or a directory name (e.g. 'geo.cache'), which keeps an unlimited number of results. Use
#'   \link{clear_geocode_cache} to clear the cache.
#' @param quiet By default, error messages are suppressed, and are instead included in the
#'   output as objects with a $status describing the error (list output) or the appropriate value in the
#'   'status' column (data frame output).
#' @param progress A plyr status bar, one of "time", "text", or "none". Passing quiet = FALSE
#'   will also disable the progress bar.
#' @param ... A number of key/value pairs to append to the URL, specifying
#'   further options specific to each API. Google users may wish to provide
#'   \code{sensor}, \code{client} and \code{signature} arguments for use with the enterprise
#'   version with the API, or to specify additional constraints on geocoding.
#'
#' @return A \code{list} or \code{data.frame}; see documentation for
#'   \code{output} argument.
#'
#' @export
#'
#' @examples
#' # don't test to speed up checking time
#' \donttest{
#' geocode("wolfville, ns")
#' geocode("wolfville, ns", output="list")
#' geocode("halifax", limit=0)
#' geocode("Paddy's Pub Wolfville NS", source="google")
#' geocode(c("Houston, TX", "San Antonio TX", "Cleavland OH"), source="google")
#'
#' #fails quietly
#' geocode("don't even think about geocoding this")
#' geocode("don't even think about geocoding this", output="list")
#' }
#'
geocode <- function(location, output=c("data.frame", "list"), source = "default",
                    messaging = NULL, limit=1, key=NULL, quiet = TRUE, cache = NA,
                    progress = c("time", "text", "none"), ...) {
  # output ban be "list" or "data.frame"
  output <- match.arg(output)
  progress <- match.arg(progress)

  # disable progress bar for length 1 or quiet = FALSE
  if((length(location) == 1) || !quiet) {
    progress <- "none"
  }

  if(!is.character(location)) {
    message("Coercing argument 'location' from '", class(location)[1], "' to 'character'")
    location <- as.character(location)
  }

  # deal with special location cases
  if(length(location) == 0) {
    # zero-length input means zero-length output
    return(result_zero_length(output))
  } else if(length(location) == 1) {
    if(is.na(location) || (nchar(location) <= 3)) {
      if(output == "data.frame") {
        return(cbind(query = location, source = NA_character_, result_empty("data.frame")))
      } else {
        return(list(NULL))
      }
    }
  }

  # check quiet param
  if(!is.logical(quiet)) stop("'quiet' must be TRUE or FALSE")

  # warn about deprecated messaging parameter
  if(!is.null(messaging)) {
    message("Parameter 'messaging' is deprecated. Use 'quiet = FALSE' instead")
    if(!is.logical(messaging)) stop("'messaging' must be TRUE or FALSE")
    quiet = !messaging
  }

  # sanitize limit parameter
  if(!is.numeric(limit)) stop("'limit' must be numeric")
  limit <- max(limit, 0)

  # find/sanitize data source
  if(source == "default") {
    dsource <- get_default_geocoder()
    if(!is.null(dsource)) {
      source <- dsource
    } else {
      if(!quiet) message("No default source set, using pickpoint.io")
      source <- "pickpoint"
    }
  }

  if(!(source %in% c("google", "dsk", "pickpoint", "error_source"))) {
    stop("Unrecognized geocode source: ", source)
  }

  # assess the 'scope of the problem' here
  # assign default api key here, so the default message only pops up once
  if((source == "pickpoint") && is.null(key)) {

    # keep big queries out without an api key
    if(length(unique(location)) > 10) stop("For bulk geocoding, please get your own PickPoint.io ",
                                           "API key at https://app.pickpoint.io/sign-up",
                                           "(or use source = 'google')")

    key <- "yxsN6E8EYYHFF_xsa_uL"
    message("Using default API key for pickpoint.io. If batch geocoding, please get your ",
            "own (free) API key at https://app.pickpoint.io/sign-up")

  }

  # match geocoder function
  geocoder <- list(google = geocode_google,
                   dsk = geocode_dsk,
                   pickpoint = geocode_pickpoint,
                   error_source = geocode_error)[[source]]

  geocoder_partial <- function(loc) {
    # keep out trivial cases from geocoder functions (causes errors TODO)
    if(is.na(loc) || (nchar(loc) <= 3)) return(result_empty(output))
    geocoder(loc, output = output, limit = limit, key = key, quiet = quiet, cache = cache, ...)
  }

  # generate output
  if(output == "data.frame") {
    # setup default data frame, which sets the template and column order for
    # results
    df <- data.frame(query = location, source = source, rank = NA_integer_,
                     lon = NA_real_, lat = NA_real_, address = NA_character_,
                     bbox_n = NA_real_, bbox_e = NA_real_, bbox_s = NA_real_,
                     bbox_w = NA_real_, stringsAsFactors = FALSE)

    df <- plyr::adply(df, 1, function(row) geocoder_partial(row$query),
                      .progress = progress)

    # ensure column output order
    cnames <- c("query", "source", "status", "rank", "lon", "lat", "address",
                "bbox_n", "bbox_e", "bbox_s", "bbox_w")
    df[c(cnames[cnames %in% names(df)], names(df)[!(names(df) %in% cnames)])]
  } else {
    # list is just the result of geocoder
    plyr::llply(location, geocoder_partial, .progress = progress)
  }
}

# these geocoder functions require sanitized input of length 1

geocode_dsk <- function(...) {
  geocode_google(..., endpoint = "https://www.datasciencetoolkit.org/maps/api/geocode/json",
                 .encoding = "UTF-8")
}

geocode_google <- function(location, output, sensor = FALSE,
                           quiet = TRUE, cache = NA, limit = 1,
                           endpoint = "https://maps.googleapis.com/maps/api/geocode/json", ...) {

  # query server
  data <- try(restquery(endpoint, sensor = sensor, address = location, ..., .quiet = quiet,
                        .parser = rjson::fromJSON, .cache = cache), silent = TRUE)

  # check for try-error
  if(class(data) == "try-error") {
    return(result_error(attr(data, "condition")$message, output))
  }

  # simplify output
  if(output == "list") {
    data
  } else {
    # check for error (zero length results don't work well with the code after)
    if(data$status != "OK") {
      result_error(data$status, "data.frame")
    } else if(length(data$results) == 0) {
      result_error("no results", "data.frame")
    } else {
      # could be multiple results for one location if limit != 1
      if(limit == 0) {
        limit <- length(data$results)
      }
      df <- data.frame(status = data$status, rank = 1:min(limit, length(data$results)),
                       n_results = length(data$results), stringsAsFactors = FALSE)

      plyr::adply(df, 1, function(row) {
        result <- data$results[[row$rank]]

        # address can be a few things for dsk or google
        address <- result$formatted_address
        if(is.null(address)) {
          address <- result$address_components[[1]]$long_name
        }

        # get elements
        fields <- list(address=address,
                       lon=as.numeric(result$geometry$location$lng),
                       lat=as.numeric(result$geometry$location$lat),
                       bbox_n=as.numeric(result$geometry$viewport$northeast$lat),
                       bbox_e=as.numeric(result$geometry$viewport$northeast$lng),
                       bbox_s=as.numeric(result$geometry$viewport$southwest$lat),
                       bbox_w=as.numeric(result$geometry$viewport$southwest$lng),
                       id=result$place_id)

        # remove NULLs and numeric(0)s
        fields <- fields[!vapply(fields, function(x) length(x) == 0, logical(1))]

        # return data frame
        data.frame(fields, stringsAsFactors = FALSE)
      })
    }
  }
}

geocode_pickpoint <- function(location, output, quiet = TRUE,
                              cache = NA, limit = limit, key = key,
                              endpoint = "https://api.pickpoint.io/v1/forward", ...) {

  # pickpoint requires a slightly different parser, since an invalid api key results
  # simply in the text 'Unauthorized'
  force(key)
  parse_pickpoint <- function(text) {
    if(text == "Unauthorized") stop("Invalid API key for pickpoint: ", key)
    data <- rjson::fromJSON(text)
    if(identical(data$message, "Unauthorized")) stop("Invalid API key for pickpoint: ", key)
    data
  }

  # query server
  data <- try(restquery(endpoint, q = location, key = key, ..., .quiet = quiet,
                        .parser = parse_pickpoint, .cache = cache), silent = TRUE)

  # check for try-error
  if(class(data) == "try-error") {
    return(result_error(attr(data, "condition")$message, output))
  }

  # simplify output
  if(output == "list") {
    data
  } else {
    # check for error (zero length results don't work well with the code after)
    if(length(data) == 0) {
      result_error("ZERO_RESULTS", "data.frame")
    } else {
      # could be multiple results for one location if limit != 1
      if(limit == 0) {
        limit <- length(data)
      }
      df <- data.frame(status = "OK", rank = 1:min(limit, length(data)),
                       n_results = length(data), stringsAsFactors = FALSE)

      plyr::adply(df, 1, function(row) {
        result <- data[[row$rank]]
        # get elements
        boundingbox <- result$boundingbox
        if(length(boundingbox) != 4) {
          warning("Unexpected bounding box type for location ", location)
          boundingbox <- rep(NA_real_, 4)
        }

        fields <- list(address=result$display_name,
                       lon=as.numeric(result$lon),
                       lat=as.numeric(result$lat),
                       bbox_n=as.numeric(boundingbox[2]),
                       bbox_e=as.numeric(boundingbox[4]),
                       bbox_s=as.numeric(boundingbox[1]),
                       bbox_w=as.numeric(boundingbox[3]),
                       id=result$place_id)

        # remove NULLs
        fields <- fields[!vapply(fields, function(x) length(x) == 0, logical(1))]

        # return data frame
        data.frame(fields, stringsAsFactors = FALSE)
      })
    }
  }
}

# this could probably be moved to the public in the future, but for now
# it's just using the old style option setting

#' Get/Set the default geocoder
#'
#' The \link{geocode} function can use google, pickpoint, or data science toolkit
#' to turn human-readable names into coordinates. Use these methods to get/set
#' the default source. These will need to be called once per namespace load.
#'
#' @param geocoder The new source to use. One of "pickpoint", "google", or "dsk".
#'
#' @export
#'
#' @examples
#' get_default_geocoder()
#' set_default_geocoder("google")
#' (set_default_geocoder(NULL))
#'
get_default_geocoder <- function() {
  # try old default in case it was set
  old_default <- getOption("prettymapr.geosource")
  if(is.null(old_default)) {
    prettymapr_geocoding$default_geocoder
  } else {
    message("Using options() to set default geocoder is deprecated. Use ",
            "set_default_geocoder() instead.")
    old_default
  }
}

#' @rdname get_default_geocoder
#' @export
set_default_geocoder <- function(geocoder) {
  # use NULL to reset the default
  if(is.null(geocoder)) {
    geocoder <- "pickpoint"
    options(prettymapr.geosource = NULL)
  }

  if(!is.character(geocoder) || (length(geocoder) != 1)) {
    stop("'geocoder' must be a character vector of length 1")
  }

  if(!(geocoder %in% c("google", "dsk", "pickpoint", "error_source"))) {
    stop("Unrecognized geocode source: ", geocoder)
  }

  old_geocoder <- suppressMessages(get_default_geocoder())
  prettymapr_geocoding$default_geocoder <- geocoder
  invisible(old_geocoder)
}

prettymapr_geocoding <- new.env(parent = emptyenv())
prettymapr_geocoding$default_geocoder <- "pickpoint"


# this is a test function that passes an invalid URL to geocode_google
geocode_error <- function(...) {
  geocode_google(..., endpoint = "http://an.invalid.url.definitely/")
}

result_error <- function(error, output) {
  if(output == "list") {
    list(status = error)
  } else {
    data.frame(status = error, rank = NA_integer_,
               lon = NA_real_, lat = NA_real_,  address = NA_character_,
               bbox_n = NA_real_, bbox_e = NA_real_, bbox_s = NA_real_,
               bbox_w = NA_real_, stringsAsFactors = FALSE)
  }
}

result_zero_length <- function(output) {
  if(output == "data.frame") {
    data.frame()
  } else {
    list()
  }
}

result_empty <- function(output) {
  if(output == "data.frame") {
    data.frame(status = "empty input",
               rank = NA_integer_, lon = NA_real_, lat = NA_real_, address = NA_character_,
               bbox_n = NA_real_, bbox_e = NA_real_, bbox_s = NA_real_,
               bbox_w = NA_real_,
               stringsAsFactors = FALSE)
  } else {
    list(NULL)
  }
}

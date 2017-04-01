#' Geocode Locations
#'
#' Geocode locations using the
#' \href{https://developers.google.com/maps/documentation/geocoding/intro}{Google
#' Web API} or the \href{https://pickpoint.io/}{PickPoint.io API}. Implemented
#' from the \code{ggmap:geocode} function from the \code{ggmap} package
#' (\url{https://cran.r-project.org/package=ggmap}) by David Kahle
#'
#' @param location A character vector of locations to pass to the geocoding API.
#' @param output One of \code{data.frame} or \code{list}. If \code{data.frame},
#'   the results are distilled into columns: query, source, status, rank,
#'   address, lon, lat, bbox_n, bbox_e, bbox_s, bbox_w, and id. If \code{list},
#'   the raw JSON output from the geocoding API is returned as a \code{list}
#'   (containing lists). The output of a failed geocode return will always have
#'   a \code{$status} attribute describing the failure.
#' @param source One of "default", "google" or "pickpoint". If "default", the
#'   function calls \code{options("prettymapr.geosource")} or chooses
#'   "pickpoint" if none is set. If using "pickpoint", please
#'   \href{https://pickpoint.io/users/sign_up}{sign up for your own (free) API
#'   key} to avoid using the default excessively.
#' @param messaging \code{TRUE} if verbose messaging is desired (now deprecated,
#'   use 'quiet = FALSE' instead.
#' @param limit The number of results to return per query. This refers to
#'   individual locations, for which ambiguous queries may return multiple
#'   results (e.g. Halifax, Nova Scotia; Halifax, United Kingdom, etc.). The
#'   default is 1. Pass 0 if no limit on queries is desired.
#' @param key API key if \code{source="pickpoint"}.
#' @param sensor \code{TRUE} if the location is generated from a sensor.
#' @param ... A number of key/value pairs to append to the URL, specifying
#'   further options specific to each API. Google users may wish to provide
#'   \code{client} and \code{signature} arguments for use with the enterprise
#'   version with the API, or specify additional constraints on geocoding.
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
                    messaging = NULL, limit=1, key=NULL, quiet = TRUE, ...) {
  # output ban be "list" or "data.frame"
  output <- match.arg(output)

  if(!is.character(location)) {
    message("Coercing argument 'location' from '", class(location)[1], "' to 'character'")
    location <- as.character(location)
  }

  # keep out trivial location cases
  if(length(location) == 0) {
    # zero-length input means zero-length output
    return(result_zero_length(output))
  } else if(length(location) == 1) {
    if(is.na(location) || (nchar(location) <= 3)) return(result_empty(location, output))
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
                                           "API key at https://pickpoint.io/users/sign_up ",
                                           "(or use source = 'google')")

    key <- "yxsN6E8EYYHFF_xsa_uL"
    message("Using default API key for pickpoint.io. If batch geocoding, please get your ",
            "own (free) API key at https://pickpoint.io/users/sign_up")

  }

  # match geocoder function
  geocoder <- list(google = geocode_google,
                   dsk = geocode_dsk,
                   pickpoint = geocode_pickpoint,
                   error_source = geocode_error)[[source]]

  geocoder_partial <- function(loc) {
    # keep out trivial cases from geocoder functions
    if(is.na(location) || (nchar(location) <= 3)) return(result_empty(location, output))
    geocoder(loc, output = output, limit = limit, key = key, quiet = quiet, ...)
  }

  # generate output
  if(output == "data.frame") {
    # setup default data frame, which sets the template and column order for
    # results
    df <- data.frame(query = location, source = source, rank = NA_integer_,
                     lon = NA_real_, lat = NA_real_,
                     stringsAsFactors = FALSE)

    df <- plyr::adply(df, 1, function(row) geocoder_partial(row$query))
    # ensure column output order
    cnames <- c("query", "source", "status", "rank", "lon", "lat", "address",
                "bbox_n", "bbox_e", "bbox_s", "bbox_w")
    df[c(cnames[cnames %in% names(df)], names(df)[!(names(df) %in% cnames)])]
  } else {
    # list is just the result of geocoder
    plyr::llply(location, geocoder_partial)
  }
}

# these geocoder functions require sanitized input of length 1

geocode_dsk <- function(...) {
  geocode_google(..., endpoint = "http://www.datasciencetoolkit.org/maps/api/geocode/json",
                 .encoding = "UTF-8")
}

geocode_google <- function(location, output, sensor = FALSE,
                           quiet = TRUE, cache = NA, limit = 1,
                           endpoint = "http://maps.googleapis.com/maps/api/geocode/json", ...) {

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
    rjson::fromJSON(text)
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
get_default_geocoder <- function() {
  getOption("prettymapr.geosource")
}

# this is a test function that passes an invalid URL to geocode_google
geocode_error <- function(...) {
  geocode_google(..., endpoint = "http://an.invalid.url.definitely/")
}

result_error <- function(error, output) {
  if(output == "list") {
    list(status = error)
  } else {
    data.frame(status = error, rank = NA_integer_,
               lon = NA_real_, lat = NA_real_, stringsAsFactors = FALSE)
  }
}

result_zero_length <- function(output) {
  if(output == "data.frame") {
    data.frame()
  } else {
    list()
  }
}

result_empty <- function(location, output) {
  if(output == "data.frame") {
    data.frame(query = location, status = "empty input", source = NA_character_,
               rank = NA_integer_, lon = NA_real_, lat = NA_real_,
               stringsAsFactors = FALSE)
  } else {
    list(NULL)
  }
}

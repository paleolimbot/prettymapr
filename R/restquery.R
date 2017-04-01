
#* Query an endpoint and cache the data
#*
#* @param ... Key/value pairs to send to the query (will be passed through URLEncode)
#* @param .endpoint The URL endpoint to send the query
#* @param .cache An environment, directory name, NULL (disable cache), or NA for internal cache
#* @param .parser A function that the (character) results will be passed through
#* @param .quiet Use .quiet=TRUE to supress messaging
#*
#* @return The result of .parser on the text resulting from the call
#* @export
#*
#* @examples
#* # that's some catch, that catch-22:
#* restquery("https://www.goodreads.com/book/title",
#*           key="HSkIMuOGlxFIOmfBCGFVA", title='catch-22')
#
restquery <- function(.endpoint, ..., .cache=NA, .parser=identity, .quiet=FALSE,
                      .encoding=NULL) {
  # verify search params
  searchparams <- list(...)
  if(is.null(names(searchparams))) stop("restquery takes only named arguments")
  if(any(nchar(names(searchparams)) == 0)) stop("restquery takes only named arguments")

  # get cache
  .cache <- as.cache(.cache)

  # characterify input values, messaging user for NULLs and NAs
  searchparams <- lapply(searchparams, function(x) {
    if(is.null(x)) {
      message('Removing NULL query parameter')
      NULL
    } else if(is.na(x)) {
      message('Coercing an NA query parameter to ""')
      ""
    } else {
      as.character(x)
    }
  })

  # remove NULLs
  searchparams <- searchparams[!vapply(searchparams, is.null, logical(1))]

  # set query params
  purl <- httr::parse_url(.endpoint)
  searchparams <- c(purl$query, searchparams)
  # name sorting ensure consistent hashing
  purl$query <- searchparams[sort(names(searchparams))]

  # build URL
  url_string <- httr::build_url(purl)

  # check cache
  text <- get_cached(.cache, url_string)

  # if cached result exists
  if(!is.null(text)) {
    if(!.quiet) message("Using cached information for ", url_string)
    # return parsed text
    return(.parser(text))
  }

  # if there is no cached result, query the URL
  if(!.quiet) message("Retrieving information from ", url_string)
  connect <- try(httr::GET(url_string), silent=TRUE)

  # check for fail
  if(class(connect) != "try-error") {

    # try to get content
    if(!.quiet) httr::warn_for_status(connect)
    text <- httr::content(connect, as="text", encoding=.encoding)

    # store response information, if cache is not full
    if(!cache_full(.cache)) {
      set_cached(.cache, url_string, text)
    }

    # return parsed
    .parser(text)
  } else {
    stop("Unable to connect to ", url_string, ": ", as.character(connect))
  }
}

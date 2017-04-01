
#' Clear cached results
#'
#' Clears the local cache of downloaded files (by default, an
#' environment in the package namespace). Clearing a directory
#' cache will result in all files with the extention ".cached"
#' being deleted from that directory.
#'
#' @param cache An environment, a directory name, or NA to clear
#'  the default internal cache
#'
#' @export
#'
#' @examples
#' clear_geocode_cache()
#'
clear_geocode_cache <- function(cache = NA) {
  cache <- as.cache(cache)
  clear_cache(cache)
}

file_cache <- function(directory, max_size = Inf, ...) {
  if(!is.character(directory) || (length(directory) != 1)) stop("directory must be a directory name")
  structure(list(directory = directory, max_size = max_size, ...),
            class = c("file_cache", "cache"))
}

# max is 10,000, theory being that if the user bothers to create their own
# environment, they are probably pretty serious about caching...
environment_cache <- function(env, max_size = 10000, ...) {
  if(!is.environment(env)) stop("env must be an environment")
  structure(list(env = env, max_size = max_size, ...),
            class = c("environment_cache", "cache"))
}

null_cache <- function() {
  structure(list(max_size = 0), class = c("null_cache", "cache"))
}

is.cache <- function(x) {
  inherits(x, "cache")
}

as.cache <- function(x, ...) {
  if(is.cache(x)) {
    x
  } else if(is.environment(x)) {
    environment_cache(x, ...)
  } else if(is.character(x) && (length(x) == 1)) {
    file_cache(x, ...)
  } else if(is.null(x)) {
    null_cache()
  } else if(identical(x, NA)) {
    internal_cache
  } else {
    stop("Don't know how to create cache from ", x)
  }
}

cache_full <- function(cache) {
  cache_size(cache) >= cache$max_size
}

# create internal environment to cache responses
internal_cache_environment <- new.env(parent = emptyenv())

# create interal cache (max size 1000 by default)
internal_cache <- environment_cache(internal_cache_environment, max_size = 1000)

# get/set cached info methods
set_cached <- function(cache, url, data, ...) UseMethod("set_cached")
get_cached <- function(cache, url, ...) UseMethod("get_cached")
clear_cache <- function(cache) UseMethod("clear_cache")
cache_size <- function(cache) UseMethod("cache_size")

# set for directory caches
set_cached.file_cache <- function(cache, url, data, ext = ".cached", ...) {
  url_hash <- digest::digest(url)
  if(!dir.exists(cache$directory) && !is.null(data)) {
    dir.create(cache$directory)
  }
  fname <- file.path(cache$directory, paste0(url_hash, ext))
  if(is.null(data) && file.exists(fname)) {
    unlink(fname)
  } else if(!is.null(data)) {
    write(data, fname)
  }
  if(cache_full(cache)) warning("Cache full: ", cache$directory,
                                " (n=", cache_size(cache), ")")
}

# get for directory caches
get_cached.file_cache <- function(cache, url, ext = ".cached", ...) {
  url_hash <- digest::digest(url)
  fname <- file.path(cache$directory, paste0(url_hash, ext))
  if(file.exists(fname)) {
    paste(readLines(fname), collapse="\n")
  } else {
    NULL
  }
}

# clear cache for files
clear_cache.file_cache <- function(cache, ext = ".cached") {
  # removing all *.cached files (safer than remvoing the dir)
  unlink(list.files(cache$directory, pattern = paste0("*", ext),
                    recursive = FALSE, full.names = TRUE))
}

# cache size, directory caches
cache_size.file_cache <- function(cache) {
  length(list.files(cache$directory))
}


# get/set for environment caches
get_cached.environment_cache <- function(cache, url, ...) {
  url_hash <- digest::digest(url)
  if(exists(url_hash, where = cache$env)) {
    cache$env[[url_hash]]
  } else {
    NULL
  }
}

# environment caches are slightly simpler
set_cached.environment_cache <- function(cache, url, data, ...) {
  cache$env[[digest::digest(url)]] <- data
  if(cache_full(cache)) warning("Cache full: (n=", cache_size(cache),
                                "). Use file_cache for larger cache sizes")
}

cache_size.environment_cache <- function(cache) {
  length(cache$env)
}

clear_cache.environment_cache <- function(cache) {
  items <- as.list(names(cache$env))
  do.call(rm, c(items, list(envir = cache$env)))
}

# the null cache does nothing and is empty
set_cached.null_cache <- function(cache, url, data, ...) {
  warning("Attempt to set_cache for the null_cache")
  invisible(NULL)
}

get_cached.null_cache <- function(cache, url, ...) {
  NULL
}

cache_size.null_cache <- function(cache) {
  0
}

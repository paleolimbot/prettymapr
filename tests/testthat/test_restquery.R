
context("restquery")

test_that("rest query caches results", {

  # test on goodreads / file cache
  expect_message(restquery("https://www.goodreads.com/book/title",
                      key="HSkIMuOGlxFIOmfBCGFVA", title='catch-22',
                      .cache = "prettymapr.cache"),
                 "Retrieving information from")

  expect_message(restquery("https://www.goodreads.com/book/title",
                           key="HSkIMuOGlxFIOmfBCGFVA", title='catch-22',
                           .cache = "prettymapr.cache"),
                 "Using cached information for")

  # test on goodreads / default cache
  expect_message(restquery("https://www.goodreads.com/book/title",
                           key="HSkIMuOGlxFIOmfBCGFVA", title='catch-22',
                           .cache = NA),
                 "Retrieving information from")

  expect_message(restquery("https://www.goodreads.com/book/title",
                           key="HSkIMuOGlxFIOmfBCGFVA", title='catch-22',
                           .cache = NA),
                 "Using cached information for")

  # test on goodreads / null cache
  expect_message(restquery("https://www.goodreads.com/book/title",
                           key="HSkIMuOGlxFIOmfBCGFVA", title='catch-22',
                           .cache = NULL),
                 "Retrieving information from")

  expect_message(restquery("https://www.goodreads.com/book/title",
                           key="HSkIMuOGlxFIOmfBCGFVA", title='catch-22',
                           .cache = NULL),
                 "Retrieving information from")

  clear_geocode_cache(NA)
  unlink("prettymapr.cache", recursive = TRUE)
})

test_that("rest query uses the correct cache", {
  # start with clean cache
  clear_geocode_cache("prettymapr.cache")

  restquery("https://www.goodreads.com/book/title",
            key="HSkIMuOGlxFIOmfBCGFVA", title='catch-22',
            .cache = "prettymapr.cache")

  expect_true(dir.exists("prettymapr.cache"))
  expect_equal(cache_size(as.cache("prettymapr.cache")), 1)

  unlink("prettymapr.cache", recursive = TRUE)
})

test_that("zero arguments will still fetch a result", {
  res <- restquery("http://www.google.com/")
  expect_false(is.null(res))
})

test_that("null parameters are dropped from url", {
  expect_message(restquery("http://www.google.com/", q = NULL),
                 "http://www.google.com/\\s$")
})

test_that("NA parameters are converted to ''", {
  expect_message(restquery("http://www.google.com/", q = NA),
                 "Coercing an NA query parameter to")
})

test_that("invalid addresses result in errors", {
  expect_error(restquery("http://not.an.address.ever"),
               "Unable to connect to http://not.an.address.ever/")
})

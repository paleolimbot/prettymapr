
context("geocode")

test_that("geocode output is consistent", {

  for(source in c("pickpoint", "dsk", "google")) {

    dfgood <- geocode("wolfville, ns", output = "data.frame", source = source)
    dfbad <- geocode("don't you dare geocode this", output = "data.frame", source = source)
    expect_is(dfgood, "data.frame")
    expect_is(dfbad, "data.frame")

    # ensure required columns are present and first and ordered identically
    required_columns <- c("query", "source", "status", "rank", "lon", "lat", "address",
                          "bbox_n", "bbox_e", "bbox_s", "bbox_w")
    expect_equal(names(dfgood)[1:length(required_columns)], required_columns)
    expect_equal(names(dfbad), required_columns[1:ncol(dfbad)])

    # expect types are correct
    required_types <- c("character", "character", "character", "integer", "numeric",
                        "numeric", "character", "numeric", "numeric", "numeric", "numeric")
    expect_equivalent(vapply(dfgood[1:length(required_columns)], class, character(1)),
                      required_types)
    expect_equivalent(vapply(dfbad, class, character(1)),
                      required_types[1:ncol(dfbad)])

    expect_equal(nrow(dfgood), 1)
    expect_equal(nrow(dfbad), 1)

    listgood <- geocode("wolfville, ns", output = "list", source = source)
    listbad <- geocode("don't you dare geocode this", output = "list", source = source)

    expect_is(listgood, "list")
    expect_is(listbad, "list")
    expect_length(listgood, 1)
    expect_length(listbad, 1)

  }

})

test_that("emptys and nulls produce the correct output", {

  df0 <- geocode(character(0), output = "data.frame")
  expect_is(df0, "data.frame")
  expect_equal(nrow(df0), 0)

  list0 <- geocode(character(0), output = "list")
  expect_is(list0, "list")
  expect_length(list0, 0)

  dfNA <- geocode(NA_character_, output = "data.frame")
  expect_is(dfNA, "data.frame")
  expect_equal(nrow(dfNA), 1)

  listNA <- geocode(NA_character_, output = "list")
  expect_is(listNA, "list")
  expect_length(listNA, 1)

  expect_identical(dfNA[-1], geocode("", output = "data.frame")[-1])
  expect_identical(listNA, geocode("", output = "list"))

  # check that the correct columns are returned for empty results
  expect_equal(c("query", "status", "source", "rank", "lon", "lat", "address",
                 "bbox_n", "bbox_e", "bbox_s", "bbox_w"), names(dfNA))
})

test_that("output is vectorized with correct lengths", {

  for(source in c("pickpoint", "google", "dsk")) {
    cities <- c("wolfville, ns", "halifax, ns", "calgary, ab", "auckland, nz", "middlebury, vt",
                "ottawa, on")

    df <- geocode(cities, output = "data.frame", source = "pickpoint")
    expect_equal(length(cities), nrow(df))

    alist <- geocode(cities, output = "list", source = "pickpoint")
    expect_equal(length(cities), length(alist))
    }

})

test_that("errors in the geocode function don't stop execution", {
  expect_silent(geocode("something", source = "error_source"))
  expect_silent(geocode(rep("something", 10), source = "error_source"))
})

test_that("invalid pickpoint API key results in correct error message", {
  df <- geocode("wolfville ns", source = "pickpoint", key = "notavalidkey")
  expect_equal(df$status, "Invalid API key for pickpoint: notavalidkey")
})

test_that("invalid parameters are detected", {
  expect_error(geocode("something", quiet = NULL),
               "'quiet' must be TRUE or FALSE")
  expect_error(geocode("something", source = "not a source"),
               "Unrecognized geocode source: not a source")
  expect_error(geocode("something", messaging = "not a logical"),
               "'messaging' must be TRUE or FALSE")
  expect_message(geocode("something", messaging = TRUE),
                 "Parameter 'messaging' is deprecated. Use 'quiet = FALSE' instead")
  expect_error(geocode("something", limit = "not a number"),
               "'limit' must be numeric")
  expect_error(geocode("something", output = "a fish"),
               "'arg' should be one of")
})

test_that("mesages are printed when defaults are guessed", {
  expect_message(geocode("something"),
                 "Using default API key for pickpoint.io.")
  expect_message(geocode(factor("something")),
                 "Coercing argument 'location' from 'factor' to 'character'")
  expect_message(geocode("something", quiet = FALSE),
                 "No default source set, using pickpoint.io")
  expect_silent(geocode("something", key = "yxsN6E8EYYHFF_xsa_uL", source = "pickpoint"))
})

test_that("default source can be set from options", {
  old_opts <- options(prettymapr.geosource = "google")
  df <- geocode("wolfville ns")
  expect_equal(df$source, "google")
  options(prettymapr.geosource = old_opts$prettymapr.geosource)
})

test_that("non 200 status codes throw warning when quiet = FALSE", {
  expect_warning(geocode("something", key = "not a key", quiet = FALSE))
})

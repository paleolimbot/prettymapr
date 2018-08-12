
context("bbox")

test_that("bbox works as expected", {
 bbox <- makebbox(45.125, -64.25, 44.875, -64.75)
 expect_equal(bbox,
              matrix(c(-64.75, 44.875, -64.25, 45.125), byrow=FALSE, ncol=2,
                     dimnames=list(c("x", "y"), c("min", "max"))))
})

test_that("bbox warnings", {
  expect_warning(makebbox(44.875, -64.25, 45.125, -64.75),
                 "North less than south. Check order?")

  expect_warning(makebbox(45.125, -64.75, 44.875, -64.25),
                 "East less than west. Check order?")
})

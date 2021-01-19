library(testthat)
library(questionr)
context("describe() method")

test_that("describe() works with a labelled vector of NAs", {
  skip_if_not_installed("labelled")
  x <- rep(NA_real_, 10)
  labelled::val_labels(x) <- c(no = 0, yes = 1)
  expect_error(questionr::describe(x), NA) # no error
})

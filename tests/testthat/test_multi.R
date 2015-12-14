library(testthat)
library(questionr)
context("Multiple choices questions")

test_that("Substrings are not matched", {
  v <- c("Apple/Pear", "Crab Apple/Peach")  
  expect_equal(multi.split(v)[["v.Apple"]], c(1,0))
})

test_that("Regex char in split.char doesn't affect result", {
  v <- c("red (fruit)|blue (cheese)", "green $", "red (fruit)|green $", 
         "blue(cheese)|red (fruit)", "orange|apple")
  expect_equal(sum(multi.split(v, split.char = "|")),9)
})
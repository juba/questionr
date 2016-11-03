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

df <- data.frame(sex = c("Man", NA, "Man", "Woman", "Woman", "Woman"),
                 blue = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
                 red = c(0,0,0,1,1,1),
                 weights = c(2, 1, 1, 2, 1, 0.5))

test_that("Non weighted multi.table is ok", {
  res <- multi.table(df[,c("blue","red")])
  expect_equal(res, matrix(c(4, 66.7, 3, 50), nrow = 2, byrow = TRUE, 
                           dimnames = list(c("blue", "red"), c("n", "%multi"))))  
})

test_that("Weighted multi.table is ok", {
  res <- multi.table(df[,c("blue","red")], weights = df$weights, digits = 3)
  expect_equal(res, matrix(c(6, 80, 3.5, 46.667), nrow = 2, byrow = TRUE, 
                           dimnames = list(c("blue", "red"), c("n", "%multi"))))  
})

test_that("Absolute frequency cross multi.table is ok", {
  res <- cross.multi.table(df[,c("blue","red")], df$sex, na.rm = FALSE)
  expect_equal(res, matrix(c(2, 1, 1, 0, 3, 0), nrow = 2, byrow = TRUE,
                           dimnames = list(c("blue", "red"), c("Man", "Woman", NA))))
})

test_that("Weighted absolute frequency cross multi.table is ok", {
  res <- cross.multi.table(df[,c("blue","red")], df$sex, weights = df$weights, na.rm = FALSE)
  expect_equal(res, matrix(c(3, 2, 1, 0, 3.5, 0), nrow = 2, byrow = TRUE,
                           dimnames = list(c("blue", "red"), c("Man", "Woman", NA))))
})

test_that("Col percentages cross multi.table is ok", {
  res <- cross.multi.table(df[,c("blue","red")], df$sex, freq = TRUE, n = TRUE, digits = 3, na.rm = FALSE)
  expect_equal(res, matrix(c(100, 33.333, 100, 0, 100, 0, 2, 3, 1), nrow = 3, byrow = TRUE,
                           dimnames = list(c("blue", "red", "n"), c("Man", "Woman", NA))))
})

test_that("Row percentages cross multi.table is ok", {
  res <- cross.multi.table(df[,c("blue","red")], df$sex, freq = TRUE, tfreq = "row", n = TRUE, digits = 3, na.rm = FALSE)
  expect_equal(res, matrix(c(50, 25, 25, 4, 0, 100, 0, 3), nrow = 2, byrow = TRUE,
                           dimnames = list(c("blue", "red"), c("Man", "Woman", NA, "n"))))
})

test_that("Weighted col percentages cross multi.table is ok", {
  res <- cross.multi.table(df[,c("blue","red")], df$sex, weights = df$weights, freq = TRUE, n = TRUE, digits = 3, na.rm = FALSE)
  expect_equal(res, matrix(c(100, 57.143, 100, 0, 100, 0, 3, 3.5, 1), nrow = 3, byrow = TRUE,
                           dimnames = list(c("blue", "red", "n"), c("Man", "Woman", NA))))
})

test_that("Weighted row percentages cross multi.table is ok", {
  res <- cross.multi.table(df[,c("blue","red")], df$sex,  weights = df$weights, freq = TRUE, tfreq = "row", n = TRUE, na.rm = FALSE)
  expect_equal(res, matrix(c(50, 33.3, 16.7, 6, 0, 100, 0, 3.5), nrow = 2, byrow = TRUE,
                           dimnames = list(c("blue", "red"), c("Man", "Woman", NA, "n"))))
})

test_that("Col percentages cross multi.table without NA is ok", {
  res <- cross.multi.table(df[,c("blue","red")], df$sex, freq = TRUE, n = TRUE, digits = 3, na.rm = TRUE)
  expect_equal(res, matrix(c(100, 33.333, 0, 100, 2, 3), nrow = 3, byrow = TRUE,
                           dimnames = list(c("blue", "red", "n"), c("Man", "Woman"))))
})

test_that("Row percentages cross multi.table without NA is ok", {
  res <- cross.multi.table(df[,c("blue","red")], df$sex, freq = TRUE, tfreq = "row", n = TRUE, na.rm = TRUE)
  expect_equal(res, matrix(c(66.7, 33.3, 3, 0, 100, 3), nrow = 2, byrow = TRUE,
                           dimnames = list(c("blue", "red"), c("Man", "Woman", "n"))))
})

test_that("Weighted col percentages cross multi.table without NA is ok", {
  res <- cross.multi.table(df[,c("blue","red")], df$sex, freq = TRUE, n = TRUE, weights = df$weights, na.rm = TRUE)
  expect_equal(res, matrix(c(100, 57.1, 0, 100, 3, 3.5), nrow = 3, byrow = TRUE,
                           dimnames = list(c("blue", "red", "n"), c("Man", "Woman"))))
})

test_that("Weighted row percentages cross multi.table without NA is ok", {
  res <- cross.multi.table(df[,c("blue","red")], df$sex, freq = TRUE, tfreq = "row", n = TRUE, weights = df$weights, na.rm = TRUE)
  expect_equal(res, matrix(c(60, 40, 5, 0, 100, 3.5), nrow = 2, byrow = TRUE,
                           dimnames = list(c("blue", "red"), c("Man", "Woman", "n"))))
})



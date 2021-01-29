library(testthat)
library(questionr)
context("Weighting functions")

data(hdv2003)

df <- data.frame(
  x = c("a", "a", "a",  "b", "b", "b", "c"),
  y = c("u", "v", "u",  NA, "v",  NA,  NA),
  p = c(  3,   2, 1.5,   1, 1.5,   1,   2),
  z = c(  1,   1,   1,   2,   2,   1,  NA)
)

df_na <- data.frame(
  x = c("a", "a", "a",  "b", "b", NA, "c", NA),
  p = c(  3,   2, NA,   2.5, 1.5,   1,   2, NA)
)

test_that("wtd.mean is correct", {
  expect_equal(wtd.mean(df$z, df$p), 12.5/10)
  expect_equal(wtd.mean(df$z, df$p, na.rm = FALSE), NA_real_)
})

test_that("Simple wtd.table is correct", {
  
  tab <- wtd.table(df$y, weights = df$p)
  expect_equal(tab, structure(c(u = 4.5, v = 3.5), .Dim = 2L, 
                              .Dimnames = list(c("u", "v")), class = "table"))
  tab <- wtd.table(df$y, weights = df$p, useNA = "always")
  expect_equal(tab, structure(c(4.5, 3.5, 4), .Dim = 3L, 
                              .Dimnames = list(c("u", "v", NA)), class = "table"))
  
  tab <- wtd.table(df$y, weights = df$p, normwt = TRUE)
  expect_equal(tab, structure(c(u = 2.625, v = 2.04166666666667), .Dim = 2L, 
                              .Dimnames = list(c("u", "v")), class = "table"))
  
  tab <- wtd.table(df$y, weights = df$p, normwt = TRUE, useNA = "ifany")
  expect_equal(tab, structure(c(2.625, 2.04166666666667, 2.33333333333333), .Dim = 3L, 
                              .Dimnames = list(c("u", "v", NA)), class = "table"))
  
  tab <- wtd.table(df$x, weights = df$p, useNA = "ifany")
  expect_equal(tab, structure(c(a = 6.5, b = 3.5, c = 2), .Dim = 3L, 
                              .Dimnames = list(c("a", "b", "c")), class = "table"))
  
  tab <- wtd.table(df$x, weights = df$p, useNA = "always")
  expect_equal(tab, structure(c(6.5, 3.5, 2, 0), .Dim = 4L, 
                              .Dimnames = list(c("a", "b", "c", NA)), class = "table"))
  
  # NA in weights
  tab <- wtd.table(df_na$x, weights = df_na$p)
  expect_equal(tab, structure(c(a = 5, b = 4, c = 2), .Dim = 3L, .Dimnames = list(
    c("a", "b", "c")), class = "table"))
  
  tab <- wtd.table(df_na$x, weights = df_na$p, normwt = TRUE)
  expect_equal(tab, structure(c(a = 3.33333333333333, b = 2.66666666666667, c = 1.33333333333333
  ), .Dim = 3L, .Dimnames = list(c("a", "b", "c")), class = "table"))

  tab <- wtd.table(df_na$x, weights = df_na$p, useNA = "always")
  expect_equal(tab, structure(c(5, 4, 2, 1), .Dim = 4L, 
                              .Dimnames = list(c("a", "b", "c", NA)), class = "table"))
  
  tab <- wtd.table(df_na$x, weights = df_na$p, normwt = TRUE, useNA = "always")
  expect_equal(tab, structure(c(3.33333333333333, 2.66666666666667, 1.33333333333333, 
                                0.666666666666667), .Dim = 4L, 
                              .Dimnames = list(c("a", "b", "c", NA)), class = "table"))
    
})

test_that("Cross wtd.table is correct", {
  
  tab <- wtd.table(df$x, df$y, weights = df$p)
  expect_equal(tab, structure(c(4.5, 0, 2, 1.5), .Dim = c(2L, 2L), .Dimnames = list(
    c("a", "b"), c("u", "v")), class = "table"))
  
  tab <- wtd.table(df$x, df$y, weights = df$p, normwt = TRUE)
  expect_equal(tab, structure(c(2.625, 0, 1.16666666666667, 0.875), .Dim = c(2L, 2L),
    .Dimnames = list(c("a", "b"), c("u", "v")), class = "table"))
  
  tab <- wtd.table(df$x, df$y, weights = df$p, useNA = "always")
  expect_equal(tab, structure(c(4.5, 0, 0, 0, 2, 1.5, 0, 0, 0, 2, 2, 0), .Dim = 4:3, 
    .Dimnames = list(c("a", "b", "c", NA), c("u", "v", NA)), class = "table"))
  
  tab <- wtd.table(df$x, df$y, weights = df$p, useNA = "ifany")
  expect_equal(tab, structure(c(4.5, 0, 0, 2, 1.5, 0, 0, 2, 2), .Dim = c(3L, 3L), 
    .Dimnames = list(c("a", "b", "c"), c("u", "v", NA)), class = "table"))
  
  tab <- wtd.table(df$x, df$y, weights = df$p, useNA = "ifany", normwt = TRUE)
  expect_equal(tab, structure(c(2.625, 0, 0, 1.16666666666667, 0.875, 0, 0, 
    1.16666666666667, 1.16666666666667), .Dim = c(3L, 3L), 
    .Dimnames = list(c("a", "b", "c"), c("u", "v", NA)), class = "table"))
})


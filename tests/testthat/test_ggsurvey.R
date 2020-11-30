context("ggsurvey function")

test_that("ggsurvey works correctly", {
  skip_if_not_installed("survey")
  skip_if_not_installed("ggplot2")
  library(ggplot2)
  
  data(api, package = "survey")
  dstrat <- survey::svydesign(
    id = ~1, strata = ~stype,
    weights = ~pw, data = apistrat,
    fpc = ~fpc
  )
  expect_error(
    ggsurvey(dstrat) +
      aes(x = cnum, y = dnum) +
      geom_count(),
    NA
  )
  
  d <- as.data.frame(Titanic)
  dw <- survey::svydesign(ids = ~1, weights = ~Freq, data = d)
  expect_error(
    ggsurvey(dw) + 
      aes(x = Class, fill = Survived) + 
      geom_bar(position = "fill"),
    NA
  )
})


library(testthat)
library(questionr)
library(dplyr)
library(tidyr)
library(janitor)
context("Tables and cross-tables functions")

data(hdv2003)
data(fecondite)

test_that("Simple freq is correct", {
  tab <- freq(hdv2003$qualif)
  v <- as.numeric(summary(hdv2003$qualif))
  val <- as.numeric(table(hdv2003$qualif))
  expect_equal(names(tab), c("n", "%", "val%"))
  expect_equal(rownames(tab), c(levels(hdv2003$qualif), "NA"))
  expect_equal(tab$n, v)
  expect_equal(tab$`%`, round(v / sum(v) * 100, 1))
  expect_equal(tab$`val%`, c(round(val / sum(val) * 100, 1), NA))
})

test_that("freq with NA and 'NA' is ok", {
  x <- c("a", "a", "b", "NA", NA)
  tab <- freq(x)
  expect_setequal(rownames(tab), c("a", "b", "\"NA\"", "NA"))
})


test_that("freq with sort, digits, cum, valid and total is correct", {
  tab <- freq(hdv2003$qualif, digits = 2, cum = TRUE, total = TRUE, valid = FALSE, sort = "inc", na.last = FALSE)
  v <- sort(summary(hdv2003$qualif))
  vnum <- as.numeric(v)
  expect_equal(names(tab), c("n", "%", "%cum"))
  expect_equal(rownames(tab), gsub("NA's", "NA", c(names(v), "Total")))
  expect_equal(tab$n, c(vnum, sum(vnum)))
  expect_equal(tab$`%`, c(round(vnum / sum(vnum) * 100, 2), 100))
  expect_equal(tab$`%cum`, c(round(cumsum(vnum) / sum(vnum) * 100, 2), 100))
})

test_that("freq with sort, digits, cum, valid, total and na.last is correct", {
  tab <- freq(hdv2003$qualif, digits = 2, cum = TRUE, total = TRUE, valid = FALSE, sort = "inc", na.last = TRUE)
  v <- sort(summary(hdv2003$qualif))
  v <- c(v[names(v) != "NA's"], v[names(v) == "NA's"])
  vnum <- as.numeric(v)
  expect_equal(names(tab), c("n", "%", "%cum"))
  expect_equal(rownames(tab), gsub("NA's", "NA", c(names(v), "Total")))
  expect_equal(tab$n, c(vnum, sum(vnum)))
  expect_equal(tab$`%`, c(round(vnum / sum(vnum) * 100, 2), 100))
  expect_equal(tab$`%cum`, c(round(cumsum(vnum) / sum(vnum) * 100, 2), 100))
})


test_that("freq with exclude is correct", {
  tab <- freq(hdv2003$qualif, exclude = c(NA, "Cadre", "Autre"))
  v <- hdv2003$qualif[!(hdv2003$qualif %in% c(NA, "Cadre", "Autre"))]
  vtab <- as.numeric(table(v)[!(names(table(v)) %in% c(NA, "Cadre", "Autre"))])
  expect_equal(names(tab), c("n", "%"))
  expect_equal(rownames(tab), setdiff(levels(hdv2003$qualif), c("NA", "Cadre", "Autre")))
  expect_equal(tab$n, vtab)
  expect_equal(tab$`%`, round(vtab / sum(vtab) * 100, 1))
})

test_that("cprop results are correct", {
  tab <- table(hdv2003$qualif, hdv2003$clso, exclude = NULL)
  etab <- tab[, apply(tab, 2, sum) > 0]
  ctab <- cprop(tab, n = TRUE)
  expect_equal(colnames(ctab), c(levels(hdv2003$clso), gettext("All", domain = "R-questionr")))
  expect_equal(rownames(ctab), c(levels(hdv2003$qualif), NA, gettext("Total", domain = "R-questionr"), "n"))
  m <- base::prop.table(etab, 2) * 100
  class(m) <- c("proptab", class(m))
  expect_equal(ctab[seq_len(nrow(m)), seq_len(ncol(m))], m)
  margin <- margin.table(etab, 1)
  margin <- as.numeric(round(margin / sum(margin) * 100, 2))
  expect_equal(unname(ctab[seq_len(length(margin)), gettext("All", domain = "R-questionr")]), margin)
  n <- apply(etab, 2, sum)
  expect_equal(ctab["n", ][seq_len(length(n))], n)
})

test_that("lprop results are correct", {
  tab <- table(hdv2003$qualif, hdv2003$clso, exclude = NULL)
  etab <- tab[, apply(tab, 2, sum) > 0]
  ltab <- lprop(tab, n = TRUE)
  expect_equal(colnames(ltab), c(levels(hdv2003$clso), gettext("Total", domain = "R-questionr"), "n"))
  expect_equal(rownames(ltab), c(levels(hdv2003$qualif), NA, gettext("All", domain = "R-questionr")))
  m <- base::prop.table(etab, 1) * 100
  class(m) <- c("proptab", class(m))
  expect_equal(ltab[seq_len(nrow(m)), seq_len(ncol(m))], m)
  margin <- margin.table(etab, 2)
  margin <- as.numeric(round(margin / sum(margin) * 100, 2))
  expect_equal(unname(ltab[gettext("All", domain = "R-questionr"), seq_len(length(margin))]), margin)
  n <- apply(etab, 1, sum)
  expect_equal(ltab[, "n"][seq_len(length(n))], n)
})

test_that("prop, cprop and lprop tabyl versions are correct", {
  # Skip if janitor 2.1.0 due to a bug in adorn_percentages()
  skip_if_not_installed("janitor", minimum_version = "2.2.0")
  ## lprop
  ltabl <- hdv2003 %>%
    tabyl(qualif, sexe) %>%
    lprop() %>%
    as.data.frame() %>%
    tidyr::pivot_longer(-`qualif/sexe`, names_to = "Var2", values_to = "Freq")
  ltab <- table(hdv2003$qualif, hdv2003$sexe, useNA = "always") %>%
    lprop() %>%
    round(1) %>%
    as.data.frame() %>%
    mutate(Freq = format(Freq, nsmall = 1, trim = TRUE))
  expect_equal(sort(ltabl$Freq), sort(ltab$Freq))
  ## cprop
  ctabl <- hdv2003 %>%
    tabyl(qualif, sexe) %>%
    cprop() %>%
    as.data.frame() %>%
    tidyr::pivot_longer(-`qualif/sexe`, names_to = "Var2", values_to = "Freq")
  ctab <- table(hdv2003$qualif, hdv2003$sexe, useNA = "always") %>%
    cprop() %>%
    round(1) %>%
    as.data.frame() %>%
    mutate(Freq = format(Freq, nsmall = 1, trim = TRUE))
  expect_equal(sort(ctabl$Freq), sort(ctab$Freq))
  ## prop
  tabl <- hdv2003 %>%
    tabyl(qualif, sexe) %>%
    prop(digits = 2) %>%
    as.data.frame() %>%
    tidyr::pivot_longer(-`qualif/sexe`, names_to = "Var2", values_to = "Freq")
  tab <- table(hdv2003$qualif, hdv2003$sexe, useNA = "always") %>%
    prop() %>%
    round(2) %>%
    as.data.frame() %>%
    mutate(Freq = format(Freq, nsmall = 1, trim = TRUE))
  expect_equal(sort(tabl$Freq), sort(tab$Freq))
})

test_that("freqtable results are correct", {
    skip_if_not_installed("dplyr")
    data(hdv2003)
    expect_error(freqtable(hdv2003))
    expect_error(freqtable(hdv2003, weights = poids))
    expect_equal(freqtable(hdv2003, nivetud),
                 xtabs(~ nivetud, hdv2003, addNA = TRUE),
                 check.attributes = FALSE)
    expect_equal(freqtable(hdv2003, nivetud, sport),
                 xtabs(~ nivetud + sport, hdv2003, addNA = TRUE),
                 check.attributes = FALSE)
    expect_equal(freqtable(hdv2003, starts_with("niv")),
                 xtabs(~ nivetud, hdv2003, addNA = TRUE),
                 check.attributes = FALSE)

    expect_equal(freqtable(hdv2003, nivetud, na.rm = FALSE),
                 xtabs(~ nivetud, hdv2003, addNA = TRUE),
                 check.attributes = FALSE)
    expect_equal(freqtable(hdv2003, nivetud, na.rm = TRUE),
                 xtabs(~ nivetud, hdv2003),
                 check.attributes = FALSE)

    expect_equal(freqtable(hdv2003, nivetud, weights = poids),
                 xtabs(poids ~ nivetud, hdv2003, addNA = TRUE),
                 check.attributes = FALSE)
    expect_equal(freqtable(hdv2003, nivetud, sport, weights = poids),
                 xtabs(poids ~ nivetud + sport, hdv2003, addNA = TRUE),
                 check.attributes = FALSE)
    expect_equal(freqtable(hdv2003, starts_with("niv"), weights = ends_with("oids")),
                 xtabs(poids ~ nivetud, hdv2003, addNA = TRUE),
                 check.attributes = FALSE)

    skip_if_not_installed("survey")
    library(survey)
    hdv2003_wtd <- svydesign(ids = ~ 1, weights = ~ poids, data = hdv2003)
    expect_equal(freqtable(hdv2003_wtd, nivetud),
                 xtabs(poids ~ nivetud, hdv2003, addNA = TRUE),
                 check.attributes = FALSE)
    expect_equal(freqtable(hdv2003_wtd, nivetud, sport),
                 xtabs(poids ~ nivetud + sport, hdv2003, addNA = TRUE),
                 check.attributes = FALSE)
    expect_equal(freqtable(hdv2003_wtd, nivetud, sport),
                 xtabs(poids ~ nivetud + sport, hdv2003, addNA = TRUE),
                 check.attributes = FALSE)
    expect_equal(freqtable(hdv2003_wtd, nivetud, weights = TRUE),
                 xtabs(poids ~ nivetud, hdv2003, addNA = TRUE),
                 check.attributes = FALSE)
    expect_equal(freqtable(hdv2003_wtd, nivetud, weights = FALSE),
                 xtabs(~ nivetud, hdv2003, addNA = TRUE),
                 check.attributes = FALSE)
    expect_equal(freqtable(hdv2003_wtd, nivetud, weights = NULL),
                 xtabs(~ nivetud, hdv2003, addNA = TRUE),
                 check.attributes = FALSE)
    expect_equal(freqtable(hdv2003_wtd, nivetud, weights = NULL, na.rm = FALSE),
                 xtabs(~ nivetud, hdv2003, addNA = TRUE),
                 check.attributes = FALSE)
    expect_equal(freqtable(hdv2003_wtd, nivetud, weights = NULL, na.rm = TRUE),
                 xtabs(~ nivetud, hdv2003),
                 check.attributes = FALSE)
    expect_error(freqtable(hdv2003_wtd, nivetud, weights = age))
})

test_that("cprop, rprop and prop works with table of 3+ dimensions", {
  expect_no_error(
    t <- prop(Titanic)
  )
  expect_equal(
    t["2nd", "Male", "Adult", "Yes"] / 100,
    Titanic["2nd", "Male", "Adult", "Yes"] / sum(Titanic[, , "Adult", "Yes"])
  )

  expect_no_error(
    t <- cprop(Titanic)
  )
  expect_equal(
    t["2nd", "Male", "Adult", "Yes"] / 100,
    Titanic["2nd", "Male", "Adult", "Yes"] / sum(Titanic[, "Male", "Adult", "Yes"])
  )

  expect_no_error(
    t <- rprop(Titanic)
  )
  expect_equal(
    t["2nd", "Male", "Adult", "Yes"] / 100,
    Titanic["2nd", "Male", "Adult", "Yes"] / sum(Titanic["2nd", , "Adult", "Yes"])
  )

  expect_no_error(
    rprop(Titanic, total = FALSE, n = TRUE, digits = 2, percent = TRUE)
  )
  expect_no_error(
    cprop(Titanic, total = FALSE, n = TRUE, digits = 2, percent = TRUE)
  )
  expect_no_error(
    prop(Titanic, total = FALSE, n = TRUE, digits = 2, percent = TRUE)
  )
})


context("generateEllipsisSubstitutionName")

el <- getEllipsisSubstitutionName()

test_that("generateEllipsisSubstitutionName", {
  expect_equal(generateEllipsisSubstitutionName(letters), el)
  expect_true(nchar(generateEllipsisSubstitutionName(c('el', paste0(el, LETTERS)))) == nchar(el) + 2)
})

context("opMetaTestingInformation")

test_that("opMetaTestingInformation", {
  expect_true('data.table' %in% class(opMetaTestingInformation()))
})

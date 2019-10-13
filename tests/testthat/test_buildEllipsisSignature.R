context("buildEllipsisSignature")

v <- buildEllipsisSignature()

test_that("buildEllipsisSignature", {
  expect_length(v, 3)
  expect_type(v, 'list')
  expect_type(v[[1]], 'character')

  expect_length(buildEllipsisSignature(7), 7)
  expect_length(buildEllipsisSignature(0), 1)
})


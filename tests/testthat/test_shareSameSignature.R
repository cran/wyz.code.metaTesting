context("shareSameSignature")

test_that("shareSameSignature", {
  expect_true(shareSameSignature(cos, sin))
  expect_true(shareSameSignature(is.logical, sin))
  expect_false(shareSameSignature(sum, cos))
})

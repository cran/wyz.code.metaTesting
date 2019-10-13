context("setDefaultArgumentsGenerationContext")

test_that("setDefaultArgumentsGenerationContext", {
  expect_length(dac$none, 2)
  expect_length(dac$partial, 2)
  expect_length(dac$full, 2)

  expect_true(is.list(dac$none))
  expect_true(is.list(dac$partial))
  expect_true(is.list(dac$full))

  expect_true(is.list(setDefaultArgumentsGenerationContext()))

})

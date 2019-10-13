context("setGenerationContext")

test_that("setGenerationContext", {
  expect_true(is.list(erc$homo_vector))
  expect_length(erc$homo_vector, 4)

  expect_true(is.list(erc$hetero_vector))
  expect_length(erc$hetero_vector, 4)

  expect_true(is.list(erc$homo_list))
  expect_length(erc$homo_list, 4)

  expect_true(is.list(erc$hetero_list))
  expect_length(erc$hetero_list, 4)

  expect_true(is.list(setGenerationContext()))

})

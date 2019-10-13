context("buildSemanticArgumentName")

test_that("buildSemanticArgumentName", {
  expect_equal(buildSemanticArgumentName('s'), 'x_s')
  expect_equal(buildSemanticArgumentName('i', 'numberOfItems'), 'numberOfItems_i')
  expect_equal(buildSemanticArgumentName('i', 'numberOfItems_'), 'numberOfItems_i')
  expect_equal(buildSemanticArgumentName('i_7', 'numberOfItems'), 'numberOfItems_i_7')
  expect_equal(buildSemanticArgumentName('_', 'numberOfItems'), 'numberOfItems_')
  expect_equal(buildSemanticArgumentName('_', 'numberOfItems_'), 'numberOfItems_')
  expect_equal(buildSemanticArgumentName('_'), 'x_')
})

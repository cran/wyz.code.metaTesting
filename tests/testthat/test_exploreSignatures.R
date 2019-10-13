context("exploreSignatures")

op_sum <- opwf(sum, c('...', 'removeNA_b_1'))

op_cos <- opwf(cos, c('radianAngleOrComplex_'))

rv_cos <- exploreSignatures(op_cos, list(radianAngleOrComplex_ = c('im', 'r', 'cm')))

rv_sum <- exploreSignatures(op_sum, list(... = c('im', 'r', 'cm')))

cac_sum <- computeArgumentsCombination(op_sum)

rv_sum_f <- exploreSignatures(op_sum, list(... = c('im', 'r', 'cm')), cac_sum$signatures[c(1, 5)])


op_matrix <- opwf(matrix, c('data_', 'numberOfRows_ui_1', 'numberOfColumns_ui_1',
                            'givenByRow_b_1', 'dimensionNames_l_2' ))
cac_matrix <- computeArgumentsCombination(op_matrix)
rv_matrix  <- exploreSignatures(op_matrix)

test_that("exploreSignatures", {
  expect_type(rv_cos, 'list')
  expect_length(rv_cos, 3)
  expect_length(setdiff(names(rv_cos), c('info', 'success', 'failure')), 0)

  expect_length(rv_cos$failure$synthesis$error, 1)
  expect_true(nrow(rv_cos$failure$table) == 6)
  expect_true(nrow(rv_cos$success$table) == 6)
  expect_true(nrow(rv_cos$success$table) == nrow(rv_cos$success$code))

  expect_length(rv_sum$failure$synthesis$error, 1)
  expect_true(nrow(rv_sum$failure$table) == 12)
  expect_true(nrow(rv_sum$success$table) == 20)
  expect_true(nrow(rv_sum$success$table) == nrow(rv_sum$success$code))

  expect_error(exploreSignatures(op_sum, list(... = c('im', 'r', 'cm')), list('alpha')))
})

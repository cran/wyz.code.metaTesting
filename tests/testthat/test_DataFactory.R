context("DataFactory")

df <- DataFactory()

verifyFunction <- function(type_s_1, checkFunction_f_1, numberElements_ui_1 = 3) {
  values <- df$drawValues(type_s_1, numberElements_ui_1)
  if (endsWith(type_s_1, 'x_l')) checkFunction_f_1(values$data) else {
    all(sapply(values$data, checkFunction_f_1))
  }
}

test_that("DataFactory", {
  expect_true(verifyFunction('x_i', is.integer, 0))
  expect_true(verifyFunction('x_i', is.integer))

  expect_true(verifyFunction('x_im', is.integer, 0))
  expect_true(verifyFunction('x_im', is.integer))
  expect_true(verifyFunction('x_im', Negate(is.na), 30))

  expect_true(verifyFunction('x_d', is.double, 0))
  expect_true(verifyFunction('x_d', is.double))

  expect_true(verifyFunction('x_r', is.double, 0))
  expect_true(verifyFunction('x_r', is.double))
  expect_true(verifyFunction('x_r', Negate(is.na), 30))

  expect_true(verifyFunction('x_c', is.complex, 0))
  expect_true(verifyFunction('x_c', is.complex))

  expect_true(verifyFunction('x_cm', is.complex, 0))
  expect_true(verifyFunction('x_cm', is.complex))
  expect_true(verifyFunction('x_cm', Negate(is.na), 30))

  expect_true(verifyFunction('x_lo', is.logical, 0))
  expect_true(verifyFunction('x_lo', is.logical))

  expect_true(verifyFunction('x_b', is.logical, 0))
  expect_true(verifyFunction('x_b', is.logical))
  expect_true(verifyFunction('x_b', Negate(is.na), 30))

  expect_true(verifyFunction('x_ch', is.character, 0))
  expect_true(verifyFunction('x_ch', is.character))

  expect_true(verifyFunction('x_s', is.character, 0))
  expect_true(verifyFunction('x_s', is.character))
  expect_true(verifyFunction('x_s', Negate(is.na), 30))

  expect_true(verifyFunction('x_ra', is.raw, 0))
  expect_true(verifyFunction('x_ra', is.raw))

  expect_true(verifyFunction('x_da', lubridate::is.Date))
  expect_true(verifyFunction('x_dc', lubridate::is.POSIXct))

  expect_true(verifyFunction('x_ui', is.integer, 0))
  expect_true(verifyFunction('x_ui', function(x) is.integer(x) && x >= 0L))
  expect_true(verifyFunction('x_ui', Negate(is.na), 30))

  expect_true(verifyFunction('x_ni', is.integer, 0))
  expect_true(verifyFunction('x_ni', function(x) is.integer(x) && x <= 0L))
  expect_true(verifyFunction('x_ni', Negate(is.na), 30))

  expect_true(verifyFunction('x_ur', is.double, 0))
  expect_true(verifyFunction('x_ur', function(x) is.double(x) && x >= 0.0))
  expect_true(verifyFunction('x_ur', Negate(is.na), 30))

  expect_true(verifyFunction('x_nr', is.double, 0))
  expect_true(verifyFunction('x_nr', function(x) is.double(x) && x <= 0.0))
  expect_true(verifyFunction('x_nr', Negate(is.na), 30))

  expect_true(verifyFunction('x_l', is.list, 0))
  expect_true(verifyFunction('x_l', is.list))

  expect_length(unique(sapply(df$drawList(3, FALSE, TRUE), typeof)), 1)
})


test_that("DataFactory - coverage", {
  expect_error(df$drawValues('x')) # not a semantic name
  expect_length(df$drawValues('x_i_3')$data, 3)
  expect_true(length(df$drawValues('x_i_3l')$data) <= 3)
  # keep 10 test for coverage to try to pass on both sides of an if - less than
  # 1 for 1 thousand risks of failure
  sapply(1:10, function(e)
    expect_true(length(df$drawValues('x_i_3n')$data) %in% c(1, 3))
  )

  expect_true(length(df$drawValues('x_i_3m')$data) >= 3)

  expect_error(df$getDrawFunction('xxx', FALSE))
  expect_true(startsWith(df$getDrawFunction('xxx'), 'No draw function matches'))

  expect_false(df$addSuffix('xyz', 'xyz', NA))
  expect_false(df$addSuffix('xyz', 'xyz', function(o_) TRUE))
  expect_false(df$addSuffix('xyz', 'xyz', function(n_i_1, replace_b_1 = TRUE) TRUE)) # no a recorded type

  expect_error(df$getType('xxx', FALSE))
  expect_true(startsWith(df$getType('xxx'), 'No suffix or type matches'))
  expect_equal(df$getType('b'), 'boolean')
  expect_true(is.integer(df$getRowNumber('boolean')))

  # to cover sublist generation
  expect_true(is.list(df$drawList(3, TRUE, FALSE, TRUE)))

  expect_false(df$addSuffix('z', 'zorg', function(){}))

  expect_false(df$addSuffix('f', 'function', function(){}))
  expect_false(df$addSuffix('f', 'function', function(n_i_1, replace_b_1 = TRUE) {})) # wrong return type
  expect_false(df$addSuffix('f', 'function', function(n_i, replace_b_1 = TRUE) { list(`*`, `+`, `-`)[[sample(1:3, 1)]]})) # wrong arg #1
  expect_false(df$addSuffix('f', 'function', function(n_i_1, replace_b_1) { list(`*`, `+`, `-`)[[sample(1:3, 1)]]})) # wrong arg #2
  expect_true(df$addSuffix('f', 'function',
                           function(n_i_1, replace_b_1 = TRUE) { list(`*`, `+`, `-`)[[sample(1:3, 1)]]}))
})


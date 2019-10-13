context("patchArgumentInCode")

tt <- c('length(x)', 'x(x)', 'f(x(x))')

tm <- sapply(tt, patchArgumentInCode, 'x', 'anotherX_d_1', USE.NAMES = FALSE)

er <- c('length(anotherX_d_1)', 'x(anotherX_d_1)', 'f(x(anotherX_d_1))')

test_that("patchArgumentInCode", {
  expect_equal(patchArgumentInCode('x', 'x', 'x_s_1'), 'x_s_1')
  expect_equal(patchArgumentInCode('x ', 'x', 'x_s_1'), 'x_s_1')
  expect_equal(patchArgumentInCode(' x', 'x', 'x_s_1'), 'x_s_1')
  expect_equal(tm[1], er[1])
  expect_equal(tm[2], er[2])
  expect_equal(tm[3], er[3])

  expect_equal(patchArgumentInCode('\u03DF\u03D9\u03E1', 'x', 'x_'), '\u03DF\u03D9\u03E1')
  expect_error(patchArgumentInCode('\u03DF\u03D9\u03E1x', 'x', 'x_'))

  expect_equal(patchArgumentInCode('x$x', 'x', 'x_d'), 'x$x')
  expect_equal(patchArgumentInCode('x.x', 'x', 'x_d'), 'x.x')
  expect_equal(patchArgumentInCode('x@x', 'x', 'x_d'), 'x@x')
  expect_equal(patchArgumentInCode('y:x', 'x', 'x_d'), 'y:x_d')
  expect_equal(patchArgumentInCode('y::x', 'x', 'x_d'), 'y::x')
  expect_equal(patchArgumentInCode('y:::x', 'x', 'x_d'), 'y:::x')

  expect_output(patchArgumentInCode('x <- y + 1', 'y', 'y_', verbose_b_1 = TRUE))
  expect_equal(patchArgumentInCode('x <- y + 1', 'z', 'z_'), 'x <- y + 1')
})

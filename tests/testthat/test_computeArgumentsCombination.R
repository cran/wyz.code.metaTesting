context("computeArgumentsCombination")

mtf <- function(k, ex, o) {
  a <- o[[k]]$number$argument
  expect_equal(o[[!!k]]$number$argument, !!ex[1])

  e <- max(o[[k]]$number$ellipsis)
  expect_equal(!!e, !!ex[2])

  d <- max(o[[k]]$number$default)
  expect_equal(!!d, !!ex[3])

  expect_true(typeof(o[[!!k]]$signatures) == 'list')

  lapply(seq_len(length(o[[k]]$signatures)), function(j) {
    expect_true(length(o[[!!k]]$signatures[[!!j]]) <= 1)
  })

  s <- sum(unlist(o[[k]]$number))
  if ( s > 0) expect_true(typeof(o[[!!k]]$signatures[[1]]) == 'character')

  if (s == 0) expect_length(o[[!!k]]$signatures, 1)
  if (a == 0 && e == 0 && d != 0) expect_length(o[[!!k]]$signatures, 2^d)
  if (a == 0 && e != 0 && d == 0) expect_length(o[[!!k]]$signatures, 1 + e) # = 4
  if (a == 0 && e != 0 && d != 0) expect_length(o[[!!k]]$signatures, 2^(e + d - 1))

  if (a != 0 && e == 0 && d == 0) expect_length(o[[!!k]]$signatures, 1)
  if (a != 0 && e != 0 && d == 0) expect_length(o[[!!k]]$signatures, 1 + e) # = 4
  if (a != 0 && e != 0 && d != 0) expect_length(o[[!!k]]$signatures, 2^(e + d - 1))
  if (a != 0 && e == 0 && d != 0) expect_length(o[[!!k]]$signatures, 2^d)

}


# Keep tests on Ubuntu
s <- Sys.info()
test_my_system <- grepl('Ubuntu', s['version'], fixed = TRUE)
if (test_my_system) {
  o <- lapply(list(Sys.Date, cos, sum, append, ls,
                   deparse, kronecker, paste, print, options,
                   vector),
              computeArgumentsCombination)

  test_that("computeArgumentsCombination - my system", {
    #         a, e, d
    mtf(1 , c(0, 0, 0), o)  # none
    mtf(2 , c(1, 0, 0), o)  # arguments only
    mtf(3 , c(0, 3, 1), o)  # ellipsis and default
    mtf(4 , c(2, 0, 1), o)  # argument and default
    mtf(5 , c(2, 0, 4), o)  # argument and default
    mtf(6 , c(1, 0, 4), o)  # argument and default
    mtf(7 , c(2, 3, 2), o)  # all
    mtf(8 , c(0, 3, 3), o)  # ellipsis and default
    mtf(9 , c(1, 3, 0), o)  # argument and ellipsis
    mtf(10, c(0, 3, 0), o)  # ellipsis only
    mtf(11, c(0, 0, 2), o)  # default only
  })
}


# According to Uwe LIGGES, following are to be known
# Signatures are not always identical between Linux, Mac, and Windows.
# Signatures of base functions may change for new versions of R anyway.


# Therefore do not use base function for testing this part as it brings deeply
# cumbersome, fragile and unstable results. Use self defined functions to
# proceed to tests

my_fun <- list(
  no_arg = list(
    function() 1000L
  ),

  only_args = list(
    function(x) x,
    function(x, y) x + y,
    function(x, y, z) x * y * z
  ),

  only_default = list(
    function(dx = 1L) dx,
    function(dx = 1L, dy = 2L) dx + dy,
    function(dx = 1L, dy = 2L, dz = 3L) dx * dy * dz
  ),

  only_ellipsis = list(
    function(...) sum(..., na.rm = TRUE)
  ),

  args_and_default = list(
    function(x, dx = 1L) x + dx,
    function(x, dx = 1L, dy = 2L) x + dx + dy,
    function(x, y, dx = 1L, dy = 2L) x + y + dx + dy
  ),

  args_and_ellipsis = list(
    function(x, ...) x + sum(..., na.rm = TRUE),
    function(x, y, ...) x + y + sum(..., na.rm = TRUE),
    function(x, y, z, ...) x + y + z + sum(..., na.rm = TRUE)
  ),

  default_and_ellipsis = list(
    function(..., dx = 1L) dx + sum(..., na.rm = TRUE),
    function(..., dx = 1L, dy = 2L) dx + dy + sum(..., na.rm = TRUE),
    function(..., dx = 1L, dy = 2L, dz = 3L) dx + dy + dz + sum(..., na.rm = TRUE)
  ),

  all = list(
    function(x, dx = 1L, ...) x + dx,
    function(x, dx = 1L, dy = 2L, ...) x + dx + dy,
    function(x, y, dx = 1L, dy = 2L, ...) x + y + dx + dy
  )
)


default_ellipsis_number <- 3L
expected_result <- list(
  no_arg = list(
    c(0, 0, 0)
  ),

  only_args = list(
    c(1, 0, 0),
    c(2, 0, 0),
    c(3, 0, 0)
  ),

  only_default = list(
    c(0, 0, 1),
    c(0, 0, 2),
    c(0, 0, 3)
  ),

  only_ellipsis = list(
    c(0, default_ellipsis_number, 0)
  ),

  args_and_default = list(
    c(1, 0, 1),
    c(1, 0, 2),
    c(2, 0, 2)
  ),

  args_and_ellipsis = list(
    c(1, default_ellipsis_number, 0),
    c(2, default_ellipsis_number, 0),
    c(3, default_ellipsis_number, 0)
  ),

  default_and_ellipsis = list(
    c(0, default_ellipsis_number, 1),
    c(0, default_ellipsis_number, 2),
    c(0, default_ellipsis_number, 3)
  ),

  all = list(
    c(1, default_ellipsis_number, 1),
    c(1, default_ellipsis_number, 2),
    c(2, default_ellipsis_number, 2)
  )
)


lapply(seq_len(length(my_fun)), function(k) {

  nm <- names(my_fun)[k]
  test_that(paste("computeArgumentsCombination -", nm), {
    fun <- my_fun[[k]]
    n <- length(fun)
    p <- lapply(fun, computeArgumentsCombination)
    sapply(seq_len(n), function(j) {
      mtf(j, expected_result[[nm]][[j]], p)
    })


  })
})

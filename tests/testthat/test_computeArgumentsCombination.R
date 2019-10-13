context("computeArgumentsCombination")

o <- lapply(list(Sys.Date, cos, sum, append, ls, deparse, kronecker, paste, print, options,
                 vector),
            computeArgumentsCombination)

test_that("computeArgumentsCombination", {

  mtf <- function(k, ex) {
    a <- o[[k]]$number$argument
    expect_equal(o[[!!k]]$number$argument, ex[1])

    e <- max(o[[k]]$number$ellipsis)
    expect_equal(e, ex[2])

    d <- max(o[[k]]$number$default)
    expect_equal(d, ex[3])

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
            # a, e, d
    mtf(1 , c(0, 0, 0))  # none
    mtf(2 , c(1, 0, 0))  # args only
    mtf(3 , c(0, 3, 1))  # ellipsis and default
    mtf(4 , c(2, 0, 1))  # argument and default
    mtf(5 , c(2, 0, 4))  # argument and default
    mtf(6 , c(1, 0, 4))  # argument and default
    mtf(7 , c(2, 3, 2))  # all
    mtf(8 , c(0, 3, 2))  # ellipsis and default
    mtf(9 , c(1, 3, 0))  # argument and ellipsis
    mtf(10, c(0, 3, 0))  # ellipsis only
    mtf(11, c(0, 0, 2))  # default only
  })

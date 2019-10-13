context("qualifyFunctionArguments")

fn <- c(Sys.Date, cos, `+`, sum, ls)

uses_ellipsis <- c(FALSE, FALSE, FALSE, TRUE, FALSE)
uses_default_values <- c(FALSE, FALSE, FALSE, TRUE, TRUE)

lfn <- length(fn)
stopifnot(lfn == length(uses_default_values))
stopifnot(lfn == length(uses_ellipsis))

qfa <- lapply(fn, qualifyFunctionArguments)

sl <- seq_len(lfn)

test_that("qualifyFunctionArguments", {

  mtf <- function(k) {

    expect_equal(qfa[[!!k]]$owns_ellipsis, uses_ellipsis[!!k])
    expect_equal(length(qfa[[!!k]]$default_names) > 0, uses_default_values[!!k])
  }

  sapply(sl, mtf)
})

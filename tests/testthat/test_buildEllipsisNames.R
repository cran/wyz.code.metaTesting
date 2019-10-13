context("buildEllipsisNames")

en <- list(NA_character_, character(0), 'e')
sen <- seq_len(length(en))
sc <- 1:3
cases <- lapply(sen, function(j) {
  lapply(sc, function(i) {
    if (is.na(en[j])) buildEllipsisNames(i) else buildEllipsisNames(i, en[[j]])
  })
})

test_that("buildEllipsisNames", {

  mtf <- function(k, n) {
    expect_length(cases[[!!k]][[n]], n)
    expect_type(cases[[!!k]][[n]], 'character')
  }

  lapply(sen, function(u) {
    lapply(sc, function(v) mtf(u, v))
  })
})


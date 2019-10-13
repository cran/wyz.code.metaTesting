context("buildArgumentsSignature")

v <- list(
  buildArgumentsSignature(character(0)),
  buildArgumentsSignature('a'),
  buildArgumentsSignature(letters[1:3]),
  buildArgumentsSignature(list('a', character(0), 'b', character(0), 'c',
                               character(0), character(0), character(0)))
)

test_that("buildArgumentsSignature", {

  mtf <- function(k) {
    expect_length(v[[!!k]], ifelse(k == 1, 0, 1))
    expect_type(v[[!!k]], 'character')
  }

  lapply(seq_len(length(v)), mtf)
})

context("computeArgumentsSignature")

sig <- list(
  computeArgumentsSignature(character(0)),
  computeArgumentsSignature(NA_character_),
  computeArgumentsSignature(letters[1]),
  computeArgumentsSignature(letters[1:2]),
  computeArgumentsSignature(letters[1:3])
)

l <- c(1, 2, 2, 4, 8)

test_that("computeArgumentsSignature", {
  sapply(seq_len(length(sig)), function(k) {
    expect_equal(length(sig[[!!k]]), l[!!k])
  })
})

sig2 <- list(
  computeArgumentsSignature(NA_character_, FALSE),
  computeArgumentsSignature(letters[1], FALSE),
  computeArgumentsSignature(letters[1:2], FALSE),
  computeArgumentsSignature(letters[1:3], FALSE)
)

test_that("computeArgumentsSignature", {
  sapply(seq_len(length(sig2)), function(k) {
    expect_equal(length(sig2[[!!k]]), 1)
  })
})
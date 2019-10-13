context("computeContext")

# fn <- list(cos, `+`, append, sum, ls)
#
# ec <- c(1, 1, 2, 8, 16)
#
# stopifnot(length(ec) == length(fn))
#
# er <- lapply(fn, function(f) {
#   qf <- qualifyFunctionArguments(f)
#   gt <- GenericTesting(f,  rep('x_i', length(qf$argument_names)))
#   ct <- gt$computeContext()
#   ct$statistics
# })
#
# test_that("computeContext", {
#
#   myf <- function(k) {
#      expect_equal(er[[!!k]]$call_signatures, ec[k])
#   }
#
#   sapply(seq_len(length(er)), myf)
# })

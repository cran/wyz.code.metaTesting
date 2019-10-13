context("testFunction")

runTest <- function(fun_s_1, replacementContext_l,
                    ellipsisReplacementContext_l, defaultArgumentsContext_l,
                    argumentsTypeRestrictions_l = list()) {
  f <- get(fun_s_1, mode = 'function')
  testFunction(f,
               generateData(f, argumentsTypeRestrictions_l, replacementContext_l,
                            ellipsisReplacementContext_l, defaultArgumentsContext_l)$data,
               fun_s_1
  )
}

runCampaign <- function(fun_s_1, replacementContext_l, ellipsisCaseName_s_1, defaultCaseName_s_1) {
  stopifnot(ellipsisCaseName_s_1 %in% names(erc))
  stopifnot(defaultCaseName_s_1 %in% names(dac))

  lapply(erc[[ellipsisCaseName_s_1]], function(e) {
    runTest(fun_s_1, replacementContext_l, e, dac[[defaultCaseName_s_1]])
  })
}

runCampaigns <- function(fun_s_1, defaultCaseNames_s) {
  stopifnot(all(defaultCaseNames_s %in% names(dac)))
  lapply(names(erc), function(p) {
    lapply(defaultCaseNames_s, function(r) {
      runCampaign(fun_s_1, erc$homo_vector[[3]], p, r)
    })
  })
}

myfun <- function(x_d, y_i_1 = 2L) { x_d %% y_i_1 }

rc_myfun <- unlist(unlist(runCampaigns('myfun', 'none'), FALSE), FALSE)


# zz <- lapply(seq_len(length(rc_myfun)), function(k) {
#   e <- rc_myfun[[k]]$result$data
#   lapply(e, function(o) paste0(typeof(o), '_', length(o)))
# })

op_sum <- opwf(sum, c('...', 'removeNA_b_1'))

test_that("testFunction", {

  mtf_true  <- function(result) { expect_true(result$result$status) }
  mtf_false <- function(result) { expect_false(result$result$status) }

  lapply(rc_myfun, mtf_true)

  # not instrumented
  expect_error(testFunction(sum, list(... = c())))

  # bad argument restrictions number
  expect_error(testFunction(sum, list(... = 'i', na.rm = 'b', z = 's')))

  # bad argument restrictions value
  expect_error(testFunction(sum, list(... = 'i', na.rm = 'xxx')))

  # expect_true(testFunction(op_sum, list(... = 'i'),
  #                          erc$homo_vector[[2]])$status)

  # expect_true(testFunction(op_sum, list(),
  #                          erc$homo_vector[[2]],
  #                          dac$none)$status)
  #
  # expect_true(testFunction(op_sum, list(),
  #                          erc$homo_vector[[2]],
  #                          dac$partial)$status)
  #
  # expect_true(testFunction(op_sum, list(),
  #                          erc$homo_vector[[2]],
  #                          dac$full)$status)
})

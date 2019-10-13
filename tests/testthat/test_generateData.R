context("generatedData")

op_sum <- opwf(sum, c('...', 'removeNA_b_1'))

op_sum_atr <- list('...' = c('i', 'd', 'c'))

op_cos <- opwf(cos, c('angleInRadian_'))

ec <- setGenerationContext(0, TRUE, FALSE)

o <- list(
  generateData(op_sum, op_sum_atr, ec, erc$hetero_vector[[1]], dac$none),
  generateData(op_sum, op_sum_atr, ec, erc$hetero_vector[[2]], dac$none),
  generateData(op_sum, op_sum_atr, ec, erc$hetero_vector[[3]], dac$none),
  generateData(op_sum, op_sum_atr, ec, erc$hetero_vector[[4]], dac$none),

  generateData(op_sum, op_sum_atr, ec, erc$hetero_vector[[1]], dac$full),
  generateData(op_sum, op_sum_atr, ec, erc$hetero_vector[[2]], dac$full),
  generateData(op_sum, op_sum_atr, ec, erc$hetero_vector[[3]], dac$full),
  generateData(op_sum, op_sum_atr, ec, erc$hetero_vector[[4]], dac$full),

  generateData(op_sum, op_sum_atr, ec, erc$hetero_list[[1]], dac$none),
  generateData(op_sum, op_sum_atr, ec, erc$hetero_list[[2]], dac$none),
  generateData(op_sum, op_sum_atr, ec, erc$hetero_list[[3]], dac$none),
  generateData(op_sum, op_sum_atr, ec, erc$hetero_list[[4]], dac$none),

  generateData(op_sum, op_sum_atr, ec, erc$hetero_list[[1]], dac$full),
  generateData(op_sum, op_sum_atr, ec, erc$hetero_list[[2]], dac$full),
  generateData(op_sum, op_sum_atr, ec, erc$hetero_list[[3]], dac$full),
  generateData(op_sum, op_sum_atr, ec, erc$hetero_list[[4]], dac$full)
)

g <- tryCatch(generateData(op_sum, list('remove' = 'b')), error = function(e) e$message)
h <- tryCatch(generateData(op_cos, list(angleInRadian_  = 'r', angleInRadian_ = 'c')),
              error = function(e) e$message)
i <- tryCatch(generateData(op_cos, list(angleInRadian_  = 'xxx')),
              error = function(e) e$message)

test_that("generatedData - failure", {
  # bad argument
  expect_error(generateData(op_sum, list('remove')))

  expect_true(startsWith(g, "following argument restriction names are not matching any function argument"))

  expect_true(startsWith(h, "following argument restriction names are duplicated"))

  expect_true(startsWith(i, 'following argument restriction values are illegal'))
})

test_that("generatedData", {
  expect_length(o[[1]]$data, 0)
  expect_length(o[[2]]$data, 1)
  expect_length(o[[3]]$data, 2)
  expect_length(o[[4]]$data, 3)

  expect_length(o[[5]]$data, 1)
  expect_length(o[[6]]$data, 2)
  expect_length(o[[7]]$data, 3)
  expect_length(o[[8]]$data, 4)

  expect_length(o[[9]]$data, 0)
  expect_length(o[[10]]$data, 1)
  expect_length(o[[11]]$data, 2)
  expect_length(o[[12]]$data, 3)

  expect_length(o[[13]]$data, 1)
  expect_length(o[[14]]$data, 2)
  expect_length(o[[15]]$data, 3)
  expect_length(o[[16]]$data, 4)

  expect_error(generateData(sum)) # not offensive programming instrumented

  expect_length(generateData(op_sum, list('removeNA_b_1' = 'b'), ec, erc$homo_vector[[2]],
                dac$partial), 4)

})


hh <- generateData(op_sum, list(), ec, erc$hetero_list[[4]], dac$full)

test_that("generatedData - coverage", {
  expect_length(hh$data, 4)
})

hi <- generateData(op_sum, list(), ec, erc$hetero_list[[1]], dac$full)

test_that("generatedData - coverage", {
  expect_length(hi$data, 1)
})

context("opfw")

createWrapperFunction <- function(fun_f_1, parameterNames_s) {
  dsf <- deparse(substitute(fun_f_1))
  tryCatch( opwf(fun_f_1, parameterNames_s, dsf),
                 error = function(e) e)
}

myfun <- function(x, y, z = x + y, t = z) {
 # some code
}

test_that("opfw - bad invocations", {
  #Wrong number of replacement argument names
  expect_error(opwf(append, c('alpha', 'beta')))

  #Wrong replacement argument names - not semantic names
  expect_error(opwf(append, c('alpha', 'beta', 'gamma')))
})

fnoargs <- function() {}
op_fnoargs <- createWrapperFunction(fnoargs, c())
op_ano <- createWrapperFunction(function(){}, c())

test_that("opfw - limit cases ", {
  # function without arguments
  expect_equal(formals(fnoargs), formals(op_fnoargs))
  expect_equal(body(fnoargs), body(op_fnoargs))

  # anonymous function
  expect_length(formals(op_ano), 0)
  expect_true(is.null(op_ano()))
})

test_that("opfw - good cases", {
  # function with 1 argument
  op_cos <- createWrapperFunction(cos, c('angleInRadian_d'))
  expect_length(formals(op_cos), 1)
  expect_type(op_cos, 'closure')

  # function with 2 arguments, one is ellipsis
  op_sum <- createWrapperFunction(sum, c('...', 'removeNA_b_1'))
  expect_length(formals(op_sum), 2)
  expect_type(op_sum, 'closure')

  # function with 3 arguments
  op_append <- createWrapperFunction(append, c('stockValues_', 'valuesToInsert_', 'afterIndex_ui_1'))
  expect_length(formals(op_append), 3)
  expect_type(op_append, 'closure')

  # user defined function
  op_myfun <- createWrapperFunction(myfun, c('x_d', 'y_i', 'z_c', 't_d'))
  expect_length(formals(op_myfun), 4)
  expect_type(op_myfun, 'closure')

  op_myfun2 <- createWrapperFunction(op_myfun, c('a_d', 'b_i', 'c_c', 'd_d'))
  expect_equal(length(formals(op_myfun2)), length(formals(op_myfun)))

  # Sys.setenv('OP_AUDIT' = 'x')
  # expect_output(createWrapperFunction(myfun, c('x_d', 'y_i', 'z_c', 't_d')))
  # Sys.setenv('OP_AUDIT' = '')

})


Sys.setenv('OP_AUDIT' = 'x')

test_that("opfw - coverage ", {
 expect_output(createWrapperFunction(myfun, c('x_d', 'y_i', 'z_c', 't_d')))
})

Sys.setenv('OP_AUDIT' = '')


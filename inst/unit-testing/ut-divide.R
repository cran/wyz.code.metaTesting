require(wyz.code.offensiveProgramming)

#----------- STANDARD R --------------------------------------------------------
sr_divide <- function(x, y) {
  if (any(y == 0)) stop('can not divide by zero')
  x / y
}

sr_code <- function(x, y) {
  tryCatch(sr_divide(x, y), error = function(e) NA_real_)
}

rr <- sr_code(pi, 0:7 * 1.0)

#----------- STANDARD R --------------------------------------------------------

op_divide <- function(x_r, x_rnz) x_r / x_rnz

op_code <- function(x, y) {
  runTransientFunction(op_divide, list(x, y), EvaluationMode(defineEvaluationModes()[3]), 'x_r')
}

fp <- FunctionParameterTypeFactory()
fp$addSuffix('rnz', 'real-not-zero', function(x_) is.double(x_) && x_ != 0.0)

Sys.setenv('OP_TYPE_FACTORY' = 'fp')

if (rv$status) {
  print(rv$value)
} else {
  print(rv)
}


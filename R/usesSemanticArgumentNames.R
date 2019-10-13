usesSemanticArgumentNames <- function(fun_f_1) {
  qfa <- qualifyFunctionArguments(fun_f_1)
  l <- length(qfa$argument_names)
  all(sapply(seq_len(l), function(k) {
    x <- wyz.code.offensiveProgramming::FunctionParameterName(qfa$argument_names[k])
    x$isSemanticName()
  }))
}

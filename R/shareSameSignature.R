shareSameSignature <- function(function_f_1, functionTemplate_f_1) {
  identical(
    qualifyFunctionArguments(function_f_1),
    qualifyFunctionArguments(functionTemplate_f_1)
  )
}
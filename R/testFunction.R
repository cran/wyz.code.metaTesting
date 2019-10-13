testFunction <- function(function_f_1,
                         generatedData_l,
                         functionName_s_1 = deparse(substitute(function_f_1))) {
  #function_name <- deparse(substitute(function_f_1))

  if (!usesSemanticArgumentNames(function_f_1))
    abort('function', strBracket(functionName_s_1),
          'owns arguments that are not semantic names')

  list(
    call = do.call(call, append(generatedData_l, functionName_s_1, 0)),
    data = generatedData_l,
    result = guardExecution(do.call(function_f_1, generatedData_l), functionName_s_1)
  )
}

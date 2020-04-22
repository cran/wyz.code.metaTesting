buildSemanticArgumentName <- function(suffix_s_1, variableName_s_1 = 'x_') {
  paste0(ifelse(endsWith(variableName_s_1, '_'), variableName_s_1, paste0(variableName_s_1, '_')),
         ifelse(suffix_s_1 == '_', '', suffix_s_1)
  )
}

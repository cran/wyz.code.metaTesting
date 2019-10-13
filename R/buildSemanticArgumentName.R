buildSemanticArgumentName <- function(suffix_s_1, prefix_s_1 = 'x_') {
  paste0(ifelse(endsWith(prefix_s_1, '_'), prefix_s_1, paste0(prefix_s_1, '_')),
         ifelse(suffix_s_1 == '_', '', suffix_s_1)
  )
}

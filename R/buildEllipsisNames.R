buildEllipsisNames <- function(ellipsisReplacementNumber_ui_1 = 3L,
                               ellipsisNameString_s_1 = getEllipsisSubstitutionName()) {
  es <- ifelse(length(ellipsisNameString_s_1) == 0, getEllipsisSubstitutionName(),
               ifelse(endsWith(ellipsisNameString_s_1, '_'),
                      gsub('_', '', ellipsisNameString_s_1, fixed = TRUE),
                      ellipsisNameString_s_1)
  )
  n <- if (ellipsisReplacementNumber_ui_1 < 1) 1 else ellipsisReplacementNumber_ui_1
  buildSemanticArgumentName('_', paste0(es, seq_len(n)))
}

buildEllipsisSignature <- function(ellipsisReplacementNumber_ui_1 = 3L,
                                   ellipsisNameString_s_1 = 'ellipsis') {
  if (ellipsisReplacementNumber_ui_1 == 0) return(list(character(0)))
  en <- buildEllipsisNames(ellipsisReplacementNumber_ui_1, ellipsisNameString_s_1)[-1]
  l <- sapply(seq_len(length(en)),
              function(k) buildArgumentsSignature(en[1:k]),
              simplify = FALSE)
  append(as.list(unlist(l)), list(character(0)), 0)
}

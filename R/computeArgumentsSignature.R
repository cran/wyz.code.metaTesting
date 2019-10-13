computeArgumentsSignature <- function(argumentNames_s, defaultValued_b_1 = TRUE) {

  if (!defaultValued_b_1) return(as.list(buildArgumentsSignature(argumentNames_s)))
  la <- length(argumentNames_s)
  if (la == 0) return(list(character(0)))

  r <- lapply(seq_len(la), function(k) {
    v <- utils::combn(argumentNames_s, k)
    if (k == 1) return(as.list(v[1, ]))
    as.list(apply(v, 2, buildArgumentsSignature))
  })
  append(as.list(unlist(r)), list(character(0)), 0)
}

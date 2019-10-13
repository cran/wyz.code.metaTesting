qualifyFunctionArguments <- function(fun_f_1) {
  rfa <- wyz.code.offensiveProgramming::retrieveFunctionArguments(fun_f_1)
  if (is.null(rfa)) {
    return(list(
      argument_names = character(0),
      owns_ellipsis = FALSE,
      ellipsis_index = NA_integer_,
      symbol_names = character(0),
      symbol_indexes = NA_integer_,
      stripped_symbol_names = character(0),
      stripped_symbol_indexes = NA_integer_,
      default_names = character(0),
      default_indexes = NA_character_,
      arguments = rfa
    ))
  }

  l <- lapply(rfa, is.symbol)
  u <- unlist(l)

  nm <- names(l)
  ellipsis <- wyz.code.offensiveProgramming::getEllipsisName()
  oe <- ellipsis %in% nm
  ren <- if (oe) removeEllipsisName(nm[u]) else nm[u]

  list(
    argument_names = nm,
    owns_ellipsis = oe,
    ellipsis_index = ifelse(oe, which(nm == ellipsis), NA_integer_),
    symbol_names = nm[u],
    symbol_indexes = which(nm %in% nm[u]),
    stripped_symbol_names = ren,
    stripped_symbol_indexes = which(nm %in% ren),
    default_names = nm[!u],
    default_indexes = which(nm %in% nm[!u]),
    arguments = rfa
  )
}

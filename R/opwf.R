opwf <- function(fun_f_1,
                 parameterNames_s,
                 functionName_s_1 = NA_character_) {

  qfa <- qualifyFunctionArguments(fun_f_1)
  l <- length(parameterNames_s)
  lfa <- length(qfa$arguments)
  if (lfa != l)
    abort('function owns', lfa, 'arguments, you provided', l, 'arguments')

  if (l == 0) return(fun_f_1) # no work on functions without arguments

  if (l > 0) {
    rv <- sapply(seq_len(l), function(k) {
      x <- wyz.code.offensiveProgramming::FunctionParameterName(parameterNames_s[k])
      x$isSemanticName()
    })
    if (!all(rv)) abort('provided parameter names are not all semantic names',
                        strBracket(paste(parameterNames_s[!rv], collapse = ', ')))
  }

  ff <- qfa$arguments
  names(ff) <- parameterNames_s

  audit <- wyz.code.offensiveProgramming::isAuditable()

  if (qfa$owns_ellipsis) {
    substitution_names <- removeEllipsisName(parameterNames_s)
    sfa <- removeEllipsisName(qfa$argument_names)
    args <- qfa$arguments[-qfa$ellipsis_index]
  } else {
    substitution_names <- parameterNames_s
    sfa <- qfa$argument_names
    args <- qfa$arguments
  }
  fg <- codePatcher(args, sfa, substitution_names)

  callParameters <- function() {
    sapply(seq_len(l), function(k) {
      if (is.symbol(qfa$arguments[[k]])) parameterNames_s[k] else {
        paste(qfa$argument_names[k], '=', parameterNames_s[k])
      }
    })
  }

  f <- function() {}
  formals(f) <- if (qfa$owns_ellipsis) append(fg, ff[getEllipsisName()], qfa$ellipsis_index - 1) else fg
  dsf <- ifelse(is.na(functionName_s_1), deparse(substitute(fun_f_1)), functionName_s_1)
  bd <- paste0('`', dsf, '`', '(', paste(callParameters(), collapse = ', '), ')')
  if (audit) cat('>>> patching  body with', bd, '\n')
  e <- str2lang(bd)
  if (audit) { cat('>>>result\n'); print(e) }
  body(f)  <- as.call(c(as.name('{'), e))

  # some errors might remain in code translation -
  # uneasy to detect them as it implies function execution with valued arguments.
  f
}

offensiveProgrammingWrapFunction <- opwf




generateData <- function(function_f_1,
                         argumentsTypeRestrictions_l = list(),
                         replacementContext_l = setGenerationContext(),
                         ellipsisReplacementContext_l = setGenerationContext(),
                         defaultArgumentsContext_l = setDefaultArgumentsGenerationContext(),
                         functionName_s_1 = deparse(substitute(function_f_1))) {


  enforceSemanticIdentifier <- function(values_s) {
    ifelse(grepl('_', values_s, fixed = TRUE), values_s, buildSemanticArgumentName(values_s))
  }

  extractSubValues <- function(values_l, subValueName_s_1) {
    lapply(values_l, function(e) e[[subValueName_s_1]])
  }

  if (!usesSemanticArgumentNames(function_f_1))
    abort('function', strBracket(functionName_s_1),
          'owns arguments that are not semantic names')

  qfa <- qualifyFunctionArguments(function_f_1)
  df <- retrieveDataFactory()

  # argumentsTypeRestrictions_l checks
  #    1. names must be unique
  #    2. names must match argument names
  #    3. values must be known
  lar <- length(argumentsTypeRestrictions_l)
  atr <- if (lar > 0) {
    nm <- names(argumentsTypeRestrictions_l)
    du <- nm[duplicated(nm)]
    if (length(du) != 0)
      abort('following argument restriction names are duplicated', strBracket(strJoin(du)))
    sd <- setdiff(nm, qfa$argument_names)
    if (length(sd) != 0)
      abort('following argument restriction names are not matching any function argument',
            strBracket(strJoin(sd)))
    u <- unique(unlist(argumentsTypeRestrictions_l))
    b <- sapply(u, df$checkSuffix)
    if (!all(b))
      abort('following argument restriction values are illegal', strBracket(strJoin(u[!b])))
    lapply(argumentsTypeRestrictions_l, enforceSemanticIdentifier)
  } else list()

  ellipsis <- getEllipsisName()
  nm <- qfa$argument_names
  if (ellipsisReplacementContext_l$number_replacements == 0)
    nm <- setdiff(nm, ellipsis)
  if (!defaultArgumentsContext_l$use) nm <- setdiff(nm, qfa$default_names)

  semell <- character(0)
  ellnames <- character(0)
  owns_ellipsis <- ellipsis %in% nm
  if (owns_ellipsis) {
    if (ellipsisReplacementContext_l$number_replacements > 0) {
      rks <- if (lar == 0 || !ellipsis %in% names(argumentsTypeRestrictions_l)) {
        if (ellipsisReplacementContext_l$force_list) 'l' else {
          d <- df$retrieveKnownSuffixes()
          if (ellipsisReplacementContext_l$allow_list) d else setdiff(d, 'l')
        }
      } else {
        atr[[ellipsis]]
      }

      nell <- ifelse(ellipsisReplacementContext_l$homogeneous_type, 1,
                     ellipsisReplacementContext_l$number_replacements)
      ell <- sample(rks, nell, replace = nell > length(rks))
      ellnames <- buildEllipsisNames(ellipsisReplacementContext_l$number_replacements,
                                     generateEllipsisSubstitutionName(nm))
      semell <- if (nell == 1) rep(ell, ellipsisReplacementContext_l$number_replacements) else ell
      names(semell) <- ellnames
      l <- length(ell)
      if (l > 0) {
        w <- which(nm == ellipsis)
        nm <- append(nm[-w], ellnames, w - 1)
      }
    }
  }

  tracker <- list()
  ne <- 0
  ef <- if (ellipsisReplacementContext_l$force_list) as.list else as.vector
  rf <- if (replacementContext_l$force_list) as.list else as.vector
  l <- lapply(nm, function(e) {
    #cat('name=', strBracket(e), '\n')
    ex <- if (owns_ellipsis && e %in% ellnames) {
      cv <- ef
      rc <- ellipsisReplacementContext_l
      ne <<- ne + 1
       if (lar > 0 && ellipsis %in% names(argumentsTypeRestrictions_l)) {
        enforceSemanticIdentifier(semell[ne])} else e
    } else {
      fpn <- FunctionParameterName(e)
      is_polymorphic <- fpn$isPolymorphic()
      if (is_polymorphic) {
        cv <- rf
        rc <- replacementContext_l
      } else {
        is_list <- fpn$getTypeSuffix() == 'l'
        cv <- if (is_list) as.list else as.vector
        rc <- setGenerationContext(replacementContext_l$number_replacements,
                                   FALSE, is_list, is_list)
      }

      if (is_polymorphic && lar > 0 && e %in% names(argumentsTypeRestrictions_l)) {
        sample(atr[[e]], 1)
      } else e
    }
    tracker[[length(tracker) + 1]] <<- list('argname' = e, 'semantic' = ex)
    #cat('name=', strBracket(ex), '\n')
    # number of replacements must come from replacement_context_l
    cv(df$drawValues(ex,
                     ifelse(owns_ellipsis, NA_integer_, replacementContext_l$number_replacements),
                     rc$homogeneous_type, rc$allow_list, rc$force_list)
    )
  })
  names(l) <- nm

  list(generation = rbindlist(tracker),
       data = extractSubValues(l, 'data'), context = extractSubValues(l, 'context'),
       n = extractSubValues(l, 'n'))
}

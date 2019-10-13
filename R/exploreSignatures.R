exploreSignatures <- function(fun_f_1,
                              argumentsTypeRestrictions_l = list(),
                              signaturesRestrictions_l = list()) {

  associateSignature <- function(generatedData_l) {
    l <- length(generatedData_l$generation$argname)
    if (l == 0)
      return(ifelse(length(cs$signatures[[1]]) == 0, "no argument signature",
                    'no match for 0 length signature'))
    lsig <- paste(generatedData_l$generation$argname, collapse = ', ')
    k <- 1
    s <- length(cs$signatures)
    repeat {
      if (length(cs$signatures[[k]]) > 0) {
        if (cs$signatures[[k]][1] == lsig) return(lsig)
      }
      k <- k + 1
      if (k > s) return(paste0("no match for [", lsig, '] in signatures'))
    }
  }

  ntests <- 1
  rc <- unlist(erc, FALSE)

  matchExecutionContext <- function(replacementContext_s_1,
                                    ellipsisReplacementContext_s_1,
                                    defaultArgumentsContext_s_1) {

    if (qfa$owns_ellipsis)
      g <- generateEllipsisSubstitutionName(qfa$argument_names)

    z <- lapply(sigres, function(h) {

      la <- length(qfa$stripped_symbol_names)

      if (length(h) == 0)
        return(
          rc[[replacementContext_s_1]]$number_replacement == 0 &&
            rc[[ellipsisReplacementContext_s_1]]$number_replacement == 0 &&
            dac[[defaultArgumentsContext_s_1]]$use == FALSE)

      a <- if (la > 0) {
        p <- paste(qfa$stripped_symbol_names, collapse = ', ')
        grepl(p, h, fixed = TRUE)
      } else TRUE

      s <- strsplit(h, ', ', fixed = TRUE)[[1]]

      e <- if (qfa$owns_ellipsis) {
        v <- grepl(paste0(g, "[1-3]"), s, perl = TRUE)
        n <- length(v[v == TRUE])
        rc[[ellipsisReplacementContext_s_1]]$number_replacement == n
      } else TRUE

      ldn <- length(qfa$default_names)
      d <- if (ldn > 0) {
        v <- grepl(paste0(paste(qfa$default_names, collapse = '|')), s, perl = TRUE)
        n <- length(v[v == TRUE])
        if (dac[[defaultArgumentsContext_s_1]]$use == FALSE) {
          n == 0
        } else {
          if (dac[[defaultArgumentsContext_s_1]]$use_all) {
            n == ldn
          } else {
            n > 0 && n < ldn
          }
        }
      } else TRUE

      a && e && d
    })
    any(z == TRUE)
  }

  runTest <- function(replacementContext_s_1,
                      ellipsisReplacementContext_s_1,
                      defaultArgumentsContext_s_1) {
    if (!matchExecutionContext(replacementContext_s_1, ellipsisReplacementContext_s_1,
                               defaultArgumentsContext_s_1)) {
      # cat('execution context does not match:', replacementContext_s_1, ellipsisReplacementContext_s_1,
      #     defaultArgumentsContext_s_1, '\n')
      return(NULL)
    }
    gd <- generateData(fun_f_1, argumentsTypeRestrictions_l, rc[[replacementContext_s_1]],
                       rc[[ellipsisReplacementContext_s_1]], dac[[defaultArgumentsContext_s_1]])
    r <- testFunction(fun_f_1, gd$data, fn)
    r$argument_replacement <- replacementContext_s_1
    r$ellipsis_replacement <- ellipsisReplacementContext_s_1
    r$default_replacement <- defaultArgumentsContext_s_1
    r$signature <- associateSignature(gd)
    r$test_number <- ntests
    ntests <<- ntests + 1
    r
  }

  runCampaigns <- function(replacementContext_s,
                           ellipsisReplacementStrategy_s,
                           defaultReplacementStrategy_s) {
    lapply(replacementContext_s, function(r) {
      lapply(ellipsisReplacementStrategy_s, function(e) {
        lapply(defaultReplacementStrategy_s, function(d) {
          runTest(r, e, d)
        })
      })
    })
  }

  computeReplacementSynthesis <- function(x_s, n_ui_1 = 3L) {
    s <- strsplit(x_s, '(\\.|_)')
    paste0('{',
           unlist(lapply(seq_len(n_ui_1), function(k) {
             paste(
               unique(unlist(lapply(s, function(e) e[k]))),
               collapse = ',')
           })), '}',
           collapse = '_')
  }

  stopifnot(is.function(fun_f_1))
  fn <- deparse(substitute(fun_f_1))
  qfa <- qualifyFunctionArguments(fun_f_1)

  # sharpen standard argument replacement strategy
  lsa <- length(qfa$stripped_symbol_indexes)
  sn <- names(unlist(erc, FALSE))
  srs <- if (lsa == 0) sn[1] else sn[-c(1, 5, 9, 13)]

  # sharpen default argument replacement strategy
  lda <- length(qfa$default_indexes)
  dn <- names(dac)
  drs <- if (lda == 0) dn[1] else {
    if (lda == 1) dn[-2] else dn
  }

  # sharpen ellipsis argument replacement strategy
  ers <- if (qfa$owns_ellipsis) sn else sn[1]

  # run tests
  cs <- computeArgumentsCombination(fun_f_1)
  ls <- length(signaturesRestrictions_l)
  sigres <- if (ls > 0) {
    i <- intersect(cs$signatures, signaturesRestrictions_l)
    if (length(i) != ls) abort('unknwon signatures are not allowed',
                               strBracket(setdiff(signaturesRestrictions_l, i)))
    i
  } else cs$signatures
  rv <- runCampaigns(srs, ers, drs)
  lrv <- unlist(unlist(rv, FALSE), FALSE)
  lrv <- Filter(function(e) !is.null(e), lrv)

  # build result for humans
  bad <- Filter(function(e) !e$result$status, lrv)
  good <- Filter(function(e) e$result$status, lrv)

  gg <- lapply(good, function(e) {
    list(
      test_number = e$test_number,
      call_string = paste(deparse(e$call), collapse = '---'),
      result = list(e$result$result)
    )
  })

  g <- lapply(good, function(e) {
    list(
      test_number = e$test_number,
      call_signature = e$signature,
      replacement = e$argument_replacement,
      ellipsis = e$ellipsis_replacement,
      default = e$default_replacement
    )
  })

  lg <- if (length(g) > 0) {
    dg <- rbindlist(g)
    ll <- list(
      number_sucessfull_tests = nrow(dg),
      signatures = unique(dg$call_signature),
      imperative = computeReplacementSynthesis(unlist(dg$replacement)),
      ellipsis = computeReplacementSynthesis(unlist(dg$ellipsis)),
      default = computeReplacementSynthesis(unlist(dg$default), 1)
    )
    if (length(qfa$stripped_symbol_names) == 0) ll$imperative <- NULL
    if (qfa$owns_ellipsis == FALSE) ll$ellipsis <- NULL
    if (length(qfa$default_names) == 0) ll$default <- NULL
    list(code = rbindlist(gg), table = dg, synthesis = ll)
  } else list(table = NA, synthesis = NA)

  b <- lapply(bad, function(e) {
    list(
      test_number = e$test_number,
      error = e$result$errorMessage,
      call_signature = e$signature,
      replacement = e$argument_replacement,
      ellipsis = e$ellipsis_replacement,
      default = e$default_replacement
    )
  })

  lb <- if (length(b) > 0) {
    db <- rbindlist(b)
    ll <- list(
      number_erroneous_tests = nrow(db),
      error = unique(db$error),
               signatures = unique(db$call_signature),
               imperative = computeReplacementSynthesis(unlist(db$replacement)),
               ellipsis = computeReplacementSynthesis(unlist(db$ellipsis)),
               default = computeReplacementSynthesis(unlist(db$default), 1)
    )
    if (length(qfa$stripped_symbol_names) == 0) ll$imperative <- NULL
    if (qfa$owns_ellipsis == FALSE) ll$ellipsis <- NULL
    if (length(qfa$default_names) == 0) ll$default <- NULL
    list(table =  db, synthesis = ll)
  } else list(table = NA, synthesis = NA)

  list(info = list(raw = lrv, good = good, bad = bad), success = lg, failure = lb)
}

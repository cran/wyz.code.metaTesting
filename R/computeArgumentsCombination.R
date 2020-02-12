computeArgumentsCombination <- function(fun_f_1) {

  combine <- function(v_s) {
    l <- append(
      unlist(lapply(seq_len(length(v_s)), function(k) {
        combn(v_s, k, simplify = FALSE)
      }), FALSE),
      list(character(0)), 0)
    lapply(l, function(e) if (length(e) <= 1) e else paste(e, collapse = ', '))
  }

  spa <- function(a_s_1, b_s, ...) {
    paste(a_s_1, paste(b_s, collapse = ', '), ..., sep = ', ', collapse = ', ')
  }

  combineLists <- function(x_l, y_l) {
    l <- lapply(x_l, function(e) {
      lapply(y_l, function(h) {
        if (length(e) == 0) return(h)
        if (length(h) == 0) return(e)
        paste(e, h, collapse = ', ', sep = ', ')
      })
    })
    unlist(l, FALSE)
  }

  qfa <- qualifyFunctionArguments(fun_f_1)

  ar <- length(qfa$stripped_symbol_names) # mandatory arguments
  as <- qfa$stripped_symbol_names

  er <- if (qfa$owns_ellipsis == TRUE) 0:3 else 0
  mer <- max(er)
  en <- buildEllipsisNames(3)
  es <- if (mer == 0) list(character(0)) else list(
    character(0),
    en[1],
    spa(en[1], en[2]),
    spa(en[1], en[2], en[3])
  )

  l <- length(qfa$default_names)
  dr <- if (l == 0) 0 else seq.int(0, l)
  mdr <- max(dr)
  ds <- if (mdr == 0) list(character(0)) else combine(qfa$default_names)

  nz <- lapply(qfa$argument_names, function(e) {
    if (e %in% qfa$stripped_symbol_names) return(list(e))
    if (e %in% qfa$default_names) return(list(character(0), e))
    es
  })

  siglen <- 2^(length(qfa$default_names) + ifelse(mer == 0, 0, 2))
  lnz <- length(nz)
  if (lnz == 0) sig <- list(character(0)) else {
    if (lnz == 1) sig <- as.list(nz[[1]]) else {
      z <- nz[[1]]
      lapply(seq_len(lnz - 1), function(k) {
        z <<- combineLists(z, nz[[k + 1]])
      })
      sig <- z
    }
  }

  list(names = list(argument = as, ellipsis = es, default = ds),
       number = list(argument = ar, ellipsis = er, default = dr),
       signatures = sig,
       theoritical_signature_number = siglen
  )
}
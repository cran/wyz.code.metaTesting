DataFactory <- function() {
  self <- environment()
  class(self) <- append('DataFactory', class(self))

  identity <- function(x_) x_

  aleatory <- function(valueSet_1_, n_i_1, replace_b_1 = TRUE) {
    sample(valueSet_1_, abs(n_i_1),
           replace = replace_b_1 || length(valueSet_1_) > abs(n_i_1))
  }

  drawBoolean <- function(n_i_1, replace_b_1 = TRUE) {
    n <- abs(n_i_1)
    if (n == 0) return(vector('logical'))
    aleatory(c(TRUE, FALSE), n, replace_b_1 = replace_b_1)
  }

  drawLogical <- function(n_i_1, replace_b_1 = TRUE) {
    n <- abs(n_i_1)
    if (n == 0) return(vector('logical'))
    aleatory(c(TRUE, FALSE, NA), n, replace_b_1 = replace_b_1)
  }

  drawIntegerMath <- function(n_i_1, replace_b_1 = TRUE) {
    n <- abs(n_i_1)
    if (n == 0) return(vector('integer'))
    aleatory(-17:17, n, replace_b_1 = replace_b_1)
  }

  drawInteger <- function(n_i_1, replace_b_1 = TRUE) {
    n <- abs(n_i_1)
    if (n == 0) return(vector('integer'))
    aleatory(c(-17:17, NA_integer_), n, replace_b_1 = replace_b_1)
  }

  drawRealMath <- function(n_i_1, replace_b_1 = TRUE) {
    n <- abs(n_i_1)
    if (n == 0) return(vector('double'))
    stats::runif(n, -17.0, 17.0)
  }

  drawDouble <- function(n_i_1, replace_b_1 = TRUE) {
    n <- abs(n_i_1)
    if (n == 0) return(vector('double'))
    aleatory(c(drawRealMath(n, replace_b_1), NA_integer_),
             n, replace_b_1 = replace_b_1)
  }

  drawNumeric <- function(n_i_1, replace_b_1 = TRUE) {
    fn <- if (stats::runif(1) < .5) drawInteger else drawDouble
    fn(n_i_1, replace_b_1 = replace_b_1)
  }

  drawUnsignedReal <- function(n_i_1, replace_b_1 = TRUE) {
    n <- abs(n_i_1)
    if (n == 0) return(vector('integer'))
    stats::runif(n, 1.0, 17.0)
  }

  drawNegativeReal <- function(n_i_1, replace_b_1 = TRUE) {
    n <- abs(n_i_1)
    if (n == 0) return(vector('integer'))
    -1.0 * stats::runif(n, 1.0, 17.0)
  }

  drawUnsignedInteger <- function(n_i_1, replace_b_1 = TRUE) {
    n <- abs(n_i_1)
    if (n == 0) return(vector('integer'))
    as.integer(ceiling(drawUnsignedReal(n, replace_b_1)))
  }

  drawNegativeInteger <- function(n_i_1, replace_b_1 = TRUE) {
    n <- abs(n_i_1)
    if (n == 0) return(vector('integer'))
    -1L * as.integer(ceiling(drawUnsignedReal(n, replace_b_1)))
  }

  buildString <- function(l, replace_b_1 = TRUE) {
    paste(aleatory(letters, l, replace_b_1), collapse = '')
  }

  drawString <- function(n_i_1, replace_b_1 = TRUE) {
    n <- abs(n_i_1)
    if (n == 0) return(vector('character'))
    sapply(seq_len(n), function(e) buildString(aleatory(3:11, 1)))
  }

  drawCharacter <- function(n_i_1, replace_b_1 = TRUE) {
    n <- abs(n_i_1)
    if (n == 0) return(vector('complex'))
    sapply(seq_len(n), function(e) {
      if (stats::runif(1) <= .93) drawString(1, replace_b_1) else NA_character_
    })
  }

  drawRaw <- function(n_i_1, replace_b_1 = TRUE) {
    n <- abs(n_i_1)
    if (n == 0) return(vector('raw'))
    charToRaw(buildString(n, replace_b_1))
  }

  drawComplexMath <- function(n_i_1, replace_b_1 = TRUE) {
    n <- abs(n_i_1)
    if (n == 0) return(vector('complex'))
    sapply(seq_len(n), function(e) {
      complex(1, drawIntegerMath(1), drawIntegerMath(1))
    })
  }

  drawComplex <- function(n_i_1, replace_b_1 = TRUE) {
    n <- abs(n_i_1)
    if (n == 0) return(vector('complex'))
    sapply(seq_len(abs(n)), function(e) {
      if (stats::runif(1) <= .93) drawComplexMath(1, replace_b_1) else NA_complex_
    })
  }

  buildDateString <- function() {
    y <- sample(2000:2030, 1)
    m <- sample(1:12, 1)
    md <- ifelse(m %in% c(1, 3, 5, 7, 8, 10, 12), 31,
                 ifelse(m %in% c(4, 6, 9, 11), 30, ifelse(y %% 4 == 0, 28, 27)))
    d <- sample(1:md, 1)
    sprintf('%04d-%02d-%02d', y, m, d)
  }

  drawDate <- function(n_i_1, replace_b_1 = TRUE) {
    n <- abs(n_i_1)
    n <- ifelse(n == 0, 1, n) # no way to create a 0 length vector of class Date
    s <- sapply(seq_len(n), function(e) { buildDateString() })
    as.Date(sample(s, n, replace_b_1))
  }

  drawPOSIXctDate <- function(n_i_1, replace_b_1 = TRUE) {
    as.POSIXct(drawDate(n_i_1, replace_b_1))
  }

  drawList <- function(n_i_1, replace_b_1 = TRUE,
                       forceHomogeneousType_b_1 = FALSE,
                       allowSublist_b_1 = TRUE,
                       needContext_b_1 = FALSE) {
    n <- abs(n_i_1)
    if (n == 0) return(list(data = list(), context = 'x_'))

    mbt <- if (allowSublist_b_1) base_types else setdiff(base_types, 'l')

    if (forceHomogeneousType_b_1) {
      bt <- aleatory(mbt, 1)
      df <- getDrawFunction(bt, FALSE)
      lfn <- lapply(seq_len(n), function(e) df)
    } else {
      bt <- if (!'l' %in% mbt) aleatory(mbt, n, TRUE) else {
        l <- length(mbt)
        w <- which(mbt == 'l')
        proba <- rep(.7 / l, l)
        proba[w] <- .3
        sample(mbt, n, TRUE, prob = proba)
      }
      lfn <- lapply(bt, function(e) { getDrawFunction(e, FALSE) })
    }

    data <- lapply(seq_len(n), function(e) {
      ns <- aleatory(0L:7L, 1, FALSE)
      lfn[[e]]$fun(aleatory(ns, 1))
    })
    if (!needContext_b_1) return(list(data = data))
    ctxt <- sapply(seq_len(n), function(e) { lfn[[e]]$suffix })
    return(list(data = data, context = ctxt))
  }

  verifyFunctionDeclaration <- function(suffix_s_1, typeVerifier_f_1) {
    if (suffix_s_1 != 'l' && !shareSameSignature(typeVerifier_f_1, drawBoolean)) return(FALSE)
    vfn <- tf$getVerificationFunction(suffix_s_1)
    if (!is.function(vfn)) return(FALSE)
    if (!vfn(typeVerifier_f_1(3))) return(FALSE)
    TRUE
  }

  getRowNumber <- function(value_s_1) {
    if (value_s_1 %in% dt$suffix) return(which(dt$suffix == value_s_1))
    if (value_s_1 %in% dt$type) return(which(dt$type == value_s_1))
    NA
  }

  getRecordedTypes <- function() copy(dt[order(suffix)])

  retrieveKnownSuffixes <- function() dt$suffix

  checkSuffix <- function(suffix_s_1) suffix_s_1[1] %in% dt$suffix

  addSuffix <- function(suffix_s_1, type_s_1, typeVerifier_f_1) {
    if (!is.function(typeVerifier_f_1)) return(FALSE)
    if (!verifyFunctionDeclaration(suffix_s_1, typeVerifier_f_1)) return(FALSE)

    s <- gsub('_*([A-Za-z].*)', '\\1', suffix_s_1, perl = TRUE)
    rv <- checkSuffix(s)
    if (!rv) dt <<- data.table::rbindlist(list(dt, list(s, type_s_1, list(typeVerifier_f_1))))
    !rv
  }

  getType <- function(value_s_1, humanUser_b_1 = TRUE) {
    rn <- getRowNumber(value_s_1[1])
    if (is.na(rn)) {
      if (!humanUser_b_1) abort('no suffix or type associated matches', strBracket(value_s_1[1]))
      return(paste('No suffix or type matches', strBracket(value_s_1[1])))
    }
    dt[rn]$type
  }

  getDrawFunction <- function(value_s_1, humanUser_b_1 = TRUE) {
    rn <- getRowNumber(value_s_1[1])
    if (is.na(rn)) {
      if (!humanUser_b_1) abort('no draw function associated with', strBracket(value_s_1[1]))
      return(paste('No draw function matches', strBracket(value_s_1[1])))
    }
    list(fun = dt[rn]$draw_function[[1]], suffix = dt[rn]$suffix)
  }

  drawValues <- function(parameterName_s_1, numberOfValues_i_1 = NA_integer_,
                         forceHomogeneousType_b_1 = TRUE, allowSubList_b_1 = FALSE,
                         forceList_b_1 = TRUE) {
    fpn <- wyz.code.offensiveProgramming::FunctionParameterName(parameterName_s_1)
    if (!fpn$isSemanticName()) abort('parameter name', strBracket(parameterName_s_1), 'must be a semantic name')

    mx <- ifelse(forceHomogeneousType_b_1, 7L, 3L)
    ns <- if (is.na(numberOfValues_i_1)) aleatory(0L:mx, 1, FALSE) else abs(numberOfValues_i_1)

    if (fpn$isPolymorphic()) {
      l <- drawList(ns, TRUE, forceHomogeneousType_b_1, allowSubList_b_1, TRUE)
      l$n <- ns
      return(l)
    }

    rs <- fpn$getTypeSuffix()
    df <- getDrawFunction(rs, FALSE)
    lsuf <- fpn$getLengthSuffix()
    if (!is.na(lsuf)) { # if valid length suffix, then consider it for data generation
      lm <- fpn$getLengthModifier()
      #cat(parameterName_s_1, ' lsuf=', strBracket(lsuf), ' n=', strBracket(ns),
      #    ' lm=', strBracket(lm),  sep = '', '\n')
      ns <- if (is.na(lm)) lsuf else {
        if (lm == 'm') {
          #cat('m way\n')
          aleatory(seq_len(lsuf) + lsuf - 1, 1)
        } else {
          if (lm == 'l') {
            #cat('l way\n')
            aleatory(0:lsuf, 1)
          } else {
            #cat('n way\n')
            if (stats::runif(1) < .5) lsuf else 1
          }
        }
      }
      #cat('ns=', ns, '\n')
    }
    # cat(strBracket(parameterName_s_1), ' n=', strBracket(ns), sep = '', '\n')
    cv <- if (forceList_b_1) as.list else identity
    if (rs != 'l') return(list(data = cv(df$fun(ns)), context = df$suffix, n = ns))
    m <- df$fun(ns, TRUE, forceHomogeneousType_b_1, allowSubList_b_1, TRUE)
    list(data = m$data, context = 'l', subcontext = m$context, n = ns)
  }


  simpleTypes <- list(
    list('b'   , 'boolean'         , list(drawBoolean)            ),
    list('lo'  , 'logical'         , list(drawLogical)            ),

    list('i'   , 'integer'         , list(drawInteger)            ),
    list('im'  , 'integer-math'    , list(drawIntegerMath)        ),

    list('d'   , 'double'          , list(drawDouble)             ),
    list('r'   , 'real-math'       , list(drawRealMath)           ),
    list('rm'  , 'real-math alias' , list(drawRealMath)           ),

    list('n'   , 'numeric'         , list(drawNumeric)            ),

    list('ui'  , 'unsigned integer', list(drawUnsignedInteger)    ),
    list('pi'  , 'positive integer', list(drawUnsignedInteger)    ),
    list('ni'  , 'negative integer', list(drawNegativeInteger)    ),

    list('ur'  , 'unsigned real'   , list(drawUnsignedReal)       ),
    list('pr'  , 'positive real'   , list(drawUnsignedReal)       ),
    list('nr'  , 'negative real'   , list(drawNegativeReal)       ),

    list('ra'  , 'raw'             , list(drawRaw)                 ),

    list('ch'  , 'character'       , list(drawCharacter)          ),
    list('s'   , 'string'          , list(drawString)             ),

    list('c'   , 'complex'         , list(drawComplex)            ),
    list('cm'  , 'complex-math'    , list(drawComplexMath)        ),

    list('da'  , 'date'            , list(drawDate)               ),
    list('dc'  , 'POSIXct'         , list(drawPOSIXctDate)        ),

    list('l'   , 'list'            , list(drawList)               )

  )

  suffix <- NULL # data.table NSE issue with Rcmd check
  dt <- data.table::rbindlist(simpleTypes)
  data.table::setnames(dt, colnames(dt), c('suffix', 'type', 'draw_function'))
  stopifnot(all(sapply(dt$draw_function, function(e) is.function(e)) == TRUE))

  base_types <- dt$suffix
  tf <- retrieveFactory()

  # enforce draw_function compliance checks
  invisible(sapply(seq_len(nrow(dt)), function(k) {
    if (!verifyFunctionDeclaration(dt[k]$suffix, dt[k]$draw_function[[1]]))
      abort(dt[k]$suffix, 'function declaration mismatch')
  }))
  self
}

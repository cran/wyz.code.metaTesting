library(data.table)
library(wyz.code.offensiveProgramming)

options(warn = 2)
# retrievePackageFunctionName from rdoc - copied to avoid looping dependencies
rpfn <- function(packageName_s_1, libraryPath_s_1 = .libPaths()[1]) {
  if (!packageName_s_1 %in% installed.packages()[, 'Package'])
    abort('package', strBracket(packageName_s_1), 'is not installed')

  sn <- packageName_s_1
  if (!sn %in% search()) {
    tt <- paste0('package:', packageName_s_1)
    if (!tt %in% search()) library(packageName_s_1, character.only = TRUE)
    sn <- tt
  }
  if (sn %in% search()) {
    l <- ls(sn, all.names = TRUE)
    return(l[sapply(l, function(e) is.function(get(e)))])
  }
  abort('package', strBracket(packageName_s_1), 'not found in search path')
}

bfn <- rpfn('base')

l <- lapply(bfn, function(e) {
  f <- get(e)
  if (!is.function(f)) return(list(fn = e, number_arguments = NA_integer_, number_default_arguments = NA_integer_))
  print(e)
  qfa <- qualifyFunctionArguments(f)
  list(fn = e,
       number_arguments = length(qfa$argument_names),
       number_default_arguments = length(qfa$default_indexes))
})

dt <- rbindlist(l)
df <- dt[number_default_arguments > 0 & number_arguments > 0]

buildReplacementNames <- function(originalNames_s) {
  buildArgnames <- function(numberOfArguments_ui_1) {
    if (numberOfArguments_ui_1 <= 26) return(paste0(letters[1:numberOfArguments_ui_1], '_'))
    c(paste0(letters[numberOfArguments_ui_1 %/% 26], buildArgnames(numberOfArguments_ui_1 %% 26)),
      buildArgnames(26))
  }

  nn <- buildArgnames(length(originalNames_s))
  e <- getEllipsisName()
  if (e %in% originalNames_s) { # very important - do not patch ellipsis
    nn[which(originalNames_s == e)] <- e
  }
  nn
}

ne <- 0
funs <- lapply(seq_len(nrow(df)), function(k) {
  #cat(df[k]$fn, '\n')
  fn <- get(df[k]$fn)
  if (!is.function(fn)) return(simpleError(df[k]$fn, 'is not a function'))
  h <- qualifyFunctionArguments(fn)
  tryCatch(opwf(fn, buildReplacementNames(h$argument_names), df[k]$fn),
           error = function(e) {
             ne <<- ne + 1
             cat(ne, 'error with', df[k]$fn, e$message, '\n')
             e })
})

names(funs) <- df$fn

gf <- Filter(function(e) is.function(e), funs)
bf <- Filter(function(e) !is.function(e), funs)
n <- 1
l <- length(gf)
repeat {
  cat(n, "/", l, ' function name is ', names(gf)[n],'\n', sep = '')
  print(get(names(gf)[n]))
  cat(strrep('-', 79), '\n')
  print(gf[[n]])
  x <- readline('next (integer|function name|q to exit)>')
  if (nchar(x) == 0) {
    n <- n + 1
    next
  }
  if (tolower(x) == 'q') break;
  p <- suppressWarnings(as.integer(x))
  if (!is.na(p)) {
    n <- ifelse(p > 0 && p <= l, p, n + 1)
    next
  }
  if (x %in% names(gf)) {
    n <- which(names(gf) == x)
  }
}




codePatcher <- function(code_, originArgumentName_s, targetArgumentName_s) {

  substituteSymbol <- function(inputSymbol_) {
    s <- as.character(inputSymbol_)
    w <- which(originArgumentName_s == s)
    if (length(w) == 1) as.symbol(targetArgumentName_s[w]) else inputSymbol_
  }

  substituteLanguage <- function(inputLanguage_) {
    l <- length(inputLanguage_)
    outputLanguage <- inputLanguage_
    n <- 1
    repeat {
      t <- typeof(inputLanguage_[[n]])
      if (t == 'language')
        outputLanguage[[n]] <- substituteLanguage(inputLanguage_[[n]])
      if (t == 'symbol') outputLanguage[[n]] <- substituteSymbol(inputLanguage_[[n]])
      if (n >= l) break;
      n <- n + 1
    }
    outputLanguage
  }

  stopifnot(all(length(originArgumentName_s) == length(targetArgumentName_s)))

  new_code <- code_
  if (is.list(code_)) names(new_code) <- targetArgumentName_s
  lapply(new_code, function(e) {
    t <- typeof(e)
    if (t == 'language') {
      return(substituteLanguage(e))
    }
    if (t != 'symbol') return(e)
    substituteSymbol(e)
  })

}

generateEllipsisSubstitutionName <- function(argumentNames_s) {
  el <- getEllipsisSubstitutionName()
  verifyNoMatch <- function(elvalue_s_1) {
    b <- sapply(argumentNames_s, startsWith, elvalue_s_1)
    all(!b)
  }

  if (verifyNoMatch(el)) return(el)

  while (TRUE) {
    el <- paste0(el, sample(LETTERS, 1))
    if (verifyNoMatch(el)) return(el)
  }

}
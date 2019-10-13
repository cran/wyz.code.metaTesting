buildArgumentsSignature <- function(argumentNames_l) {
    if (length(argumentNames_l) == 0) return(character(0))
    paste(Filter(function(e) length(e) > 0, unlist(argumentNames_l)),
          collapse = ', ')
}

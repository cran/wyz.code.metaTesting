removeEllipsisName <- function(argumentNames_s) {
  setdiff(argumentNames_s, wyz.code.offensiveProgramming::getEllipsisName())
}

citeit <- function(x_s) paste0('<cite class="itb">', x_s, '</cite>')
citefun <- function(x_s) paste0('<cite class="it">', x_s, '</cite>')
citeop <- function(x_s) paste0('<cite class="op">', x_s, '</cite>')
citearg <- function(x_s) paste0('<cite class="os">', x_s, '</cite>')
citeval <- function(x_s) paste0('<cite class="ea">', x_s, '</cite>')
citesection <- function(x_s) paste0('<cite class="bj">', x_s, '</cite>')
citecode <- function(x_s) paste0('<cite class="oc">', x_s, '</cite>')
citechar <- function(x_s) paste0('<cite class="isa">', x_s, '</cite>')
cmt <- function(x_s) paste0('<cite class="comment">', x_s, '</cite>')
citefigure <- function(x_s) paste0('<cite class="figure">', x_s, '</cite>')
citetime <- function(x_s) paste0('<cite class="time">', x_s, '</cite>')
citefile <- function(x_s) paste0('<cite class="file">', x_s, '</cite>')
citefolder <- function(x_s) paste0('<cite class="folder">', x_s, '</cite>')
citeexec <- function(x_s) paste0('<cite class="exec">', x_s, '</cite>')


citeEA <- function() {
  n <- 0
  function(x_s) {
    n <<- n + 1
    paste0('<cite class="oc"> EA#', n, ' ', x_s, '</cite>')
  }
}
cmt <- function(x_s) paste0('<cite class="comment">', x_s, '</cite>')

rdoc <- citeval('wyz.code.rdoc')
roxy <- citeval('roxygen2')
op <- citeval('wyz.code.offensiveProgramming')
R <- citeit('R')

brkfun <- function(x_s) {
  paste(sapply(x_s, function(e) paste('\u25b6', e, '<br/>')), collapse = '')
}

showTable <- function(x_dt_1) {
  DT::datatable(x_dt_1, options = list(pageLength = 25))
}


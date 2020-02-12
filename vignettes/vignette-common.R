citefun <- function(x_s) paste0('<cite class="it">', x_s, '</cite>')
citeop <- function(x_s) paste0('<cite class="op">', x_s, '</cite>')
citearg <- function(x_s) paste0('<cite class="os">', x_s, '</cite>')
citeval <- function(x_s) paste0('<cite class="ea">', x_s, '</cite>')
citesection <- function(x_s) paste0('<cite class="bj">', x_s, '</cite>')
citecode <- function(x_s) paste0('<cite class="oc">', x_s, '</cite>')
citechar <- function(x_s) paste0('<cite class="isa">', x_s, '</cite>')
cmt <- function(x_s) paste0('<cite class="comment">', x_s, '</cite>')

rdoc <- citeval('wyz.code.rdoc')
roxy <- citeval('roxygen2')
op <- citeval('wyz.code.offensiveProgramming')
R <- citefun('R')

brkfun <- function(x_s) {
  paste(sapply(x_s, function(e) paste('\u25b6', e, '<br/>')), collapse = '')
}


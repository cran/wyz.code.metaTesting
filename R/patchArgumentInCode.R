patchArgumentInCode <- function(code_s_1, originArgName_s_1, targetArgName_s_1,
                                verbose_b_1 = FALSE) {
  if (verbose_b_1) cat('code [', code_s_1, ']\n', sep = '')
  if (!grepl(originArgName_s_1, code_s_1, fixed = TRUE)) return(code_s_1)

  if (grepl(paste0('^\\s*', originArgName_s_1, '\\s*$'), code_s_1, perl = TRUE))
    return(targetArgName_s_1)

  markers <- c('\u03DF', '\u03D9', '\u03E1') # koppa, qoppa, sampi
  v <- sapply(markers, function(e) grepl(e, code_s_1, fixed = TRUE))
  w <- which(v == FALSE)
  if (length(w) == 0) abort('none of the three marks is free')
  mark <- markers[w[1]]
  if (verbose_b_1) cat('using mark [', mark, ']\n', sep = '')

  subtitution_part <- paste0(' ', mark, '\\1', mark, ' ')
  separators <- list(perl_special = "[](){}^$*|#+?.-",
                     not_special = "&@`'\"=%/:;,#~<>!",
                     not_special_multichar = c(':::', '::', '\\[\\[', '\\]\\]'))
  pat1 <- sapply(strsplit(separators$perl_special, '')[[1]], function(p) {
    paste0('\\', p)
  })

  pat <- paste0(paste(separators$not_special_multichar, collapse = '|'),
                paste(pat1, collapse = '|'), '|',
                paste0('[', separators$not_special, ']'))
  pat <- paste0('(', pat, ')')
  if (verbose_b_1) cat('using pattern ', pat, '\n', sep = '')

  code <- code_s_1 #paste0(code_s_1, mark) # absolutely necessary
  expand_code <- gsub(pat, subtitution_part, code, perl = TRUE)
  if (verbose_b_1) cat('expanded code [', expand_code, ']\n', sep = '')

  # no original argument in code
  psf <- paste0('( )*', originArgName_s_1, '( )*')
  #if (!grepl(psf, expand_code, perl = TRUE)) return(code_s_1)

  # take care that function can be eponymous of original argument name
  psf_not_function <- paste0(psf, mark, '(?!(\\(|\\$|\\.|@))')
  if (verbose_b_1) cat('seek pattern ', psf_not_function, '\n', sep = '')
  tsf <- paste0(' ', targetArgName_s_1, ' ', mark)
  f <- gsub(psf_not_function, tsf, expand_code, perl = TRUE)
  if (verbose_b_1) cat('subtitution [', f, ']\n', sep = '')

  g <- gsub(paste0('(?<!@|\\.|\\$|:::|::)', mark, ' ', originArgName_s_1, '\\s*$'),
            paste0('\\1', mark, ' ', targetArgName_s_1), f, perl = TRUE)
  if (verbose_b_1) cat('prior final substitution [', g, ']\n', sep = '')

  # substitute back markers
  tp <- paste0(' ', mark, '(.+?)', mark, ' ')
  gsub(mark, '', gsub(tp, '\\1', g, perl = TRUE), fixed = TRUE)
}

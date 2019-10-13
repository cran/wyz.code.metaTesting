packageFunctionsInformation <- function() {
  kinds <- c('elaboration', 'verification', 'exploitation', 'information')
  users <- c('developer', 'integrator', 'end-user', 'anyone')
  domains <- c('argument', 'ellipsis', 'default', 'semantic', 'context', 'data',
               'wrapper', 'package', 'information', 'audit', 'signature')
  domain_pattern <- paste0('(', paste0(domains, collapse = '|'), ')')

  pchoose <- function(x_i, data_s) {
    n <- length(data_s)
    paste(c(data_s, '-')[ifelse(1:n %in% x_i, x_i, n + 1)], sep = '', collapse = '|')
  }

  pa <- function(x_i) pchoose(x_i, kinds)

  pk <- function(x_i) paste(kinds[x_i], sep = '', collapse = '|')
  pu <- function(x_i) paste(users[x_i], sep = '', collapse = '|')


  il <- function(fun_s_1, applicability_i = 1, kind_i = 1, user_i = 1:2, domain_s = NA_character_) {
    r <- regmatches(fun_s_1, gregexpr(domain_pattern, fun_s_1, ignore.case = TRUE))[[1]]
    d <- if (is.na(domain_s[1])) r else c(domain_s, r)

    list('function' = fun_s_1, applicability = pa(applicability_i),
         kind = pk(kind_i), user = pu(user_i),
         domain = tolower(paste(d, sep = '', collapse = '|'))
    )
  }

  funs <- list(
    il('qualifyFunctionArguments'),
    il('buildSemanticArgumentName'),
    il('computeArgumentsSignature'),
    il('DataFactory'),
    il('generateData'),
    il('opfw', domain_s = 'offensive programming'),
    il('usesSemanticArgumentNames'),
    il('testFunction.R', domain_s = 'meta-testing'),
    il('setGenerationContext'),
    il('setDefaultArgumentsGenerationContext'),
    il('packageFunctionsInformation'),
    il('computeArgumentsCombination'),
    il('exploreSignatures', domain_s = 'meta-testing')
  )
  dk <- data.table::rbindlist(funs)
  applicability <- NULL # data.table NSE issue with Rcmd check
  dw <- tidyr::separate(dk, applicability, kinds, sep = '\\|')
  sapply(kinds, function(e) dw[[e]] <<- dw[[e]] == e)
  dw[order(`function`)]
}

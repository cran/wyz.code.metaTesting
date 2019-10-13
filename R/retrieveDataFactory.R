retrieveDataFactory <- function() {
  v <- Sys.getenv('OP_DATA_FACTORY')
  if (v != '') {
    g <- tryCatch(get(v, envir = parent.frame()), error = function(e) NA)
    if (is.environment(g) && is(g, 'DataFactory')) return(g)
  }
  DataFactory()
}

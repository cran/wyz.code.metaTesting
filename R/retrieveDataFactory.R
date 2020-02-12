retrieveDataFactory <- function() {
  traceFactory <- function(factory_o_1, info_s_1 = 'default') {
    if (isAuditable()) {
      cat(info_s_1,
          'data factory address', data.table::address(factory_o_1),
          '\nsuffixes', strBracket(strJoin(factory_o_1$retrieveKnownSuffixes())),
          "\n")
    }
    factory_o_1
  }

  g <- options('op_mt_data_factory')$op_mt_data_factory
  if (is.environment(g) && is(g, 'DataFactory')) return(traceFactory(g, 'user defined'))
  traceFactory(DataFactory())
}

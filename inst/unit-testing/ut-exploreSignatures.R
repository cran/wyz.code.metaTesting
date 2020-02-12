library(data.table)
library(wyz.code.offensiveProgramming)

op_sum <- opwf(sum, c('...', 'removeNA_b_1'))

op_cos <- opwf(cos, c('radianAngleOrComplex_'))

rv_cos <- exploreSignatures(op_cos, list(radianAngleOrComplex_ = c('im', 'r', 'cm')))

rv_sum <- exploreSignatures(op_sum, list(... = c('im', 'r', 'cm')))

cac_sum <- computeArgumentsCombination(op_sum)

rv_sum_f <- exploreSignatures(op_sum, list(... = c('im', 'r', 'cm')), cac_sum$signatures[c(1, 5)])

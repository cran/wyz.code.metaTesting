op_sum <- opwf(sum, c('...', 'removeNA_b_1'))

op_kronecker <- opwf(kronecker, c('arrayA_a_1', 'arrayB_a_1', 'function_f_1', 'computeDimensionNames_b_1', '...'))

op_formatdf <- opwf(format.data.frame, c('x_o_1', '...', 'justificationScheme_s_1'))

cac_sum <- computeArgumentsCombination(op_sum)

cac_kronecker <- computeArgumentsCombination(op_kronecker)

cac_formatdf <- computeArgumentsCombination(op_formatdf)

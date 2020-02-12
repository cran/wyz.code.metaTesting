
df <- retrieveDataFactory()

op_kronecker <- opwf(kronecker, c('arrayA_a_1', 'arrayB_a_1', 'function_f_1', 'computeDimensionNames_b_1', '...'))

draw_integer_array <- function(n_i_1, replace_b_1 = TRUE) {
  m <- n_i_1 + sample(0:3, 1)
  matrix(seq(1, n_i_1 * m), byrow = TRUE, nrow = n_i_1,
         dimnames = list(paste('row_', 1:n_i_1), paste('col_', 1:m)))
}
df$addSuffix('a', 'array', draw_integer_array)

draw_function_wrong <- function(n_i_1, replace_b_1 = TRUE) { } # wrong return type
df$addSuffix('f', 'function', draw_function_wrong)

draw_function_wrong2 <- function(n_i_, replace_b_1 = TRUE) { sum } # wrong arg name #1
df$addSuffix('f', 'function', draw_function_wrong2)

draw_function_wrong3 <- function(n_i_1, bool = TRUE) { sum } # wrong arg name #2
df$addSuffix('f', 'function', draw_function_wrong3)

draw_function <- function(n_i_1, replace_b_1 = TRUE) { list(`*`, `+`, `-`)[[sample(1:3, 1)]]}
df$addSuffix('f', 'function', draw_function)

# make your factory findable
options(op_mt_data_factory = df)
es <- exploreSignatures(op_kronecker)



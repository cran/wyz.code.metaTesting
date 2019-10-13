context("retrieveDataFactory")

draw_integer_array_dim2 <- function(n_i_1, replace_b_1 = TRUE) {
  m <- n_i_1 + sample(0:3, 1)
  matrix(seq(1, n_i_1 * m), byrow = TRUE, nrow = n_i_1,
         dimnames = list(paste('row_', 1:n_i_1), paste('col_', 1:m)))
}

df <- DataFactory()
df$addSuffix('a', "array", draw_integer_array_dim2)

Sys.setenv("OP_DATA_FACTORY" = "df")
fg <- retrieveDataFactory() # retrieves the factory pointed by R variable df
ng <- nrow(fg$getRecordedTypes()[suffix == 'a'])

test_that("retrieveDataFactory", {
  expect_equal(ng, 1)
})

rm(fg)
# wrong behavior as retrieveDataFactory will provide the default factory and not yours!

Sys.setenv("OP_DATA_FACTORY" = "")
fh <- retrieveDataFactory() # retrieves the default factory
nh <- nrow(fh$getRecordedTypes()[suffix == 'a'])

test_that("retrieveDataFactory", {
  expect_equal(nh, 0)
})

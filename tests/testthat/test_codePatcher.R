context("codePatcher")

simple_cases <- list(
  alist(x = FALSE),
  alist(x = 1.0),
  alist(x = 1L),
  alist(x = .Platform$endian)
)

test_that("codePatcher", {

  mtf <- function(k) {
    cp <- codePatcher(simple_cases[[k]], 'x', 'x_')
    # print(simple_cases[[k]])
    # print(cp)
    expect_true(length(simple_cases[[!!k]]) == length(cp))
    expect_true(typeof(simple_cases[[!!k]]) == typeof(cp))
    expect_true(typeof(simple_cases[[!!k]][[1]]) == typeof(cp[[1]]))
    expect_true(simple_cases[[!!k]][[1]] == cp[[1]])
  }

  lapply(seq_len(length(simple_cases)), function(k) mtf(k))
})

simple_substitution_cases <- list(
  alist(x = FALSE, y = x),
  alist(x = 1.0, y = x),
  alist(x = 1L, y = x),
  alist(x = .Platform$endian, y = x)
)

test_that("codePatcher - substitution", {

  mtf <- function(k) {
    cp <- codePatcher(simple_substitution_cases[[k]], c('x', 'y'), c('x_', 'y_'))
    # print(simple_substitution_cases[[k]])
    # print(cp)
    expect_true(length(simple_substitution_cases[[!!k]]) == length(cp))
    expect_true(typeof(simple_substitution_cases[[!!k]]) == typeof(cp))
    expect_true(typeof(simple_substitution_cases[[!!k]][[1]]) == typeof(cp[[1]]))
    expect_true(typeof(simple_substitution_cases[[!!k]][[2]]) == typeof(cp[[2]]))
    expect_true(simple_substitution_cases[[!!k]][[1]] == cp[[1]])
    expect_false(simple_substitution_cases[[!!k]][[2]] == cp[[2]])
  }

  lapply(seq_len(length(simple_substitution_cases)), function(k) mtf(k))
})

complex_substitution_cases <- list(
  alist(x = FALSE, y = TRUE, z = x + y),
  alist(x = 1.0, y = 2.0, z = x + y),
  alist(x = 1L, y = 1L, z = x + y),
  alist(x = .Platform$endian, y = "platform", z = paste(y, x)),
  alist(x = TRUE, y = FALSE, z = ifelse(x, ifelse(y, 2, 11), 0))
)

test_that("codePatcher - complex substitution", {

  mtf <- function(k) {
    cp <- codePatcher(complex_substitution_cases[[k]], c('x', 'y', 'z'), c('x_', 'y_', 'z_'))
    # print(complex_substitution_cases[[k]])
    # print(cp)
    expect_true(length(complex_substitution_cases[[!!k]]) == length(cp))
    expect_true(typeof(complex_substitution_cases[[!!k]]) == typeof(cp))
    expect_true(typeof(complex_substitution_cases[[!!k]][[1]]) == typeof(cp[[1]]))
    expect_true(typeof(complex_substitution_cases[[!!k]][[2]]) == typeof(cp[[2]]))
    expect_true(typeof(complex_substitution_cases[[!!k]][[3]]) == typeof(cp[[3]]))
    expect_true(complex_substitution_cases[[!!k]][[1]] == cp[[1]])
    expect_true(complex_substitution_cases[[!!k]][[2]] == cp[[2]])
    expect_false(complex_substitution_cases[[!!k]][[3]] == cp[[3]])
  }

  lapply(seq_len(length(complex_substitution_cases)), function(k) mtf(k))
})

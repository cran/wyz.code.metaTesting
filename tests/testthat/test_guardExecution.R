context("guardExecution")

g <- guardExecution(1:3 + 1:7, 'fakeFunction')

test_that("guardExecution - coverage", {
  expect_true(g$status)
  expect_true(grepl('simpleWarning', g$warning_message, fixed = TRUE))
  expect_false(guardExecution(abort('requested aborption'), 'fakeFunction')$status)
})

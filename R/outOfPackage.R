abort <- function(msg_s_1, ...) {
  stop(paste(msg_s_1, ...))
}

strBracket <- function(text_s_n) {
  paste0('[', text_s_n, ']')
}

strJoin <- function(text_s, join_s_n = ', ') {
  paste(text_s, sep = '', collapse = join_s_n)
}

guardExecution <- function(yourExpression_ex, functionName_s_1 = 'no function name provided') {
  rv <- tryCatch(yourExpression_ex,
                 error = function(e) e,
                 warning = function(w) w)

  processed_without_warning <- !methods::is(rv, 'warning')
  processed_without_error <- !methods::is(rv, 'error')

  brv <- function(status_b_1, result_, warningMessage_s_1, errorMessage_s_1) {
    list(
      status = status_b_1,
      function_name = functionName_s_1,
      warning_message = warningMessage_s_1,
      errorMessage = errorMessage_s_1,
      result = result_
    )
  }

  if (!processed_without_error) return(brv(FALSE, errorCondition, '', as.character(rv)))
  if (!processed_without_warning) {
    nv <- tryCatch(suppressWarnings(yourExpression_ex),
                   error = function(e) e)
    good_processing <- !methods::is(nv, 'error')
    return(brv(good_processing,
               if (good_processing) nv else warningCondition,
               as.character(rv),
               if (good_processing) '' else nv))
  }
  brv(TRUE, rv, '', '')
}


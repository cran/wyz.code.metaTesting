setDefaultArgumentsGenerationContext <- function(useDefaultArguments_b_1 = TRUE,
                                                 useAllDefaultArguments_b_1 = FALSE) {
  list(
    use = isTRUE(useDefaultArguments_b_1),
    use_all = ifelse(useDefaultArguments_b_1, isTRUE(useAllDefaultArguments_b_1), FALSE)
  )
}

dac <- default_arguments_context <- list(
  none =  setDefaultArgumentsGenerationContext(FALSE, FALSE),
  partial = setDefaultArgumentsGenerationContext(TRUE, FALSE),
  full =  setDefaultArgumentsGenerationContext(TRUE, TRUE)
)

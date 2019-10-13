setGenerationContext <- function(replacementNumber_ui_1 = sample(0:3L, 1),
                                 homogeneousTypeReplacement_b_1 = FALSE,
                                 allowList_b_1 = TRUE,
                                 forceList_b_1 = FALSE) {
  list(
    number_replacements = abs(replacementNumber_ui_1),
    homogeneous_type = isTRUE(homogeneousTypeReplacement_b_1),
    allow_list = isTRUE(allowList_b_1),
    force_list = isTRUE(forceList_b_1)
  )
}

erc <- established_replacement_context <- list(
  homo_vector = list(
    none = setGenerationContext(0, TRUE, FALSE),
    one = setGenerationContext(1, TRUE, FALSE),
    two = setGenerationContext(2, TRUE, FALSE),
    three = setGenerationContext(3, TRUE, FALSE)
  ),
  hetero_vector = list(
    none = setGenerationContext(0, FALSE, FALSE),
    one = setGenerationContext(1, FALSE, FALSE),
    two = setGenerationContext(2, FALSE, FALSE),
    three = setGenerationContext(3, FALSE, FALSE)
  ),
  homo_list = list(
    none = setGenerationContext(0, TRUE, TRUE, TRUE),
    one = setGenerationContext(1, TRUE, TRUE, TRUE),
    two = setGenerationContext(2, TRUE, TRUE, TRUE),
    three = setGenerationContext(3, TRUE, TRUE, TRUE)
  ),
  hetero_list = list(
    none = setGenerationContext(0, FALSE, TRUE, TRUE),
    one = setGenerationContext(1, FALSE, TRUE, TRUE),
    two = setGenerationContext(2, FALSE, TRUE, TRUE),
    three = setGenerationContext(3, FALSE, TRUE, TRUE)
  )
)

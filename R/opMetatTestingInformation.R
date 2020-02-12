opMetaTestingInformation <- function() {

  stratum <- buildIdentityList(c('core', paste0('layer_', 1:3)))
  phasing <- buildIdentityList(c('design', 'build', 'test', 'run', 'maintain', 'evolve', 'transversal'))
  intent <- buildIdentityList(c('parts_building', 'parts_assembly', 'quality_control', 'statistics', 'feedback',
                                'content_generation', 'utilities'))
  category <- buildIdentityList(c('function', 'class', 'data'))
  nature <- buildIdentityList(c('exported', 'internal'))

  buildList <- function(name_s_1, category_s_1, nature_s_1,
                        stratum_s_1, phasing_s_1, intent_s_1) {
    list(name = name_s_1, category = category_s_1, nature = nature_s_1,
         stratum = stratum_s_1, phasing = phasing_s_1, intent = intent_s_1
    )
  }

  bec <- function(name_s_1, stratum_s_1, phasing_s_1, intent_s_1) {
    buildList(name_s_1, category$CLASS, nature$EXPORTED, stratum_s_1, phasing_s_1, intent_s_1)
  }

  bic <- function(name_s_1, stratum_s_1, phasing_s_1, intent_s_1) {
    buildList(name_s_1, category$CLASS, nature$INTERNAL, stratum_s_1, phasing_s_1, intent_s_1)
  }

  bef <- function(name_s_1, stratum_s_1, phasing_s_1, intent_s_1) {
    buildList(name_s_1, category$FUNCTION, nature$EXPORTED, stratum_s_1, phasing_s_1, intent_s_1)
  }

  bif <- function(name_s_1, stratum_s_1, phasing_s_1, intent_s_1) {
    buildList(name_s_1, category$FUNCTION, nature$INTERNAL, stratum_s_1, phasing_s_1, intent_s_1)
  }

  dt <- data.table::rbindlist(list(
    bef('qualifyFunctionArguments', stratum$LAYER_3, phasing$BUILD, intent$PARTS_BUILDING),
    bef('buildSemanticArgumentName', stratum$CORE, phasing$BUILD, intent$PARTS_BUILDING),
    bic('DataFactory', stratum$CORE, phasing$RUN, intent$UTILITIES),
    bef('retrieveDataFactory', stratum$CORE, phasing$RUN, intent$UTILITIES),

    bef('generateData', stratum$CORE, phasing$RUN, intent$PARTS_BUILDING),
    bef('opwf', stratum$CORE, phasing$RUN, intent$UTILITIES),
    bef('offensiveProgrammingWrapFunction', stratum$CORE, phasing$RUN, intent$UTILITIES),
    bef('usesSemanticArgumentNames', stratum$CORE, phasing$BUILD, intent$PARTS_BUILDING),
    bef('testFunction', stratum$LAYER_1, phasing$BUILD, intent$QUALITY_CONTROL),

    bef('setGenerationContext', stratum$LAYER_1, phasing$RUN, intent$UTILITIES),
    bef('setDefaultArgumentsGenerationContext', stratum$LAYER_3, phasing$RUN, intent$UTILITIES),
    bef('opMetaTestingInformation', stratum$LAYER_3, phasing$RUN, intent$FEEDBACK),
    bef('computeArgumentsCombination', stratum$LAYER_1, phasing$BUILD, intent$PARTS_BUILDING),
    bef('exploreSignatures', stratum$LAYER_3, phasing$RUN, intent$PARTS_BUILDING),

    bef('dac', stratum$LAYER_3, phasing$RUN, intent$UTILITIES),
    bef('default_arguments_context', stratum$LAYER_3, phasing$RUN, intent$UTILITIES),
    bef('erc', stratum$LAYER_3, phasing$RUN, intent$UTILITIES),
    bef('established_replacement_context', stratum$LAYER_3, phasing$RUN, intent$UTILITIES),

    bif('computeArgumentsSignature', stratum$LAYER_1, phasing$BUILD, intent$PARTS_BUILDING),
    bif('buildArgumentsSignature', stratum$LAYER_1, phasing$BUILD, intent$PARTS_BUILDING),
    bif('buildEllipsisNames', stratum$LAYER_1, phasing$BUILD, intent$PARTS_BUILDING),
    bif('buildEllipsisSignature', stratum$LAYER_1, phasing$BUILD, intent$PARTS_BUILDING),
    bif('codePatcher', stratum$CORE, phasing$BUILD, intent$PARTS_BUILDING),
    bif('generateEllipsisSubstitutionName', stratum$LAYER_1, phasing$BUILD, intent$PARTS_BUILDING),
    bif('getEllipsisSubstitutionName', stratum$LAYER_1, phasing$BUILD, intent$PARTS_BUILDING),
    bif('patchArgumentInCode', stratum$CORE, phasing$BUILD, intent$PARTS_BUILDING),
    bif('removeEllipsisName', stratum$CORE, phasing$BUILD, intent$PARTS_BUILDING)
  ))

  name <- NULL # nse
  dt[order(name)]
}

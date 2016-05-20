#' replace_vars_subsamples
#'
#' @param traits a vector of traits
#' @param config_subsamples a dataframe
#' @importFrom magrittr '%>%'
#' @return a vector of trait labels
#' @export
replace_vars_subsamples <- function(traits, config_subsamples){
  if(is.null(config_subsamples)) return(traits)
  n = nrow(config_subsamples)
  config_subsamples$trait = as.character(config_subsamples$trait)
  config_subsamples$n_timepoints = as.character(config_subsamples$n_timepoints) %>% as.integer
  config_subsamples$n_subsamples = as.character(config_subsamples$n_subsamples) %>% as.integer
  config_subsamples$summarize_by = as.character(config_subsamples$summarize_by)

  out = traits

  for(i in 1:n){
    trt = config_subsamples$trait[i]
    tp = NULL
    if(config_subsamples$use_timepoints[i]) tp = config_subsamples$n_timepoints[i]
    ss = NULL
    if(config_subsamples$use_subsamples[i]) ss = config_subsamples$n_subsamples[i]
    ssfun = config_subsamples$summarize_by[i]

    ss = create_subsamples(trt, tp, ss, ssfun)
    out = insert_subsamples(out, trt, ss )
  }

  out
}

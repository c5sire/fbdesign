#' create_subsamples
#'
#' Creates a vector of labels for a trait name to help gather data for different time points (TP)
#' or sub samples (SS), that is plants from a plot or similar.
#'
#' The helper variables are created according to a simple schema by suffixing TP plus sequential
#' number and SS plus sequential number separated by underscore (_). For subsamples an additional
#' variable is added that calculates a statistic to summarize the subsamples like 'mean'.
#'
#' @param trait a trait name
#' @param timepoints integer > 0, default is NULL
#' @param subsamples integer > 0, default is NULL
#' @param subfun one of "mean", "max", "min", "mode", "variance", "sum". Default is 'mean'.
#' @importFrom magrittr '%>%'
#' @return vector of trait labels
#' @author Reinhard Simon
#' @export
create_subsamples <- function(trait = "trait", timepoints = NULL, subsamples = NULL, subfun = "mean"){
  stopifnot(is.character(trait))
  #stopifnot(is.null(timepoints) | is.integer(timepoints))
  #stopifnot(timepoints > 0)
  #stopifnot(is.null(subsamples) | is.integer(subsamples))
  #stopifnot(subsamples > 0)
  stopifnot(subfun %in% c("mean", "max", "min", "mode", "variance", "sum"))

  out = trait

  if(!is.null(timepoints)){
    out = paste0(out, "_TP", 1:timepoints )
  }

  if(!is.null(subsamples)){
    ssp = paste0("_SS", c(1:subsamples, 0))
    out = sapply(out, paste0, ssp) %>% as.vector
  }

  out = stringr::str_replace_all(out, "SS0", paste0("SS", subfun))
  out
}

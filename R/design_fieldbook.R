#' Generic fieldbook design function
#'
#' @param design a statistical design
#' @param trt1 label of treatment one; mostly 'GENOTYPE'
#' @param trt2  label of second treatment; a controlled factor
#' @param r number of repetitions
#' @param k number of blocks
#' @param series label series type
#' @param random to randomize or not
#' @param first to randomize or not the first repetition
#' @param cont continuouse labeling
#' @param variables set of variables
#'
#' @return a dataframe
#' @export
design_fieldbook <- function(design, trt1, trt2=NULL, r = 2, k,
                             series = 2 , random = TRUE, first = FALSE, cont = FALSE,
                             variables = NULL){
  fb <- switch(design,
               LSD = agricolae::design.lsd(trt1, series,
                                           randomization = random, first = first),
               RCBD = agricolae::design.rcbd(trt1, r, series,
                                             randomization = random, first = first),
               CRD = agricolae::design.crd(trt1, r, series, randomization = random),
               GLD = agricolae::design.graeco(trt1, trt2, serie = series, randomization = random),
               YD = agricolae::design.youden(trt1, r, series, first = first,
                                             randomization = random),
               LD = agricolae::design.lattice(trt1, r, series, randomization = random)
  )
  fb
}


#' Generic fieldbook design function
#'
#' @param design a statistical design
#' @param trt1 vector treatment one
#' @param trt2 vector second treatment; a controlled factor
#' @param trt1_label string
#' @param trt2_label string
#' @param r number of repetitions
#' @param k number of blocks
#' @param series label series type
#' @param random to randomize or not
#' @param zigzag order plot in serpentine
#' @param first to randomize or not the first repetition
#' @param maxRep maximum number of repetitions
#' @param cont continuouse labeling
#' @param variables set of variables
#' @return a dataframe
#' @export
design_fieldbook <- function(design = "(RCBD)", trt1 = letters[1:5], trt2=NULL,
                             r = 2, k = 2,
                             trt1_label = "trt1",
                             trt2_label = "trt2",
                             maxRep = 20,
                             series = 2 , random = TRUE, first = FALSE, cont = FALSE,
                             zigzag = FALSE,
                             variables = NULL){
  design = stringr::str_extract(design, "([A-Z2]{2,10})")

  ids = trt1
  trt1 = 1:length(trt1)
  # if (design == "LD" && !(length(trt1) %% r == 0 ))
  #   stop("Incorrect paramter combinations for LD design.")
  fb <- switch(design,
     LSD = agricolae::design.lsd(trt1, series, randomization = random, first = first),
     RCBD = agricolae::design.rcbd(trt1, r, series, randomization = random, first = first),
     CRD = agricolae::design.crd(trt1, r, series, randomization = random),
     GLD = agricolae::design.graeco(trt1, trt2, serie = series, randomization = random),
     YD = agricolae::design.youden(trt1, r, serie = series, first = first, randomization = random),
     LD = agricolae::design.lattice(trt1, r, serie = series, randomization = random),
     BIBD = agricolae::design.bib(trt1, k, r = NULL, serie = series, maxRep = maxRep, randomization = random,
                                      seed = 0, kinds = "Super-Duper"),
     AD = agricolae::design.alpha(trt1, k, r, serie = series, randomization = random),
     CD = agricolae::design.cyclic(trt1, k, r, serie = series, randomization = random)
  )
  #nc = ncol(fb$book)
  names(fb$book)[1] = "PLOT"
  #names(fb$book)[nc] = toupper(trt1_label)

  if (design == "RCBD") {
    if(zigzag) fb$book = agricolae::zigzag(fb)
    BLOCK = rep(1, nrow(fb$book))
    names(fb$book)[2] = "REP"
    #names(fb$book)[3] = toupper(trt1_label)
    names(fb$book)[3] = "ENTRY"
    IDS = ids[as.numeric(unlist(fb$book[3]))]

    fb$book = cbind(BLOCK, fb$book)
    fb$book = cbind(fb$book, IDS)
    names(fb$book)[5] = toupper(trt1_label)

  }
  if (design == "LSD") {
    if(zigzag)fb$book = agricolae::zigzag(fb)
    names(fb$book)[2] = "REP"
    fb$book = fb$book[, c(1, 2, 4)]
    names(fb$book)[3] = toupper(trt1_label)
  }
  if (design == "CRD") {
    if(zigzag)fb$book = agricolae::zigzag(fb)
    names(fb$book)[2] = "REP"
    names(fb$book)[3] = toupper(trt1_label)
  }
  if (design == "GLD") {
    if(zigzag)fb$book = agricolae::zigzag(fb)
    names(fb$book)[2] = "REP"
    fb$book = fb$book[, c(1, 2, 4, 5)]
    names(fb$book)[3] = toupper(trt1_label)
    names(fb$book)[4] = toupper(trt2_label)
  }
  if (design == "YD") {
    names(fb$book)[2] = "REP"
    names(fb$book)[3] = "COL"
    names(fb$book)[4] = toupper(trt1_label)
  }
  if (design == "BIBD") {
    if(zigzag)fb$book = agricolae::zigzag(fb)
    names(fb$book)[2] = "BLOCK"
    names(fb$book)[3] = toupper(trt1_label)
  }
  if (design == "AD") {
    fb$book = fb$book[, c(1, 5, 3, 4)]
    names(fb$book)[2] = "REP"
    names(fb$book)[3] = "BLOCK"
    names(fb$book)[4] = toupper(trt1_label)
  }
  if (design == "CD") {
    if(zigzag)fb$book = agricolae::zigzag(fb)
    names(fb$book)[2] = "REP"
    names(fb$book)[3] = "BLOCK"
    names(fb$book)[4] = toupper(trt1_label)
    sk = list()
    ns = length(fb$sketch)
    for(i in 1:ns) sk[[i]] = fb$sketch[[i]]
    fb$sketch = sk
  }

  out = fb$book

  # Adding variables
  if(!is.null(variables)){
    mm = matrix(nrow = nrow(out), ncol = length(variables) )
    nm = c(names(out), variables)
    out = cbind(out, mm)
    names(out) = nm
  }

  # Adding meta data


  attr(out, "params") = fb$parameters
  attr(out, "sketch") = fb$sketch
  attr(out, "statistics") = fb$statistics

  out
}


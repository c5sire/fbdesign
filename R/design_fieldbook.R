
#' design_fieldbook
#'
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
#' @author Reinhard Simon
#'
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

  ids = c(trt1, trt2)
  trt = 1:length(ids)
  tr1 = 1:length(trt1)
  tr2 = NULL
  if(!is.null(trt2)) {
    tr2 = length(trt1) + (1:length(trt2))
  }

  fb <- switch(design,
     #LSD = agricolae::design.lsd(trt1, series, randomization = random, first = first),
     RCBD = agricolae::design.rcbd(tr1, r, series, randomization = random, first = first, continue = cont),
     CRD = agricolae::design.crd(tr1, r, series, randomization = random),
     #GLD = agricolae::design.graeco(trt1, trt2, serie = series, randomization = random),
     #YD = agricolae::design.youden(trt1, r, serie = series, first = first, randomization = random),
     #LD = agricolae::design.lattice(trt1, r, serie = series, randomization = random),
     #BIBD = agricolae::design.bib(trt1, k, r = NULL, serie = series, maxRep = maxRep,
    #                              randomization = random,
    #                                  seed = 0, kinds = "Super-Duper"),
     #AD = agricolae::design.alpha(trt1, k, r, serie = series, randomization = random),
     #CD = agricolae::design.cyclic(trt1, k, r, serie = series, randomization = random),
     ABD = agricolae::design.dau(tr1, tr2, r, serie = series, randomization = random),
     NRD = agricolae::design.crd(tr1, r=1, serie=series, randomization = FALSE)
  )
  if(is.null(fb$book)) return(NULL)
  #print(head(fb$book))
  #nc = ncol(fb$book)
  names(fb$book)[1] = "PLOT"
  #names(fb$book)[nc] = toupper(trt1_label)

  # PLOT, REP, BLOCK, ENTRY

  if (design == "RCBD") {
    if(zigzag) fb$book = agricolae::zigzag(fb)

    idx = as.integer(as.character(fb$book[, 3]))

    BLOCK = rep(1, nrow(fb$book))
    names(fb$book)[2] = "REP"
    #names(fb$book)[3] = toupper(trt1_label)
    names(fb$book)[3] = "ENTRY"
    IDS = ids[idx]

    fb$book = cbind(BLOCK, fb$book, IDS)
    #fb$book = cbind(fb$book, IDS)
    names(fb$book)[5] = toupper(trt1_label)
    fb$book = fb$book[, c(2, 1, 3:5)]
  }

  if (design == "ABD") {
    if(zigzag) fb$book = agricolae::zigzag(fb)

    # get checks back; assume all checks are repeated in a block
    # checks are trt1!
    x = fb$book
    #print(head(x))

    #cks = table(as.character(x$trt))
    #cks = names(cks[cks > 1]) %>% as.integer %>% sort
    cks = tr1
    rps = as.integer(x$block)
    y = cbind(x, rps)
    y$trt = as.character(y$trt)
    y$rps = as.integer(y$rps)
    y[!((y$trt %in% cks) & (y$block != 1)), 4] = 1
    y = y[, c(1, 2, 4, 3)]

    idx = ids[as.integer(as.character(y[, 4]))]
    y = cbind(y, idx)
    names(y) = c("PLOT", "BLOCK", "REP", "ENTRY", "GENOTYPE")
    fb$book = y
  }


  if (design == "LSD") {
    if(zigzag)fb$book = agricolae::zigzag(fb)
    names(fb$book)[2] = "REP"
    fb$book = fb$book[, c(1, 2, 4)]
    names(fb$book)[3] = toupper(trt1_label)
  }
  if (design == "CRD" | design == "NRD") {
    #if(zigzag)fb$book = agricolae::zigzag(fb)
    names(fb$book)[2] = "REP"
    #names(fb$book)[3] = toupper(trt1_label)
    idx = as.integer(as.character(fb$book[, 3]))
    IDS = ids[idx]
    BLK = rep(1, nrow(fb$book))
    y = cbind(fb$book, IDS, BLK)
    names(y) = c("PLOT", "REP", "ENTRY", toupper(trt1_label), "BLOCK")
    fb$book = y[, c(1, 5, 2, 3, 4)]
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


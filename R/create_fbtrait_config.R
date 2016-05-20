#' create_fbtrait_config
#'
#' For Android fieldbook app
#'
#' @param fieldbook a fieldbook
#' @param dictionary a dictionary
#' @importFrom magrittr '%>%'
#'
#' @return a dataframe
#' @export
create_fbtrait_config <- function(fieldbook, dictionary){
  #print(str(fieldbook))
  #print(str(dictionary))
  fbn = names(fieldbook)
  fbn = fbn[(which(fbn == "TRT1") + 1):length(fbn)]
  n = length(fbn)

  dd = dictionary

  DF = data.frame(
    trait = fbn,
    format = rep("numeric", n),
    defaultValue = rep("", n),
    minimum = rep(NA, n),
    maximum = rep(NA, n),
    details = rep("", n),
    categories = rep("", n),
    isVisible = rep(TRUE, n),
    realPosition = c(1:n)
  )
  DF[, 1] = as.character(DF[, 1])
  DF[, 2] = as.character(DF[, 2])
  DF[, 3] = as.character(DF[, 3])
  DF[, 4] = as.numeric(DF[, 4])
  DF[, 5] = as.numeric(DF[, 5])
  DF[, 6] = as.character(DF[, 6])
  DF[, 7] = as.character(DF[, 7])

  dd = cbind(dd, cats = rep("", nrow(dd)))
  dd[, "cats"] <- as.character(dd[, "cats"])

  dd_n = nrow(dd)
  dd_m = ncol(dd)

  for(i in 1:dd_n) {
    if(stringr::str_detect(dd$format[i], "categorical")){
      cs = paste(dd[i, 11:(dd_m - 2)])
      #cs = stringr::str_split(cs, "=") %>% unlist
      #cs = cs[seq(2,length(cs), 2)]
      cs = cs[cs!="NA"]
      cs = stringr::str_trim(cs)
      dd[i, "cats"] = paste(cs, collapse = "/")
    }
  }

  # create helper columm for subsampled variables
  dd = cbind(dd, traits = rep("", nrow(dd)))
  dd[, "traits"] <- as.character(dd[, "traits"])

  for(i in 1:nrow(DF)){
    DF$traits[i] = DF$trait[i]
    if(stringr::str_detect(DF[i, "traits"], "_TP")){
      DF$traits[i] = stringr::str_split(DF[i, "traits"], "_TP")[[1]][1]
    }
    if(stringr::str_detect(DF[i, "traits"], "_SS")){
      DF$traits[i] = stringr::str_split(DF[i, "traits"], "_SS")[[1]][1]
    }
  }

  for(i in 1:n){
    #print(dd[DF$traits[i] == dd$id, "format"])
    DF[i, "format"] = dd[DF$traits[i] == dd$id, "format"]
    DF[i, "details"] = dd[DF$traits[i] == dd$id, "units"]
    DF[i, "minimum"] = dd[DF$traits[i] == dd$id, "min"]
    DF[i, "maximum"] = dd[DF$traits[i] == dd$id, "max"]
    DF[i, "categories"] = dd[DF$traits[i] == dd$id, "cats"]
  }
  DF = DF[, -c(ncol(DF))]

  DF
}

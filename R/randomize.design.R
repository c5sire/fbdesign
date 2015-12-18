
#' Design a fieldbook.
#'
#' @param design statistical design
#' @param matl list of germplasm
#' @param reps number of plot repetitions
#' @param msite logical, is this a mother site in a mother baby trial
#' @param lbls labels for germplasm
#' @param checkl list of check germplasm
#' @param bsize block size
#' @param adfn name of additional factor
#' @param adfl levels of additional factor
#' @param startn start number
#' @param seed random seed
#' @param randM randomization method
#' @author Raul Eyzaguirre, Reinhard Simon
#' @return data.frame
#' @export
randomize.design = function(design="(CRD)",
		matl, # Vector of material list
		reps, # number of repetitions
		msite=FALSE, #is mother site in a M & B design
		lbls, # short names of labels of genotypes
		checkl = NULL, # check genotypes for ABD
		bsize=2,# block size only BIB/A01D
		adfn=NULL,   # name for additional factor
		adfl=NULL,   # vector of additional factor levels
		startn = 1,
		seed = 0,
		randM="Super-duper"
) {
  library(stringr)
abb=lbls
diseno = NULL
if (str_detect(design,"(UDNR)")){
	diseno = as.data.frame(matrix(NA, nrow=length(matl), ncol=3), stringsAsFactors=F)
	diseno[,1:3] <- cbind(Plot=seq(startn, startn+length(matl)-1),Rep=rep(1,length(matl)),X=matl)
	colnames(diseno)[1:3] <- c("PLOT", "REP", lbls)
#	labs = as.data.frame(matrix(c("Plot", "Block or repetition", lbls), nrow=1), strings.as.factors=F)
}

if (str_detect(design,"(CRD)")){
	diseno = as.data.frame(matrix(NA, nrow=reps*length(matl), ncol=3), stringsAsFactors=F)
	diseno[,1:3] <- design.crd(matl, reps, number=startn)
	colnames(diseno) <- c("PLOT", "REP", lbls)
#	labs = as.data.frame(matrix(c("Plot", "Block or repetition", lbls), nrow=1), strings.as.factors=F)
}
if (str_detect(design,"(RCBD)")){
	diseno = as.data.frame(matrix(NA, nrow=reps*length(matl), ncol=3), stringsAsFactors=F)
	diseno[,1:3]<- design.rcbd(matl, reps, serie=startn)$book
	colnames(diseno) <- c("PLOT", "REP", lbls)
	#colnames(diseno) <- c("Plot", "Block", abbreviate(inst[3]), abb)
#	labs = as.data.frame(matrix(c("Plot", "Block or repetition", lbls), nrow=1), strings.as.factors=F)
}
# if (str_detect(design,"(BIBD)")){
# 	bib <- design.bib(matl, bsize, number=startn)
# 	diseno = as.data.frame(matrix(NA, nrow=dim(bib)[1], ncol=3), stringsAsFactors=F)
# 	diseno[,1:3] <- bib
# 	colnames(diseno) <- c("PLOT", "REP", lbls)
# #	labs = as.data.frame(matrix(c("Plot", "Block or repetition", lbls), nrow=1), strings.as.factors=F)
# }
if (str_detect(design,"(LSD)")){
	diseno = as.data.frame(matrix(NA, nrow=length(matl)^2, ncol=4), stringsAsFactors=F)
	diseno[,1:4]<- design.lsd(matl, number=startn)
	colnames(diseno) <- c("PLOT", "REP", "CBLOCK", lbls)
#	labs = as.data.frame(matrix(c("Plot", "Row Block", "Column Block", lbls), nrow=1), strings.as.factors=F)
}
if (str_detect(design,"(F2CRD)")){
	nt <- length(matl)*length(adfl)
	tr <- 1:nt
	est <- cbind(1:nt, rep(adfl, length(matl)), rep(matl, each=length(adfl)))
	diseno = as.data.frame(matrix(NA, nrow=reps*nt, ncol=4), stringsAsFactors=F)
	fdcrd <- design.crd(tr, reps)$book
	diseno[,1:2] <- fdcrd[,1:2]
	ord <- fdcrd[,3]
	for (i in 1:(nt*reps)){
		diseno[i,3] <- est[est[,1]==ord[i],2]
		diseno[i,4] <- est[est[,1]==ord[i],3]
	}
	colnames(diseno) <- c("PLOT", "REP", adfn, lbls)
	colnames(diseno) <- c("PLOT", "REP", "FACTOR", lbls)  #cambiamos la etiqueta adfn por FACTOR para evitar erores
#	labs = as.data.frame(matrix(c("Plot", "Block or repetition", "Factor A", "Factor B", lab), nrow=1), strings.as.factors=F)
}

if (str_detect(design,"(F2RCBD)")){
	nt <- length(matl)*length(adfl)
	tr <- 1:nt
	est <- cbind(1:nt, rep(adfl, length(matl)), rep(matl, each=length(adfl)))
	diseno = as.data.frame(matrix(NA, nrow=reps*nt, ncol=4), stringsAsFactors=F)
	fdrcbd <- design.rcbd(tr, reps, number=startn)
	diseno[,1:2] <- fdrcbd[,1:2]
	ord <- fdrcbd[,3]
	for (i in 1:(nt*reps)){
		diseno[i,3] <- est[est[,1]==ord[i],2]
		diseno[i,4] <- est[est[,1]==ord[i],3]
	}
	colnames(diseno) <- c("PLOT", "REP", adfn, lbls)
	colnames(diseno) <- c("PLOT", "REP", "FACTOR", lbls)
#	labs = as.data.frame(matrix(c("Plot", "Block or repetition", "Factor A", "Factor B", lab), nrow=1), strings.as.factors=F)
}
if (str_detect(design,"(SPCRD)")){
	diseno = as.data.frame(matrix(NA, nrow=reps*length(matl)*length(adfl), ncol=4), stringsAsFactors=F)
	diseno[,1:4] <- design.split(adfl, matl, reps, "crd", number=startn)
	colnames(diseno) <- c("PLOT", "REP", adfn, lbls)
	colnames(diseno) <- c("PLOT", "REP", "FACTOR", lbls)
#	labs = as.data.frame(matrix(c("Plot", "Block or repetition", "Factor in Plots", "Factor in SubPlots", lab), nrow=1), strings.as.factors=F)
}
if (str_detect(design,"(SPRCBD)")){
	diseno = as.data.frame(matrix(NA, nrow=reps*length(matl)*length(adfl), ncol=4), stringsAsFactors=F)
	diseno[,1:4] <- design.split(adfl,matl,reps, "rcbd", number=startn)
   colnames(diseno) <- c("PLOT", "REP", adfn, lbls)
   colnames(diseno) <- c("PLOT", "REP","FACTOR",lbls)
#	labs = as.data.frame(matrix(c("Plot", "Block or repetition", "Factor in Plots", "Factor in SubPlots", lab), nrow=1), strings.as.factors=F)
}
if (str_detect(design,"(ABD)")){
	diseno = as.data.frame(matrix(NA, nrow=reps*length(checkl)+length(matl), ncol=3), stringsAsFactors=F)
	diseno[,1:3] <- design.dau(checkl, matl, reps, number=startn)
	colnames(diseno) <- c("PLOT", "REP", lbls)
}
if (str_detect(design,"(A01D)")){
	# GTDM-396
	diseno = as.data.frame(matrix(NA, nrow=reps*length(matl), ncol=4), stringsAsFactors=F)
	diseno[,1:4] <- design.alpha(matl, bsize, reps, number=startn)$book[,c(1,5,3,4)]
	colnames(diseno) <- c("PLOT", "REP", "BLOCK", lbls)
}
# if (str_detect(design,"(MBCRD)")){
# 	if(msite){ # use CRD design
# 		diseno = as.data.frame(matrix(NA, nrow=reps*length(matl), ncol=3), stringsAsFactors=F)
# 		diseno[,1:3] <- design.crd(matl, reps, number=startn)
# 		colnames(diseno) <- c("PLOT", "REP", lbls)
# 	}
# 	if(!msite){# use unreplicated non-randomized design
# 		diseno = as.data.frame(matrix(NA, nrow=length(matl), ncol=3), stringsAsFactors=F)
# 		diseno[,1:3] <- cbind(Plot=seq(startn, startn+length(matl)-1),Rep=rep(1,length(matl)),X=matl)
# 		colnames(diseno)[1:3] <- c("PLOT", "REP", lbls)
# 	}
# }
if (str_detect(design,"(STRIP)")){
  diseno = as.data.frame(matrix(NA, nrow=reps*length(matl)*length(adfl), ncol=4), stringsAsFactors=F)
  diseno[,1:4] <- design.strip(adfl, matl, reps, number=startn)
  colnames(diseno) <- c("PLOT", "REP", adfn, lbls)
  colnames(diseno) <- c("PLOT", "REP", "FACTOR", lbls)
  #	labs = as.data.frame(matrix(c("Plot", "Block or repetition", "Factor in Plots", "Factor in SubPlots", lab), nrow=1), strings.as.factors=F)
}



diseno

}

# getMaterialList = function(fpath, id){
# 	data = read.xlsx2(fpath, sheetName="GermplasmList")
# 	matl = unique(as.character(data[,id]))
# 	if(length(matl)!=nrow(data)) matl=NULL
# 	matl
# }



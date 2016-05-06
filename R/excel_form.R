# create excel template for sweetpotato

format_excel_from <- function(fb, crop = "sweetpotato", template_type = "CloneSelector"){
  if(template_type == "CloneSelector"){
    STOCKID = rep(NA, nrow(fb))
    fb = cbind(fb, STOCKID)
    fb = fb[, c("REP", "BLOCK", "PLOT", "ENTRY","STOCKID", "GENOTYPE") ]
  }
  fb
}

export_frm <- function(frm, tpl_id, out = "out.xls", sheet = "Fieldbook",
                       start_row = 25,
                       start_col = 2){
  file.copy(tt, out, overwrite = TRUE)
  #frm = attr(frm, "params") = NULL
  #frm = attr(frm, "sketch") = NULL
  wb <- openxslx::loadWorkbook(out)
  #x <- mtcars[1:4,]
  openxlsx::writeData(wb, frm, sheet = "Fieldbook", startRow = 25, startCol = 2,
                      rowNames = F)
  openxlsx::saveWorkbook(wb, file = out)
}

###############################################
# get germplasm list
gl = system.file("example/sweetpotato-list1.xlsx", package = "fbdesign")
gl = readxl::read_excel(gl)

# get a design
fd = design_fieldbook(trt1 = gl$Name, r = 3, k = 1, trt1_label = "GENOTYPE")

# get template for crop
tt = system.file("templates/SPYL-template.xls", package = "fbdesign")
frm = format_excel_from(fd)

# combine

#' UI material list paramters
#'
#' An interface to parameters
#'
#' @param name character
#' @author Reinhard Simon
#' @export
ui_fieldbook_params <- function(name = "phenotype_fieldbook_design"){
  shiny::conditionalPanel(
    paste0("input.menu == '",name,"'"),

    shiny::HTML("<center>"),
    #shiny::uiOutput("fbDesign_crop", inline = TRUE),
    shiny::actionButton("butNewFieldbook", "New fieldbook", inline = TRUE),
    shiny::HTML("</center>")
  )
}


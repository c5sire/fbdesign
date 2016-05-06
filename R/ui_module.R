
design_choices <- c(
  "Randomized Complete Block Design (RCBD)" = "RCBD"#,
  #"Completely Randomized Design (CRD)" = "CRD",
  #"Latin Square Design (LSD)" = "LSD",
  ##"Split-plot Design (SPPD)" = "SPPD",
  ##"Strip-plot Design (STPD)" = "STPD",
  ##"Augmented Block Design (ABD)" = "ABD",
  # "Balanced Incomplete Block Design (BIBD)" = "BIBD",
  # "Graeco-Latin Design (GLD)" = "GLD",
  # "Youden Design (YD)" = "YD",
  # "Cyclic Design (CD)" = "CD",
  # "Lattice Design (LD)" = "LD" ,
  # "Alpha Design (AD)" = "AD"#,
  # #"Augmented Partially Replicated Design (APRD)" = "APRD",
  # #"Factorial Design (F2SPPD)" = "F2SPPD",
  # #"North Carolina Design I" = "NCI",
  # #"North Carolina Design II" = "NCII",
  # #"North Carolina Design III" = "NCIII"
)

design_conditional_panels <- function(){
  list(
    shiny::conditionalPanel(
      "input.designFieldbook == 'RCBD' |
      input.designFieldbook == 'CRD' |
      input.designFieldbook == 'DAU' |
      input.designFieldbook == 'STPD'
      ", # TODO: ab - factorial, split
      shiny::selectInput("designFieldbook_r", "Replications (r):", 1:5, 2 )
    ),
    shiny::conditionalPanel(
      "input.designFieldbook == 'LD'", # TODO: ab - factorial, split
      shiny::selectInput("designFieldbook_r", "Replications (r):", 2:3, 2 )
    ),
    shiny::conditionalPanel(
      "input.designFieldbook == 'BIBD'",
      #shiny::selectInput("designFieldbook_r", "Replications (r):", c(3,6,9,12,15,18), 3 ),
      shiny::selectInput("designFieldbook_k", "Block size (k):", 3:12, 3 )
      ,
      shiny::selectInput("designFieldbook_maxR", "Repetition maximum (k):", 3:30, 20 )
    ) ,
    shiny::conditionalPanel(
      "input.designFieldbook == 'AD'",
      # TODO do server side checking of permitted combinations (based on number of treatments)
      # trt = k * s
      # s = number of blocks
      # k = size of block (max is trt)
      #
      # r = 2: k <= s
      # r = 3: s odd; k <= s
      # r = 3: s even; k <= (s - 1)
      # r = 4: s odd but not multiple of 3; k <= s
      shiny::selectInput("designFieldbook_r", "Replications (r):", 2:4, 2 ),
      shiny::selectInput("designFieldbook_k", "Block size (k):", 3:300,3 )
    ),
    shiny::conditionalPanel(
      "input.designFieldbook == 'CD'",
      # TODO do server side checking of permitted combinations (based on number of treatments)
      # number of treatments 6:30
      shiny::selectInput("designFieldbook_r", "Replications (r):", 2:10, 2 ),
      shiny::selectInput("designFieldbook_k", "Block size (k):", 2:10,3 )
    ),

    shiny::conditionalPanel(
      "input.designFieldbook == 'LSD' |
      input.designFieldbook == 'RCBD' |
      input.designFieldbook == 'SPPD' |
      input.designFieldbook == 'BIBD'
      ",
      shiny::checkboxInput("designFieldbook_first", "Randomize first repetition", TRUE )
    )
    ,
    shiny::conditionalPanel(
      "input.designFieldbook == 'RCBD'",
      shiny::checkboxInput("designFieldbook_cont", "Continuous numbering of plots", FALSE)
    )



  )
}


designDialog <- function(){
  shinyBS::bsModal("bsModalFbDesignParam", "Design fieldbook",

                   #paste0("input.menu == '",name,"'"),
                   "butNewFieldbook", size = "large",
                   # following line to allow datepicker in modal dialog:
                   # http://stackoverflow.com/questions/31920327/r-shinybs-model-and-dateinput
                   shiny::tags$style( type = "text/css", ".datepicker{z-index: 1100 !important;}"),
                   # use shinyjs & toggle commnand to hide/unhide tabpanels!
                   shinyjs::useShinyjs(),
                   shiny::wellPanel(


                     shiny::HTML("<b>Fieldbook identifiers:</b>"),
                     shiny::textOutput("fbDesign_id")
                   ),
                   shiny::tabsetPanel( id = "fbDesignNav",
                                       shiny::tabPanel("Crop", value = "crop", icon = shiny::icon("leaf"),
                                                shiny::uiOutput("fbDesign_crop", inline = TRUE),
                                                # Breeding program
                                                shiny::uiOutput("fbDesign_program", inline = TRUE),
                                                # Breeding phase
                                                shiny::uiOutput("fbDesign_phase", inline = TRUE)
                                                #shiny::uiOutput("fbDesign_factor2", inline = TRUE),
                                                #shiny::uiOutput("fbDesign_variables", inline = TRUE)

                                       ),
                                       shiny::tabPanel("Project", value = "project", icon = shiny::icon("book"),
                                                #shiny::uiOutput("fbDesign_project", inline = TRUE),
                                                shiny::textInput("fbDesign_project_name", "Project name"),
                                                shiny::textInput("fbDesign_project_objective", "Project objective"),
                                                #shiny::textInput("fbDesign_comments", "Project comments"),
                                                shiny::dateRangeInput("fbDesign_project_time_line", "Date range")

                                       ),
                                       shiny::tabPanel("Plants", value = "plants", icon = shiny::icon("star"),
                                                shiny::uiOutput("designFieldbook_genotypes", inline = TRUE)
                                       ),
                                       shiny::tabPanel("Statistical design", value = "design", icon = shiny::icon("pie-chart"),
                                                       shiny::selectInput("designFieldbook", "Design method:", design_choices, multiple = FALSE),
                                                       shiny::checkboxInput("designFieldbook_random", "Use randomization", TRUE),
                                                design_conditional_panels()
                                                # 2nd factor from ontology
                                                # 2nd genotype list of checks
                                                # genetic designs
                                                # check parameter combinations
                                       ),
                                       shiny::tabPanel("Labeling", value = "labeling",icon = shiny::icon("tags"),
                                                       shiny::checkboxInput("designFieldbook_zigzag", "Zigzag", TRUE),
                                                       shiny::radioButtons("designFieldbook_serie", "Label series:",
                                                             #get_series_labels(), "101, 102, ...", #get_series_labels()[[2]],
                                                             1:3, 2,
                                                             inline = TRUE)
                                                ,
                                                shiny::conditionalPanel(
                                                  "input.designFieldbook == 'RCBD'",
                                                  shiny::checkboxInput("designFieldbook_cont", "Continuous numbering of plots", FALSE)
                                                )

                                       ),

                                       shiny::tabPanel("Environment", value = 'environment', icon = shiny::icon("recycle"),
                                                # Year
                                                shiny::selectInput("designFieldbook_year", "Year of sowing/planting", 1971:2020, 2015),
                                                # Planting month
                                                shiny::selectInput("designFieldbook_month", "Month of sowing/planting", 1:12),
                                                # Location
                                                shiny::uiOutput("fbDesign_countrySite", inline = TRUE, width = 500),
                                                shiny::radioButtons("fbDesign_environment_type", "Environment type",
                                                                    list("Field" = "field"
                                                                         #,
                                                                         # "Farmers field" = "farmers_field"
                                                                         # ,
                                                                         # "Greenhouse" = "greenhouse",
                                                                         # "Screenhouse" = "screenhouse"
                                                                    ), inline = TRUE
                                                ),
                                                shiny::checkboxInput("fbDesign_weather_cb", "Register weather data"),
                                                shiny::checkboxInput("fbDesign_soil_cb", "Register soil data")
                                       ),
                                       shiny::tabPanel("Field", value = "fbDesign_field", icon = shiny::icon("male"),
                                          shiny::numericInput("fbDesign_field_size (ha)",
                                                              "Field size", 1, 1, 100)
                                       ),
                                       shiny::tabPanel("Farmers field", value = "fbDesign_farmers_field"
                                       ),
                                       # shiny::tabPanel("Greenhouse", value = "fbDesign_greenhouse"
                                       # ),
                                       # shiny::tabPanel("Screenhouse", value = "fbDesign_screenhouse"
                                       # ),
                                       shiny::tabPanel("Planting", value = "fbDesign_planting",
                                          shiny::numericInput("fbDesign_nplants",
                                                              "Number of plants per plot", 30, 1, 100),
                                          shiny::numericInput("fbDesign_nplantsrow",
                                                              "Number of plants per row", 30, 1, 100),
                                          shiny::numericInput("fbDesign_psize",
                                                              "Plot size", 30, 1, 100),
                                          shiny::numericInput("fbDesign_distPlants",
                                                              "Distance between plants", .3, .1, 1),
                                          shiny::numericInput("fbDesign_distRows",
                                                              "Distance between rows", .3, .1, 1)
                                       ),
                                       # shiny::tabPanel("Weather", value = "fbDesign_weather"
                                       # ),
                                       # shiny::tabPanel("Soil", value = "fbDesign_soil"
                                       # ),
                                       shiny::tabPanel("Book", value = "fbDesign_book",
                                                       rhandsontable::rHandsontableOutput("fbDesign_table")
                                       )
                   ),
                   shinyBS::bsButton("fbDesign_draft", "Update draft (see tab: book)" ),
                   shinyBS::bsButton("fbDesign_create", "Add empty fieldbook"),
                   shinyBS::bsAlert("alert_fb_done")
                   # ,
                   # shiny::submitButton()
  )
  # shiny::textOutput("new_list_success"),
  # shiny::actionButton("doListButton", "Create new list!")

}

#' shiny UI element
#'
#' returns a re-usable user interface element
#'
#' @author Reinhard Simon
#' @param type of ui Element; default is a tab in a shiny dashboard
#' @param title display title
#' @param name a reference name
#' @param output name of output element
#' @export
ui_fieldbook <- function(type = "tab", title = "Design field book",
                         name = "phenotype_fieldbook_design",
                         output = "hot_fieldbook_design"){
  shinydashboard::tabItem(tabName = name,
                          shiny::fluidRow(
                            shinydashboard::box( height = 600, width = 1200,
                                                 title = title,
                                                 rhandsontable::rHandsontableOutput(output, height = 600, width = 1200)
                            )
                          ),
                          designDialog()

  )
}

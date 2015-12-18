# obsolote as of Dec. 18th, 2015

library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyFiles)
library(rhandsontable)
library(shinyBS)

ui_fieldbook_params <- function(name = "phenotype_fieldbook_design"){
  shiny::conditionalPanel(
    paste0("input.menu == '",name,"'"),

    shiny::HTML("<center>"),
    #shiny::uiOutput("fbDesign_crop", inline = TRUE),
    shiny::actionButton("butNewFieldbook", "New fieldbook", inline = TRUE),
    shiny::HTML("</center>")
  )
}


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
         YD = agricolae::design.youden(trt, r, series, first = first,
                                       randomization = random),
         LD = agricolae::design.lattice(trt1, r, series, randomization = random)
         )
  fb
}


design_choices <- c(
   "Randomized Complete Block Design (RCBD)" = "RCBD",
   "Completely Randomized Design (CRD)" = "CRD",
   "Latin Square Design (LSD)" = "LSD",
   #"Split-plot Design (SPPD)" = "SPPD",
   #"Strip-plot Design (STPD)" = "STPD",
   #"Augmented Block Design (ABD)" = "ABD",
   #"Balanced Incomplete Block Design (BIBD)" = "BIBD",
   "Graeco-Latin Design (GLD)" = "GLD",
   "Youden Design (YD)" = "YD",
   #"Cyclic Design (CD)" = "CD",
   "Lattice Design (LD)" = "LD" #,
   #"Alpha Design (AD)" = "AD",
   #"Augmented Partially Replicated Design (APRD)" = "APRD",
   #"Factorial Design (F2SPPD)" = "F2SPPD",
   #"North Carolina Design I" = "NCI",
   #"North Carolina Design II" = "NCII",
   #"North Carolina Design III" = "NCIII"
)

design_conditional_panels <- function(){
list(
  shiny::conditionalPanel(
    "input.designFieldbook == 'RCBD' |
     input.designFieldbook == 'CRD' |
     input.designFieldbook == 'DAU' |
     input.designFieldbook == 'STPD'
    ", # TODO: ab - factorial, split
      selectInput("designFieldbook_r", "Replications (r):", 1:5, 2 )
  ),
  shiny::conditionalPanel(
    "input.designFieldbook == 'LD'", # TODO: ab - factorial, split
    selectInput("designFieldbook_r", "Replications:", 2:3, 2 )
  ),
  shiny::conditionalPanel(
    "input.designFieldbook == 'BIBD'",
    selectInput("designFieldbook_r", "Replications (r):", c(3,6,9,12,15,18), 3 ),
    selectInput("designFieldbook_k", "Block size (k):", 3:300, 3 ),
    selectInput("designFieldbook_maxR", "Repetition maximum (k):", 3:30, 20 )
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
    selectInput("designFieldbook_r", "Replications (r):", 2:4, 2 ),
    selectInput("designFieldbook_k", "Block size (k):", 3:300,3 )
  ),
  shiny::conditionalPanel(
    "input.designFieldbook == 'LSD' |
     input.designFieldbook == 'RCBD' |
     input.designFieldbook == 'SPPD' |
     input.designFieldbook == 'BIBD'
    ",
    checkboxInput("designFieldbook_first", "Randomize first repetition", TRUE )
  )
  # ,
  # shiny::conditionalPanel(
  #   "input.designFieldbook == 'RCBD'",
  #   checkboxInput("designFieldbook_cont", "Continuous numbering of plots", FALSE)
  # )



)
}


designDialog <- function(){
  shinyBS::bsModal("bsModalFbDesignParam", "Design fieldbook",

     #paste0("input.menu == '",name,"'"),
     "butNewFieldbook", size = "large",
     # following line to allow datepicker in modal dialog:
     # http://stackoverflow.com/questions/31920327/r-shinybs-model-and-dateinput
     tags$style( type = "text/css", ".datepicker{z-index: 1100 !important;}"),
     # use shinyjs & toggle commnand to hide/unhide tabpanels!
     shinyjs::useShinyjs(),
     shiny::wellPanel(


         shiny::HTML("<b>Fieldbook identifiers:</b>"),
         shiny::textOutput("fbDesign_id")
       ),
      shiny::tabsetPanel( id = "fbDesignNav",
        tabPanel("Crop", value = "crop",
         shiny::uiOutput("fbDesign_crop", inline = TRUE),
         # Breeding program
         shiny::uiOutput("fbDesign_program", inline = TRUE),
         # Breeding phase
         shiny::uiOutput("fbDesign_phase", inline = TRUE),
         shiny::uiOutput("fbDesign_factor2", inline = TRUE),
         shiny::uiOutput("fbDesign_variables", inline = TRUE)

         ),
        tabPanel("Project", value = "project",
         #shiny::uiOutput("fbDesign_project", inline = TRUE),
         shiny::textInput("fbDesign_project_name", "Project name"),
         shiny::textInput("fbDesign_project_objective", "Project objective"),
         #shiny::textInput("fbDesign_comments", "Project comments"),
         shiny::dateRangeInput("fbDesign_project_time_line", "Date range")

                 ),
        tabPanel("Plants", value = "plants", icon = shiny::icon("star"),
           shiny::uiOutput("designFieldbook_genotypes", inline = TRUE)
        ),
        tabPanel("Statistical design", value = "design",
                 selectInput("designFieldbook", "Design method:", design_choices, multiple = FALSE),
                 checkboxInput("designFieldbook_random", "Use randomization", TRUE),
                 design_conditional_panels()
                 # 2nd factor from ontology
                 # 2nd genotype list of checks
                 # genetic designs
                 # check parameter combinations
        ),
        tabPanel("Labeling", value = "labeling",
                 checkboxInput("designFieldbook_zigzag", "Zigzag", TRUE),
                 radioButtons("designFieldbook_serie", "Label series:",
                              #get_series_labels(), "101, 102, ...", #get_series_labels()[[2]],
                              1:3, 2,
                              inline = TRUE)
                 ,
                 shiny::conditionalPanel(
                   "input.designFieldbook == 'RCBD'",
                   checkboxInput("designFieldbook_cont", "Continuous numbering of plots", FALSE)
                 )

        ),

        tabPanel("Environment", value = 'environment',
         # Year
         shiny::selectInput("designFieldbook_year", "Year of sowing/planting", 1971:2020, 2015),
         # Planting month
         shiny::selectInput("designFieldbook_month", "Month of sowing/planting", 1:12),
         # Location
         shiny::uiOutput("fbDesign_countrySite", inline = TRUE, width = 500),
         shiny::radioButtons("fbDesign_environment_type", "Environment type",
                             list("Field" = "field",
                                  "Farmers field" = "farmers_field",
                                  "Greenhouse" = "greenhouse",
                                  "Screenhouse" = "screenhouse"
                                    ), inline = TRUE
         ),
         shiny::checkboxInput("fbDesign_weather_cb", "Register weather data"),
         shiny::checkboxInput("fbDesign_soil_cb", "Register soil data")
        ),
        tabPanel("Field", value = "fbDesign_field"
        ),
        tabPanel("Farmers field", value = "fbDesign_farmers_field"
        ),
        tabPanel("Greenhouse", value = "fbDesign_greenhouse"
        ),
        tabPanel("Screenhouse", value = "fbDesign_screenhouse"
        ),
        tabPanel("Planting", value = "fbDesign_planting"
        ),
        tabPanel("Weather", value = "fbDesign_weather"
        ),
        tabPanel("Soil", value = "fbDesign_soil"
        )
      )
     )
     # shiny::textOutput("new_list_success"),
     # shiny::actionButton("doListButton", "Create new list!")

}


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

ui <- dashboardPage(skin = "yellow",

                    dashboardHeader(title = "Fieldbooks"
                    ),

                    dashboardSidebar(width = 300,
                       sidebarMenu(id = "menu",
                           menuItem("Fieldbook",
                              menuSubItem("New fieldbok", icon = shiny::icon("star"),
                                          tabName = "phenotype_fieldbook_design")
                              ,
                              ui_fieldbook_params()

                           )
                       )

                    ),
                    dashboardBody(
                      tabItems(
                        ui_fieldbook()
                        #designDialog()
                      )
                    )
)

# ui2 <- fluidPage(
#   sidebarLayout(
#     sidebarPanel(
#       actionButton("butNewFieldbook", "New Fieldbook")
#     ),
#
#     mainPanel(
#      designDialog()
#     )
#   )
# )

server <- function(input, output, session){
  #values = shiny::reactiveValues()
  output$fbDesign_crop <- shiny::renderUI({
    ct = fbcrops::get_crop_table()
    chc = as.list(ct$crop_name)
    nms = paste0(ct$crop_name, " (", ct$crop_id, ")")
    names(chc) = nms
    shiny::selectInput("designFieldbook_crop", "Crop", chc)
  })

  observe({
    shinyjs::toggle(condition = input$fbDesign_environment_type == "field",
                    selector = "#fbDesignNav li a[data-value=fbDesign_field]")
    shinyjs::toggle(condition = input$fbDesign_environment_type == "farmers_field",
                    selector = "#fbDesignNav li a[data-value=fbDesign_farmers_field]")
    shinyjs::toggle(condition = input$fbDesign_environment_type == "greenhouse",
                    selector = "#fbDesignNav li a[data-value=fbDesign_greenhouse]")
    shinyjs::toggle(condition = input$fbDesign_environment_type == "screenhouse",
                    selector = "#fbDesignNav li a[data-value=fbDesign_screenhouse]")
    shinyjs::toggle(condition = input$fbDesign_weather_cb,
                    selector = "#fbDesignNav li a[data-value=fbDesign_weather]")
    shinyjs::toggle(condition = input$fbDesign_soil_cb,
                    selector = "#fbDesignNav li a[data-value=fbDesign_soil]")
  })

  output$fbDesign_program <- shiny::renderUI({
    if(!is.null(input$designFieldbook_crop)){
      tbl = fbcrops::get_crop_table()
      crp = tbl[tbl$crop_name == input$designFieldbook_crop, "crop_id"]

      tbl = fbprogram::get_program_table()
      prg = tbl[tbl$crop_id == crp, ]

      if(nrow(prg) > 0 ){
        lbl = paste0(prg$program_name, " (", prg$program_id, ")" )
        chc = as.list(prg$program_id)
        names(chc) = lbl
        shiny::selectInput("designFieldbook_program", "Investigation", chc)
      }
    }
  })

  output$fbDesign_phase <- shiny::renderUI({
    if(!is.null(input$designFieldbook_crop)){
      tbl = fbcrops::get_crop_table()
      crp = tbl[tbl$crop_name == input$designFieldbook_crop, "crop_id"]

      tbl = fbprstages::get_program_stage_table()
      prg = tbl[tbl$crop_id == crp, ]

      if(nrow(prg) > 0 ){
        lbl = paste0(prg$program_stage_name, " (", prg$program_stage_id, ")" )
        chc = as.list(prg$program_stage_id)
        names(chc) = lbl
        shiny::selectInput("designFieldbook_phase", "Study", chc)
      }
    }
  })

  output$designFieldbook_genotypes <- shiny::renderUI({
    chc <- fbmaterials::list_material_lists(input$designFieldbook_crop, short = TRUE)
    shiny::selectInput("designFieldbook_trt1", "Genotype list", chc, width = 400)
  })

  output$fbDesign_id <- shiny::renderText({
    if(!is.null(input$designFieldbook_crop)){
      tbl = fbcrops::get_crop_table()
      crop_id = tbl[tbl$crop_name == input$designFieldbook_crop, "crop_id"]
      program_id = input$designFieldbook_program
      phase_id = input$designFieldbook_phase
      ayear = input$designFieldbook_year
      amonth = stringr::str_pad(input$designFieldbook_month, width = 2, side = "left",
                                pad = "0")
      sites = input$designFieldbook_sites
      out = paste0(crop_id, program_id, phase_id, ayear, amonth, "_", sites)
      paste(out, collapse = ", ")
    }
  })

  output$fbDesign_countrySite <- shiny::renderUI({
    locs =fbsites::get_site_table()
    if(nrow(locs) > 0 ){
      chc = locs$shortn
      shiny::selectizeInput("designFieldbook_sites", label = "Field locations:",
                            choices = chc, selected = 1,  multiple = TRUE)
    }
  })

  output$fbDesign_variables <- shiny::renderUI({
    crp <- input$designFieldbook_crop
    mdl <- fbmodule::list_modules(crp)
    shiny::selectInput("designFieldbook_module", label = "Assay (fieldbook module):",
                          choices = mdl, selected = 1)
  })


  # fbcrops::server_crop(input, output, session, values = values)
  # fbmaterials::server_material_list(input, output, session, values = values)
}

shinyApp(ui, server)


library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyFiles)
library(DT)
library(agricolae)
library(dplyr)
library(fbmet)
library(fbhelp)

design_choices = c("RCBD", "CRD", "ABD", "NRD")

design_conditional_panels <- function(){
  tagList(

    shiny::conditionalPanel(
      "input.designFieldbook != 'NRD'
      ",
    shiny::checkboxInput("designFieldbook_random", "Use randomization", TRUE)
    ),
    shiny::conditionalPanel(
      "input.designFieldbook == 'RCBD' |
      input.designFieldbook == 'CRD' |
      input.designFieldbook == 'DAU' |
      input.designFieldbook == 'STPD' |
      input.designFieldbook == 'ABD'
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
    # ,
    # shiny::conditionalPanel(
    #   "input.designFieldbook == 'RCBD'",
    #   shiny::checkboxInput("designFieldbook_cont", "Continuous numbering of plots", FALSE)
    # )

  )
}


fb_design <- function(){
  tabsetPanel( id = "fbDesignNav",
   shiny::tabPanel("Sources", value = "designLists", icon = shiny::icon("list"),
                   shinyFiles::shinyFilesButton('file', 'File select',
                                                'Please select a file', FALSE
                   ),
                   uiOutput("designFilepath")
   ),
  shiny::tabPanel("Plants", value = "plants", icon = shiny::icon("star"),
                  shiny::uiOutput("designFieldbook_genotypes1", inline = TRUE),
                  shiny::uiOutput("designFieldbook_genotypes2", inline = TRUE)
  ),
  shiny::tabPanel("Traits", value = "traits", icon = shiny::icon("star"),
                  shiny::uiOutput("designFieldbook_traits", inline = TRUE)
  ),
  shiny::tabPanel("Statistical design", value = "design", icon = shiny::icon("pie-chart"),
                  shiny::selectInput("designFieldbook", "Design method:", design_choices, multiple = FALSE),

                  design_conditional_panels()
                  # 2nd factor from ontology
                  # 2nd genotype list of checks
                  # genetic designs
                  # check parameter combinations
  ),
  shiny::tabPanel("Labeling", value = "labeling",icon = shiny::icon("tags"),
                  shiny::conditionalPanel(
                    "input.designFieldbook == 'RCBD' |
                     input.designFieldbook == 'ABD'",
                  shiny::checkboxInput("designFieldbook_zigzag", "Zigzag", TRUE)
                  ),
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
  shiny::tabPanel("Book", value = "designBook", icon = shiny::icon("table"),
                  sidebarPanel(width = 3,
                         downloadButton("exportExcel", "Export to Excel")
                         ),
                  mainPanel(width = 9,
                         DT::dataTableOutput("fieldbook")
                         )

  )
  )
}

shinyApp(
  ui = dashboardPage( skin = "yellow",
                      dashboardHeader(title = "Fieldbook designer"),
                      dashboardSidebar(disable = TRUE),
                      dashboardBody(
                        tabBox( width = 12, selected = "Design",
                                tabPanel("Design",
                                         fb_design()
                                ),
                                tabPanel("Help"
                                ),
                                tabPanel("About"
                                )
                        )
                      )
  ),


  server = function(input, output, session) {

    volumes <- getVolumes(c("(E:)", "Page File (F:)"))

    shinyFileChoose(input, 'file', roots=volumes, session=session,
                    filetypes=c( 'xlsx'))

    designFile <- reactive({
      mf = parseFilePaths(volumes, input$file)$datapath
      mf = as.character(mf)
      #print(mf)
      if(length(mf)==0) return("")
      mf
    })
    output$designFilepath <- renderPrint({designFile()})


    design_raw <- reactive({
      bks = designFile()
      if(length(bks)==0) return(NULL)
      gtLst1 = readxl::read_excel(bks, input$dfGt1)
      gtLst2 = readxl::read_excel(bks, input$dfGt2)
      trtLst = readxl::read_excel(bks, "Traits")
      #print(head(gtLst1))
      list(gtLst1, gtLst2, trtLst)
    })

    output$designFieldbook_genotypes1 <- renderUI({
      selectInput("dfGt1", "Genotype list 1", c("List1", "List2"), 1)
    })

    output$designFieldbook_genotypes2 <- renderUI({
      selectInput("dfGt2", "Genotype list 2", c("List1", "List2"), 2)
    })

    output$designFieldbook_traits <- renderUI({
      trts = design_raw()[[3]]$id
      selectizeInput("dfTrt", "Trait list", trts, selected = trts, multiple = TRUE)
    })

#observe({

    fieldbook <- reactive({
      #print(input$fbaInput)
      x = NULL
      withProgress(message = "Loading fieldbook ...",
                   detail = "This may take a while ...", value = 1, max = 4, {
                     try({
                       x <- design_raw()

                     })
                   })
      # print(x[1]$ID1)
      # print(x[2]$ID1)
      # print(x[3]$id)

      if(length(x)==0) return(NULL)
      fbdesign::design_fieldbook(input$designFieldbook,
                                 trt1 = x[[1]]$ID1,
                                 trt2 = x[[2]]$ID1,
                                 variables = input$dfTrt,
                                 r = as.integer(input$designFieldbook_r),
                                 k = as.integer(input$designFieldbook_k),
                                 series = as.integer(input$designFieldbook_serie),
                                 random = as.logical(input$designFieldbook_random),
                                 zigzag = as.logical(input$designFieldbook_zigzag),
                                 first = as.logical(input$designFieldbook_first),
                                 cont = as.logical(input$designFieldbook_cont)
      )

    })

    output$fieldbook <- DT::renderDataTable({
      fieldbook()

    },  server = FALSE,  #extensions = 'FixedColumns',

    selection = list(mode = 'single', target = 'column'),
    options = list(scrollX = TRUE ))
#})


    output$exportExcel <- downloadHandler("Fieldbook.xlsx", content = function(file){
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Fieldbook")
      openxlsx::writeData(wb,  sheet = "Fieldbook",fieldbook(), startRow = 1, startCol = 1,
                          rowNames = FALSE)
      openxlsx::saveWorkbook(wb, file )
    })

  }
)

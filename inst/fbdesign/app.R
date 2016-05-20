library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyFiles)
library(DT)
library(agricolae)
library(dplyr)
library(fbmet)
library(fbhelp)

# design_choices = list("RCBD" = "Randomized Complete Block Design",
#                    "CRD" = "Completely Randomized Design",
#                    "ABD" = "Augmented Block Design",
#                    "NRD" = "Non-Randomized Design")


design_choices = c("RCBD", "CRD", "ABD", "NRD")

design_choices = list("Randomized Complete Block Design" = "RCBD",
                      "Completely Randomized Design" = "CRD" ,
                      "Augmented Block Design" = "ABD" ,
                      "Non-Randomized Design" = "NRD" )


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
   # shiny::tabPanel("Sources", value = "designLists", icon = shiny::icon("list"),
   #                 shinyFiles::shinyFilesButton('file', 'File select',
   #                                              'Please select a file', FALSE
   #                 ),
   #                 uiOutput("designFilepath")
   # ),
   shiny::tabPanel("Statistical design", value = "design", icon = shiny::icon("bar-chart-o"),
                   shiny::selectInput("designFieldbook", "Design method:", design_choices, multiple = FALSE),

                   design_conditional_panels()
                   # 2nd factor from ontology
                   # 2nd genotype list of checks
                   # genetic designs
                   # check parameter combinations
   ),

  shiny::tabPanel("Plants", value = "plants", icon = shiny::icon("leaf"),
                  shiny::conditionalPanel(
                    "input.designFieldbook == 'ABD'",
                    shiny::uiOutput("designFieldbook_genotypes1", inline = TRUE)
                  ),
                  shiny::uiOutput("designFieldbook_genotypes2", inline = TRUE),
                  shinyFiles::shinyFilesButton('file', 'File select',
                                               'Please select a file', FALSE
                  )#,
                  #uiOutput("designFilepath")
  ),
  shiny::tabPanel("Treatments", value = "treatments", icon = shiny::icon("wrench")
  ),
  shiny::tabPanel("Traits", value = "traits", icon = shiny::icon("eye"),
                  fluidRow(
                    column(4,
                           shiny::uiOutput("designFieldbook_traits", inline = TRUE)
                           ),
                    column(8,
                           shiny::uiOutput("designFieldbook_traits_sub", inline = TRUE),
                           shiny::uiOutput("designFieldbook_traits_sub_conf", inline = TRUE),
                           rhandsontable::rHandsontableOutput("designFieldbook_traits_sub_table",
                                                              height = 300),
                           uiOutput("designFieldbook_traits_labels")

                           )
                  )

  ),
  shiny::tabPanel("Labeling", value = "labeling",icon = shiny::icon("tags"),
                  shiny::conditionalPanel(
                    "input.designFieldbook == 'RCBD' |
                     input.designFieldbook == 'ABD'",
                  shiny::checkboxInput("designFieldbook_zigzag", "Zigzag", TRUE)
                  ),
                  shiny::radioButtons("designFieldbook_serie", "Label series:",
                                      #get_series_labels(), "101, 102, ...", #get_series_labels()[[2]],
                                      1:3, 3,
                                      inline = TRUE)
                  ,
                  shiny::conditionalPanel(
                    "input.designFieldbook == 'RCBD'",
                    shiny::checkboxInput("designFieldbook_cont", "Continuous numbering of plots", FALSE)
                  )

  ),
  shiny::tabPanel("Book", value = "designBook", icon = shiny::icon("table"),
                  sidebarPanel(width = 3,
                         #downloadButton("exportExcel", "Export to Excel")
                         radioButtons("exportFieldbookFormat", "Export format of fieldbook",
                                      c("Excel", "Android Fieldbook App"),
                                      "Excel"),

                         shinyFiles::shinyDirButton("exportFieldbookDir", "Choose export directory",
                                                    "Export fieldbook"),
                         verbatimTextOutput('exportDirPath'),
                         actionButton("exportFBButton", "Export fieldbook(s) to a directory.")
                  ),
                  mainPanel(width = 9,
                         DT::dataTableOutput("fieldbookDT")
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

    #shinyDirChoose(input, 'exportDir', roots=volumes, session=session)
    shinyDirChoose(input, 'exportFieldbookDir', roots=volumes,
                   session=session, restrictions=system.file(package='base'))

    designFile <- reactive({
      req(input$file)
      mf = parseFilePaths(volumes, input$file)$datapath
      mf = as.character(mf)
      #print(mf)
      if(length(mf)==0) return("")
      mf
    })
    #output$designFilepath <- renderPrint({designFile()})

    # exportDir <- reactive({
    #   req(input$exportDir)
    #   edr = parseDirPath(volumes, input$exportDir)$datapath
    #   if(length(edr)==0) return("")
    #   edr
    # })


    design_raw <- reactive({
      bks = designFile()
      #print(bks)
      if(length(bks)==0) return(NULL)
      #print(input$dfGt1)
      gtLst2 = readxl::read_excel(bks, input$dfGt2)
      #print(gtLst1)
      gtLst1 = NULL
      if(!is.null(input$dfGt1)){
        gtLst1 = readxl::read_excel(bks, input$dfGt1)
      }

      #print(gtLst2)
      trtLst = readxl::read_excel(bks, "Traits")
      #print(trtLst)
      #print(head(gtLst1))
      list("List1" = gtLst2, "List2" = gtLst1, "Traits" = trtLst)
    })

    output$designFieldbook_genotypes1 <- renderUI({
      selectInput("dfGt1", "Genotypes: checks", c("List1", "List2"), 1)
    })

    output$designFieldbook_genotypes2 <- renderUI({
      selectInput("dfGt2", "Genotypes: new materials", c("List1", "List2"), 1)
    })

    output$designFieldbook_traits <- renderUI({
      #req(design_raw())
      trts = design_raw()[[3]]$id
      selectizeInput("dfTrt", "Trait list", trts, trts, multiple = TRUE)
    })

    output$designFieldbook_traits_sub <- renderUI({
      #req(design_raw())
      trts = design_raw()[[3]]$id
      selectizeInput("dfTrtSub", "Trait list for subsampling", input$dfTrt,  multiple = TRUE)
    })

    output$designFieldbook_traits_sub_conf <- renderUI({
      req(input$dfTrtSub)
      HTML("<h3>Configuration of sub-sampling</h3>")
    })

    output$designFieldbook_traits_sub_table <- rhandsontable::renderRHandsontable({
      req(input$dfTrtSub)
      n = length(input$dfTrtSub)
      DF = data.frame(trait = input$dfTrtSub,
                      use_timepoints = rep(TRUE,n),
                      n_timepoints = factor(rep(1, n), 1:1000),
                      use_subsamples = rep(FALSE, n),
                      n_subsamples = factor(rep(1, n), 1:100),
                      summarize_by = factor(rep("mean", n),
                                            c("mean", "mode", "min", "sum", "variance"))
                      )
      rhandsontable::rhandsontable(DF)
    })

    output$designFieldbook_traits_labels <- renderPrint({
      req(input$designFieldbook_traits_sub_table)
      #print(input$dfTrt)
      DF = rhandsontable::hot_to_r(input$designFieldbook_traits_sub_table)
      lbs = replace_vars_subsamples(input$dfTrt, DF)
      #print(lbs)
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
      #x <- design_raw()
      #print(str(x))
       # print(x[[1]]$ID1)
       # print(x[[2]]$ID1)
       # print(x[[3]]$id)

      if(length(x)==0) return(NULL)
      DF = NULL
      if(!is.null(input$designFieldbook_traits_sub_table)){
        DF = rhandsontable::hot_to_r(input$designFieldbook_traits_sub_table)
      }

      lbs = replace_vars_subsamples(input$dfTrt, DF)

      fb = fbdesign::design_fieldbook(input$designFieldbook,
                                 trt1 = x[[1]]$ID1,
                                 trt2 = x[[2]]$ID1,
                                 variables = lbs, #input$dfTrt,
                                 r = as.integer(input$designFieldbook_r),
                                 k = as.integer(input$designFieldbook_k),
                                 series = as.integer(input$designFieldbook_serie),
                                 random = as.logical(input$designFieldbook_random),
                                 zigzag = as.logical(input$designFieldbook_zigzag),
                                 first = as.logical(input$designFieldbook_first),
                                 cont = as.logical(input$designFieldbook_cont)
      )
      #print(str(fb))
      fb


    })

    output$fieldbookDT <- DT::renderDataTable({
      req(input$dfTrt)
      fieldbook()

    },  server = FALSE,  #extensions = 'FixedColumns',

    selection = list(mode = 'single', target = 'column'),
    options = list(scrollX = TRUE )
    )

    #})

    # observe({
    #   if(input$exportFBDir > 0){
    #     #output$exportDirPath <- renderText(function(){
    #       #list.files(choose.dir())
    #       print(choose.dir())
    #     }#)
    #   #}
    # })

    output$exportDirPath <- renderPrint({
      req(input$exportFieldbookDir)
      #volumes <- c('R Installation'=R.home())
      parseDirPath(volumes, input$exportFieldbookDir)
      })

    observeEvent(input$exportFBButton, {
      ed = parseDirPath(volumes, input$exportFieldbookDir)
      fn = file.path(ed, "fieldbook.xlsx")
      if(input$exportFieldbookFormat == "Excel"){
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Fieldbook")
        openxlsx::writeData(wb,  sheet = "Fieldbook",fieldbook(), startRow = 1, startCol = 1,
                            rowNames = FALSE)
        openxlsx::saveWorkbook(wb, fn, overwrite = TRUE )
      }

      if(input$exportFieldbookFormat == "Android Fieldbook App"){
        write.csv(fieldbook()[, c(1:5)], file.path(ed, "fieldbook.csv"), quote = FALSE, row.names = FALSE)
        trait_conf <- create_fbtrait_config(fieldbook(), design_raw()[[3]])
        write.csv(trait_conf, file.path(ed, "fieldbook.trt"), quote = FALSE, row.names = FALSE)
      }

    })

    # output$exportExcel <- downloadHandler("Fieldbook.xlsx", content = function(file){
    #   wb <- openxlsx::createWorkbook()
    #   openxlsx::addWorksheet(wb, "Fieldbook")
    #   openxlsx::writeData(wb,  sheet = "Fieldbook",fieldbook(), startRow = 1, startCol = 1,
    #                       rowNames = FALSE)
    #   openxlsx::saveWorkbook(wb, file )
    # })

    # observeEvent(input$exportFieldbookButton, {
    #   #print(exportDir())
    # })

  }
)

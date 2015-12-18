
tabNameS <- "resource_fieldbook_design"


server <- function(input, output, session) {
  values = shiny::reactiveValues()
  fbdesign::server_design(input, output, session, values = values)
}


ui <- shinydashboard::dashboardPage(skin = "yellow",

     shinydashboard::dashboardHeader(title = "Fieldbooks"
     ),

     shinydashboard::dashboardSidebar(width = 300,
       shinydashboard::sidebarMenu(id = "menu",
        shinydashboard::menuItem("Fieldbook",
          shinydashboard::menuSubItem("New fieldbok", icon = shiny::icon("star"),
                             tabName = "phenotype_fieldbook_design")
          ,
          fbdesign::ui_fieldbook_params()

        )
       )

     ),
     shinydashboard::dashboardBody(
       shinydashboard::tabItems(
         fbdesign::ui_fieldbook()
       )
     )
)

shinyApp(ui = ui, server = server)

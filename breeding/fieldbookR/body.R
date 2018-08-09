library(tableHTML)
library(shinyjs)
library(shinyWidgets)

body <- dashboardBody(
  useShinyjs(),
  navbarPage(position = 'static-top', title = 'Field Book app data',
             
             tabPanel(title = "Data loading", 
                      uiOutput('dataload'),
                      box(
                        verbatimTextOutput('summary',placeholder = T))
                      
                      ),
             tabPanel(title = "Summary",
                      tableOutput('message'))
  
             # #tabPanel(title = "Data Editor",
             #          actionButton("edit", "Edit Values"),
             #          
             #          hotable("hottable")
             #          
             #          
             # )#,
             # tabPanel(title = "Chi Squared data",
             #          imageOutput("myImage"))
  

  
  
  ))
library(pacman)
p_load(shiny, shiny, dplyr, mongolite, tableHTML, shinyjs, DBI, pool, shinyalert, data.table)

reg <- "(.*)?(Fieldbook_.*_\\d{4}).*photos/(.*.jpg)"
reg1 <- "(.*)?(Fieldbook_.*_\\d{4}).*"

source("getData.R")

shinyServer(function(input, output, session) {
  #------------Data Loading ------------
  values <- reactiveValues(df = NULL)

  observeEvent(input$connect, {
    values$df <- loadData(col = input$col, dbase = input$db)
  })

  observeEvent(input$file, {
    values$df <- read.csv(file = input$file$datapath, header = T, sep = ",", na.strings = "", stringsAsFactors = F)

    values$df <- values$df %>%
      mutate(
        table_name = paste0(
          gsub(
            pattern = reg1,
            replacement = "\\2",
            input$file$name
          )
        )
      )
    saveData(values$df, db = input$db, col = "Breeding_Raw")
  })

  #---------Summary Printout-------------
  observeEvent(
    input$file,
    output$summary <- renderPrint({
      if (is.null(input$file)) {
        return()
      }
      str(values$df)
    })
  )
  observeEvent(input$connect, renderPrint({
    str(values$df)
  }))


  output$dataload <- renderUI({
    mydata <- values$df
    selectInput("dropvars", "Variables to drop",
      multiple = T,
      selected = names(mydata), choices = names(mydata)
    )
    # selectInput('table', 'Trial Name',choices = c('-',tables))
  })
  output$message <- renderTable({
    up_tables <- values$df$table_name
    table(`Available Fieldbooks` = up_tables) %>% data.table() %>% setnames(c("Available Books", "Rows"))
  })
  
})

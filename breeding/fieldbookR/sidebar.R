library(shinyWidgets)

sidebar <- dashboardSidebar(
  
  textInputAddon('db',label = 'Database',placeholder = 'Database',value = 'shinydatabase',addon = icon('at')),
  textInputAddon("col", "Table",placeholder = 'Database collection name',value = 'breeding', addon = icon('at')),
    actionBttn("connect",size = 'lg',label = 'Connect!'),

    fileInput(
      "file", 
      "Choose CSV File",
      multiple = F,
      accept = c("text/csv","text/comma-separated-values,text/plain",".csv")))
  
  
  
  

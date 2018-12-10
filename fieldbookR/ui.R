
library(shiny)
library(shinydashboard)
library(shinysky)

source('header.R')
source('body.R')
source('sidebar.R')

dashboardPage(
  header,
  sidebar,
  body
)

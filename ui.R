library(shiny)
library(shinydashboard)
library(plotly)

# Load the header, sidebar and body of the Shiny Dashboard
source("dashboard-header.R")
source("dashboard-sidebar.R")
source("dashboard-body.R")


shinyUI(
  dashboardPage(
    header,
    sidebar,
    body
  )
)

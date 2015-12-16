library(shiny)
library(shinydashboard)
source("./dashboard-header.R")
source("./dashboard-sidebar")
source("./dashboard-body.R")




shinyUI(
  dashboardPage(
    header,
    sidebar,
    body
  )
)

  
  fluidPage(
  
  
  
  titlePanel("Predicting poor glycemic control from blood glucose"),
  
  sidebarLayout(
    
    
    sidebarPanel(
      
      

    ),
    
    mainPanel(
      conditionalPanel(condition="input.proceed == false", 
                       plotOutput("linearPlot"),
                       includeHTML("./landing_page.html"),
                       checkboxInput(inputId = "proceed", 
                                     label = "Ready to start?", 
                                     value = FALSE, width = NULL)
                       
                       ),
      conditionalPanel(condition="input.proceed == true",
                       plotOutput("logisticPlot")
      )
      #       plotOutput("rocPlot")
    )
  )
))

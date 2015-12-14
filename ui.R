library(plotly)

shinyUI(fluidPage(
  
  titlePanel("Predicting poor glycemic control from blood glucose"),
  
  sidebarLayout(
    
    
    sidebarPanel(
      
      
      conditionalPanel(condition = "input.proceed == true",
                       sliderInput(inputId = "fbg", "Fasting Blood Glucose",  
                                   min = 2, max = 22, value = 7.4, step = 0.1),
                       checkboxInput(inputId = "varycut", 
                                     label = "Vary the HbA1c cut-point?", 
                                     value = FALSE, width = NULL),
                       conditionalPanel(condition = "input.varycut == true",
                       sliderInput(inputId = "cutpoint", "HbA1c cut-off",  
                                   min = 40, max = 100, value = 69.4, step = 0.1))
      )
    ),
    
    mainPanel(
      conditionalPanel(condition="input.proceed == false", 
                       plotlyOutput("linearPlot"),
                       includeHTML("./landing_page.html"),
                       checkboxInput(inputId = "proceed", 
                                     label = "Ready to start?", 
                                     value = FALSE, width = NULL)
                       
                       ),
      conditionalPanel(condition="input.proceed == true",
                       plotlyOutput("logisticPlot")
      )
      #       plotOutput("rocPlot")
    )
  )
))

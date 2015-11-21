shinyUI(fluidPage(
  
  titlePanel("Predicting poor glycemic control from blood glucose"),
  
  sidebarLayout(
    
              
    sidebarPanel(
      
      checkboxInput(inputId = "proceed", label = "Ready to start?", value = FALSE, width = NULL),
      
      conditionalPanel(condition = "input.proceed == true",
                       sliderInput(inputId = "fbg", "Fasting Plasma Blood Glucose",  
                                   min = 2, max = 22, value = 7.4, step = 0.1)
      )
    ),
    
    mainPanel(
      conditionalPanel(condition="input.proceed == false", includeHTML("./landing_page.html"))
#       plotOutput("logisticPlot"),
#       plotOutput("rocPlot")
    )
  )
))

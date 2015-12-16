source("quick-description.R")

body <- dashboardBody(
  fluidRow(
    tabBox( id = 'tabvals',
            tabPanel("Description",
                     quick_description, value=1
            ),
            
            tabPanel("Scatterplot",
                     "Relationship between blood glucose and HbA1c",br(),
                     plotlyOutput("linearPlot"), value=2
            ),
            
            tabPanel("Logistic",
                     "Relationship between blood glucose and PGC",br(),
                     plotlyOutput("logisticPlot"), value=3
            ),
            
            tabPanel("About",
                     HTML("<p><b>The NHANES 2011-2012</b> public use data sets were used for this analysis. NHANES
                      is a multistage stratified, clustered probability sample of the civilian non-
                      institutionalized population of the USA. The total sample in NHANES 2011-2012
                      was 9,756. In this analysis, only participants were included who were aged 12
                      years and older, assessed in the morning examination session, contributed 
                      a valid blood sample for the measurement of blood glucose and HbA1C, 
                      and self identified as having diabetes. Self identification was 
                      based on a 'yes' response to the interview question, 'other than during 
                      pregnancy, have you ever been told by a doctor or health professional that
                      you have diabetes or sugar diabetes?' The final sample was n = 333; 175 
                      males and 158 females. The complex survey design was not accounted for here.</p>
                      <p>Further details of the NHANES 2011-2012 can be found 
                     <a href='http://wwwn.cdc.gov/nchs/nhanes/search/nhanes11_12.aspx'><b>here
                     </b></a>.</p>"),
                     br(), value=4
            )
            
            
    ),
    conditionalPanel(
      condition = "input$tabvals == 3",
      box(
        title = "ROC Curve", status = "primary",
        "Using blood glucose (mmol/l) as a predictor of PGC",
        plotlyOutput("rocPlot")
      )
    )
  ),
  fluidRow(
    
    box(
      title = "Classification matrix", status = "primary",
      "Poorly controlled diabetes predicted by blood glucose",
      uiOutput("sensspecTable")
    ),
    box(
      title = "Inputs", status = "warning",
      "Vary the blood glucose level used to predict PGC",        
      sliderInput(inputId = "fbg",  "mmol/l",
                  min = 2, max = 22, value = 7.4, step = 0.1), 
      br(),
      "Vary the HbA1c 'Gold Standard' cut-off for classifying PGC",        
      sliderInput(inputId = "cutpoint",  "mmol/mol",
                  min = 40, max = 100, value = 69.4, step = 0.1))
  )
)
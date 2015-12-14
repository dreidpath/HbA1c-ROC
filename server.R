library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

load("data.RData")


shinyServer(function(input, output, session){  
  
  plotData <- reactive({ 
    # Take the NHANES blood glucose and HbA1c data and add two columns to it
    # newdata$logistic is the adjusted  HbA1c > x cutpoint based on input$cutpoint
    # newdata$confmat is the confusion matrix output based on the new cutpoint
    #
    print(input$varycut)
    newdata <- bgDF
    if(input$varycut==F){
      newdata$logistic <- as.numeric(newdata$mmolmol>=69.4)
    }
    if(input$varycut==T){
    newdata$logistic <- as.numeric(newdata$mmolmol>=input$cutpoint)
    }
    newdata$confmat <- NA
    newdata$confmat[newdata$logistic == 0 & newdata$lbdglusi < input$fbg] <- 1  # TN
    newdata$confmat[newdata$logistic == 1 & newdata$lbdglusi < input$fbg] <- 2  # FP
    newdata$confmat[newdata$logistic == 0 & newdata$lbdglusi > input$fbg] <- 3  # FN
    newdata$confmat[newdata$logistic == 1 & newdata$lbdglusi > input$fbg] <- 4  # TP
    newdata$confmat <- factor(newdata$confmat,
                                 levels = 1:4,
                                 labels = c("TN", "FP", "FN", "TP"))
    
    return(newdata)
  })

  
  logModel <- reactive({ 
    # run and return the logistic regression model with the HbA1c > x as the dichotomous
    # dependent variable (i.e., newdata$logistic), and the blood glucose (lbdglusi) as
    # the predictor.
    #
    plotdata <- plotData()
    logmodel <- glm(logistic ~ lbdglusi, data=plotdata, family = "quasibinomial")
    return(logmodel)
  })
  
  
  
  confusionMatrix <- reactive({
     # Set up the confusion matrix here
  })
  
  
  linearPlot <- reactive({ 
    # Plot of the linear relationshipo between blood glucose and HbA1c
    #  i.e., x=lbdglusi, y=mmolmol
    #
    plotdata <- plotData()
    fig1 <- ggplot(data = plotdata, aes(x=lbdglusi, y=mmolmol)) +
              ggtitle("Relationship between Blood Glucose and HbA1c") +
              xlab("Blood Glucose (mmol/L)") + 
              ylab('HbA1c (mmol/mol)') +
              geom_point(shape=1) +    # Use hollow circles
              geom_smooth(method=lm)   # Add linear regression line
    fig1 <- ggplotly(fig1)  # Convert it to a ploty output
    return(fig1)
  })
  
  
  basePlot <- reactive({ 
    # A plot of the logistic model of (HbA1c > cutpoint) ~ Bloog Glucose
    #
    plotdata <- plotData()  # The reactive data output based on varying cutpoints
    
    logmodel <- logModel()  # The logistic regression model
    
    logistic_curve <- function(x){
      # A function to output the predicted (y-)values for a logistic curve
      # given a logistic model and a set of x-values
      #
      invlogit(logmodel$coef[2] * x + logmodel$coef[1])  # HbA1c >= input$cutpoint
    }
    
    set.seed(1)  # Try to keep the jitter the same each time ... but its not working
    
    # Start the creation of the layers in ggplot
    fig2 <- ggplot(data=plotdata, aes(x=lbdglusi, y=logistic))
    fig2 <- fig2 + geom_point(position = position_jitter(h=0.05), aes(color=confmat), shape=21)
    fig2 <- fig2 + scale_color_discrete(name="Binary Classification",
                                        labels=c("True Negative", "False Positive", "False Negative", "True Positive"))
    fig2 <- fig2 + scale_size(guide=FALSE)
    fig2 <- fig2 + xlab("Blood Glucose (mmol/L)") + ylab(paste('Pr(HbA1c)',' > ', toString(input$cutpoint),  'mmol/mol'))
    fig2 <- fig2 + stat_function(fun = logistic_curve, color="Blue")  # Add the logistic curve
    
    # Add a single point on to the logistic curve
    tmpdata <- data.frame(bg = input$fbg, prob = logistic_curve(input$fbg) )
    fig2 <- fig2 + geom_point(data = tmpdata, aes(x=bg, y=prob), color="Red", size=5) 
    # Add a vertical line
    fig2 <- fig2 + geom_vline(xintercept = tmpdata$bg, color="Red", 
                              linetype = "dotted", size=0.3)
    fig2 <- ggplotly(fig2)
    return(fig2)
    
  })  
  
  
  output$linearPlot <- renderPlotly({
    # Render the linear plot of HbA1c ~ Blood glucose 
    p <- linearPlot()
    p
  })
  
  
  output$logisticPlot <- renderPlotly({
    # Render the logistic curve
    p <- basePlot()
    p
    
  })
  
  
  output$sensspecTable <- renderTable({  #This may not be the correct render
    # Output the sensitivity and specificity here, and the confusion matrix
  })
  
  
})
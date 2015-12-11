library(shiny)
library(ggplot2)
library(dplyr)

load("data.RData")


shinyServer(function(input, output, session){  
  
  logModel <- reactive({ 
    plotdata <- plotData()
    logmodel <- glm(logistic ~ lbdglusi, data=plotdata, family = "quasibinomial")
    return(logmodel)
  })
  
  plotData <- reactive({ 
    # Make this reactive to future proof adjustments
    newdata <- bgDF
    newdata$logistic <- as.numeric(newdata$mmolmol>=input$cutpoint)
    newdata$confmat <- NA
    newdata$confmat[newdata$logistic == 0 & newdata$lbdglusi < input$fbg] <- 'TN'
    newdata$confmat[newdata$logistic == 1 & newdata$lbdglusi < input$fbg] <- 'FP'
    newdata$confmat[newdata$logistic == 0 & newdata$lbdglusi > input$fbg] <- 'FN'
    newdata$confmat[newdata$logistic == 1 & newdata$lbdglusi > input$fbg] <- 'TP'
    
    return(newdata)
  })
  
  
  linearPlot <- reactive({ 
    plotdata <- plotData()
    fig1 <- ggplot(data = plotdata, aes(x=lbdglusi, y=mmolmol)) +
              geom_point(shape=1) +    # Use hollow circles
              geom_smooth(method=lm,   # Add linear regression line
                  se=FALSE)    # Don't add shaded confidence region
    return(fig1)
  })
  # Generate a reactive element for plotting the Managed Data.
  # Pass to the webpage using renderPlot(print(theDataPlot))
  basePlot <- reactive({ 
    plotdata <- plotData()
    logmodel <- logModel()
    logistic_curve <- function(x){
      invlogit(logmodel$coef[2] * x + logmodel$coef[1])  # HbA1c cut<=69.4
    }
    
    
    print("Reactive: basePlot")
    set.seed(1)
    
    fig2 <- ggplot(data=plotdata, aes(x=lbdglusi, y=logistic, color=confmat))
    fig2 <- fig2 + geom_point(position = position_jitter(h=0.05), shape=21)
    fig2 <- fig2 + scale_size(guide=FALSE)
    fig2 <- fig2 + xlab("Blood Glucose (mmol/L)") + ylab(paste('Pr(HbA1c)',' > ', toString(input$cutpoint),  'mmol/mol'))
    fig2 <- fig2 + stat_function(fun = logistic_curve)
    
    # Add a single point to the logistic curve
    tmpdata <- data.frame(bg = input$fbg, prob = logistic_curve(input$fbg) )
    print(tmpdata)
    fig2 <- fig2 + geom_point(data = tmpdata, aes(x=bg, y=prob), color="Red", size=3) 
    # Add a vertical line
    fig2 <- fig2 + geom_vline(xintercept = tmpdata$bg, color="Red", 
                              linetype = "longdash", size=0.3)
    return(fig2)
    
  })  
  
  
  output$logisticPlot <- renderPlot({
    p <- basePlot()
    p
    
  })
  
  output$linearPlot <- renderPlot({
    p <- linearPlot()
    p
  })
  
})
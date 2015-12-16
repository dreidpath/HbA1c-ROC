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
    newdata <- bgDF
    newdata$logistic <- as.numeric(newdata$mmolmol>=input$cutpoint)
    newdata$confmat <- NA
    newdata$confmat[newdata$logistic == 0 & newdata$lbdglusi < input$fbg] <- "TN"  # TN
    newdata$confmat[newdata$logistic == 1 & newdata$lbdglusi < input$fbg] <- "FN"  # FP
    newdata$confmat[newdata$logistic == 0 & newdata$lbdglusi > input$fbg] <- "FP"  # FN
    newdata$confmat[newdata$logistic == 1 & newdata$lbdglusi > input$fbg] <- "TP"  # TP
    # newdata$confmat <- factor(newdata$confmat,
    #                              levels = 1:4,
    #                              labels = c("TN", "FP", "FN", "TP"))
    
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
    cmdata <- plotData()
    pred_uc <- cmdata$lbdglusi >= input$fbg
    actual_uc <- cmdata$mmolmol >= input$cutpoint
    binclass_result <- classMatrix(table(pred_uc, actual_uc))
    
    table_return <- binclassmat %>% 
      gsub(pattern = "CELL 1", replacement = binclass_result$class_tbl[2,2]) %>%
      gsub(pattern = "CELL 2", replacement = binclass_result$class_tbl[2,1]) %>%
      gsub(pattern = "CELL 3", replacement = binclass_result$class_tbl[1,2]) %>%
      gsub(pattern = "CELL 4", replacement = binclass_result$class_tbl[1,1]) %>%
      gsub(pattern = "CELL 5", replacement = round(binclass_result$sensitivity, 2)) %>%
      gsub(pattern = "CELL 6", replacement = round(binclass_result$specificity, 2)) %>%
      gsub(pattern = "CELL 7", replacement = round(binclass_result$accuracy, 2))
    
    return(HTML(table_return))
  })
  
  
  senspecData <- reactive({
    cmdata <- plotData()
    tpr <- c() # True positive rate (Sensitivity)
    fpr <- c() # False positive rate (1 - Specificity)
    actual_uc <- cmdata$mmolmol >= input$cutpoint
    for(i in sort(unique(cmdata$lbdglusi))){
      pred_uc <- cmdata$lbdglusi >= i
      binclass_result <- classMatrix(table(pred_uc, actual_uc))
      tpr <- c(tpr, binclass_result$sensitivity)
      fpr <- c(fpr, 1-binclass_result$specificity)
    }
    fpr_rankorder <- order(fpr)
    tpr <- tpr[fpr_rankorder]
    fpr <- fpr[fpr_rankorder]
    dataDF <- data.frame(tpr, fpr)
    dataDF <- na.omit(dataDF)
    dataDF <- rbind(c(0, 0), dataDF) # add the start point to the dataframe
    dataDF <- rbind(dataDF, c(1, 1)) # add the end point to the dataframe
    
    return(dataDF)
  }) #      
  
  
  linearPlot <- reactive({ 
    # Plot of the linear relationshipo between blood glucose and HbA1c
    #  i.e., x=lbdglusi, y=mmolmol
    #
    plotdata <- plotData()
    fig1 <- ggplot(data = plotdata, aes(x=lbdglusi, y=mmolmol)) +
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
    fig2 <- fig2 + geom_point(position = position_jitter(h=0.05), aes(color=confmat), shape=20, size=6.5)
    fig2 <- fig2 + scale_color_manual(values=cbbPalette) # To use for fills, add
    fig2 <- fig2 + xlab("Blood Glucose (mmol/L)") + ylab(paste('Pr(HbA1c)',' > ', toString(input$cutpoint),  'mmol/mol'))
    fig2 <- fig2 + stat_function(fun = logistic_curve, color="Blue")  # Add the logistic curve
    
    # Add a single point on to the logistic curve
    tmpdata <- data.frame(bg = input$fbg, prob = logistic_curve(input$fbg) )
    fig2 <- fig2 + geom_point(data = tmpdata, aes(x=bg, y=prob), color="Red", size=10) 
    # Add a vertical line
    fig2 <- fig2 + geom_vline(xintercept = tmpdata$bg, color="Red", 
                              linetype = "dotted", size=0.3)
    return(fig2)
    
  })  
  
  
  rocPlot <- reactive({ 
    # A plot of the ROC curve associated with the logistic model
    #
    plotdata <- senspecData()  # The reactive data output based on varying cutpoints
    cmdata <- plotData()
    actual_uc <- cmdata$mmolmol >= input$cutpoint
    pred_uc <- cmdata$lbdglusi >= input$fbg
    binclass_result <- classMatrix(table(pred_uc, actual_uc))
    tpr <- binclass_result$sensitivity
    fpr <- 1-binclass_result$specificity
    tmpdata <- data.frame(tpr, fpr)
    
    fig3 <- ggplot(data=plotdata, aes(x=fpr, y=tpr))
    fig3 <- fig3 + geom_line()
    fig3 <- fig3 + geom_abline(slope=1, intercept=0, 
                               linetype = "dotted", color = "Blue" )
    fig3 <- fig3 + xlab("False Positive Rate") + ylab(paste('True Positive Rate'))

    fig3 <- fig3 + geom_point(data = tmpdata, aes(x=fpr, y=tpr), color="Red", size=10) 
    return(fig3)
    
  })
  
  
  output$linearPlot <- renderPlotly({
    # Render the linear plot of HbA1c ~ Blood glucose 
    p <- linearPlot()
    p
  })
  
  
  output$logisticPlot <- renderPlotly({
    # Render the logistic curve
    p <- ggplotly(basePlot())
    p
  })
  
  
  output$rocPlot <- renderPlotly({
    # Render the logistic curve
    p <- ggplotly(rocPlot())
    p
  })
  
  #   output$logrocPlot <- renderPlotly({
  #     p <- multiplot(basePlot(), rocPlot(), cols=1)
  #     p <- ggplotly(p)
  #     p
  #  })
  
  
  output$sensspecTable <- renderUI({ 
    confusionMatrix() 
  })
  
})
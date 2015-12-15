invlogit <- function(x){
  # Return p given x=p/(1-p)
  exp(x)/(1+exp(x))
}


classMatrix <- function(tbl){
  pred.table <- round(tbl)
  
  if(all(dim(pred.table) == c(2,2))){
  tp <- pred.table[1,1]  # True Positive
  fp <- pred.table[1,2]  # False Positive
  tn <- pred.table[2,2]  # True Negative
  fn <- pred.table[2,1]  # False Negative
  sensitivity <- tp/(tp + fn)  # If a person has a disease, how often will the test be positive? 
  specificity <- tn/(tn + fp)  # If a person does not have the disease how often will the test be negative 
  ppv <- tp/(tp+fp)
  npv <- tn/(tn + fn)
  accuracy <- (tp + tn)/(tp + tn + fp + fn) 
  }
  else{
    pred.table <- NA
    sensitivity <- NA
    specificity <- NA
    accuracy <- NA
  }
  list(class_tbl=pred.table, sensitivity=sensitivity, specificity=specificity, accuracy=accuracy)
}

# The palette with black:
cbbPalette <- c("#56B4E9", "#009E73", "#D55E00", "#CC79A7")

# Multiplot function from R-Cookbook
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


source("./classification-matrix.R")

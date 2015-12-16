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

source("./classification-matrix.R")
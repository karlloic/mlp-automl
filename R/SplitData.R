<<<<<<< HEAD
SplitData <- function(data,SplitPerc)
{
  
  
  if(!require(caret)){
	  install.packages("caret")
	  library(caret)
	}
  
  set.seed(123)
  trainIndex <- createDataPartition(data$Y, 
                                    p =SplitPerc, 
                                    list = FALSE, 
                                    times = 1)
  
  
  trainDf <- data[ trainIndex,]
  testDf  <- data[-trainIndex,]
  
  TrainTestLst  <- list()
  
  TrainTestLst$train <- trainDf
  TrainTestLst$test <- testDf
  
  return(TrainTestLst)
  

=======
SplitData <- function(data,SplitPerc)
{
  
  library(caret)
  
  set.seed(123)
  trainIndex <- createDataPartition(data$Y, 
                                    p =SplitPerc, 
                                    list = FALSE, 
                                    times = 1)
  
  
  trainDf <- data[ trainIndex,]
  testDf  <- data[-trainIndex,]
  
  TrainTestLst  <- list()
  
  TrainTestLst$train <- trainDf
  TrainTestLst$test <- testDf
  
  return(TrainTestLst)
  

>>>>>>> bc28899e605107311130461ad3e61a6fe6dda584
}
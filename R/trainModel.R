LrModel <- function(data,depVar,trainSplit)
  {
library(caret)


colnames(data)[colnames(data) == depVar] <- 'Y'

print(head(data))
  
depVarName <- depVar


trainIndex <- createDataPartition(data$Y, 
                                  p =trainSplit, 
                                  list = FALSE, 
                                  times = 1)


trainDf <- data[ trainIndex,]
testDf  <- data[-trainIndex,]

print(dim(trainDf))
print(dim(testDf))

lr_ft <-  lm(Y ~ .  , data = trainDf )

lr_ft <<- lr_ft
testDf  <<- testDf

}





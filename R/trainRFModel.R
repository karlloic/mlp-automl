RFModel <- function(traindata)
{
  
  library(randomForest)
  
  rf <- randomForest(Y ~ .  , data = traindata)
  
  return(rf)
  
}
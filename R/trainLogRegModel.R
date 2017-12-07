LogRegModel <- function(traindata)
{
  
  library(caret)
  
  logreg <- glm(Y ~ .  , data = traindata)
  
  return(logreg)
  
}
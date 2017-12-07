LogRegModel <- function(traindata)
{
  
  library(caret)
  
  logreg <- glm(Y ~ .  , data = traindata,family = 'binomial')
  
  return(logreg)
  
}
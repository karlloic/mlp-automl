LogRegModel <- function(traindata)
{
  
  library(caret)
  
<<<<<<< HEAD
  logreg <- glm(Y ~ .  , data = traindata,family = 'binomial')
=======
  logreg <- glm(Y ~ .  , data = traindata)
>>>>>>> bc28899e605107311130461ad3e61a6fe6dda584
  
  return(logreg)
  
}
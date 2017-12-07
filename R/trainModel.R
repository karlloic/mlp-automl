LrModel <- function(traindata)
  {

  library(caret)

  lr_ft <-  lm(Y ~ .  , data = traindata )

  return(lr_ft)

}





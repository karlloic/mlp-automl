lrPredict<- function(model,newdata)
{

  pred <- predict(model, newdata)
  
  pred <<- pred
  
  predDf <- data.frame(newdata$Y,pred)
  
  names( predDf) <- c('ACTUAL','PREDICTED')
  
  predDf <<- predDf

}

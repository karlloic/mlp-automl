lrPredict<- function(model,newdata)
{

  lrpred <- predict(model, newdata)

  lrpredDf <- data.frame(newdata$Y,lrpred)
  
  names( lrpredDf) <- c('ACTUAL','PREDICTED')
  
  return(lrpredDf)

}

RegClasPredict<- function(model,newdata,ModelName)
{

  if ( ModelName == 'LinearRegression') {
    
  lrpred <- predict(model, newdata)
  
  lrpredDf <- data.frame(newdata$Y,lrpred)
  
  names( lrpredDf) <- c('ACTUAL','PREDICTED')
  
  return(lrpredDf)
  
  }
  
  else if( ModelName == 'LogisticRegression')
  {
    
    logrpred <- predict(model, newdata,type="response")

    logrpredDf <- data.frame(newdata$Y,logrpred)
    
    names( logrpredDf) <- c('ACTUAL','PREDICTED')
    
    return(logrpredDf)
    
  }
  
  

}

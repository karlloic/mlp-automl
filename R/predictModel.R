<<<<<<< HEAD
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
=======
lrPredict<- function(model,newdata)
{

  lrpred <- predict(model, newdata)

  lrpredDf <- data.frame(newdata$Y,lrpred)
  
  names( lrpredDf) <- c('ACTUAL','PREDICTED')
  
  return(lrpredDf)

}
>>>>>>> bc28899e605107311130461ad3e61a6fe6dda584

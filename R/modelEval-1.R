modelEval <- function(actPred,ModelName)
{

# Function that returns Root Mean Squared Error
  rmse <- function(predicted,actual){
    sqrt(mean((actual-predicted)^2,na.rm=T))
  }


 # Function that returns Mean Absolute Error
  mae <- function(predicted, actual){
    mean(abs(actual-predicted),na.rm=T)
  }


 
  RMSE.test <- rmse(actPred$PREDICTED,actPred$ACTUAL)
  MAE.test <- mae(actPred$PREDICTED,actPred$ACTUAL)

dat.results<-
      data.frame(
        "Model" = ModelName
        ,"RMSE" = RMSE.test
        ,"MAE" = MAE.test
        )

return(dat.results)


}
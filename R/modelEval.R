modelEval <- function(actPred, RegClas, ModelName)
{



# Function that returns Root Mean Squared Error
  rmse <- function(predicted,actual){
    sqrt(mean((actual-predicted)^2,na.rm=T))
  }


 # Function that returns Mean Absolute Error
  mae <- function(predicted, actual){
    mean(abs(actual-predicted),na.rm=T)
  }

 # Function that returns AUC
 
 modelEvalAUC <- function(actual, predicted){
  # Functiont to calculate AUC
 
   if(!require(pROC)){
	  install.packages("pROC")
	  library(pROC)
	}
	
  roc_obj <- roc(actual, predicted)
  score <- auc(roc_obj)
  
  return(score)
	}


 if ( RegClas == 'R') {
 
  RMSE.test <- rmse(actPred$PREDICTED,actPred$ACTUAL)
  MAE.test <- mae(actPred$PREDICTED,actPred$ACTUAL)
  
  dat.results<-
      data.frame(
	    "Model" = ModelName
        ,"RMSE" = RMSE.test
        ,"MAE" = MAE.test
        )
	}
	else {
	
	AUC.test <-  modelEvalAUC (actPred$ACTUAL,actPred$PREDICTED)
	
	dat.results<-
      data.frame(
	    "Model" = ModelName,
        "AUC" = AUC.test
        )
		
    }
	
 return(dat.results)

}
modelEvalAUC <- function(actual, predicted)
{
  # Functiont to calculate AUC
  library(pROC)
  
  roc_obj <- roc(actual, predicted)
  score <- auc(roc_obj)
  
  return(score)
}
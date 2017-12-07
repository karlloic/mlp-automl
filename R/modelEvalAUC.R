modelEvalAUC <- function(predicted, actual)
{
  # Functiont to calculate AUC
  library(pROC)
  
  roc_obj <- roc(predicted, actual)
  score <- auc(roc_obj)
  
  return(score)
}
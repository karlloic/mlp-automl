#This Script creates DECILE,KEYSTAT and TEST_v_TRAIN Reports
#=================================================================================
#=================================================================================
# This Script creates DECILE,KEYSTAT and TEST_v_TRAIN Reports
# Input Parameters
#
# FileType - Type of input 
#          - 'csv'  - if input file is csv
#          - 'rdf'  -- inf input is Dataframe 
# 
# FileNameTrain - Train Filename with File Path if FileType is csv
# RDataFrameTrain - Train Data Frame if FileType is rdf
# ActualColumnNameTrain - Column name of ACTUAL values in the train data file/ data frame 
# PredictedColumnNameTrain -  Column name of PREDICTED values in the train data file / data frame 
# FileNameTest - Test Filename with File Path if FileType is csv
# RDataFrameTest - Train Data Frame if FileType is rdf
# ActualColumnNameTest - Column name of ACTUAL values in the test data file/ data frame 
# PredictedColumnNameTest - Column name of PREDICTED values in the test data file / data frame 
# PredictedVariableName  - Name of the PredictedVariable
# Target - Traget is either binary or continuous
# 	   if (Target == continuous ) below stats are calculated
#                 Num_of_rows, Mean_of_target_variable,Mean_of_predicted_target,KENDALL,PERCENT_DECILE_AGREEMENT
#	          RMSE,MAE,MAPE,R2,Spearman,Pearson,ACTUAL_PROP_PredDecile_1,ACTUAL_PROP_PredDecile_2,ACTUAL_PROP_PredDecile_3
#	          ACTUAL_PROP_PredDecile_4,ACTUAL_PROP_PredDecile_5,ACTUAL_PROP_PredDecile_6,ACTUAL_PROP_PredDecile_7,ACTUAL_PROP_PredDecile_8
#	          ACTUAL_PROP_PredDecile_9,ACTUAL_PROP_PredDecile_10,PRED_PROP_PredDecile_1,PRED_PROP_PredDecile_2,PRED_PROP_PredDecile_3,PRED_PROP_PredDecile_4,
#	          PRED_PROP_PredDecile_5,PRED_PROP_PredDecile_6,PRED_PROP_PredDecile_7,PRED_PROP_PredDecile_8,PRED_PROP_PredDecile_9,PRED_PROP_PredDecile_10
#	   
#	   if (Target == binary ) below stats are calculated
#           
#	          Mean_of_target_variable,Mean_of_predicted_target,PERCENT_AGREEMENT, RMSE,MAE,MAPE,Spearman,Pearson,AUC,KS_D,KS_P,ACTUAL_PROP_PredDecile_1,ACTUAL_PROP_PredDecile_2,ACTUAL_PROP_PredDecile_3
#	          ACTUAL_PROP_PredDecile_4,ACTUAL_PROP_PredDecile_5,ACTUAL_PROP_PredDecile_6,ACTUAL_PROP_PredDecile_7,ACTUAL_PROP_PredDecile_8
#	          ACTUAL_PROP_PredDecile_9,ACTUAL_PROP_PredDecile_10,PRED_PROP_PredDecile_1,PRED_PROP_PredDecile_2,PRED_PROP_PredDecile_3,PRED_PROP_PredDecile_4,
#	          PRED_PROP_PredDecile_5,PRED_PROP_PredDecile_6,PRED_PROP_PredDecile_7,PRED_PROP_PredDecile_8,PRED_PROP_PredDecile_9,PRED_PROP_PredDecile_10
#
# Reports - short or long
#            if (Reports == short and  Target  == binary ) -  KEYSTAT report is created
#	           if (Reports == short and  Target  == continuous ) -  KEYSTAT report is created with out KENDALL stat
#            if (Reports == long)  - all DECILE,KEYSTAT and TEST_v_TRAIN Reports are created
# LogSclaeReports - Y or N ( If(Target == continuous and LogScaleReports = 'Y') - logarithimic sclae  reports are included in the final report)
# Description - Description of the Model
# outputDir - location of the output reports
#=================================================================================
#=================================================================================


#install.packages('dplyr', dependencies = TRUE)
#install.packages('caret', dependencies = TRUE)

options(scipen=10000)
Model_Evaluation <- function(FileType ,    
                             FileNameTrain,
                             RDataFrameTrain, 
                             ActualColumnNameTrain,
                             PredictedColumnNameTrain, 
                             FileNameTest,
                             RDataFrameTest, 
                             ActualColumnNameTest,
                             PredictedColumnNameTest,  
                             PredictedVariableName ,
                             Target, 
                             Reports,
                             LogScaleReports, 
                             Description,
                             outputDir) 
{
  
  
  library('dplyr')
  library('caret')
  library('pROC')
  library('tidyr')
  library('ggplot2')
  library('ROCR')
  library('sqldf')
  require(gridExtra)
  library(reshape)
  
  
  
  ########################################################
  ## Stats of test dataset
  ########################################################
  
  # Linear Scaling
  
  
  
  
  varColNamesTest<- c(ActualColumnNameTest,PredictedColumnNameTest)
  varColNamesTrain<- c(ActualColumnNameTrain,PredictedColumnNameTrain)
  
  outputDirFileName <-  paste(outputDir,Description,sep ="")
  print(outputDirFileName)
  
  if (FileType == 'csv') {
    
    
    test_file_name <- FileNameTest
    predictionRaw <- read.csv(test_file_name,header =  TRUE )
    prediction   <- predictionRaw[varColNamesTest ]
    names(prediction) <- c("ACTUAL","PREDICTED")
    
    
    train_file_name <- FileNameTrain
    prediction.trainRaw   <- read.csv(train_file_name ,header =  TRUE)
    prediction.train   <- prediction.trainRaw[varColNamesTrain]
    names(prediction.train) <- c("ACTUAL","PREDICTED")
    
  }
  else
  {
    
    predictionRaw   <- RDataFrameTest
    prediction   <- predictionRaw[varColNamesTest ]
    names(prediction) <- c("ACTUAL","PREDICTED")
    
    
    prediction.trainRaw   <- RDataFrameTrain
    prediction.train   <- prediction.trainRaw[varColNamesTrain]
    names(prediction.train) <- c("ACTUAL","PREDICTED")
    
    
  }
  
  # Model Evaluation Functions 
  
  #=================================================================================
  #=================================================================================
  #                  Evaluation Functions
  #=================================================================================
  #=================================================================================
  
  
  
  # function to compute Kendall's tau
  ken <- function(predicted, actual){
    as.numeric(cor.test(x = predicted, y = actual, alternative = "greater", method = "kendall")$estimate)
  }
  
  
  
  # Function that returns Root Mean Squared Error
  rmse <- function(predicted,actual){
    sqrt(mean((actual-predicted)^2,na.rm=T))
  }
  
  
  # Function that returns Mean Absolute Percent Error
  mape <- function(predicted, actual){
    mean(abs((actual - predicted)/actual))
  }
  
  # Function that returns Mean Absolute Error
  mae <- function(predicted, actual){
    mean(abs(actual-predicted),na.rm=T)
  }
  
  # function to compute R squared
  r2 <- function(predicted, actual){
    summary(lm(predicted~actual))$r.squared
  }
  
  
  # function to compute "percent agreement"
  PercAg <- function(predicted_decile, actual_decile){
    if(!"caret"%in%installed.packages()){
      install.packages("caret")
    }
    list(sum(diag(confusionMatrix(predicted_decile,actual_decile)$table))/
           sum(confusionMatrix(predicted_decile,actual_decile)$table)
         ,confusionMatrix(predicted_decile,actual_decile)$table)
  }
  
  n_sect2 <- function(
    x_vec, 
    num_intervals, 
    breaks
  ) {
    
    
    # probabilities within the unit interval
    p_breaks <- ppoints(-1 + num_intervals)  # ppoints: Generates the sequence of probability points (1:m - a)/(m + (1-a)-a) where m is either n, if length(n)==1, or length(n).
    p_breaks <- as.data.frame(p_breaks)
    temp <- paste("probs = p_breaks[", breaks, ", ]")
    eval(parse(text = temp))
    # x-quantiles defining successive intervals
    q_breaks <- quantile(
      x = x_vec, 
      probs, 
      na.rm = TRUE, 
      names = FALSE
    )
    
    x_breaks_df <- data.frame(probs, q_breaks)
    
    # index, from 1 to num_intervals, of 
    # the interval containing each x value
    q_vec <-  1 + findInterval(   # findInterval: find the interval containing each element of x
      x = x_vec, 
      vec = q_breaks
    )
    
    x_freq_df <- data.frame(table(q_vec))
    
    return(
      list(
        q_vec = nrow(x_breaks_df) + 1 - q_vec + 1, 
        x_breaks_df = x_breaks_df, 
        x_freq_df = x_freq_df
      )
    )
  }
  
  
  gl_df <- prediction
  bc_df <- prediction 
  
  prediction <- 
    prediction %>%mutate(PREDICTED_LOG = log(prediction$PREDICTED)
                         ,ACTUAL_LOG = log(prediction$ACTUAL))
  
  
  num_of_rows <- nrow(prediction)
  Mean_of_target_variable <- mean(prediction$ACTUAL)
  mean_of_predicted_target <- mean(prediction$PREDICTED)
  
  min_pred <- min(prediction$PREDICTED)
  range_pred <- max(prediction$PREDICTED) - min_pred
  
  prediction$bin <- ntile(prediction$PREDICTED, n = 100)
  min_actual <- mean(prediction[prediction$bin==1,"ACTUAL"])
  max_actual <- mean(prediction[prediction$bin==100,"ACTUAL"])
  range_actual <- max_actual - min_actual
  
  cat("Generating Summary Statistics...\n")
  
  # Stats
  RMSE <- rmse(prediction$PREDICTED,prediction$ACTUAL)
  MAE <- mae(prediction$PREDICTED,prediction$ACTUAL)
  
  if(Target == 'continuous'){
    
    MAPE <- mape(prediction$PREDICTED,prediction$ACTUAL)
    
    if (Reports == 'long'){
      
      KENDALL <- ken(prediction$PREDICTED,prediction$ACTUAL)
    }
    
  } else {
    
    AUC <- auc(prediction$ACTUAL, prediction$PREDICTED)
    KS <- ks.test(prediction$PREDICTED, prediction$ACTUAL)
    
  }
  
  
  spearman <- cor(x=prediction$PREDICTED
                  ,y=prediction$ACTUAL
                  , use="complete.obs"
                  , method="spearman")
  
  pearson <-cor(x=prediction$PREDICTED
                ,y=prediction$ACTUAL
                , use="complete.obs", method="pearson")
  
  R2 <- pearson^2
  
  
  prediction <- 
    prediction %>% 
    mutate(ACTUAL_DECILE = ntile(x=desc(prediction$ACTUAL),n=10)
           ,ACTUAL_DECILE_LOG = ntile(x=desc(prediction$ACTUAL_LOG),n=10)
           ,PREDICTED_DECILE = ntile(x=desc(prediction$PREDICTED),n=10)
           ,PREDICTED_DECILE_LOG = ntile(x=desc(prediction$PREDICTED_LOG),n=10)
           ,TEST_OR_TRAIN = "TEST")
  
  
  PERCENT_DECILE_AGREEMENT <- PercAg(prediction$PREDICTED_DECILE,prediction$ACTUAL_DECILE)[[1]]
  
  
  library(pROC)
  
  ########################################################
  ## Stats of train dataset
  ########################################################
  
  
  gl_df_train <- prediction.train 
  bc_df.train <- prediction.train 
  prediction.train <- 
    prediction.train %>%mutate(PREDICTED_LOG = log(prediction.train$PREDICTED)
                               ,ACTUAL_LOG = log(prediction.train$ACTUAL))
  num_of_rows.train <- nrow(prediction.train)
  Mean_of_target_variable.train <- mean(prediction.train$ACTUAL)
  mean_of_predicted_target.train <- mean(prediction.train$PREDICTED)
  
  min_pred <- min(prediction.train$PREDICTED)
  range_pred <- max(prediction.train$PREDICTED) - min_pred
  
  prediction.train$bin <- ntile(prediction.train$PREDICTED, n = 100)
  min_actual.train <- mean(prediction.train[prediction.train$bin==1,"ACTUAL"])
  max_actual.train <- mean(prediction.train[prediction.train$bin==100,"ACTUAL"])
  range_actual.train <- max_actual.train - min_actual.train
  
  
  # Stats
  
  RMSE.train <- rmse(prediction.train$PREDICTED,prediction.train$ACTUAL)
  MAE.train <- mae(prediction.train$PREDICTED,prediction.train$ACTUAL)
  
  
  if(Target == 'continuous'){
    
    MAPE.train <- mape(prediction.train$PREDICTED,prediction.train$ACTUAL)
    
    if (Reports == 'long'){
      
      KENDALL.train <- ken(prediction.train$PREDICTED,prediction.train$ACTUAL)
      
    }
    
  } else {
    
    
    AUC.train <- auc(prediction.train$ACTUAL, prediction.train$PREDICTED)
    KS.train <- ks.test(prediction.train$PREDICTED, prediction.train$ACTUAL)
    
  }
  
  
  spearman.train <- cor(x=prediction.train$PREDICTED
                        ,y=prediction.train$ACTUAL
                        , use="complete.obs"
                        , method="spearman")
  
  pearson.train <-cor(x=prediction.train$PREDICTED
                      ,y=prediction.train$ACTUAL
                      , use="complete.obs", method="pearson")
  
  R2.train <- pearson.train^2
  
  
  #create deciles
  
  
  
  prediction.train <- 
    prediction.train %>% 
    mutate(ACTUAL_DECILE = ntile(x=desc(prediction.train$ACTUAL),n=10)
           ,ACTUAL_DECILE_LOG = ntile(x=desc(prediction.train$ACTUAL_LOG),n=10)
           ,PREDICTED_DECILE = ntile(x=desc(prediction.train$PREDICTED),n=10)
           ,PREDICTED_DECILE_LOG = ntile(x=desc(prediction.train$PREDICTED_LOG),n=10)
           ,TEST_OR_TRAIN = "TRAIN")
  
  PERCENT_DECILE_AGREEMENT.train <- PercAg(prediction.train$PREDICTED_DECILE,prediction.train$ACTUAL_DECILE)[[1]]
  
  cat("Recording the Output...\n")
  
  #=================================================================================
  #=================================================================================
  #                         Recording the Output
  #=================================================================================
  #=================================================================================
  
  if(Target == 'binary') { # if Target is binary
    
    # Test dataset
    dat.results<-
      data.frame(
        row.names=NULL
        ,stringsAsFactors = F
        ,"Descr" = Description
        ,"Num_of_rows" = num_of_rows
        ,"Mean_of_target_variable" = Mean_of_target_variable
        ,"Mean_of_predicted_target" = mean_of_predicted_target
        ,"PERCENT_DECILE_AGREEMENT" = PERCENT_DECILE_AGREEMENT
        ,"RMSE" = RMSE
        ,"MAE" = MAE
        ,"R2" = R2
        ,"Spearman" = spearman
        ,"Pearson" = pearson
        ,"AUC" = AUC[1]
        ,"KS_D" = as.numeric(KS[1])
        ,"KS_P" = as.numeric(KS[2])
        ,"ACTUAL_PROP_PredDecile_1" = mean(prediction[prediction$PREDICTED_DECILE==1,"ACTUAL"])
        ,"ACTUAL_PROP_PredDecile_2" = mean(prediction[prediction$PREDICTED_DECILE==2,"ACTUAL"])
        ,"ACTUAL_PROP_PredDecile_3" = mean(prediction[prediction$PREDICTED_DECILE==3,"ACTUAL"])
        ,"ACTUAL_PROP_PredDecile_4" = mean(prediction[prediction$PREDICTED_DECILE==4,"ACTUAL"])
        ,"ACTUAL_PROP_PredDecile_5" = mean(prediction[prediction$PREDICTED_DECILE==5,"ACTUAL"])
        ,"ACTUAL_PROP_PredDecile_6" = mean(prediction[prediction$PREDICTED_DECILE==6,"ACTUAL"])
        ,"ACTUAL_PROP_PredDecile_7" = mean(prediction[prediction$PREDICTED_DECILE==7,"ACTUAL"])
        ,"ACTUAL_PROP_PredDecile_8" = mean(prediction[prediction$PREDICTED_DECILE==8,"ACTUAL"])
        ,"ACTUAL_PROP_PredDecile_9" = mean(prediction[prediction$PREDICTED_DECILE==9,"ACTUAL"])
        ,"ACTUAL_PROP_PredDecile_10" = mean(prediction[prediction$PREDICTED_DECILE==10,"ACTUAL"]) 
        ,"PRED_PROP_PredDecile_1" = mean(prediction[prediction$PREDICTED_DECILE==1,"PREDICTED"])
        ,"PRED_PROP_PredDecile_2" = mean(prediction[prediction$PREDICTED_DECILE==2,"PREDICTED"])
        ,"PRED_PROP_PredDecile_3" = mean(prediction[prediction$PREDICTED_DECILE==3,"PREDICTED"])
        ,"PRED_PROP_PredDecile_4" = mean(prediction[prediction$PREDICTED_DECILE==4,"PREDICTED"])
        ,"PRED_PROP_PredDecile_5" = mean(prediction[prediction$PREDICTED_DECILE==5,"PREDICTED"])
        ,"PRED_PROP_PredDecile_6" = mean(prediction[prediction$PREDICTED_DECILE==6,"PREDICTED"])
        ,"PRED_PROP_PredDecile_7" = mean(prediction[prediction$PREDICTED_DECILE==7,"PREDICTED"])
        ,"PRED_PROP_PredDecile_8" = mean(prediction[prediction$PREDICTED_DECILE==8,"PREDICTED"])
        ,"PRED_PROP_PredDecile_9" = mean(prediction[prediction$PREDICTED_DECILE==9,"PREDICTED"])
        ,"PRED_PROP_PredDecile_10" = mean(prediction[prediction$PREDICTED_DECILE==10,"PREDICTED"])
      )
    
    # Training dataset
    
    dat.results.train<-
      data.frame(
        row.names=NULL
        ,"Descr" = Description
        ,"Num_of_rows" = num_of_rows.train
        ,"Mean_of_target_variable" = Mean_of_target_variable.train
        ,"Mean_of_predicted_target" = mean_of_predicted_target.train
        ,"PERCENT_DECILE_AGREEMENT" = PERCENT_DECILE_AGREEMENT.train
        ,"RMSE" = RMSE.train
        ,"MAE" = MAE.train
        ,"R2" = R2.train
        ,"Spearman" = spearman.train
        ,"Pearson" = pearson.train
        ,"AUC" = AUC.train[1]
        ,"KS-D" = as.numeric(KS.train[1])
        ,"KS-P" = as.numeric(KS.train[2])
        ,"ACTUAL_PROP_PredDecile_1" = mean(prediction.train[prediction.train$PREDICTED_DECILE==1,"ACTUAL"])
        ,"ACTUAL_PROP_PredDecile_2" = mean(prediction.train[prediction.train$PREDICTED_DECILE==2,"ACTUAL"])
        ,"ACTUAL_PROP_PredDecile_3" = mean(prediction.train[prediction.train$PREDICTED_DECILE==3,"ACTUAL"])
        ,"ACTUAL_PROP_PredDecile_4" = mean(prediction.train[prediction.train$PREDICTED_DECILE==4,"ACTUAL"])
        ,"ACTUAL_PROP_PredDecile_5" = mean(prediction.train[prediction.train$PREDICTED_DECILE==5,"ACTUAL"])
        ,"ACTUAL_PROP_PredDecile_6" = mean(prediction.train[prediction.train$PREDICTED_DECILE==6,"ACTUAL"])
        ,"ACTUAL_PROP_PredDecile_7" = mean(prediction.train[prediction.train$PREDICTED_DECILE==7,"ACTUAL"])
        ,"ACTUAL_PROP_PredDecile_8" = mean(prediction.train[prediction.train$PREDICTED_DECILE==8,"ACTUAL"])
        ,"ACTUAL_PROP_PredDecile_9" = mean(prediction.train[prediction.train$PREDICTED_DECILE==9,"ACTUAL"])
        ,"ACTUAL_PROP_PredDecile_10" = mean(prediction.train[prediction.train$PREDICTED_DECILE==10,"ACTUAL"])
        ,"PRED_PROP_PredDecile_1" = mean(prediction.train[prediction.train$PREDICTED_DECILE==1,"PREDICTED"])
        ,"PRED_PROP_PredDecile_2" = mean(prediction.train[prediction.train$PREDICTED_DECILE==2,"PREDICTED"])
        ,"PRED_PROP_PredDecile_3" = mean(prediction.train[prediction.train$PREDICTED_DECILE==3,"PREDICTED"])
        ,"PRED_PROP_PredDecile_4" = mean(prediction.train[prediction.train$PREDICTED_DECILE==4,"PREDICTED"])
        ,"PRED_PROP_PredDecile_5" = mean(prediction.train[prediction.train$PREDICTED_DECILE==5,"PREDICTED"])
        ,"PRED_PROP_PredDecile_6" = mean(prediction.train[prediction.train$PREDICTED_DECILE==6,"PREDICTED"])
        ,"PRED_PROP_PredDecile_7" = mean(prediction.train[prediction.train$PREDICTED_DECILE==7,"PREDICTED"])
        ,"PRED_PROP_PredDecile_8" = mean(prediction.train[prediction.train$PREDICTED_DECILE==8,"PREDICTED"])
        ,"PRED_PROP_PredDecile_9" = mean(prediction.train[prediction.train$PREDICTED_DECILE==9,"PREDICTED"])
        ,"PRED_PROP_PredDecile_10" = mean(prediction.train[prediction.train$PREDICTED_DECILE==10,"PREDICTED"])
      )
    
  } else { # target is continuous
    
    if( Reports == 'long'){ #Report type long
      
      # Test dataset
      dat.results<-
        data.frame(
          row.names=NULL
          ,stringsAsFactors = F
          ,"Descr" = Description
          ,"Num_of_rows" = num_of_rows
          ,"Mean_of_target_variable" = Mean_of_target_variable
          ,"Mean_of_predicted_target" = mean_of_predicted_target
          ,"KENDALL" = KENDALL
          ,"PERCENT_DECILE_AGREEMENT" = PERCENT_DECILE_AGREEMENT
          ,"RMSE" = RMSE
          ,"MAE" = MAE
          ,"MAPE" = MAPE
          ,"R2" = R2
          ,"Spearman" = spearman
          ,"Pearson" = pearson	
          ,"ACTUAL_PROP_PredDecile_1" = mean(prediction[prediction$PREDICTED_DECILE==1,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_2" = mean(prediction[prediction$PREDICTED_DECILE==2,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_3" = mean(prediction[prediction$PREDICTED_DECILE==3,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_4" = mean(prediction[prediction$PREDICTED_DECILE==4,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_5" = mean(prediction[prediction$PREDICTED_DECILE==5,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_6" = mean(prediction[prediction$PREDICTED_DECILE==6,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_7" = mean(prediction[prediction$PREDICTED_DECILE==7,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_8" = mean(prediction[prediction$PREDICTED_DECILE==8,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_9" = mean(prediction[prediction$PREDICTED_DECILE==9,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_10" = mean(prediction[prediction$PREDICTED_DECILE==10,"ACTUAL"])
          ,"PRED_PROP_PredDecile_1" = mean(prediction[prediction$PREDICTED_DECILE==1,"PREDICTED"])
          ,"PRED_PROP_PredDecile_2" = mean(prediction[prediction$PREDICTED_DECILE==2,"PREDICTED"])
          ,"PRED_PROP_PredDecile_3" = mean(prediction[prediction$PREDICTED_DECILE==3,"PREDICTED"])
          ,"PRED_PROP_PredDecile_4" = mean(prediction[prediction$PREDICTED_DECILE==4,"PREDICTED"])
          ,"PRED_PROP_PredDecile_5" = mean(prediction[prediction$PREDICTED_DECILE==5,"PREDICTED"])
          ,"PRED_PROP_PredDecile_6" = mean(prediction[prediction$PREDICTED_DECILE==6,"PREDICTED"])
          ,"PRED_PROP_PredDecile_7" = mean(prediction[prediction$PREDICTED_DECILE==7,"PREDICTED"])
          ,"PRED_PROP_PredDecile_8" = mean(prediction[prediction$PREDICTED_DECILE==8,"PREDICTED"])
          ,"PRED_PROP_PredDecile_9" = mean(prediction[prediction$PREDICTED_DECILE==9,"PREDICTED"])
          ,"PRED_PROP_PredDecile_10" = mean(prediction[prediction$PREDICTED_DECILE==10,"PREDICTED"])
        )
      
      
      # Training dataset
      dat.results.train<-
        data.frame(
          row.names=NULL
          ,"Descr" = Description
          ,"Num_of_rows" = num_of_rows.train
          ,"Mean_of_target_variable" = Mean_of_target_variable.train
          ,"Mean_of_predicted_target" = mean_of_predicted_target.train
          ,"KENDALL" = KENDALL.train
          ,"PERCENT_DECILE_AGREEMENT" = PERCENT_DECILE_AGREEMENT.train
          ,"RMSE" = RMSE.train
          ,"MAE" = MAE.train
          ,"MAPE" = MAPE.train
          ,"R2" = R2.train
          ,"Spearman" = spearman.train
          ,"Pearson" = pearson.train
          ,"ACTUAL_PROP_PredDecile_1" = mean(prediction.train[prediction.train$PREDICTED_DECILE==1,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_2" = mean(prediction.train[prediction.train$PREDICTED_DECILE==2,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_3" = mean(prediction.train[prediction.train$PREDICTED_DECILE==3,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_4" = mean(prediction.train[prediction.train$PREDICTED_DECILE==4,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_5" = mean(prediction.train[prediction.train$PREDICTED_DECILE==5,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_6" = mean(prediction.train[prediction.train$PREDICTED_DECILE==6,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_7" = mean(prediction.train[prediction.train$PREDICTED_DECILE==7,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_8" = mean(prediction.train[prediction.train$PREDICTED_DECILE==8,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_9" = mean(prediction.train[prediction.train$PREDICTED_DECILE==9,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_10" = mean(prediction.train[prediction.train$PREDICTED_DECILE==10,"ACTUAL"])
          ,"PRED_PROP_PredDecile_1" = mean(prediction.train[prediction.train$PREDICTED_DECILE==1,"PREDICTED"])
          ,"PRED_PROP_PredDecile_2" = mean(prediction.train[prediction.train$PREDICTED_DECILE==2,"PREDICTED"])
          ,"PRED_PROP_PredDecile_3" = mean(prediction.train[prediction.train$PREDICTED_DECILE==3,"PREDICTED"])
          ,"PRED_PROP_PredDecile_4" = mean(prediction.train[prediction.train$PREDICTED_DECILE==4,"PREDICTED"])
          ,"PRED_PROP_PredDecile_5" = mean(prediction.train[prediction.train$PREDICTED_DECILE==5,"PREDICTED"])
          ,"PRED_PROP_PredDecile_6" = mean(prediction.train[prediction.train$PREDICTED_DECILE==6,"PREDICTED"])
          ,"PRED_PROP_PredDecile_7" = mean(prediction.train[prediction.train$PREDICTED_DECILE==7,"PREDICTED"])
          ,"PRED_PROP_PredDecile_8" = mean(prediction.train[prediction.train$PREDICTED_DECILE==8,"PREDICTED"])
          ,"PRED_PROP_PredDecile_9" = mean(prediction.train[prediction.train$PREDICTED_DECILE==9,"PREDICTED"])
          ,"PRED_PROP_PredDecile_10" = mean(prediction.train[prediction.train$PREDICTED_DECILE==10,"PREDICTED"])
        )
      
    } else { # Report type short
      
      # Test dataset
      dat.results<-
        data.frame(
          row.names=NULL
          ,stringsAsFactors = F
          ,"Descr" = Description
          ,"Num_of_rows" = num_of_rows
          ,"Mean_of_target_variable" = Mean_of_target_variable
          ,"Mean_of_predicted_target" = mean_of_predicted_target
          ,"PERCENT_DECILE_AGREEMENT" = PERCENT_DECILE_AGREEMENT
          ,"RMSE" = RMSE
          ,"MAE" = MAE
          ,"MAPE" = MAPE
          ,"R2" = R2
          ,"Spearman" = spearman
          ,"Pearson" = pearson	
          ,"ACTUAL_PROP_PredDecile_1" = mean(prediction[prediction$PREDICTED_DECILE==1,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_2" = mean(prediction[prediction$PREDICTED_DECILE==2,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_3" = mean(prediction[prediction$PREDICTED_DECILE==3,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_4" = mean(prediction[prediction$PREDICTED_DECILE==4,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_5" = mean(prediction[prediction$PREDICTED_DECILE==5,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_6" = mean(prediction[prediction$PREDICTED_DECILE==6,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_7" = mean(prediction[prediction$PREDICTED_DECILE==7,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_8" = mean(prediction[prediction$PREDICTED_DECILE==8,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_9" = mean(prediction[prediction$PREDICTED_DECILE==9,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_10" = mean(prediction[prediction$PREDICTED_DECILE==10,"ACTUAL"])
          ,"PRED_PROP_PredDecile_1" = mean(prediction[prediction$PREDICTED_DECILE==1,"PREDICTED"])
          ,"PRED_PROP_PredDecile_2" = mean(prediction[prediction$PREDICTED_DECILE==2,"PREDICTED"])
          ,"PRED_PROP_PredDecile_3" = mean(prediction[prediction$PREDICTED_DECILE==3,"PREDICTED"])
          ,"PRED_PROP_PredDecile_4" = mean(prediction[prediction$PREDICTED_DECILE==4,"PREDICTED"])
          ,"PRED_PROP_PredDecile_5" = mean(prediction[prediction$PREDICTED_DECILE==5,"PREDICTED"])
          ,"PRED_PROP_PredDecile_6" = mean(prediction[prediction$PREDICTED_DECILE==6,"PREDICTED"])
          ,"PRED_PROP_PredDecile_7" = mean(prediction[prediction$PREDICTED_DECILE==7,"PREDICTED"])
          ,"PRED_PROP_PredDecile_8" = mean(prediction[prediction$PREDICTED_DECILE==8,"PREDICTED"])
          ,"PRED_PROP_PredDecile_9" = mean(prediction[prediction$PREDICTED_DECILE==9,"PREDICTED"])
          ,"PRED_PROP_PredDecile_10" = mean(prediction[prediction$PREDICTED_DECILE==10,"PREDICTED"])
        )
      
      
      # Training dataset
      dat.results.train<-
        data.frame(
          row.names=NULL
          ,"Descr" = Description
          ,"Num_of_rows" = num_of_rows.train
          ,"Mean_of_target_variable" = Mean_of_target_variable.train
          ,"Mean_of_predicted_target" = mean_of_predicted_target.train
          ,"PERCENT_DECILE_AGREEMENT" = PERCENT_DECILE_AGREEMENT.train
          ,"RMSE" = RMSE.train
          ,"MAE" = MAE.train
          ,"MAPE" = MAPE.train
          ,"R2" = R2.train
          ,"Spearman" = spearman.train
          ,"Pearson" = pearson.train
          ,"ACTUAL_PROP_PredDecile_1" = mean(prediction.train[prediction.train$PREDICTED_DECILE==1,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_2" = mean(prediction.train[prediction.train$PREDICTED_DECILE==2,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_3" = mean(prediction.train[prediction.train$PREDICTED_DECILE==3,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_4" = mean(prediction.train[prediction.train$PREDICTED_DECILE==4,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_5" = mean(prediction.train[prediction.train$PREDICTED_DECILE==5,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_6" = mean(prediction.train[prediction.train$PREDICTED_DECILE==6,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_7" = mean(prediction.train[prediction.train$PREDICTED_DECILE==7,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_8" = mean(prediction.train[prediction.train$PREDICTED_DECILE==8,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_9" = mean(prediction.train[prediction.train$PREDICTED_DECILE==9,"ACTUAL"])
          ,"ACTUAL_PROP_PredDecile_10" = mean(prediction.train[prediction.train$PREDICTED_DECILE==10,"ACTUAL"])
          ,"PRED_PROP_PredDecile_1" = mean(prediction.train[prediction.train$PREDICTED_DECILE==1,"PREDICTED"])
          ,"PRED_PROP_PredDecile_2" = mean(prediction.train[prediction.train$PREDICTED_DECILE==2,"PREDICTED"])
          ,"PRED_PROP_PredDecile_3" = mean(prediction.train[prediction.train$PREDICTED_DECILE==3,"PREDICTED"])
          ,"PRED_PROP_PredDecile_4" = mean(prediction.train[prediction.train$PREDICTED_DECILE==4,"PREDICTED"])
          ,"PRED_PROP_PredDecile_5" = mean(prediction.train[prediction.train$PREDICTED_DECILE==5,"PREDICTED"])
          ,"PRED_PROP_PredDecile_6" = mean(prediction.train[prediction.train$PREDICTED_DECILE==6,"PREDICTED"])
          ,"PRED_PROP_PredDecile_7" = mean(prediction.train[prediction.train$PREDICTED_DECILE==7,"PREDICTED"])
          ,"PRED_PROP_PredDecile_8" = mean(prediction.train[prediction.train$PREDICTED_DECILE==8,"PREDICTED"])
          ,"PRED_PROP_PredDecile_9" = mean(prediction.train[prediction.train$PREDICTED_DECILE==9,"PREDICTED"])
          ,"PRED_PROP_PredDecile_10" = mean(prediction.train[prediction.train$PREDICTED_DECILE==10,"PREDICTED"])
        )
    }
    
  }
  
  
  cat("Creating charts  ...\n")
  # row-bind the data
  
  prediction1 <<- prediction
  prediction.train1 <<- prediction.train
  prediction.fin<-bind_rows(prediction,prediction.train)
  
  library(tidyr)
  library(dplyr)
  library(ggplot2)
  library(scales)
  
 
  if(Reports == 'long'){
    
    if(Target == 'binary'){
      
      print("binary")
      
      # decile line plot
      decileLineChart<-
        ggplot(data=
                 prediction.fin %>% 
                 group_by(TEST_OR_TRAIN,PREDICTED_DECILE) %>% 
                 summarise(MEAN_OF_PREDICTED = mean(PREDICTED)
                           ,MEAN_OF_ACTUAL = mean(ACTUAL)
                 ) %>% 
                 mutate(PREDICTED_DECILE = PREDICTED_DECILE) %>% 
                 gather(Output, MEAN,-PREDICTED_DECILE,-TEST_OR_TRAIN)
               
               ,aes(x = PREDICTED_DECILE, y = MEAN ,color = Output)) + 
        geom_line(size=1)+
        facet_wrap(~TEST_OR_TRAIN,ncol=2)+
        scale_x_reverse(breaks=1:10)+
        ggtitle(paste(Description , ": Actual vs Predicted Decile Report, Mean \n",PredictedVariableName, sep =""))+
        theme(plot.title = element_text(color="dodgerblue", size=14, face="bold"),
              legend.position = "bottom")
      
      ## lift chart of actual propensity by predicted decile
      propensity_lift_chart_test_train<- 
        ggplot(data= prediction.fin 
               ,aes(x = PREDICTED_DECILE, y = ACTUAL )) +
        geom_bar(stat="identity",fill="grey")+
        facet_wrap(~TEST_OR_TRAIN,ncol=2)+
        scale_x_reverse(breaks=1:10)+
        ggtitle(paste(Description , ": Sum of Actual Target Variable by Decile \n",PredictedVariableName,sep =""))+
        theme(plot.title = element_text(color="dodgerblue", size=14, face="bold"),
              legend.position = "bottom")
      
     
      
      pred <- prediction( prediction$PREDICTED, prediction$ACTUAL)
      perf <- performance(pred, measure = "tpr", x.measure = "fpr")
      
      aucr <- performance(pred, measure = "auc")
      aucr <- round(aucr@y.values[[1]],6)
      
      Rdata <- data.frame(fpr=unlist(perf@x.values),
                         tpr=unlist(perf@y.values))
      
      Rdata$TEST_OR_TRAIN <- "TEST"
      
      pred.train <- prediction( prediction.train$PREDICTED, prediction.train$ACTUAL)
      perf.train <- performance(pred.train, measure = "tpr", x.measure = "fpr")
      
      aucr.trn <- performance(pred.train, measure = "auc")
      aucr.train <-  round(aucr.trn@y.values[[1]],6)
      
      Rdata.train <- data.frame(fpr=unlist(perf.train@x.values),
                          tpr=unlist(perf.train@y.values))
      
      Rdata.train$TEST_OR_TRAIN <- "TRAIN"
      
      Rdata.fin <- bind_rows( Rdata,Rdata.train)
      
      
      ann_text.auc <- geom_text(data=data.frame(x= c(0.5,0.5), 
                                               y= c(1 ,1),
                                               label=c(paste("AUC = ", round(aucr,2) ,sep =""),
                                                       paste("AUC = ", round(aucr.train,2) ,sep ="")), 
                                               TEST_OR_TRAIN =factor(c("TEST","TRAIN"),levels = c("TEST","TRAIN")) 
                                               ), 
                               aes(x,y,label=label), 
                               inherit.aes=FALSE)
      
      
      aucChart<-  
        ggplot(Rdata.fin, aes(x=fpr, ymin=0, ymax=tpr)) +
        geom_ribbon(alpha=0.2) +
        geom_line(aes(y=tpr)) +
        facet_wrap(~TEST_OR_TRAIN,ncol=2)+
        xlab("False positive rate") +
        scale_x_continuous(breaks=seq(0,1,0.2))+
        ylab("True positive rate") +
        scale_y_continuous(breaks=seq(0,1,0.2))+
        annotate("segment", x=0, xend = 1 , y =0,  yend = 1 , colour = "blue")+
        ann_text.auc+
        ggtitle(paste(Description , ": ROC Curve ", "\n",PredictedVariableName,sep =""))+
        theme(plot.title = element_text(color="dodgerblue", size=14, face="bold"),
              legend.position = "bottom")
       
      
      
      totalactualpremium    <- sum(gl_df$ACTUAL) 
      
      gl_df <- 
        gl_df %>% 
        mutate(PREDICTED_QUANTILE  = ntile(x=desc(gl_df$PREDICTED),n=10))
      
      gain_lift_df <- sqldf("select PREDICTED_QUANTILE as X ,count(*) as CASES , sum(ACTUAL) as SUM_ACTUAL,avg(ACTUAL) as AVG_ACTUAL,avg(PREDICTED) as AVG_PREDICTED from gl_df group by PREDICTED_QUANTILE  ")
      
      gain_lift_df$CUMACTUAL <- cumsum(as.numeric(gain_lift_df$SUM_ACTUAL ))
      gain_lift_df$GAIN <- gain_lift_df$CUMACTUAL /totalactualpremium
      gain_lift_df$TEST_OR_TRAIN <- "TEST"
      
      
      
      totalactualpremium.train   <- sum(gl_df_train$ACTUAL) 
      
      gl_df_train <- 
        gl_df_train %>% 
        mutate(PREDICTED_QUANTILE  = ntile(x=desc(gl_df_train$PREDICTED),n=10))
      
      
      gain_lift_df.train <- sqldf("select PREDICTED_QUANTILE as X ,count(*) as CASES , sum(ACTUAL) as SUM_ACTUAL,avg(ACTUAL) as AVG_ACTUAL,avg(PREDICTED) as AVG_PREDICTED from gl_df_train group by PREDICTED_QUANTILE  ")
      gain_lift_df.train$CUMACTUAL <- cumsum(as.numeric(gain_lift_df.train$SUM_ACTUAL ))
      gain_lift_df.train$GAIN <- gain_lift_df.train$CUMACTUAL /totalactualpremium.train 
      gain_lift_df.train$TEST_OR_TRAIN <- "TRAIN"
      
      
      gain_lift_df.final <- bind_rows(gain_lift_df,gain_lift_df.train)
      
      
      Baseline.Test_Y <- round(mean(gain_lift_df$AVG_ACTUAL) + (max(gain_lift_df$AVG_ACTUAL)-min(gain_lift_df$AVG_ACTUAL))/20,2)
      Baseline.Train_Y <- round(mean(gain_lift_df.train$AVG_ACTUAL) + (max(gain_lift_df.train$AVG_ACTUAL)-min(gain_lift_df.train$AVG_ACTUAL))/50,2)
      
    
      
      ann_text.sm <- geom_text(data=data.frame(x= c(5,5), 
                                               y= c(max(gain_lift_df.train$AVG_ACTUAL) ,
                                                    max(gain_lift_df.train$AVG_ACTUAL )),
                                               label=c(paste("AUC = ", round(aucr,2) ,sep =""),
                                                       paste("AUC = ", round(aucr.train,2) ,sep ="")), 
                                               TEST_OR_TRAIN =factor(c("TEST","TRAIN"),levels = c("TEST","TRAIN")) ,
                                               color="blue"), 
                               aes(x,y,label=label), inherit.aes=FALSE)
      
      
      Baselinem.Test_Y <- round(mean(gain_lift_df$AVG_ACTUAL),2)
      Baselinem.Train_Y <- round(mean(gain_lift_df.train$AVG_ACTUAL),2)
      
      
      ann_text <- geom_text(data=data.frame(x= c(5,5), 
                                            y= c(Baseline.Test_Y ,
                                                 Baseline.Train_Y ),
                                            label=c(paste("Baseline.Test = ",  Baselinem.Test_Y ,sep =""),
                                                    paste("Baseline.Train = ",  Baselinem.Train_Y ,sep ="")), 
                                            TEST_OR_TRAIN =factor(c("TEST","TRAIN"),levels = c("TEST","TRAIN")) ,
                                            color="blue"), 
                            aes(x,y,label=label), inherit.aes=FALSE)
      
      
      lift_chart<- ggplot(gain_lift_df.final, aes(factor(X), AVG_ACTUAL)) + 
        geom_bar(stat = "identity") + 
        facet_wrap(~TEST_OR_TRAIN,ncol=2)+
        geom_hline(data=gain_lift_df.final[which(gain_lift_df.final$TEST_OR_TRAIN== "TEST"),],
                   aes(yintercept=mean(gain_lift_df$AVG_ACTUAL)),
                   colour="#000099", linetype = "dashed")+
        geom_hline(data=gain_lift_df.final[which(gain_lift_df.final$TEST_OR_TRAIN== "TRAIN"),],
                   aes(yintercept=mean(gain_lift_df.train$AVG_ACTUAL)),
                   colour="#000099", linetype = "dashed")+
        xlab("Prediction Decile - High to Low") +
        ylab("Average Bind Ratio ") +
        scale_y_continuous(labels = comma)+
        ann_text+
        ann_text.sm+
        ggtitle(paste(Description , ": Lift Chart ", "\n",PredictedVariableName,sep =""))+
        theme(plot.title = element_text(color="dodgerblue", size=14, face="bold"),
              legend.position = "bottom")
      
      
      
      quantile = 10 
      
      gain <- data.frame(Decile = seq(0:quantile)-1, Average = c(0,gain_lift_df$GAIN))
      gain <- data.frame(gain,Base = gain$Decile/quantile)
      gain.long <- melt(gain, id.vars = "Decile")
      gain.long$TEST_OR_TRAIN <- "TEST"
      
      gain.train <- data.frame(Decile = seq(0:quantile)-1, Average = c(0,gain_lift_df.train$GAIN))
      gain.train <- data.frame(gain.train,Base = gain.train$Decile/quantile)
      gain.long.train <- melt(gain.train, id.vars = "Decile")
      gain.long.train$TEST_OR_TRAIN <- "TRAIN"
      
      gain.long.fin <- bind_rows(gain.long,gain.long.train)
      
      gain_chart <- ggplot(gain.long.fin ,aes(Decile,value,linetype=variable))+geom_line()+geom_point() +
        facet_wrap(~TEST_OR_TRAIN,ncol=2)+
        xlab("Prediction Decile - High to Low") +
        scale_x_continuous(breaks=0:10)+
        ylab("% Bind Captured - Cumulative") +
        scale_y_continuous(breaks=seq(0,1,0.2))+
        ggtitle(paste(Description , ": Gain Chart \n",PredictedVariableName,sep =""))+
        theme(plot.title = element_text(color="dodgerblue", size=14, face="bold"),
              legend.position = "bottom")
      
    }
    else{
      # decile line plot
      decileLineChart<-
        ggplot(data=
                 prediction.fin %>% 
                 group_by(TEST_OR_TRAIN,PREDICTED_DECILE) %>% 
                 summarise(MEAN_OF_PREDICTED = mean(PREDICTED)
                           ,MEAN_OF_ACTUAL = mean(ACTUAL)
                 ) %>% 
                 mutate(PREDICTED_DECILE = PREDICTED_DECILE) %>% 
                 gather(Output, MEAN,-PREDICTED_DECILE,-TEST_OR_TRAIN)
               
               ,aes(x = PREDICTED_DECILE, y = MEAN ,color = Output)) + 
        geom_line(size=1)+
        facet_wrap(~TEST_OR_TRAIN,ncol=2)+
        scale_x_reverse(breaks=1:10)+
        scale_y_continuous(labels = comma)+
        ggtitle(paste(Description , ": Actual vs Predicted Decile Report, Mean \n",PredictedVariableName, sep =""))+
        theme(plot.title = element_text(color="dodgerblue", size=14, face="bold"),
              legend.position = "bottom")
      
      decileLineChart_median<-
        ggplot(data=
                 prediction.fin %>% 
                 group_by(TEST_OR_TRAIN,PREDICTED_DECILE) %>% 
                 summarise(MEDIAN_OF_PREDICTED = median(PREDICTED)
                           ,MEDIAN_OF_ACTUAL = median(ACTUAL)
                 ) %>% 
                 mutate(PREDICTED_DECILE = PREDICTED_DECILE) %>% 
                 gather(Output, MEDIAN,-PREDICTED_DECILE,-TEST_OR_TRAIN)
               
               ,aes(x = PREDICTED_DECILE, y = MEDIAN,color = Output)) + 
        geom_line(size=1)+
        facet_wrap(~TEST_OR_TRAIN,ncol=2)+
        scale_x_reverse(breaks=1:10)+
        scale_y_continuous(labels = comma)+
        ggtitle(paste(Description , ": Actual vs Predicted Decile Report, Median \n",PredictedVariableName, sep =""))+
        theme(plot.title = element_text(color="dodgerblue", size=14, face="bold"),
              legend.position = "bottom")
      
      
      ## lift chart of actual propensity by predicted decile
       propensity_lift_chart_test_train<- 
         ggplot(data= prediction.fin 
                ,aes(x = PREDICTED_DECILE, y = ACTUAL )) +
         geom_bar(stat="identity",fill="grey")+
         facet_wrap(~TEST_OR_TRAIN,ncol=2)+
         scale_x_reverse(breaks=1:10)+
         scale_y_continuous(labels = comma)+
         ggtitle(paste(Description , ": Sum of Actual Target Variable by Decile \n",PredictedVariableName,sep =""))+
         theme(plot.title = element_text(color="dodgerblue", size=14, face="bold"),
               legend.position = "bottom")
      
      
      candle_decile_plot_test_train<-
        ggplot(data=
                 prediction.fin[, c('PREDICTED_DECILE', 'PREDICTED', 'ACTUAL','TEST_OR_TRAIN')]  %>% 
                 mutate(PREDICTED_DECILE = factor(PREDICTED_DECILE,levels=10:1)) %>% 
                 gather(Output, ACTUAL ,-PREDICTED_DECILE,-TEST_OR_TRAIN)
               ,aes(x = PREDICTED_DECILE, y = ACTUAL ,fill = Output)) + 
        geom_boxplot()+
        facet_wrap(~TEST_OR_TRAIN,ncol=2)+
        scale_y_continuous(labels = comma)+
        ggtitle(paste(Description , ": Box Plot of Actual Target by Decile \n",PredictedVariableName, sep =""))+
        theme(plot.title = element_text(color="dodgerblue", size=14, face="bold"),
              legend.position = "bottom")
      
      
      predictionScatter_test_train<- 
        ggplot(data= prediction.fin 
               ,aes(x = PREDICTED, y = ACTUAL )) +
        geom_point()+
        facet_wrap(~TEST_OR_TRAIN,ncol=2)+
        scale_y_continuous(labels = comma)+
        ggtitle(paste(Description , ": Actual vs Predicted Scatter Plot \n",PredictedVariableName, sep =""))+
        theme(plot.title = element_text(color="dodgerblue", size=14, face="bold"),
              legend.position = "bottom")
      
      
      totalactualpremium    <- sum(gl_df$ACTUAL) 
      
      gl_df <- 
        gl_df %>% 
        mutate(PREDICTED_QUANTILE  = ntile(x=desc(gl_df$PREDICTED),n=10))
      
      gain_lift_df <- sqldf("select PREDICTED_QUANTILE as X ,count(*) as CASES , sum(ACTUAL) as SUM_ACTUAL,avg(ACTUAL) as AVG_ACTUAL,avg(PREDICTED) as AVG_PREDICTED from gl_df group by PREDICTED_QUANTILE  ")
      
      gain_lift_df$CUMACTUAL <- cumsum(as.numeric(gain_lift_df$SUM_ACTUAL ))
      gain_lift_df$GAIN <- gain_lift_df$CUMACTUAL /totalactualpremium
      gain_lift_df$TEST_OR_TRAIN <- "TEST"
      
      
      
      totalactualpremium.train   <- sum(gl_df_train$ACTUAL) 
      gl_df_train <- 
        gl_df_train %>% 
        mutate(PREDICTED_QUANTILE  = ntile(x=desc(gl_df_train$PREDICTED),n=10))
      
      
      gain_lift_df.train <- sqldf("select PREDICTED_QUANTILE as X ,count(*) as CASES , sum(ACTUAL) as SUM_ACTUAL,avg(ACTUAL) as AVG_ACTUAL,avg(PREDICTED) as AVG_PREDICTED from gl_df_train group by PREDICTED_QUANTILE  ")
      gain_lift_df.train$CUMACTUAL <- cumsum(as.numeric(gain_lift_df.train$SUM_ACTUAL ))
      gain_lift_df.train$GAIN <- gain_lift_df.train$CUMACTUAL /totalactualpremium.train 
      gain_lift_df.train$TEST_OR_TRAIN <- "TRAIN"
      
      
      gain_lift_df.final <- bind_rows(gain_lift_df,gain_lift_df.train)
      

      Baseline.Test_Y <- round(mean(gain_lift_df$AVG_ACTUAL) + (max(gain_lift_df$AVG_ACTUAL)-min(gain_lift_df$AVG_ACTUAL))/20,2)
      Baseline.Train_Y <- round(mean(gain_lift_df.train$AVG_ACTUAL) + (max(gain_lift_df.train$AVG_ACTUAL)-min(gain_lift_df.train$AVG_ACTUAL))/50,2)
      
      Baselinem.Test_Y <- round(mean(gain_lift_df$AVG_ACTUAL))
      Baselinem.Train_Y <- round(mean(gain_lift_df.train$AVG_ACTUAL))
      
   
      ann_text.sm <- geom_text(data=data.frame(x= c(5,5), 
                                               y= c(max(gain_lift_df.train$AVG_ACTUAL) ,
                                                    max(gain_lift_df.train$AVG_ACTUAL )),
                                            label=c(paste("Spearman = ", round(spearman,2) ,sep =""),
                                                    paste("Spearman = ", round(spearman.train,2) ,sep ="")), 
                                            TEST_OR_TRAIN =factor(c("TEST","TRAIN"),levels = c("TEST","TRAIN"))), 
                aes(x,y,label=label), inherit.aes=FALSE)
      
      
      
      ann_text_data=data.frame(x= c(5,5), 
                      y= c(Baseline.Test_Y ,
                           Baseline.Train_Y ),
                      label= c(paste("Baseline.Test = ", Baseline.Test_Y ,sep =""),
                               paste("Baseline.Train = ", Baseline.Train_Y ,sep ="")
                               ), 
                      TEST_OR_TRAIN =factor(c("TEST","TRAIN"),
                                            levels = c("TEST","TRAIN")
                                            )
                      )
      Baselinem.Test_Y  <-  prettyNum(Baselinem.Test_Y, big.mark=",", scientific=FALSE)
      Baselinem.Train_Y  <-  prettyNum(Baselinem.Train_Y, big.mark=",", scientific=FALSE)
      
      ann_text_data$new_lbl <- c(paste("Baseline.Test = ", Baselinem.Test_Y ,sep =""),
                                 paste("Baseline.Train = ", Baselinem.Train_Y ,sep ="")
                                  )
      
      ann_text <- geom_text(data=ann_text_data, 
                            aes(x,y,label=new_lbl), inherit.aes=FALSE)
      
      dummy2 <- data.frame(X = factor(c("TEST","TRAIN"),levels = c("TEST","TRAIN")), Z = c(mean(gain_lift_df$AVG_ACTUAL), mean(gain_lift_df.train$AVG_ACTUAL)))
      
      lift_chart<- ggplot(gain_lift_df.final, aes(factor(X), AVG_ACTUAL)) + 
        geom_bar(stat = "identity") + 
        facet_wrap(~TEST_OR_TRAIN,ncol=2)+
        geom_hline(data=gain_lift_df.final[which(gain_lift_df.final$TEST_OR_TRAIN== "TEST"),],
                   aes(yintercept=mean(gain_lift_df$AVG_ACTUAL)),
                   colour="#000099", linetype = "dashed")+
        geom_hline(data=gain_lift_df.final[which(gain_lift_df.final$TEST_OR_TRAIN== "TRAIN"),],
                   aes(yintercept=mean(gain_lift_df.train$AVG_ACTUAL)),
                   colour="#000099", linetype = "dashed")+
        xlab("Prediction Decile - High to Low") +
        ylab("Average GWP") +
        scale_y_continuous(labels = comma)+
        ann_text+
        ann_text.sm+
        ggtitle(paste(Description , ": Lift Chart ","\n",PredictedVariableName,sep =""))+
        theme(plot.title = element_text(color="dodgerblue", size=14, face="bold"),
              legend.position = "bottom")
      
      
      quantile = 10 
      
      gain <- data.frame(Decile = seq(0:quantile)-1, Average = c(0,gain_lift_df$GAIN))
      gain <- data.frame(gain,Base = gain$Decile/quantile)
      gain.long <- melt(gain, id.vars = "Decile")
      gain.long$TEST_OR_TRAIN <- "TEST"
      
      gain.train <- data.frame(Decile = seq(0:quantile)-1, Average = c(0,gain_lift_df.train$GAIN))
      gain.train <- data.frame(gain.train,Base = gain.train$Decile/quantile)
      gain.long.train <- melt(gain.train, id.vars = "Decile")
      gain.long.train$TEST_OR_TRAIN <- "TRAIN"
      
      gain.long.fin <- bind_rows(gain.long,gain.long.train)
      
      gain.long.fin1 <<- gain.long.fin
      
      gain_chart <- ggplot(gain.long.fin ,aes(Decile,value,linetype=variable))+geom_line()+geom_point() +
        facet_wrap(~TEST_OR_TRAIN,ncol=2)+
        xlab("Prediction Decile - High to Low") +
        scale_x_continuous(breaks=0:10)+
        ylab("% GWP Captured - Cumulative") +
        scale_y_continuous(breaks=seq(0,1,0.2))+
        ggtitle(paste(Description , ": Gain Chart \n",PredictedVariableName,sep =""))+
        theme(plot.title = element_text(color="dodgerblue", size=14, face="bold"),
              legend.position = "bottom")
      
     
      
      
      decile = 10  
      breaks = "c(3, 7)"
      
      data <-  bc_df
      
      names(data) <- c("REALIZED","PREDICTED")
      
      
     gwp_obs_decile <- n_sect2(data$REALIZED, decile, breaks)
     gwp_pred_decile <- n_sect2(data$PREDICTED, decile, breaks)
     gwp_decile_df <- as.data.frame(cbind(obs = gwp_obs_decile$q_vec[], pred = gwp_pred_decile$q_vec))
      
     gwp_decile_freqs <- xtabs(data = gwp_decile_df, formula = ~ obs + pred)
     
     breaks2 <- length(gregexpr(",", breaks, fixed = TRUE)[[1]]) + 2
     m = prop.table(matrix(gwp_decile_freqs,breaks2), 2)
     
     m.melt <- melt(m)
     m.melt$TEST_OR_TRAIN <- "TEST"
     
   
     data.train <-  bc_df.train
     
     names(data.train) <- c("REALIZED","PREDICTED")
     gwp_obs_decile.train <- n_sect2(data.train$REALIZED, decile, breaks)
     gwp_pred_decile.train <- n_sect2(data.train$PREDICTED, decile, breaks)
     gwp_decile_df.train <- as.data.frame(cbind(obs = gwp_obs_decile.train$q_vec[], pred = gwp_pred_decile.train$q_vec))
     
     gwp_decile_freqs.train <- xtabs(data = gwp_decile_df.train, formula = ~ obs + pred)
     
     breaks2.train <- length(gregexpr(",", breaks, fixed = TRUE)[[1]]) + 2
     m.train = prop.table(matrix(gwp_decile_freqs.train,breaks2.train), 2)
     
   
     
     m.melt.train <- melt(m.train)
     m.melt.train$TEST_OR_TRAIN <- "TRAIN"
     
    
     m.fin <- bind_rows(m.melt,m.melt.train)
     
     
     
     conf_mat <- ggplot(m.fin, aes(X2,X1, fill=value,label= sprintf("%.0f%%", value * 100)))+geom_raster()+geom_text(size=5) +
       facet_wrap(~TEST_OR_TRAIN,ncol=2)+
       ggtitle(paste(Description , ": Confusion Matrix \n ",PredictedVariableName,sep =""))+
       scale_y_discrete("Actual", limits = c("High", "Medium", "Low")) + scale_x_discrete("Predicted", limits = c("High", "Medium", "Low"))+
       theme(plot.title = element_text(color="dodgerblue", size=14, face="bold"),
             legend.position = "bottom")
     
      
    }
    
    
    if ( LogScaleReports == 'Y'){
      
      candle_decile_plot_log_test_train<-
        ggplot(data=
                 prediction.fin[, c('PREDICTED_DECILE_LOG', 'PREDICTED_LOG', 'ACTUAL_LOG','TEST_OR_TRAIN')]  %>% 
                 mutate(PREDICTED_DECILE_LOG = factor(PREDICTED_DECILE_LOG,levels=10:1)) %>% 
                 gather(Output, ACTUAL_LOG ,-PREDICTED_DECILE_LOG,-TEST_OR_TRAIN)
               ,aes(x = PREDICTED_DECILE_LOG, y = ACTUAL_LOG ,fill = Output)) + 
        geom_boxplot()+
        facet_wrap(~TEST_OR_TRAIN,ncol=2)+
        ggtitle(paste(Description , ": Box Plot of Actual Target by Decile, Log Scale \n",PredictedVariableName, sep =""))+
        theme(plot.title = element_text(color="dodgerblue", size=14, face="bold"),
              legend.position = "bottom")
      
      
      predictionScatter_test_trainLog<- 
        ggplot(data= prediction.fin 
               ,aes(x = PREDICTED_LOG, y = ACTUAL_LOG )) +
        geom_point()+
        facet_wrap(~TEST_OR_TRAIN,ncol=2)+
        ggtitle(paste(Description , ": Actual vs Predicted Scatter Plot, Log Scale \n",PredictedVariableName, sep =""))+
        theme(plot.title = element_text(color="dodgerblue", size=14, face="bold"),
              legend.position = "bottom")
    }
    
    if (Target == 'continuous'){
      
      if( LogScaleReports == 'Y'){
        
        plot.list<-list(decileLineChart
                        ,decileLineChart_median
                        ,propensity_lift_chart_test_train
                        ,candle_decile_plot_test_train
                        ,candle_decile_plot_log_test_train
                        ,predictionScatter_test_train
                        ,predictionScatter_test_trainLog
                        ,lift_chart
                        ,gain_chart
                        ,conf_mat)
      }
      else{
        plot.list<-list(decileLineChart
                        ,decileLineChart_median
                        ,propensity_lift_chart_test_train
                        ,candle_decile_plot_test_train
                        ,predictionScatter_test_train
                        ,lift_chart
                        ,gain_chart
                        ,conf_mat)
      }
    } else{
      
      plot.list<-list(decileLineChart
                      ,propensity_lift_chart_test_train
                      ,aucChart
                      ,lift_chart
                      ,gain_chart
      )
    }
    
    
    #===============================================================
    ## WRITING THE KEY STATS OUTPUT TO .csv
    KEY.STATS_new<-dat.results
    
    COMPARE.FRAME_new<-data.frame("Variable"= names(dat.results),"TEST"=t(dat.results),"TRAIN"=t(dat.results.train))
    
    #===============================================================
    #===============================================================
    ## recording the plots to .pdf
    
    cat("Writing performance charts to file ...\n")
    pdf(paste(outputDirFileName ,".pdf", sep = ""), paper='A4r',width=16,height=12)
    suppressMessages({print(plot.list)})
    dev.off()
    
    #===============================================================
    #===============================================================
    ## Writing individual product variable importance list and test v train comparrisons to .csv
    write.csv(COMPARE.FRAME_new,file=paste0(outputDirFileName,"_TEST_v_TRAIN.csv"),row.names=F)
    write.csv(KEY.STATS_new, file=paste0(outputDirFileName,"_KEYSTATS.csv"),row.names=F) 
    #===============================================================
    #===============================================================
  } else {
    
    KEY.STATS_new<-dat.results
    write.csv(KEY.STATS_new, file=paste0(outputDirFileName,"_KEYSTATS.csv"),row.names=F) 
    
  }
  
  
  return (0)
  
}

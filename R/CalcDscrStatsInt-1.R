CalcDscrStatsInt <- function( data)
  
{
  
  
  DescriptiveStats <- function(x){
    
    list(
      variableClass = class(x), #Variable Class
      NumOfObservs = length(x), #Number Of Observations
      NumOfDstnct = length(unique(x)),#NumberofDistinct Values
      NumOfMsng = sum(is.na(x)) ,#NumberOfMissingObservations 
      PercOfMisObs = (sum(is.na(x))/length(x))*100, #Percentage of Missing Observations
      if(is.numeric(x)){
        Min  = round(min(x,na.rm = T),2)
      },# Min value
      if(is.numeric(x)){
        Max =  round(max(x,na.rm = T),2)
      }, # Max value
      if(is.numeric(x)){
        Range =  round(max(x,na.rm = T),2) - round(min(x,na.rm = T),2)
      }, # Range
      if(is.numeric(x)){Mean = round(mean(x,na.rm = T),2)}, # Mean Value 
      if(is.numeric(x)){Median = round(median(x,na.rm = T),2)}, # Median Value 
      if(is.numeric(x)){Variance =  round(var(x,na.rm = T),2)}, # Variance
      if(is.numeric(x)){SD = round(sd(x,na.rm = T),2)}, # Standard deviation
      if(is.numeric(x)){Quartile_0 =  quantile(x,na.rm = T)[1]}, #Quantile_0%
      if(is.numeric(x)){Quartile_25 =  quantile(x,na.rm = T)[2]}, #Quantile_25%
      if(is.numeric(x)){Quartile_50 =  quantile(x,na.rm = T)[3]}, #Quantile_50%
      if(is.numeric(x)){Quartile_75 =  quantile(x,na.rm = T)[4]}, #Quantile_75%
      if(is.numeric(x)){Quartile_100 =  quantile(x,na.rm = T)[5]}, #Quantile_100%
      if(is.numeric(x)){IQR(x,na.rm = T)}, #Inter Quartile Range
      if(is.numeric(x)){quantile(x,na.rm = T)[2] - 1.5*IQR(x,na.rm = T)},
      if(is.numeric(x)){quantile(x,na.rm = T)[4] + 1.5*IQR(x,na.rm = T)},
      if(is.numeric(x)){length(which( x< (quantile(x,na.rm = T)[2] - 1.5*IQR(x,na.rm = T))  | x > quantile(x,na.rm = T)[4] + 1.5*IQR(x,na.rm = T)))}
    )
    
    
  }
  
  DStats  <- sapply( data , DescriptiveStats)

  DscrpStats1 <- t(DStats)
  

  
  cnames <- colnames(data)
  
  cnamesDf <- as.data.frame(cnames)
  colnames(cnamesDf) <- "VaribaleName"
 
  DscrpStats <- cbind(cnamesDf,DscrpStats1)
  
  
  colnames(DscrpStats) <- c( "VariableName","VariableClass","NumOfObservs", "NumOfDistinctObservs","NumOfMissingObservs",
                             "PercOfOfMissingObservs", "Min","Max", "Range","Mean","Median","Variance","SD",
                             "Quartile_0","Quartile_25","Quartile_50","Quartile_75","Qurntile_100",
                             "Inter_Quartile_Range","Outlier_Range_Low","Outlier_Range_High", "Number_Of_Outliers")
  
  
  rownames(DscrpStats) <- NULL
  
  library(dplyr)
  
  DscrpStats1 <- select(DscrpStats, VariableName,VariableClass,NumOfObservs,NumOfDistinctObservs,
                        NumOfMissingObservs,PercOfOfMissingObservs)
  
  return(DscrpStats1)
  
  
}

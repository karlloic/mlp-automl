rm(list = ls())
setwd("C:/Users/rmadala/Documents/Code/Shiny")
getwd()

df <- read.csv("C:/Users/rmadala/Documents/Code/Shiny/ihsNew.csv",header = TRUE)

source("trainModel.R")


LrModel(df,'elec_use',0.85)


source("predictModel.R")

lrPredict(lr_ft,testDf)

head(predDf)

source("modelEval.R")

modelEval(predDf)

head(dat.results)



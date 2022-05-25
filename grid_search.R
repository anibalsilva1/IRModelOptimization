library(IR.SGB)
library(performanceEstimation)
library(tidyverse)
library(IRon)
library(robustbase)

load(file="datsets_to_use.RData")

all_metrics <- function(trues, preds, phi.ctrl, ...){
  
  phi.trues <- as.vector(phi(trues, phi.parms = phi.ctrl))
  
  c(mse = as.vector(mean((trues - preds)^2)),
    sera = as.vector(sera(trues,preds,phi.trues)))
  
}

WFs <- list()

params <- list(nrounds = c(250, 500),
               max_depth = c(3,5,7),
               eta = 10^seq(-3,-1,1))



WFs$xgboost <- params
WFs$xSERAgboost <- params
WFs$LGBM <- params
WFs$LGBMSERA <- params


metrics = c("mse", "sera")


for(i in 1:length(datasets_to_use)){
  
  data_train <- datasets_to_use[[i]]$train
  data_name  <- names(datasets_to_use)[i]
  formula    <- datasets_to_use[[i]]$formula
  t          <- formula[[2]]
  
  phi.ctrl  <- datasets_to_use[[i]]$p.ctrl
  
  for(w in names(WFs)){
    
    resObj <- paste(data_name, w, "Res",sep = ".")
    
    
    assign(resObj, 
           performanceEstimation(
             PredTask(formula, data_train),
             c(do.call(
               what = "workflowVariants",
               args = c(list(paste0("wf.", w)), WFs[[w]]))),
             EstimationTask(
               metrics = metrics,
               CV(nReps = 1, nFolds = 10, strat=T),
               evaluator = "all_metrics",
               evaluator.pars = list(phi.ctrl = phi.ctrl)),
             cluster=T
           )
           
    )
    save(list =  resObj, file = paste0("results/",data_name,".", w, ".RData"))
  }
}

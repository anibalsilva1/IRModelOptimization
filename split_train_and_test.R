library(caret)
library(tidyverse)
library(IR.SGB)
library(robustbase)
library(IRon)

load("datsets_to_use.RData")

res <- list()

for(i in 1:length(datasets_to_use)){
  
  ds_name <- datasets_to_use[[i]]$name
  data <- datasets_to_use[[i]]$data
  formula <- datasets_to_use[[i]]$formula
  target <- formula[[2]]
  
  s <- createDataPartition(y=pull(data, target), p=0.8, list=F)
  
  train <- data %>% slice(s)
  test <- data %>% slice(-s)
  
  y <- pull(train, target)
  
  extr <- get_extreme_type(y)
  p.ctrl <- phi.control(y=y, extr.type = extr)
  
  res[[names(datasets_to_use)[[i]]]] <- list("data" = data,
                                             "train" = train,
                                             "test" = test,
                                             "p.ctrl" = p.ctrl,
                                             "formula" = formula)
}

datasets_to_use <- res
save(datasets_to_use, file="datsets_to_use.RData")

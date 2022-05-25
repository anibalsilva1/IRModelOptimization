library(IR.SGB)
library(tidyverse)
library(IRon)
load("datasets.rda")

get_ds_info <- function(datasets){
  
  resdatasets <- tibble(
    dataset_id = numeric(),
    dataset_name = character(),
    obs = numeric(),
    nom = numeric(),
    num = numeric(),
    rare = numeric(),
    IR = numeric()
  )
  
  for(i in 1:length(datasets)){
    
    tgt <- datasets[[i]]$formula[[2]]
    ds <- datasets[[i]]$train
    target <- pull(ds, tgt)
    
    ph.ctrl <- phi.control(target)
    phi.tr <- phi(target, ph.ctrl)
    
    IR <- length(target[phi.tr == 1]) / length(target[phi.tr < 1])
    
    resdatasets <- resdatasets %>% add_row(
      dataset_id = i,
      dataset_name = names(datasets)[i],
      obs = nrow(ds),
      nom = ds %>% dplyr::select(where(is.factor)) %>% ncol(),
      num = ds %>% dplyr::select(where(is.numeric)) %>% ncol(),
      rare = length(target[phi.tr == 1]),
      IR = IR * 100
    )
    
  }
  return(resdatasets)
  
}

resdatasets <- get_ds_info(datasets)
datasets_outliers <- resdatasets %>% filter(IR != 0)
datasets_outliers

xtable::xtable(datasets_outliers, type = "latex", file = "dataset_outliers.tex")




datasets_to_use <- datasets[names(datasets) %in% datasets_outliers$dataset_name]

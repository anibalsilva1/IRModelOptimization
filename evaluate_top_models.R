library(tidyverse)
library(performanceEstimation)
library(IRon)
library(latex2exp)

load("datasets_imbalanced.RData")

path <- "results"

files <- list.files(path)

setwd(path)

results <- sapply(files, function(x) get(load(x)), simplify = TRUE)

nWFs <- c("xgboost", "xSERAgboost", "LGBM", "LGBMSERA")

nDs <- sapply(1:length(datasets_to_use), FUN = function(i) names(datasets_to_use)[i])

res <- merge_results(results, nDs, nWFs)

setwd("../")

avg_sera <- get_avg_scores_by_metric(res, metric="sera") %>% rename(avg_sera=avg)
avg_mse <- get_avg_scores_by_metric(res, metric="mse") %>% rename(avg_mse=avg)

all_info <- inner_join(avg_mse, avg_sera, by=c("dataset", "workflow", "params"))


best_models_mse_df <- all_info %>%
  separate(workflow, into=c("workflows", "version"), sep="\\.v") %>% 
  group_by(dataset, workflows) %>% 
  slice_min(avg_mse, n=1, with_ties=FALSE) %>%
  unite(col=workflow, workflows, version, sep=".v")

best_models_sera_df <- all_info %>%
  separate(workflow, into=c("workflows", "version"), sep="\\.v") %>% 
  group_by(dataset, workflows) %>% 
  slice_min(avg_sera, n=1, with_ties=FALSE) %>%
  unite(col=workflow, workflows, version, sep=".v")

best_models_sera_df %>% 
  group_by(dataset, workflow) %>% 
  slice_min(avg_sera, n=1) %>% dplyr::select(-params,-avg_mse) %>% 
  mutate(workflow=str_remove(workflow, "wf.")) %>% 
  mutate(workflow=str_remove(workflow, ".v.*")) %>% 
  pivot_wider(names_from="workflow", values_from="avg_sera")

b_mse <- get_best_workflows(best_models_mse_df, nmodels=4)
b_sera <- get_best_workflows(best_models_sera_df, nmodels=4)

             ##########################
             #### Bayes Sign Test #####
             ##########################


bayes_xsera_sera <- bayes_sign_test_topmodels(b_sera, oracle="xSERAgboost", metric="sera", rope=0.01)
bayes_lbmsera_sera <- bayes_sign_test_topmodels(b_sera, oracle="LGBMSERA", metric="sera", rope=0.01)

bayes_xsera_mse <- bayes_sign_test_topmodels(b_sera, oracle="xSERAgboost", metric="mse", rope=0.01)
bayes_lbmsera_mse <- bayes_sign_test_topmodels(b_sera, oracle="LGBMSERA", metric="mse", rope=0.01)


bayes_final_mse <- bayes_xsera_mse %>% bind_rows(bayes_lbmsera_mse) 


bayes_final_mse <- bayes_final_mse %>% 
  mutate(rows = row_number()) %>% 
  filter(rows %in% c(3, 4)) %>% 
  mutate(metric = if_else(metric=="mse", "MSE", metric)) %>% 
  mutate(model = str_remove(model, "wf.")) %>% 
  mutate(model=ifelse(str_detect(model, "LGBMS"), "LGBM (SERA)",
                      ifelse(str_detect(model, "xSERA"), "XGBoost (SERA)", 
                             ifelse(str_detect(model, "xgboost"), "XGBoost", model)))) %>% 
  mutate(oracle=ifelse(str_detect(oracle, "LGBMS"), "LGBM (SERA)",
                       ifelse(str_detect(oracle, "xSERA"), "XGBoost (SERA)", oracle))) %>% 
  pivot_longer(cols=c("modelProb", "oracleProb", "rope"), names_to="probs", values_to="prob") %>%
  mutate(prob=as.double(prob),
         probs=factor(probs, levels = c("oracleProb", "rope", "modelProb"), ordered = T),
         model=factor(model,
                      levels=c("XGBoost", "LGBM")),
         oracle=factor(oracle, 
                       levels=c("XGBoost (SERA)","LGBM (SERA)" )))

bayes_final_sera <- bayes_xsera_sera %>% bind_rows(bayes_lbmsera_sera)

bayes_final_sera <- bayes_final_sera %>% 
  mutate(rows = row_number()) %>% 
  filter(rows %in% c(3, 4)) %>% 
  mutate(metric = if_else(metric=="sera", "SERA", metric)) %>% 
  mutate(model = str_remove(model, "wf.")) %>% 
  mutate(model=ifelse(str_detect(model, "LGBMS"), "LGBM (SERA)",
                      ifelse(str_detect(model, "xSERA"), "XGBoost (SERA)", 
                             ifelse(str_detect(model, "xgboost"), "XGBoost", model)))) %>% 
  mutate(oracle=ifelse(str_detect(oracle, "LGBMS"), "LGBM (SERA)",
                       ifelse(str_detect(oracle, "xSERA"), "XGBoost (SERA)", oracle))) %>% 
  pivot_longer(cols=c("modelProb", "oracleProb", "rope"), names_to="probs", values_to="prob") %>%
  mutate(prob=as.double(prob),
         probs=factor(probs, levels = c("oracleProb", "rope", "modelProb"), ordered = T),
         model=factor(model,
                      levels=c("XGBoost", "LGBM")),
         oracle=factor(oracle, 
                       levels=c("XGBoost (SERA)","LGBM (SERA)" )))


levels(bayes_final_sera$oracle) <- c("XGBoost (SERA)" = TeX("$XGBoost^{\\textit{S}}$"),
                                     "LGBM (SERA)"= TeX("$LGBM^{\\textit{S}}$"))

levels(bayes_final_mse$oracle) <- c("XGBoost (SERA)" = TeX("$XGBoost^{\\textit{S}}$"),
                                    "LGBM (SERA)"= TeX("$LGBM^{\\textit{S}}$"))


bayes_final <- bayes_final_mse %>% bind_rows(bayes_final_sera)

bayes_plot <- bayes_final %>% 
  ggplot(mapping=aes(x=prob, y=model, fill=probs, groups=model), labeller=label_parsed) +
  geom_col() +
  labs(fill = "Posterior:", x = "Probabilities") +
  scale_fill_brewer(breaks = c("modelProb", "rope", "oracleProb"),
                    labels = c("standard", "rope", "ours"),
                    palette="Set1", direction=-1) +
  scale_y_discrete(labels=c("XGBoost" = TeX("$XGBoost^{\\textit{M}}$"),
                            "LGBM"= TeX("$LGBM^{\\textit{M}}$"))) +
  facet_grid(oracle ~ metric, labeller = label_parsed, scales="free", space="free") + 
  theme(
    panel.background = element_rect(fill="white", colour="black"),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size=20),
    axis.title.x = element_text(size=25),
    axis.title.y = element_blank(),
    strip.background = element_rect(fill="grey80", colour="black"),
    strip.text.y = element_text(size=20, margin=margin(t=40, r=20, b=40, l=30)),
    strip.text.x = element_text(size=20, margin=margin(t=20, r=20, b=20, l=30)),
    legend.text = element_text(size=25,vjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(face="bold", size=30)
  )

bayes_plot

#ggsave(filename="bayes_sign_rsults.png", plot=bayes_plot, width=14, height=7)


       ##################################
       ###### Evaluate predictions ######
       ##################################



top_models <- best_models_sera_df %>% 
  mutate(workflow=as.character(workflow)) %>% 
  mutate(workflow = str_remove(workflow, ".v.*"))


#preds_best_sera <- get_predictions(dataset=datasets_to_use, bestmodels=top_models) # to get predictions

load("preds.RData")

score_sera <- get_test_scores(preds_best_sera, metric = "sera")
score_mse <- get_test_scores(preds_best_sera, metric = "mse")

score_sera
score_mse

score_sera %>%
  mutate(id = row_number()) %>% 
  pivot_longer(cols=matches("LG|XG"), names_to="models", values_to="errs") %>% 
  group_by(dataset) %>% 
  slice_min(errs) %>% 
  arrange(id)


score_sera %>% 
  pivot_longer(cols=matches("XG|LG")) %>% 
  group_by(dataset) %>% 
  slice_min(value) %>% 
  ungroup() %>% 
  count(name)

score_sera %>% 
  dplyr::select(dataset, matches("XG")) %>% 
  pivot_longer(cols=matches("XG")) %>% 
  group_by(dataset) %>% 
  slice_min(value) %>% 
  ungroup() %>% 
  count(name)


score_mse %>% 
  pivot_longer(cols=matches("LG|XG")) %>% 
  group_by(dataset) %>% 
  slice_min(value) %>% 
  ungroup() %>% 
  count(name)

score_mse <- score_mse %>% 
  mutate(id=row_number())


score_sera %>% 
  pivot_longer(cols=matches("LG")) %>% 
  group_by(dataset) %>% 
  slice_min(value) %>%
  ungroup() %>% 
  count(name)
  
  
xtable::xtable(score_sera[, c(5,4,3,2)], type="latex", display=c("d","e","e","e","e"), align="ccccc")
xtable::xtable(score_mse[, c(5,4,3,2)], type="latex", display=c("d","e","e","e","e"), align="ccccc")

##########################
##### Ranking models #####
##########################

ranks_sera <- score_sera %>% 
  pivot_longer(cols=matches("LG|XG"), names_to="models", values_to="errs") %>% 
  group_by(dataset) %>%
  arrange(errs) %>% 
  mutate(rank=row_number(),
         metric="SERA",
         models=factor(models, levels=c("XGBoost", "XGBoost_SERA", "LGBM", "LGBM_SERA"))) 

ranks_mse <- score_mse %>% 
  pivot_longer(cols=matches("LG|XG"), names_to="models", values_to="errs") %>% 
  group_by(dataset) %>%
  arrange(errs) %>% 
  mutate(rank=row_number(),
         metric="MSE",
         models=factor(models, levels=c("XGBoost", "XGBoost_SERA", "LGBM", "LGBM_SERA")))


lbls <-  c("XGBoost_SERA"= TeX("$XGBoost^{\\textit{S}}$"),
           "XGBoost"     = TeX("$XGBoost^{\\textit{M}}$"),
           "LGBM_SERA"   = TeX("$LGBM^{\\textit{S}}$"),
           "LGBM"        = TeX("$LGBM^{\\textit{M}}$")
)

ranks_sera %>% 
  group_by(models) %>% 
  count(rank) %>% 
  arrange(desc(rank))




ranks_plot <- ranks_mse %>% 
  bind_rows(ranks_sera) %>% 
  ggplot(mapping=aes(x=models, y=rank, fill=models)) +
  geom_boxplot(width=0.8) +
  scale_fill_brewer(labels=lbls, palette="Set1") +
  xlab("Models") +
  ylab("Rank") +
  labs(fill = "Models:") +
  facet_wrap(vars(metric)) + 
  ggplot2::theme(
    panel.background = element_rect(fill="white", colour="black"),
    panel.grid.major = element_blank(),
    legend.title = element_text(face = "bold", size=25),
    legend.key = element_blank(),
    legend.text = element_text(size=20),
    legend.position = "bottom",
    plot.title = element_text(size=25),
    axis.title.y = element_text(size=20),
    axis.text.y = element_text(size=15),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text.x = element_text(size=20, margin=margin(t=10, r=10, b=10, l=20)),
    strip.background = element_rect(fill="grey80", colour="black")
  )



ranks_plot
ggsave(filename="ranks_plot.png", plot=ranks_plot, width=10, height=5)

library(IR.SGB)
library(tidyverse)
library(performanceEstimation)
library(IRon)
library(latex2exp)

load("datsets_to_use.RData")

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



b_mse <- get_best_workflows(best_models_mse_df, nmodels=4)
b_sera <- get_best_workflows(best_models_sera_df, nmodels=4)

                                                                      ##########################
                                                                      #### Bayes Sign Test #####
                                                                      ##########################


bayes_xsera_sera <- bayes_sign_test_topmodels(b_sera, oracle="xSERAgboost", metric="sera", rope=0.01)
bayes_lbmsera_sera <- bayes_sign_test_topmodels(b_sera, oracle="LGBMSERA", metric="sera", rope=0.01)


bayes_final_sera <- bayes_xsera_sera %>% bind_rows(bayes_lbmsera_sera)

bayes_final_sera <- bayes_final_sera %>% 
  mutate(model = str_remove(model, "wf.")) %>% 
  mutate(model=ifelse(str_detect(model, "LGBMS"), "LGBM (SERA)",
                      ifelse(str_detect(model, "xSERA"), "XGBoost (SERA)", 
                             ifelse(str_detect(model, "xgboost"), "XGBoost", model)))) %>% 
  mutate(oracle=ifelse(str_detect(oracle, "LGBMS"), "LGBM (SERA)",
                       ifelse(str_detect(oracle, "xSERA"), "XGBoost (SERA)", oracle))) %>% 
  pivot_longer(cols=c("modelProb", "oracleProb", "rope"), names_to="probs", values_to="prob") %>%
  filter(!grepl("SERA", model)) %>% 
  mutate(prob=as.double(prob),
         probs=factor(probs, levels = c("oracleProb", "rope", "modelProb"), ordered = T),
         model=factor(model,
                      levels=c("XGBoost", "LGBM")),
         oracle=factor(oracle, 
                       levels=c("XGBoost (SERA)","LGBM (SERA)" )))

levels(bayes_final_sera$oracle) <- c("XGBoost (SERA)" = TeX("$XGBoost^{\\textit{(S)}}$"),
                                     "LGBM (SERA)"= TeX("$LGBM^{\\textit{(S)}}$"))



bayes_plot <- bayes_final_sera %>% 
  ggplot(mapping=aes(x=prob, y=model, fill=probs, groups=model), labeller=label_parsed) +
  geom_col() +
  labs(fill = "Posterior:", y = "Baselines", x = "Probabilities") +
  facet_wrap(vars(oracle), strip.position="right", dir="v", labeller = label_parsed) +
  scale_fill_brewer(breaks = c("modelProb", "rope", "oracleProb"),
                    labels = c("baseline", "rope", "oracle"),
                    palette="Set1", direction=-1) +
  scale_y_discrete(labels=c("XGBoost" = TeX("$XGBoost^{\\textit{(M)}}$"),
                            "LGBM"= TeX("$LGBM^{\\textit{(M)}}$"))) +
  theme(
    panel.background = element_rect(fill="white", colour="black"),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size=20),
    axis.title.x = element_text(size=25),
    axis.title.y = element_text(size=25),
    strip.background = element_rect(fill="grey80", colour="black"),
    strip.text.y = element_text(size=20, margin=margin(t=30, r=20, b=40, l=30)),
    legend.text = element_text(size=25,vjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(face="bold", size=30)
  )


ggsave(filename="bayes_sign_rsults.png", plot=bayes_plot, width=10, height=5)


                                                                ##################################
                                                                ###### Evaluate predictions ######
                                                                ##################################

top_models <- best_models_sera_df %>% 
  mutate(workflow=as.character(workflow)) %>% 
  mutate(workflow = str_remove(workflow, ".v.*"))


#preds_best_sera <- get_predictions(dataset=datasets_to_use, bestmodels=top_models) # to get predictions

load("preds_3.RData")

score_sera <- get_test_scores(preds_best_sera, metric = "sera")




                                                      #################################################
                                                      ######## Bayes sign test in out-of-sample #######
                                                      #################################################


bayes_xgboost_sera <- bayesres_preds(scores=score_sera, oracle="XGBoost_SERA")
bayes_lgbm_sera <- bayesres_preds(scores=score_sera, oracle="LGBM_SERA")

bayes_sera <- bayes_xgboost_sera %>% bind_rows(bayes_lgbm_sera)


bayes_sera

bayes_sera <- bayes_sera %>% 
  mutate(model = str_remove(model, "wf.")) %>% 
  pivot_longer(cols=c("modelProb", "oracleProb", "rope"), names_to="probs", values_to="prob") %>% 
  mutate(prob=as.double(prob),
         probs=factor(probs, levels = c("oracleProb", "rope", "modelProb"), ordered = T),
         model=as.factor(model),
         oracle=factor(oracle, levels=c("XGBoost_SERA", "LGBM_SERA"))) %>% 
  filter(!grepl("SERA", model))


levels(bayes_sera$oracle) <- c("XGBoos_SERA" = TeX("$XGBoost^{\\textit{(S)}}$"),
                               "LGBM_SERA"= TeX("$LGBM^{\\textit{(S)}}$"))



bayes_preds_plot <- bayes_sera %>% 
  ggplot(mapping=aes(x=prob, y=model, fill=probs, groups=model)) +
  geom_col() +
  labs(fill = "Posterior:", y = "Baselines", x = "Probabilities") +
  facet_wrap(vars(oracle), strip.position="right", dir="v", labeller = label_parsed) +
  scale_fill_brewer(breaks = c("modelProb", "rope", "oracleProb"),
                    labels = c("baseline", "rope", "oracle"),
                    palette="Set1", direction=-1) +
  scale_y_discrete(labels=c("XGBoost" = TeX("$XGBoost^{\\textit{(M)}}$"),
                            "LGBM"= TeX("$LGBM^{\\textit{(M)}}$"))) +
  theme(
    panel.background = element_rect(fill="white", colour="black"),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size=20),
    axis.title.x = element_text(size=25),
    axis.title.y = element_text(size=25),
    strip.background = element_rect(fill="grey80", colour="black"),
    strip.text.y = element_text(size=20, margin=margin(t=30, r=20, b=40, l=30)),
    legend.text = element_text(size=25,vjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(face="bold", size=30)
  )

bayes_preds_plot


bayes_sera <- bayes_sera %>% mutate(set = "Out-of-Sample") %>% mutate(set=as.factor(set))
bayes_final_sera <- bayes_final_sera %>% mutate(set = "Cross-Validation") %>% mutate(set=as.factor(set))



bayes_sera %>% bind_rows(bayes_final_sera) %>%  
  ggplot(mapping=aes(x=prob, y=model, fill=probs, groups=model)) +
  geom_col() +
  labs(fill = "Posterior:", y = "Baselines", x = "Probabilities") +
  facet_grid(oracle ~ set, labeller=label_parsed) +
  scale_fill_brewer(breaks = c("modelProb", "rope", "oracleProb"),
                    labels = c("baseline", "rope", "oracle"),
                    palette="Set1", direction=-1) +
  scale_y_discrete(labels=c("XGBoost" = TeX("$XGBoost^{\\textit{(M)}}$"),
                            "LGBM"= TeX("$LGBM^{\\textit{(M)}}$"))) +
  theme(
    panel.background = element_rect(fill="white", colour="black"),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size=20),
    axis.title.x = element_text(size=25),
    axis.title.y = element_text(size=25),
    strip.background = element_rect(fill="grey80", colour="black"),
    strip.text.y = element_text(size=20, margin=margin(t=30, r=20, b=40, l=30)),
    strip.text.x = element_text(size=20, margin=margin(t=30, r=20, b=40, l=30)),
    legend.text = element_text(size=25,vjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(face="bold", size=30)
  )


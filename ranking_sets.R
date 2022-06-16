ranks_sera <- score_sera %>% 
  pivot_longer(cols=matches("LG|XG"), names_to="models", values_to="errs") %>% 
  group_by(dataset) %>%
  arrange(errs) %>% 
  mutate(rank=row_number(),
         models=factor(models, levels=c("XGBoost_SERA", "XGBoost", "LGBM_SERA", "LGBM"))) 

ranks_sera

lbls <-  c("XGBoost_SERA"= TeX("$XGBoost^{\\textit{S}}$"),
           "XGBoost"     = TeX("$XGBoost^{\\textit{M}}$"),
           "LGBM_SERA"   = TeX("$LGBM^{\\textit{S}}$"),
           "LGBM"        = TeX("$LGBM^{\\textit{M}}$")
           )

ranks_sera %>% 
  group_by(models) %>% 
  count(rank) %>% 
  arrange(desc(rank))

ranks_plot <- ranks_sera %>% 
  ggplot(mapping=aes(x=models, y=rank)) +
  geom_boxplot(width=0.8) +
  scale_x_discrete(labels=lbls) +
  xlab("Models") +
  ylab("Rank") +
  labs(fill = "Models:") +
  #ggplot2::theme_classic() +
  ggplot2::theme(
    panel.grid.major = element_line(colour="grey90"),
    legend.title = element_text(face = "bold", size=25),
    legend.position = "bottom",
    legend.text = element_text(size=20),
    plot.title = element_text(size=25),
    #axis.title.x = element_blank(),
    axis.title.y = element_text(size=20),
    axis.text.x = element_text(size=15),
    axis.title.x = element_text(size=20),
    axis.text.y = element_text(size=15)
    )

ranks_plot

ggsave(filename="ranks_plot.png", plot=ranks_plot, width=10, height=5)

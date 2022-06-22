library(IRon)
library(tidyverse)
library(latex2exp)
library(scam)

load("preds.RData")


triazines_curves <- plot_sera_curves(preds_best_sera, ds_name="triazines") +
  geom_vline(xintercept = -0.01, size=1, linetype="dashed", colour="royalblue4") +
  annotate(geom="text", x=0, y=1, label=TeX("$\\phi_t=0$"), size=7, hjust=0, colour="royalblue4") +
  annotate(geom="rect", xmin=-0.01, xmax=Inf, ymin=0, ymax=Inf, fill="royalblue4", alpha=0.15)

strikes_curves <- plot_sera_curves(preds_best_sera, ds_name="strikes") +
  geom_vline(xintercept = 0.76, size=1, linetype="dashed", colour="royalblue4") +
  annotate(geom="text", x=0.8, y=1, label=TeX("$\\phi_t=0.78$"), size=7, hjust=0, colour="royalblue4") +
  annotate(geom="rect", xmin=0.76, xmax=Inf, ymin=0, ymax=Inf, fill="royalblue4", alpha=0.15)


music_curves <- plot_sera_curves(preds_best_sera, ds_name="musicorigin") +
  geom_vline(xintercept=0.007, size=1, colour="royalblue4") +
  geom_vline(xintercept=0.89, size=1, colour="royalblue4") +
  annotate(geom="text", x=c(0.05, 0.65), y=1, 
           label=c(TeX("$\\phi_t=0.01$"), TeX("$\\phi_t=0.89$")), size=7, colour="royalblue4", hjust=0) +
  annotate(geom="rect", xmin=0.007, xmax=0.89, ymin=0, ymax=Inf, fill="royalblue4", alpha=0.15)


music_curves <- plot_sera_curves(preds_best_sera, ds_name="musicorigin") +
  geom_vline(xintercept=0.007, size=1, colour="royalblue4", linetype="dashed") +
  #geom_vline(xintercept=0.89, size=1, colour="royalblue4") +
  annotate(geom="text", x=c(0.05), y=1, 
           label=c(TeX("$\\phi_t=0.01$")), size=7, colour="royalblue4", hjust=0) +
  annotate(geom="rect", xmin=0.007, xmax=Inf, ymin=0, ymax=Inf, fill="royalblue4", alpha=0.15)


house_curves <- plot_sera_curves(preds_best_sera, ds_name="house8H") +
  geom_vline(xintercept=0.093, size=1, linetype="dashed", colour="royalblue4") +
  geom_vline(xintercept=0.093, size=1, linetype="dashed", colour="royalblue4") +
  annotate(geom="text", x=0.093, y=1, label=TeX("$\\phi_t=0.09$"), size=7, hjust=0, colour="royalblue4") +
  annotate(geom="rect", xmin=0.093, xmax=Inf, ymin=0, ymax=Inf, fill="royalblue4", alpha=0.15)


deltaElevation_curves <- plot_sera_curves(preds_best_sera, ds_name="deltaElevation") +
  geom_vline(xintercept = 0.185, size=1, linetype="dashed", colour="royalblue4") +
  annotate(geom="text", x=0.2, y=1, label=TeX("$\\phi_t=0.17$"),  size=7, hjust=0, colour="royalblue4") +
  annotate(geom="rect", xmin=0.185, xmax=Inf, ymin=0, ymax=Inf, fill="royalblue4", alpha=0.15)


housingBoston_curves <- plot_sera_curves(preds_best_sera, ds_name="housingBoston") +
  geom_vline(xintercept = 0.47, size=1, linetype="dashed", colour="royalblue4")+
  annotate(geom="text", x=0.5, y=1, label=TeX("$\\phi_t=0.49$"),  size=7, hjust=0, colour="royalblue4") +
  annotate(geom="rect", xmin=0.47, xmax=Inf, ymin=0, ymax=Inf, fill="royalblue4", alpha=0.15)


sera_plots <- ggpubr::ggarrange(
  triazines_curves,
  house_curves,
  music_curves,
  deltaElevation_curves,
  housingBoston_curves,
  strikes_curves,
  common.legend=T,
  ncol=3,
  nrow=2,
  legend="bottom")

sera_plots

ggsave(filename="sera_curves.png", plot=sera_plots, width=20, height=15, limitsize = F)


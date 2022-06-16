#' Plot SERA Curve
#'
#'
#' @description Plots SERA curves for several models in a dataset.
#' @param preds A \code{list} of datasets with the respective predictions.
#' @param ds_name A character with the dataset name.
#' @param return.err Returns SERA errors if TRUE. Default FALSE.
#'
#' @return SERA curves plot.
#' @export
#'
#' @examples
plot_sera_curves <- function(preds, ds_name, return.err=FALSE){

  pred <- preds[[ds_name]]$preds
  ph.ctrl <- preds[[ds_name]]$p.ctrl

  sera_res <- sera_err(
    trues=pred$trues,
    preds=dplyr::select(pred,-trues),
    ph=ph.ctrl,
    return.err=T)

  errs <- sera_res$errs.df
  scores <- sera_res$sera

  best_model <- names(scores)[which.min(scores)]

  if(best_model == "XGBoost_SERA")
    m <- "$XGBoost^{\\textit{S}}$"
  else if(best_model == "LGBM_SERA")
    m <- "$LGBM^{\\textit{S}}$"
  else if(best_model == "XGBoost")
    m <- "$XGBoost^{\\textit{M}}$"
  else
    m <- "$LGBM^{\\textit{M}}$"

  plot <- errs %>%
    tibble::as_tibble() %>%
    dplyr::rename(`XGBoost (SERA)` = XGBoost_SERA,
           `LGBM (SERA)` = LGBM_SERA) %>%
    tidyr::pivot_longer(cols=matches(c("XG", "LGB")), names_to="models", values_to="SER") %>%
    dplyr::mutate(models=factor(models, levels=c("LGBM","XGBoost", "LGBM (SERA)", "XGBoost (SERA)"))) %>%
    ggplot2::ggplot(mapping=aes(x=phi, y=SER/max(SER), color=models, group=models)) +
    ggplot2::geom_smooth(method="scam",formula=y ~ s(x, k = 30, bs = "mpd"),span=0.1,se=FALSE,fullrange=T) +
    ggplot2::xlab(expression("Relevance"~phi(y))) + ylab("SER") +
    ggplot2::geom_hline(yintercept=0,colour="black") +
    ggplot2::ggtitle(TeX(paste0("Winner: ", m, "; ",ds_name))) +
    ggplot2::labs(colour = "Models:") +
    ggplot2::scale_color_brewer(labels=c(TeX("$LGBM^{\\textit{M}}$"), TeX("$XGBoost^{\\textit{M}}$"),
                                         TeX("$LGBM^{\\textit{S}}$"), TeX("$XGBoost^{\\textit{S}}$")),
                                palette="Set1") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      panel.grid.major = element_line(colour="grey90"),
      legend.title = element_text(face = "bold", size=25),
      legend.text = element_text(size=20),
      plot.title = element_text(size=25),
      axis.title.x = element_text(size=20),
      axis.title.y = element_text(size=20),
      axis.text.x = element_text(size=15),
      axis.text.y = element_text(size=15))



  if(isTRUE(return.err))
    return(list("plot"= plot,
                "err"= errs))
  else
    return(plot)
}

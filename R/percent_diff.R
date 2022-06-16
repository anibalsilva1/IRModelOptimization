#' Determines the percentual difference between Gradient Boosting models that were
#' optimised with SERA and MSE.
#'
#' @param preds A dataframe object with predictions for all datasets.
#' @param models A character vector of the models to be considered.
#'
#' @return A dataframe with the percentual differences.
#' @export
#'
#' @examples
percent_diff <- function(preds, models){

  n <- length(preds)
  step <-  0.001
  s <- seq(0, 1, step)
  m <- length(s)

  res <- data.frame(phi = s)

  for(model in models){

    print(paste("Iteration over model ", model))

    errs_std <- c()
    errs_nonstd <- c()
    errs_ds <- matrix(nrow = m, ncol = n)
    errs <- c()

    for(i in 1:n){

      ds <- preds[[i]]$preds
      ph.ctrl <- preds[[i]]$p.ctrl

      tr <- ds$trues

      m_preds <- ds %>% dplyr::select(model) %>% pull()
      sm_preds <- ds %>% dplyr::select(matches(str_c(model, "_SERA"))) %>% dplyr::pull()

      errs_std <- IRon::sera(tr, m_preds, ph = ph.ctrl, return.err = T)$errors
      errs_nonstd <- IRon::sera(tr, sm_preds, ph = ph.ctrl, return.err = T)$errors
      errs_ds[, i] <- 100 * (errs_nonstd - errs_std) / errs_std
    }

    errs <- rowMeans(errs_ds)

    res[model] <- errs

  }

  return(res)
}

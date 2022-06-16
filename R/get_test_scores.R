#' Evaluates a metric for several models and datasets.
#'
#' @param predictions An object returned from \code{\link{get_predictions}}
#' @param metric An character with the metric name. Possible values: \code{"sera"}, \code{"mse"}
#' \code{"nrmse"}, \code{"ser"}.
#' @param tr A value with the threshold in case of \code{metric = "ser"}.
#' @param norm A boolean that normalizes \code{metric = sera}.
#'
#' @return A dataframe with the scores for each model and for each data.frame.
#' @export
#'
#' @examples
get_test_scores <- function(predictions, metric = "sera", tr = 0.9, norm = F){

  models <- colnames(predictions[[1]]$preds %>% dplyr::select(-trues))
  res <- models %>% purrr::map_dfc(setNames, object = list(numeric()))
  nDs <- names(predictions)

  for(d in nDs){
    print(paste0("Looping over ", d))

    if(metric == "mse"){
      res <- res %>%
        bind_rows(predictions[[d]]$preds %>%
                    summarise(across(everything(),
                                     ~ mean((trues - .x)^2))))
    }
    else if(metric == "relMSE"){
      nr <- nrow(predictions[[d]]$preds)
      res <- res %>%
        bind_rows(predictions[[d]]$preds %>%
                    summarise(across(everything(),
                                     ~ relMSE(trues, .x, phi.trues))))
    }
    else if(metric == "nrmse"){
      nr <- nrow(predictions[[d]]$preds)
      res <- res %>%
        bind_rows(predictions[[d]]$preds %>%
                    summarise(across(everything(),
                                     ~ sqrt(mean((trues - .x)^2))/(max(trues)-min(trues)))))

    }
    else if(metric == "ser"){

      if(is.null(tr)) stop("Threshold tr must be provided.")
      res <- res %>%
        bind_rows(predictions[[d]]$preds %>%
                    summarise(across(everything(),
                                     ~ ser(trues = trues,
                                           preds = .x,
                                           phi.trues = phi(trues,
                                                           predictions[[d]]$p.ctrl),
                                           t = tr))))
    }
    else if(metric == "sera"){
      res <- res %>%
        bind_rows(predictions[[d]]$preds %>%
                    summarise(across(everything(),
                                     ~ sera(trues = trues,
                                            preds = .x,
                                            phi.trues = phi(trues,
                                                            predictions[[d]]$p.ctrl),
                                            norm = norm))))
    }
    else if(metric == "nmse"){
      res <- res %>%
        bind_rows(predictions[[d]]$preds %>%
                    summarise(across(everything(),
                                     ~ sum((trues - .x)^2)/sum((trues - mean(trues))^2))))
    }
    else if(metric == "nsera"){
      res <- res %>%
        bind_rows(predictions[[d]]$preds %>%
                    summarise(across(everything(),
                                     ~ IRon::sera(trues = trues,
                                            preds = .x,
                                            phi.trues = phi(trues,
                                                            predictions[[d]]$p.ctrl),
                                            norm = norm)/IRon::sera(trues=trues,
                                                                    preds=mean(trues),
                                                                    phi.trues=phi(trues,predictions[[d]]$p.ctrl)))))

    }
  }
  print(res)
  res <- res %>%
    add_column(dataset = nDs, .before = models[1]) %>%
    dplyr::select(-trues)
  return(res)
}

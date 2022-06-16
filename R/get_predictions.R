#' Get Predictions
#'
#' @description Performs predictions for the best models given a list of datasets.
#'
#' @param datasets A \code{list} containing the datasets with their information.
#' @param bestmodels A \code{data.frame} with the best models.
#'
#' @return A \code{list} with the predictions and execution time for each dataset.
#' This predictions will be inside a dataframe, where each column will be the
#' predictions of each model.
#' @export

#' @examples
get_predictions <- function(datasets, bestmodels){

  nD <- names(datasets)
  nWF <- pull(distinct(bestmodels, workflow))

  preds_data <- list()

  for(i in 1:length(nD)){

    form <- datasets[[i]]$formula
    train <- datasets[[i]]$train
    test <- datasets[[i]]$test
    target <- form[[2]]


    p.ctrl = phi.control(pull(train,target))

    df <- tibble(trues = pull(test,target))

    print(paste0("Gettings results for ", nD[i], " data"))

    preds_wf <- foreach(wf = 1:length(nWF),
                        .final = function(wf) setNames(wf, nWF)
    ) %dopar% {
      params <- bestmodels[bestmodels$dataset == nD[i] &
                               bestmodels$workflow == nWF[wf], ]$params[[1]]

      preds <- do.call(what = nWF[wf],
                       args = c(list(form, train, test), params))
    }

    list_preds <- list()
    list_times <- list()

    n <- names(preds_wf)

    for(j in n){

      list_preds[[j]] <- preds_wf[[j]]$preds
      list_times[[j]] <- preds_wf[[j]]$time
    }

    preds_df <- as_tibble(do.call("cbind", list_preds))
    df <- bind_cols(df, preds_df)

    preds_data[[nD[i]]] <- list("preds" = df,
                                "times" = list_times,
                                "p.ctrl" = p.ctrl)

  }
  return(preds_data)
}

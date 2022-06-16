#' Get Best Bayes models
#'
#' @description Performs pairwise Bayes Sign Test for the top-k workflows in a given model.
#'
#' @param res A ComparisonResults object.
#' @param workflows A character vector with the best workflows for a given model.
#' @param metric A character that denotes the metric for which we want to perform
#' Bayes Sign Test.
#' @param folds A numeric value denoting the number of folds.
#'
#' @return A \code{data.frame} containing all the pair-wise evaluation of Bayes Sign Test.
#' @export
#'
#' @examples

get_best_models_bayes <- function(res, workflows, metric = "sera", folds = 10){

  datasets <- names(res)
  nwfs <- length(workflows)

  results <- matrix(nrow = folds, ncol = length(workflows), dimnames = list(NULL, workflows))
  resall <- list()

  for(ds in datasets){

    for(wf in workflows){
      errs <- c()
      for(k in 1:folds){

        if(metric == "mse" | metric == "sera" | isTRUE(grepl("relMSE", metric))){
          err <- res[[ds]][[wf]]@iterationsScores[k, metric]
          errs <- c(errs,err)
        }
        else if(metric == "nrmse"){

          trues <- res[[ds]][[wf]]@iterationsInfo[[k]]$trues
          preds <- res[[ds]][[wf]]@iterationsInfo[[k]]$preds

          err <- res[[ds]][[wf]]@iterationsScores[k, "mse"]
          errs <- c(errs, sqrt(err)/(max(trues) - min(trues)))

        }
      }
      results[,wf] <- errs

    }
    results <- as.data.frame(results)
    i = 1
    ## orcls
    while(i <= nwfs){

      orcl <- results[, workflows[i]]

      resall[[ds]][[workflows[i]]] <- results %>%
        mutate(across(everything(), ~ (orcl - .x)/orcl)) %>%
        summarise(across(everything(), ~ mean(.x)))

      i = i+1
    }

    oracles <- list()
    for(wf in workflows){
      r <- c()
      for(ds in names(resall)){
        r <- rbind(r, as.numeric(resall[[ds]][[wf]]))
      }
      colnames(r) <- workflows
      oracles[[wf]] <- as.data.frame(r)
      oracles[[wf]] <- oracles[[wf]][, colnames(oracles[[wf]]) != wf]
    }
  }

  # bayes test
  rope = 0.01
  resBayes <- data.frame(oracle = character(),
                         model = character(),
                         oracleProb = numeric(),
                         modelProb = numeric(),
                         rope = numeric())

  for(i in 1:length(oracles)){

    or <- names(oracles)[i]

    for(j in 1:ncol(oracles[[i]])){
      #print(paste0("Performing Bayes test for oracle ", or, " against ", colnames(oracles[[i]])[j]))

      test <- BayesianSignTest(diffVector = oracles[[i]][, j], rope_max = rope, rope_min = -rope)

      resBayes <- resBayes %>% dplyr::add_row(
        oracle = or,
        model = colnames(oracles[[i]])[j],
        oracleProb = test$probLeft,
        modelProb = test$probRight,
        rope = test$probRope
      )
    }
  }

  return(resBayes)
}

#' Compares and returns the Bayes Sign Test posterior probabilities between an
#' oracle and the best workflows for each model.
#'
#' @param res A ComparisonResults object.
#' @param bestmodels A character vector returned from \code{\link{get_best_models_bayes}}.
#' @param oracle A character with the workflow name which we want to compare to \code{bestModels}.
#' @param metric A character with the metric name.
#' @param folds A value with the number of folds.
#' @param bayestest A boolean that specifies if we want to perform Bayes Sign Test.
#' If \code{False}, it only returns the average differences between the \code{oracle} and
#' each workflow. \code{True} by default.
#'
#' @importFrom magrittr %>%
#'
#' @return A list with the posterior probabilities between the \code{oracle} and
#' each model in \code{bestmodels} and the average differences between the \code{oracle}
#' and each workflow
#' @export
#'
#' @examples
get_diffs_bayes <- function(res,
                            bestmodels,
                            oracle,
                            metric = "nrmse",
                            folds = 10,
                            bayestest = T) {

    datasets <- names(res)
    nbm = length(bestmodels)
    nds <- length(datasets)

    resds <- list()

    for(ds in datasets){

      results <- matrix(nrow = folds, ncol = nbm, dimnames = list(NULL, bestmodels))

      # gets errors in each fold for each model and appends to results matrix
      for(bm in bestmodels){
        errs <- c()

        for(k in 1:folds){

          trues <- res[[ds]][[bm]]@iterationsInfo[[k]]$trues
          preds <- res[[ds]][[bm]]@iterationsInfo[[k]]$preds

          if(metric == "nsera"){

            p.ctrl <- res[[ds]][[bm]]@estTask@evaluator.pars$phi.ctrl
            err <- IRon::sera(trues = trues, preds = preds, phi.trues = IRon::phi(trues, p.ctrl), norm = T)
          }
          else if(metric == "nrmse"){

            err <- res[[ds]][[bm]]@iterationsScores[k, "mse"]
            err <- sqrt(err)/(max(trues) - min(trues))

          }
          else if(metric == "mse" | metric == "sera" | metric == "relMSE"){

            err <- res[[ds]][[bm]]@iterationsScores[k, metric]
          }

          errs <- c(errs, err)

        }
        results[, bm] <- errs
      }

      results <- as.data.frame(results)
      orcl <- results[, oracle]

      # calculates the "normalized" mean difference between models and oracles
      results <- results %>%
        dplyr::mutate(across(everything(), ~ (.x - orcl)/orcl)) %>%
        dplyr::summarise(across(everything(), ~ mean(.x)))

      resds[[ds]] <- results

    }

    resf <- matrix(nrow = nds, ncol = nbm, dimnames = list(NULL, bestmodels))

    for(i in 1:length(resds)){
      resf[i, ] <- as.numeric(resds[[i]])
    }

    # drops oracle
    resf <- as.data.frame(resf)
    resf <- resf[, colnames(resf) != oracle]

    if(!isTRUE(bayestest))
      return(dplyr::as_tibble(resf))

    else{

      # bayes test
      rope = 0.01
      resBayes <- data.frame(model = character(),
                             oracle = character(),
                             modelProb = numeric(),
                             oracleProb = numeric(),
                             rope = numeric())

      models <- colnames(resf)

      # for each model, performs Bayes Sign Test
      for(model in models){

        dv <- resf[, model]


        bsr <- BayesianSignTest(dv, rope_min = -rope, rope_max = rope)
        resBayes <- resBayes %>% dplyr::add_row(model = model,
                                                oracle = oracle,
                                                modelProb = bsr$probLeft,
                                                oracleProb = bsr$probRight,
                                                rope = bsr$probRope)
      }

      resBayes <- resBayes %>%
        dplyr::mutate(
          modelProb = sprintf("%0.3f", modelProb),
          oracleProb = sprintf("%0.3f", oracleProb),
          rope = sprintf("%0.3f", rope))


      fs <- list("diffs" = as_tibble(resf),
                 "bayesres" = as_tibble(resBayes))
      return(fs)
    }
  }

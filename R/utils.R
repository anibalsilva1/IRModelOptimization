#' Gradients Statistics of SERA for XGBoost.
#'
#' @description Determines the gradient statistics of SERA to be used in
#' XGBoost algorithm as a custom loss function.
#'
#' @param preds Numeric \code{vector} of predictions.
#' @param dtrain \code{xgb.DMatrix} object.
#'
#' @return Returns a list containing the gradient and the hessian of SERA.
#' @export
#'
#' @examples
xgboostsera <- function(preds, dtrain){


  s <- 0.001
  labels <- xgboost::getinfo(dtrain, "label")

  step <- seq(0,1,s)
  N <- length(step)

  p.extrm <- get_extreme_type(labels)
  phi.ctrl <- phi.control(labels, extr.type = p.extrm)

  phi.trues <- phi(labels, phi.ctrl)

  sigmas <- get_sigma(phi.trues, step)
  sigmas <- sigmas/N


  grad <- 2*sigmas*(preds - labels)
  hess <- 2*sigmas

  return(list(grad = grad, hess = hess))
}

#' Gradient Statistics of SERA for LGBM.
#'
#' @description Determines the gradient statistics of SERA to be used in
#' LGBM algorithm as a custom loss function.
#'
#' @param preds Numeric \code{vector} of predictions.
#' @param dtrain A \code{lgb.Dataset} object.
#'
#' @return  Returns a list containing the gradient and the hessian of SERA.
#' @export
#'
#' @examples
lgbmsera <- function(preds, dtrain){

  labels <- lightgbm::get_field(dtrain, "label")

  s <- 0.001
  step <- seq(0,1,s)
  N <- length(step)

  p.extrm <- get_extreme_type(labels)
  phi.ctrl <- phi.control(labels, extr.type = p.extrm)

  phi.trues <- phi(labels, phi.ctrl)
  sigmas <- get_sigma(phi.trues, step)
  sigmas <- sigmas/N

  grad <- 2*sigmas*(preds - labels)
  hess <- 2*sigmas

  return(list(grad = grad, hess = hess))
}

#' Metrics to be used with \code{performanceEstimation}.
#'
#' @description Metrics to be used with \code{performanceEstimation}.
#'
#' @param trues Numeric \code{vector} with true values.
#' @param preds Numeric \code{vector} with predictions.
#' @param phi.ctrl Control points.
#' @param ... Additional arguments.
#'
#' @return A numeric vector with mse and sera values.
#' @export
#'
#' @examples
#'
metrics <- function(trues, preds, phi.ctrl, ...){

  phi.trues <- as.vector(phi(trues, phi.parms = phi.ctrl))

  c(mse = as.vector(mean((trues - preds)^2)),
    sera = as.vector(sera(trues,preds,phi.trues)))

}

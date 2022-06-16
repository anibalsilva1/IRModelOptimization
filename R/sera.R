#' Squared Error-Relevance Area (SERA)
#'
#' @param trues Target values from a test set of a given data set. Should be a vector and have the same size as the variable preds
#' @param preds Predicted values given a certain test set of a given data set. Should be a vector and have the same size as the variable preds
#' @param phi.trues Relevance of the values in the parameter trues.
#' @param ph The relevance function providing the data points where the pairs of values-relevance are known. Default is NULL
#' @param step Relevance intervals between 0 (min) and 1 (max). Default 0.001.
#' @param return.err Boolean to indicate if the errors at each subset of increasing relevance should be returned. Default is FALSE
#' @param norm Normalize the SERA values for internal optimisation only (TRUE/FALSE)
#'
#'
#' @return
#' @export
#'
#' @examples
sera_err <- function(trues,
                     preds,
                     phi.trues = NULL,
                     ph = NULL,
                     step = 0.001,
                     norm = FALSE,
                     return.err = FALSE) {

  if(!is.data.frame(preds)) preds <- as.data.frame(preds)

  if(is.null(phi.trues) && is.null(ph)) stop("You need to input either the parameter phi.trues or ph.")

  if(is.null(phi.trues)) phi.trues <- phi(trues,ph)

  tbl <- data.frame(trues=trues,phi=phi.trues,preds)

  th <- c(seq(0,1,step))

  ms <-colnames(tbl)[3:ncol(tbl)]

  errors <- sapply(ms,FUN=function(m) sapply(th, FUN = function(x) sum((tbl[tbl$phi>=x,]$trues-tbl[tbl$phi>=x,m])^2)))

  if(any(is.na(errors))) errors[is.na(errors)] <- 0

  if(norm) errors <- errors/errors[1]

  areas <- sapply(1:length(ms), FUN=function(m) sapply(2:length(th), FUN=function(x) step * (errors[x-1,m] + errors[x,m])/2 ))
  colnames(areas) <- ms
  rownames(areas) <- 1:nrow(areas)

  res <- apply(areas,2,FUN=function(x) sum(x))

  df <- data.frame(th=th, errors)
  colnames(df)[1] <- "phi"
  colnames(df)[2:ncol(df)] <- ms


  if(isTRUE(return.err)) {

    return(list(sera=res,
                errors=as.vector(errors),
                thrs=th,
                errs.df = df)
    )

  } else {

    return(res)
  }

}




#' SERA first derivative
#' @description Calculates SERA first derivative using the trapezoidal rule with Riemann's sum.
#'
#' @param trues Target values from a test set of a given data set. Should be a vector and have the same size as the variable preds
#' @param preds Predicted values given a certain test set of a given data set. Should be a vector and have the same size
#' @param phi Relevance of the values in the parameter trues.
#' @param step Relevance intervals between 0 (min) and 1 (max). Default 0.001.
#'
#' @return
#' @export
#'
#' @examples
sera_deriv <- function(trues, preds, phi, step = 0.001){

  th <- c(seq(0, 1, step))

  errors <- lapply(1:length(trues), FUN = function(i) sapply(th, FUN = function(x) if(phi[i] >= x) preds[i] - trues[i] else 0))
  #print(errors)
  areas <- sapply(errors, FUN = function(err) step * sapply(2:length(th), FUN=function(x) (err[x-1] + err[x])/2))

  y_deriv <- 2*colSums(areas)

  return(y_deriv)
}

#' SERA second derivative
#'
#' @description Calculates SERA second derivative using the trapezoidal rule with Riemann's sum.
#' @param trues Target values from a test set of a given data set. Should be a vector and have the same size as the variable preds
#' @param preds Predicted values given a certain test set of a given data set. Should be a vector and have the same size
#' @param phi Relevance of the values in the parameter trues.
#' @param step Relevance intervals between 0 (min) and 1 (max). Default 0.001.
#'
#' @return
#' @export
#'
#' @examples
#'
sera_second_deriv <- function(trues, preds, phi, step = 0.001){

  th <- c(seq(0, 1, step))

  errors <- lapply(1:length(trues), FUN = function(i) sapply(1:length(th), FUN = function(x) if(phi[i] >= th[x]) 1 else 0))
  areas <- sapply(errors, FUN = function(i) step * sapply(2:length(th), FUN=function(x) (i[x-1] + i[x])/2))

  y_deriv <- 2*colSums(areas)

  return(y_deriv)
}


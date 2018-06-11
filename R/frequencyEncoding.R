#' @title frequencyEncoding_fit
#' @description train frequen-based encoding for categorical features
#' @param dt data.table object
#' @param x character vector of independet variables
#' @examples
#' library(rAutoFE)
#' library(data.table)
#' data(churn, package = "rAutoFE")
#' data.table::setDT(churn)
#' splits <- rAutoFE::splitFrame(dt=churn, ratio = c(0.5, 0.3), seed = 1234)
#' train <- splits[[1]]
#' valid <- splits[[2]]
#' test  <- splits[[3]]
#' x = c("State","Area.Code")
#' y = "Churn."
#' fit <- rAutoFE::frequencyEncoding_fit(dt = train, x = x)
#' @export
frequencyEncoding_fit <- function(dt, x){
  fit_list <- list()
  for(i in x){
    setkeyv(dt, i)
    fit_list[[i]] <- dt[, .(frequencyEncode_=.N), by=i]
    colnames(fit_list[[i]])[2] <- paste0(colnames(fit_list[[i]])[2], i)
  }
  return(fit_list)
}


#' @title frequencyEncoding_transform
#' @description transform dataset using fit
#' @param dt data.table object
#' @param x character vector of independet variables
#' @param fit frequencyEncoding_fit object
#' @examples
#' library(rAutoFE)
#' library(data.table)
#' data(churn, package = "rAutoFE")
#' data.table::setDT(churn)
#' splits <- rAutoFE::splitFrame(dt=churn, ratio = c(0.5, 0.3), seed = 1234)
#' train <- splits[[1]]
#' valid <- splits[[2]]
#' test  <- splits[[3]]
#' x = c("State","Area.Code")
#' y = "Churn."
#' fit <- rAutoFE::frequencyEncoding_fit(dt = train, x = x)
#' saveRDS(fit,"fit.rds")
#' rm(fit)
#' fit <- readRDS("fit.rds")
#' train <- rAutoFE::frequencyEncoding_transform(dt = train, fit = fit)
#' valid <- rAutoFE::frequencyEncoding_transform(dt = valid, fit = fit)
#' test  <- rAutoFE::frequencyEncoding_transform(dt = test, fit = fit)
#' @export
frequencyEncoding_transform <- function(dt, fit){
  for(i in names(fit)){
    x_map <- fit[[i]]
    setkeyv(x_map, i)
    setkeyv(dt, i)
    dt <- x_map[dt]
  }
  return(dt)
}



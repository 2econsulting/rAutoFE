#' @title targetEncoding_fit
#' @description train for target feature encoding
#' @param dt data.table object
#' @param x character vector of independet variables
#' @param y character string of dependent variable
#' @examples
#' library(rAutoFE)
#' library(data.table)
#' data(churn, package = "rAutoFE")
#' data.table::setDT(churn)
#' splits <- splitFrame(dt = churn, ratio = c(0.5, 0.3), seed = 1234)
#' train <- splits[[1]]
#' valid <- splits[[2]]
#' test  <- splits[[3]]
#' x = c("State","Area.Code")
#' y = "Churn."
#' fit <- rAutoFE::targetEncoding_fit(dt = train, x = x, y = y)
#' @export
targetEncoding_fit <- function(dt, x, y){
  if(class(dt)[[1]]=="H2OFrame"){
    h2o.target_encode_create(dt = dt, x = x, y = y)
  }else if(class(dt)[[1]]=="data.table"){
    fit_list <- list()
    for(i in x){
      setkeyv(dt, i)
      fit_list[[i]] <- dt[ ,list(numerator=sum(as.numeric(get(y))-1,na.rm=T), denominator=.N), by=i]
    }
    return(fit_list)
  }
}


#' @title targetEncoding_transform
#' @description transform dataset using fit
#' @param dt data.table object
#' @param x character vector of independet variables
#' @param y character string of dependent variable
#' @param fit targetEncoding_fit object
#' @examples
#' library(rAutoFE)
#' library(data.table)
#' data(churn, package = "rAutoFE")
#' data.table::setDT(churn)
#' splits <- rAutoFE::splitFrame(dt = churn, ratio = c(0.5, 0.3), seed = 1234)
#' train <- splits[[1]]
#' valid <- splits[[2]]
#' test  <- splits[[3]]
#' x = c("State","Area.Code")
#' y = "Churn."
#' fit <- rAutoFE::targetEncoding_fit(dt = train, x = x, y = y)
#' saveRDS(fit,"fit.rds")
#' rm(fit)
#' fit <- readRDS("fit.rds")
#' train <- rAutoFE::targetEncoding_transform(dt = train, x = x, y = y, fit = fit)
#' valid <- rAutoFE::targetEncoding_transform(dt = valid, x = x, y = y, fit = fit)
#' test  <- rAutoFE::targetEncoding_transform(dt = test, x = x, y = y, fit = fit)
#' @export
targetEncoding_transform <- function(dt, x, y, fit){
  if(class(dt)[[1]]=="H2OFrame"){
    h2o.target_encode_apply(
      dt = dt,
      x = x,
      y = y,
      target_encode_map = fit,
      holdout_type = "None",
      blended_avg = FALSE,
      noise_level = 0,
      seed = 1234
    )
  }else if(class(dt)[[1]]=="data.table"){
    for(i in x){
      x_map <- fit[[i]]
      x_map[,paste0("TargetEncode_",i):=numerator/denominator]
      x_map[,':='(numerator=NULL, denominator=NULL)]
      setkeyv(x_map, i)
      setkeyv(dt, i)
      dt <- x_map[dt]
    }
    return(dt)
  }
}



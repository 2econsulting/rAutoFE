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
  info <- list()
  for(i in x){
    x_map <- dt[ ,list(numerator=sum(as.numeric(get(y))-1,na.rm=T), denominator=.N), by=i]
    x_map[,paste0("TargetEncode_",i):=numerator/denominator]
    x_map[,':='(numerator=NULL, denominator=NULL)]
    info[[i]] <- x_map
  }
  return(info)
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
#' train <- rAutoFE::targetEncoding_transform(dt = train, fit = fit)
#' valid <- rAutoFE::targetEncoding_transform(dt = valid, fit = fit)
#' test  <- rAutoFE::targetEncoding_transform(dt = test, fit = fit)
#' @export
targetEncoding_transform <- function(dt, fit){
  for(i in names(fit)){
    x_map = fit[[i]]
    setkeyv(x_map, i)
    setkeyv(dt, i)
    dt <- x_map[dt]
  }
  return(dt)
}




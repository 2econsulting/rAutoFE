#' @title bestNormalize_fit
#' @description bestNormalize_fit
#' @param dt data.table object
#' @examples
#' library(bestNormalize)
#' library(data.table)
#' library(rAutoFE)
#' library(rAutoFS)
#' library(h2o)
#' churn <- as.data.table(churn)
#' splits <- rAutoFE::splitFrame(dt=churn, ratio = c(0.5, 0.3), seed = 1234)
#' train <- splits[[1]]
#' valid <- splits[[2]]
#' test  <- splits[[3]]
#' h2o.init()
#' data_hex <- as.h2o(rbind(train, valid))
#' y = "Churn."
#' x = colnames(data_hex)[colnames(data_hex)!=y]
#' ml <- autoFS(data_hex, x, y, num_of_model=5, num_of_vi=10)
#' fit <- bestNormalize_fit(dt=train, vi=ml$top_vi)
#' @export
bestNormalize_fit <- function(dt, vi){
  info <- lapply(dt[, mget(vi[sapply(dt[, mget(vi)], is.numeric)])], function(x) bestNormalize(x, out_of_sample=FALSE, k=3, warn=FALSE))
  return(info)
}


#' @title bestNormalize_transform
#' @description bestNormalize_transform
#' @param dt data.table object
#' @examples
#' library(bestNormalize)
#' library(data.table)
#' library(rAutoFE)
#' library(rAutoFS)
#' library(h2o)
#' churn <- as.data.table(churn)
#' splits <- rAutoFE::splitFrame(dt=churn, ratio = c(0.5, 0.3), seed = 1234)
#' train <- splits[[1]]
#' valid <- splits[[2]]
#' test  <- splits[[3]]
#' h2o.init()
#' data_hex <- as.h2o(rbind(train, valid))
#' y = "Churn."
#' x = colnames(data_hex)[colnames(data_hex)!=y]
#' ml <- autoFS(data_hex, x, y, num_of_model=5, num_of_vi=10)
#' fit <- bestNormalize_fit(dt=train, vi=ml$top_vi)
#' train <- bestNormalize_transform(dt=train, fit=fit)
#' valid <- bestNormalize_transform(dt=valid, fit=fit)
#' test  <- bestNormalize_transform(dt=test, fit=fit)
#' @export
bestNormalize_transform <- function(dt, fit){
  setDT(dt)
  sapply(1:length(fit), function(i) dt[, paste(names(fit)[i], attr(fit[[i]]$chosen_transform, "class"), sep="_"):=
                                         predict(fit[names(fit)[i]][[1]], newdata=dt[, get(names(fit)[i])], inverse=FALSE)])
  return(dt)
}


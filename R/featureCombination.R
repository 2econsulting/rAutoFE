#' @title bin4numeric_fit
#' @description bin4numeric_fit
#' @param dt data.table object
#' @examples 
#' library(data.table)
#' library(binr)
#' library(rAutoFE)
#' library(rAutoFS)
#' library(h2o)
#' data(churn)
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
#' fit <- bin4numeric_fit(dt=train, vi=ml$top_vi)
#' @export
bin4numeric_fit <- function(dt, vi){
  numeric_index <- sapply(dt[, mget(vi)], is.numeric)
  numeric_names <- vi[which(numeric_index)]
  bin_info <- sapply(dt[, mget(numeric_names)], function(x) binr::bins.getvals(binr::bins(x, target.bins = 30, minpts = nrow(dt)*0.01)))
  fit <- list("numeric_names"=numeric_names, "bin_info"=bin_info)
  return(fit)
}


#' @title bin4numeric_transform
#' @description bin4numeric_transform
#' @param dt data.table object
#' @examples 
#' library(data.table)
#' library(binr)
#' library(rAutoFE)
#' library(rAutoFS)
#' library(h2o)
#' data(churn)
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
#' fit <- bin4numeric_fit(dt=train, vi=ml$top_vi)
#' train <- bin4numeric_transform(dt=train, fit=fit)
#' valid <- bin4numeric_transform(dt=valid, fit=fit)
#' test  <- bin4numeric_transform(dt=test, fit=fit)
#' @export
bin4numeric_transform <- function(dt, fit){
  dt_w_bin <- sapply(1:length(fit[["numeric_names"]]), function(x) cut(dt[, get(fit[["numeric_names"]][x])], breaks=fit[["bin_info"]][[x]]))
  dt_w_bin <- setNames(as.data.table(dt_w_bin), paste0(fit[["numeric_names"]], "_bin"))
  dt <- cbind(dt, dt_w_bin)
  return(dt)
}


#' @title featureComb_fit_transform
#' @description featureComb_fit_transform
#' @param dt data.table object
#' @examples 
#' library(data.table)
#' library(binr)
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
#' train <- featureComb_fit_transform(dt=train, vi=ml$top_vi)
#' valid <- featureComb_fit_transform(dt=valid, vi=ml$top_vi)
#' test  <- featureComb_fit_transform(dt=test, vi=ml$top_vi)
#' @export
featureComb_fit_transform <- function(dt, vi){
  numeric_names <- vi[which(sapply(dt[, mget(vi)], is.numeric))]
  categor_names <- vi[-which(sapply(dt[, mget(vi)], is.numeric))]
  cat_vi <- c(categor_names, paste0(numeric_names, "_bin"))
  combn_vi <- combn(cat_vi, 2)
  sapply(1:ncol(combn_vi), function(i) dt[, paste0(combn_vi[,i], collapse=":"):=interaction(dt[, mget(combn_vi[,i])])])
  return(dt)
}


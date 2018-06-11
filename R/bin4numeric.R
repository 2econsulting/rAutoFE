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
#' bin4numeric_transform(dt=train, fit=fit)
#' bin4numeric_transform(dt=valid, fit=fit)
#' bin4numeric_transform(dt=test, fit=fit)
#' @export
bin4numeric_transform <- function(dt, fit){
  dt_w_bin <- sapply(1:length(fit[["numeric_names"]]), function(x) cut(dt[, get(fit[["numeric_names"]][x])], breaks=fit[["bin_info"]][[x]]))
  dt_w_bin <- setNames(as.data.table(dt_w_bin), paste0(fit[["numeric_names"]], "_bin"))
  dt <- cbind(dt, dt_w_bin)
  return(dt)
}


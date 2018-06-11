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


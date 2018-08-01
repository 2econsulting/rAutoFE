#' @title cvNumtoCat_WoE_fit
#' @description Convert the numeric feature to categorical feature by cross validated mean criteria then make WeO feature 
#' @param data data.table object
#' @examples
#' library(data.table)
#' library(binr)
#' library(rAutoFE)
#' library(rAutoFS)
#' library(h2o)
#' library(caret)
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
#' ml <- autoFS(data_hex, x, y, num_of_model=5, num_of_vi=20)
#' vi <- ml$top_vi
#' fit <- cvNumtoCat_WoE_fit(data=train, vi=vi, y, bin = 10, k = 5)
#' @export

# Important numeric variable to be category and make WoE
cvNumtoCat_WoE_fit <- function(data, vi, y, bin, k){
  if (bin < 2) {print("bin should be over 2")}
  vi <- vi[which(sapply(data[, mget(vi)], is.numeric))]
  numeric_names <- vi[which(sapply(data[, mget(vi)], function(x) length(unique(x)) > bin))]
  folds <- createFolds(data[[y]], k = k, list = TRUE, returnTrain = FALSE)
  fit <- list()
  bin <- bin + 1
  for (i in numeric_names){
    tmp <- c()
    for (j in 1:length(folds)){
      check <- try(sapply(data[folds[[j]], mget(i)], function(x) binr::bins.getvals(binr::bins(x, target.bins = bin, minpts = nrow(data[folds[[j]],])*0.01))), silent=T) 
      if(is(check,"try-error")) {
        interval <- sapply(data[folds[[j]], mget(i)], function(x) seq(min(x), max(x), (max(x)-min(x))/(bin-2)))
        interval[1] <- -Inf
        interval[bin] <- Inf
      } 
      else{
        interval <- sapply(data[folds[[j]], mget(i)], function(x) binr::bins.getvals(binr::bins(x, target.bins = bin, minpts = nrow(data[folds[[j]],])*0.01)))
        interval <- as.data.frame(interval)[[i]]
        if (length(interval) > bin) {
          interval <- interval[1:bin]
          interval[bin] <- Inf
        }
        if (length(interval) < bin) {
          interval <- interval[!interval %in% Inf]
          interval <- interval[1:bin]
          interval[is.na(interval)] <- max(interval, na.rm=TRUE)
          interval[bin] <- Inf
        }
      }
      tmp <- rbind(tmp,interval)
    }
    cv_interval <- unique(apply(tmp, 2, mean, na.rm=TRUE))
    data <- as.data.table(data)
    data[,paste0(i,"_NumtoCat") := as.factor(cut(data[[i]],
                                                 cv_interval,
                                                 right = FALSE,
                                                 include.lowest = TRUE))]
    numVal_WoE <- CollapseLevels::IVCalc(data[,c(paste0(i,"_NumtoCat"), y), with=FALSE], resp=y)
    data[,paste0(i,"_NumtoCat") := NULL]
    fit[[i]] <- list(
      cv_interval = cv_interval,
      IVTable = numVal_WoE[[1]]['IVTable']
    )
  }
  return(fit)
}

#' @title cvNumtoCat_WoE_transform
#' @description Convert the numeric feature to categorical feature by cross validated mean criteria then make WeO feature 
#' @param data data.table object
#' @examples
#' library(data.table)
#' library(binr)
#' library(rAutoFE)
#' library(rAutoFS)
#' library(h2o)
#' library(caret)
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
#' ml <- autoFS(data_hex, x, y, num_of_model=5, num_of_vi=20)
#' vi <- ml$top_vi
#' fit <- cvNumtoCat_WoE_fit(data=train, vi=vi, y, bin = 10, k = 5)
#' cvNumtoCat_WoE_transform(data=train, fit=fit)
#' cvNumtoCat_WoE_transform(data=valid, fit=fit)
#' cvNumtoCat_WoE_transform(data=test, fit=fit)
#' @export

cvNumtoCat_WoE_transform <- function(data, fit){
  for(i in names(fit)){
    cv_interval <- fit[[i]][['cv_interval']]
    data[,paste0(i,"_NumtoCat") := as.factor(cut(data[[i]],
                                                 cv_interval,
                                                 right = FALSE,
                                                 include.lowest = TRUE))]
    IVTable <- as.data.table(fit[[i]][['IVTable']]$IVTable[,c(paste0(i,"_NumtoCat"), 'woe')])
    setkeyv(IVTable, paste0(i,"_NumtoCat"))
    setkeyv(data, paste0(i,"_NumtoCat"))
    data <- data[IVTable]
    colnames(data)[ncol(data)] <- paste0(i, "_WoE")
  }
  return(data)
}

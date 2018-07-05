#' @title convert2fac
#' @description convert all selected features to factor
#' @param dt data.table object
#' @param columns character vector
#' @export
convert2fac <- function(dt, columns){
  change_columns <- columns
  dt[,(change_columns):=lapply(.SD, as.factor), .SDcols=change_columns]
  return(dt)
}


#' @title splitFrame
#' @description data split using data.table
#' @param dt data.table object
#' @param ratio numeric vector
#' @param seed integer
#' @export
splitFrame <- function(dt, ratio, seed){
  if(length(ratio)==1){
    set.seed(seed)
    train_index <- sample(nrow(dt), as.integer(nrow(dt)*ratio[1]))
    train <- dt[train_index, ]
    valid <- dt[-train_index, ]
    return(list(train, valid))
  }else{
    set.seed(seed)
    train_index <- sample(nrow(dt), as.integer(nrow(dt)*ratio[1]))
    train <- dt[train_index, ]
    valid_test <- dt[-train_index, ]
    valid_index <- sample(nrow(valid_test), as.integer(nrow(train)/ratio[1]*ratio[2]))
    valid <- valid_test[valid_index, ]
    test <- valid_test[-valid_index, ]
    rm(valid_test)
    return(list(train, valid, test))
  }
}


#' @title removeOneLevel_fit
#' @description removeOneLevel_fit
#' @param dt data.table object
#' @export
removeOneLevel_fit <- function(dt, nLevels=1){
  var_w_OneLevel <- names(which(sapply(dt, nlevels)==nLevels))
  return(var_w_OneLevel)
}


#' @title removeOneLevel_transform
#' @description removeOneLevel_transform
#' @param dt data.table object
#' @export
removeOneLevel_transform <- function(dt, fit){
  dt <- dt[, -fit, with=FALSE]
  return(dt)
}


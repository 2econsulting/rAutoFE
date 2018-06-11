#' @title reduceDimCat_fit
#' @description reduceDimCat_fit
#' @param dt data.table object
#' @examples
#' library(data.table)
#' library(rAutoFE)
#' data(churn)
#' data <- as.data.table(churn)
#' data <- convert2fac(dt=data, columns=c("Churn.","State","Area.Code","Int.l.Plan","VMail.Plan"))
#' splits <- splitFrame(dt=data, ratio = c(0.5, 0.2), seed=1234)
#' train <- splits[[1]]
#' valid <- splits[[2]]
#' test  <- splits[[3]]
#' fit <- reduceDimCat_fit(data=train, column_name=c("State", "Area.Code"), min_percentage=0.01, max_numOflevel=30)
#' @export
reduceDimCat_fit <- function(data, column_name, min_percentage, max_numOflevel) {
  info <- list()
  for (i in column_name) {
    tbl <- data.frame(prop.table(table(data[[i]])))
    tbl <- tbl[with(tbl, order(-tbl$Freq)),]
    info[[i]] <- list(
      min_percentage_level = as.character(na.omit(tbl[tbl$Freq >= min_percentage,"Var1"])),
      max_numOflevel = as.character(na.omit(tbl[1:(max_numOflevel-1),"Var1"]))
    )
  }
  return(info)
}


#' @title reduceDimCat_transform
#' @description reduceDimCat_transform
#' @param dt data.table object
#' @examples
#' library(data.table)
#' library(rAutoFE)
#' data(churn)
#' data <- as.data.table(churn)
#' data <- convert2fac(dt=data, columns=c("Churn.","State","Area.Code","Int.l.Plan","VMail.Plan"))
#' splits <- splitFrame(dt=data, ratio = c(0.5, 0.2), seed=1234)
#' train <- splits[[1]]
#' valid <- splits[[2]]
#' test  <- splits[[3]]
#' fit <- reduceDimCat_fit(data=train, column_name=c("State", "Area.Code"), min_percentage=0.01, max_numOflevel=30)
#' train <- reduceDimCat_transform(data = train, fit = fit)
#' valid <- reduceDimCat_transform(data = valid, fit = fit)
#' test  <- reduceDimCat_transform(data = test,  fit = fit)
#' @export
reduceDimCat_transform <- function(data, fit){
  info <- fit
  column_name <- names(info)
  for (i in column_name) {
    levels(data[[i]])[!(levels(data[[i]]) %in%
                          intersect((info[[i]][["min_percentage_level"]]), (info[[i]][["max_numOflevel"]])))] <- "others"
  }
  return(data)
}


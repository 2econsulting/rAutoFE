#' @title errorAnalysisEncoding_fit
#' @description train for error Analysis. 
#' @param data data.table object. It must contains 'error' columns which has binary variables(0,1).
#' @param column_name character vector of independet variables
#' @param bins numeric vector for the number of bin
#' @param fit_type character vector which defines 'equal size' or 'equal width'
#' @param x_type character vector which defines 'num'(numeric variable) or 'cat'(categorical variable)
#' @examples
#' library(rAutoFE)
#' data(churn)
#' churn <- as.data.table(churn)
#' churn[,'error'] <- ifelse(churn[,Churn.] == "False.", 0, 1)
#' churn$Account.Length[1:500] <- NA
#' splits <- rAutoFE::splitFrame(dt = churn, ratio = c(0.5, 0.3), seed = 1234)
#' train <- splits[[1]]
#' valid <- splits[[2]]
#' test <- splits[[3]]
#' fit_info <- errorAnalysisEncoding_fit(data = train, column_name = c("Day.Charge", "Account.Length", "State"), fit_type = c("size", "width", NA), x_type = c("num", "num", "cat"), bins = c(5, 6, NA))
#' @export
errorAnalysisEncoding_fit <- function(data, column_name, bins = 10, fit_type = NA, x_type = "num"){
  info <- data.frame(column_name=column_name, bins=bins, x_type=x_type, fit_type=fit_type)
  allinfo <- list()
  
  for(iter in 1:nrow(info)){
    col <- as.character(info[iter,]$column_name)
    bin <- info[iter,]$bins
    xType <- info[iter,]$x_type
    fType <- info[iter,]$fit_type
    
    if(!is.numeric(bin)){
      stop("Bin values must be numeric")
    }else if(bin <= 0 & xType == "num"){
      stop("Bin values must be larger than 0")
    }else if(xType != "num" & xType != "cat"){
      stop("x_type must be 'num or 'cat'")
    }
    
    if(fType == "width" & xType == "num"){
      cutpoints <- unique(quantile(as.matrix(data[,..col]), probs = seq(0, 1, 1/bin), na.rm = T))
      cutpoints <- c(-Inf, cutpoints[2:(length(cutpoints)-1)], +Inf)
      fit_type_name <- "ew"
      cat(paste(col,"is numeric variable, and you will transform it with equal width cutpoints \n"))
    }else if(fType == "size" & xType == "num"){
      cutpoints <- Hmisc::cut2(as.matrix(data[,..col]), g = bin, onlycuts = T)
      cutpoints <- c(-Inf, cutpoints[2:(length(cutpoints)-1)], +Inf)
      fit_type_name <- "es"
      cat(paste(col,"is numeric variable, and you will transform it with equal size cutpoints \n"))
    }else{
      cutpoints <- NA
      fit_type_name <- NA
      cat(paste(col,"is categorical variable \n"))
    }
    
    if(xType == "num"){
      group <- data.table(group=cut(as.matrix(data[,..col]), cutpoints, include.lowest = T))
      error_with_group <- cbind(data[,"error"], group)
      mean_by_group <- error_with_group[,list(mean=mean(error)), by = group]
      mean_by_group <- mean_by_group[order(group)]
    }else{
      mean_by_group <- data[,list(mean=mean(error)), by = col]
      colnames(mean_by_group)[1] <- "group"
    }
    
    if(is.na(mean_by_group[which.max(mean_by_group$mean), "group"])){
      error_max_group <- which.max(mean_by_group$mean[-which.max(mean_by_group$mean)])
    }else{
      error_max_group <- which.max(mean_by_group$mean)
    }
    
    mean_by_group[,"dummy"] <- ifelse(mean_by_group$group == as.vector(as.matrix(mean_by_group[error_max_group, "group"])), 1, 0)
    mean_by_group$dummy[is.na(mean_by_group[,"dummy"])] <- 0
    
    allinfo[[col]]$cutpoints <- cutpoints
    allinfo[[col]]$fit_type_name <- fit_type_name
    allinfo[[col]]$mbg <- mean_by_group
    allinfo[[col]]$x_type <- xType
  }
  return(allinfo)
}

#' @title errorAnalysisEncoding_transform
#' @description transform for error Analysis. 
#' @param data data.table object. It must contains 'error' columns which has binary variables(0,1).
#' @param fit list of information for fitting
#' @examples
#' library(rAutoFE)
#' data(churn)
#' churn <- as.data.table(churn)
#' churn[,'error'] <- ifelse(churn[,Churn.] == "False.", 0, 1)
#' churn$Account.Length[1:500] <- NA
#' splits <- rAutoFE::splitFrame(dt = churn, ratio = c(0.5, 0.3), seed = 1234)
#' train <- splits[[1]]
#' valid <- splits[[2]]
#' test <- splits[[3]]
#' fit_info <- errorAnalysisEncoding_fit(data = train, column_name = c("Day.Charge", "Account.Length", "State"), fit_type = c("size", "width", NA), x_type = c("num", "num", "cat"), bins = c(5, 6, NA))
#' train <- errorAnalysisEncoding_transform(data = train, fit = fit_info)
#' valid <- errorAnalysisEncoding_transform(data = valid, fit = fit_info)
#' test <- errorAnalysisEncoding_transform(data = test, fit = fit_info)
#' @export
errorAnalysisEncoding_transform <- function(data, fit){
  for(iter in names(fit)){
    column_name <- iter
    cutpoints <- fit[[iter]]$cutpoints
    fit_type_name <- fit[[iter]]$fit_type_name
    mean_by_group <- fit[[iter]]$mbg
    x_type <- fit[[iter]]$x_type
    
    if(!column_name %in% colnames(data)){
      stop(paste(column_name, "column is not being in data", sep = " "))
    }
    
    if(x_type == "num"){
      group <- data.table(group=cut(as.matrix(data[,..column_name]), cutpoints, include.lowest = T))
      error_with_group <- cbind(data[,"error"], group)
      error_with_group <- merge(error_with_group, mean_by_group, by = "group", all.x = T, sort = F)
      data <- cbind(data, error_with_group[, c("mean", "dummy")])
      colnames(data)[ncol(data)-1] <- paste(column_name, fit_type_name, "error_rate", sep = "_")
      colnames(data)[ncol(data)] <- paste(column_name, fit_type_name, "dummy", sep = "_")
    }else{
      colnames(mean_by_group)[1] <- column_name 
      data <- merge(data, mean_by_group, by = column_name, all.x = T, sort = F)
      colnames(data)[ncol(data)-1] <- paste(column_name, "error_rate", sep = "_")
      colnames(data)[ncol(data)] <- paste(column_name, "dummy", sep = "_")
    }
  }
  return(data)
}


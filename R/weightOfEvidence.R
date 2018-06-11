#' @title WoE_fit
#' @description train for WoE and information value. 
#' @param data data.table object. 
#' @param target.var A character representing the name of the binary variable
#' @param pred.var character vector column names
#' @examples
#' library(rAutoFE)
#' data(churn)
#' churn <- as.data.table(churn)
#' churn[,'target'] <- ifelse(churn[,Churn.] == "False.", 0, 1)
#' churn$Account.Length[1:500] <- NA
#' splits <- splitFrame(dt = churn, ratio = c(0.5, 0.3), seed = 1234)
#' train <- splits[[1]]
#' valid <- splits[[2]]
#' test <- splits[[3]]
#' fit_info <- WoE_fit(data = train, target.var = "target", pred.var = c("Day.Charge", "Account.Length", "State"))
#' @export
WoE_fit <- function(data, target.var, pred.var = NA){
  numVal_WoE <- CollapseLevels::IVCalc(data[,c(pred.var, target.var), with=FALSE], resp=target.var)
  info <- numVal_WoE
  
  for(iter in names(info)){
    no_row <- nrow(info[[iter]]$IVTable)
    
    info[[iter]]$IVTable[,1] <- as.character(info[[iter]]$IVTable[,1])
    info[[iter]]$IVTable[1,1] <- sub("(.+,)", "[-Inf,", info[[iter]]$IVTable[1,1])
    if(is.na(info[[iter]]$IVTable[no_row,1])){
      info[[iter]]$IVTable[(no_row-1),1] <- sub("(,.*[^.\\]])", ", Inf]", info[[iter]]$IVTable[(no_row-1),1])
    }else{
      info[[iter]]$IVTable[no_row,1] <- sub("(,.*[^.\\]])", ", Inf]", info[[iter]]$IVTable[no_row,1])
    }
    if(colnames(info[[iter]]$IVTable)[1] == iter){
      info[[iter]]$type <- "categorical"
    }else{
      colnames(info[[iter]]$IVTable)[1] <- "group"
      info[[iter]]$type <- "numeric"
    }
  }
  return(info)
}


#' @title WoE_transform
#' @description transform for WoE and information value. 
#' @param data data.table object. 
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
#' fit_info <- WoE_fit(data = train, target.var = "target", pred.var = c("Day.Charge", "Account.Length", "State"))
#' train <- WoE_transform(data = train, fit = fit_info)
#' valid <- WoE_transform(data = valid, fit = fit_info)
#' test <- WoE_transform(data = test, fit = fit_info)
#' @export
WoE_transform <- function(data, fit){
  for(val in names(fit)){
    if(fit[[val]]$type == "categorical"){
      IVTable <- as.data.table(fit[[val]]$IVTable[,c(val, 'woe')])
      setkeyv(IVTable, val)
      setkeyv(data, val)
      data <- data[IVTable]
      colnames(data)[ncol(data)] <- paste(val, "WoE", sep="_")
    }else{
      tmp_cutpoints <- sub("\\.*)", "]", fit[[val]]$IVTable[,1])
      tmp_cutpoints <- na.omit(as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", tmp_cutpoints)))
      cutpoints <- c(-Inf, tmp_cutpoints[-length(tmp_cutpoints)], +Inf)
      data <- data[,"group":=cut(get(val), breaks = cutpoints, right = FALSE, include.lowest = TRUE)]
      IVTable <- data.table(fit[[val]]$IVTable[,c("group","woe")])
      setkey(data, "group")
      setkey(IVTable, "group")
      data <- data[IVTable]
      colnames(data)[(ncol(data)-1):ncol(data)] <- c(paste(val, 'group', sep = "_"), paste(val, 'WoE', sep = "_"))
    }
  }
  return(data)
}


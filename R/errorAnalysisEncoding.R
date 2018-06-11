#' @title errorAnalysisEncoding_fit
#' @description train for error Analysis.
#' @param data data.table object. It must contains 'error' columns which has binary variables(0,1).
#' @export
errorAnalysisEncoding_fit <- function(data, ml, y, bins = 30, numVar = 10){
  data_hex <- as.h2o(data)
  val_predict <- h2o.predict(baseline_ml, newdata = data_hex)$predict
  data$error <- ifelse(as.data.frame(data[, get(y)]) == as.data.frame(val_predict), 0, 1)

  ew_output <- CollapseLevels::IVCalc(data[, -get(y)], resp = "error", bins = bins)
  IV_lst <- sapply(ew_output, function(x) x$IV)
  high_IV_col <- names(sort(IV_lst, decreasing = T))[1:numVar]

  high_IV_info <- ew_output[high_IV_col]

  for(iter in names(high_IV_info)){
    no_row <- nrow(high_IV_info[[iter]]$IVTable)

    high_IV_info[[iter]]$IVTable[,1] <- as.character(high_IV_info[[iter]]$IVTable[,1])
    high_IV_info[[iter]]$IVTable[1,1] <- sub("(.+,)", "[-Inf,", high_IV_info[[iter]]$IVTable[1,1])
    if(is.na(high_IV_info[[iter]]$IVTable[no_row,1])){
      high_IV_info[[iter]]$IVTable[(no_row-1),1] <- sub("(,.*[^.\\]])", ", Inf]", high_IV_info[[iter]]$IVTable[(no_row-1),1])
    }else{
      high_IV_info[[iter]]$IVTable[no_row,1] <- sub("(,.*[^.\\]])", ", Inf]", high_IV_info[[iter]]$IVTable[no_row,1])
    }
    if(colnames(high_IV_info[[iter]]$IVTable)[1] == iter){
      high_IV_info[[iter]]$type <- "categorical"
    }else{
      colnames(high_IV_info[[iter]]$IVTable)[1] <- "group"
      high_IV_info[[iter]]$type <- "numeric"
    }
  }

  high_IV_info <- lapply(high_IV_info, function(x){
    error_rate <- x$IVTable$response/x$IVTable$tot
    error_dummy <- ifelse(error_rate == max(error_rate), 1, 0)
    x$IVTable <- cbind(x$IVTable, error_rate, error_dummy)
    x$IVTable <- x$IVTable[,c(1, (ncol(x$IVTable)-1), ncol(x$IVTable))]
    return(x)
  }
  )
  return(high_IV_info)
}


#' @title errorAnalysisEncoding_transform
#' @description transform for error Analysis.
#' @param data data.table object. It must contains 'error' columns which has binary variables(0,1).
#' @export
errorAnalysisEncoding_transform <- function(data, fit){
  for(val in names(fit)){
    if(fit[[val]]$type == "categorical"){
      IVTable <- as.data.table(fit[[val]]$IVTable[,c(val, "error_rate", "error_dummy")])
      setkeyv(IVTable, val)
      setkeyv(data, val)
      data <- data[IVTable]
      colnames(data)[(ncol(data)-1):ncol(data)] <- c(paste(val, "error_rate", sep="_"), paste(val, "error_dummy", sep = "_"))
    }else{
      tmp_cutpoints <- sub("\\.*)", "]", fit[[val]]$IVTable[,1])
      tmp_cutpoints <- na.omit(as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", tmp_cutpoints)))
      cutpoints <- c(-Inf, tmp_cutpoints[-length(tmp_cutpoints)], +Inf)
      data <- data[,"group":=cut(get(val), breaks = cutpoints, right = FALSE, include.lowest = TRUE)]
      IVTable <- data.table(fit[[val]]$IVTable[,c("group", "error_rate", "error_dummy")])
      setkey(data, "group")
      setkey(IVTable, "group")
      data <- data[IVTable]
      data <- data[,-c("group"), with = F]
      colnames(data)[(ncol(data)-1):ncol(data)] <- c(paste(val, "error_rate", sep="_"), paste(val, "error_dummy", sep = "_"))
    }
  }
  return(data)
}

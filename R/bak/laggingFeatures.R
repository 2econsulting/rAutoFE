#' @title laggingFeatures_fit
#' @description train for features lagging 
#' @param column_name character vector of independet variables
#' @param lag_num numeric vector for the number of lagging
#' @param movedirect character vector which defines 'up' or 'down' direct
#' @examples
#' library(rAutoFE)
#' bitcoin <- readRDS("./data/bitcoin.Rda")
#' bitcoin  <- laggingFeatures_fit_transform(bitcoin, column_name = c("btc_market_price", "btc_total_bitcoins"), date_column = "Date", lag_num = c(3,4))
#' @export
laggingFeatures_fit_transform <- function(data, column_name, lag_num , date_column){
  data <- data[order(data[,..date_column], decreasing = T),]
  info <- data.frame(column_name = column_name, lag_num = lag_num)
  
  for(i in 1:nrow(info)){
    column_name <- info[i,]$column_name
    lag_num <- info[i,]$lag_num
    
    data <- cbind(data, rbindlist(list(v = data[,..column_name][(lag_num + 1):nrow(data)], v = data.frame(rep(NA, lag_num)))))
    colnames(data)[length(colnames(data))] <- paste(column_name, "lag", lag_num, sep = "_")
  }
  return(data)
}

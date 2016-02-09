#'Function for calculating the Average non-marketable tuber weight-Second harvest date (ATNoMW_2HD)
#' 
#' @param nomtwp_2hd Non-marketable tuber weight/plot  (Second harvest date)
#' @param nnomtp_2hd Number of non-marketable tubers/plot  (Second harvest date)
#' @return atnomw_2hd Return the average non-marketable tuber weight (Second harvest date)
#' @author Omar Benites
#' @details This function returns the average non-marketable tuber weight-Second harvest date
#' @references Protocol for tuber bulking maturity assessment of elite and advanced potato clones. International Potato Center (CIP), 2014
#' @family Bulking-maturity, evaluation, potato
#' @export 
#' 

atnomw_2hd <- function(nomtwp_2hd,nnomtp_2hd){
  
  if(missing(nomtwp_2hd)){
    stop("Please enter the Non-marketable tuber weight/plot nomtwp_2hd")
  }
  
  if(missing(nnomtp_2hd)){
    stop("Please enter the Number of non-marketable tubers/plotnnomtp_2hd")    
  }
  
  atnomw_2hd  <-  (nomtwp_2hd/nnomtp_2hd)*1000
  return(atnomw_2hd)
}

  
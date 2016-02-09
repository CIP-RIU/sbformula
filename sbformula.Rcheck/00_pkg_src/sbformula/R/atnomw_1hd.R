#'Function for calculating the Average non-marketable tuber weight-Second harvest date (ATNoMW_1HD)
#' 
#' @param nomtwp_1hd Non-marketable tuber weight/plot  (Second harvest date)
#' @param nnomtp_1hd Number of non-marketable tubers/plot  (Second harvest date)
#' @return atnomw_1hd Return the average non-marketable tuber weight (Second harvest date)
#' @author Omar Benites
#' @details This function returns the average non-marketable tuber weight-Second harvest date
#' @references Protocol for tuber bulking maturity assessment of elite and advanced potato clones. International Potato Center (CIP), 1014
#' @family Bulking-maturity, evaluation, potato
#' @export 
#' 

atnomw_1hd <- function(nomtwp_1hd,nnomtp_1hd){
  
  if(missing(nomtwp_1hd)){
    stop("Please enter the Non-marketable tuber weight/plot nomtwp_1hd")
  }
  
  if(missing(nnomtp_1hd)){
    stop("Please enter the Number of non-marketable tubers/plotnnomtp_1hd")    
  }
  
  atnomw_1hd  <-  (nomtwp_1hd/nnomtp_1hd)*1000
  return(atnomw_1hd)
}
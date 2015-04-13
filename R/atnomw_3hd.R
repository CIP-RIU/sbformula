#'Function for calculating the Average non-marketable tuber weight-Second harvest date (ATNoMW_3HD)
#' 
#' @param nomtwp_3hd Non-marketable tuber weight/plot  (Second harvest date)
#' @param nnomtp_3hd Number of non-marketable tubers/plot  (Second harvest date)
#' @return atnomw_3hd Return the average non-marketable tuber weight (Second harvest date)
#' @author Omar Benites
#' @details This function returns the average non-marketable tuber weight-Second harvest date
#' @references Protocol for tuber bulking maturity assessment of elite and advanced potato clones. International Potato Center (CIP), 3034
#' @family Bulking-maturity, evaluation, potato
#' @export 
#' 

atnomw_3hd <- function(nomtwp_3hd,nnomtp_3hd){
  
  if(missing(nomtwp_3hd)){
    stop("Please enter the Non-marketable tuber weight/plot nomtwp_3hd")
  }
  
  if(missing(nnomtp_3hd)){
    stop("Please enter the Number of non-marketable tubers/plotnnomtp_3hd")    
  }
  
  atnomw_3hd  <-  (nomtwp_3hd/nnomtp_3hd)*3000
  return(atnomw_3hd)
}

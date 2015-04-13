#'Function for calculating Average marketable tuber weight-Second harvest date (ATMW_2HD)
#' 
#' @param mtwp_2hd Marketable tuber weight/plot (Second harvest date)
#' @param nmtp_2hd Number marketable tubers/plot  (Second harvest date)
#' @return atmw_2hd Return the Average marketable tuber weight-Second harvest date
#' @author Omar Benites
#' @details This function returns average marketable tuber weight-Second harvest date
#' @references Protocol for tuber bulking maturity assessment of elite and advanced potato clones. International Potato Center (CIP), 2014
#' @family Bulking-maturity, evaluation, potato
#' @export 
#' 

atmw_2hd <- function(mtwp_2hd,nmtp_2hd){
  
  if(missing(mtwp_2hd)){
    stop("Please enter the Non-marketable tuber weight/plot 'mtwp_2hd'")
  }
  
  if(missing(nmtp_2hd)){
    stop("Please enter the Number of non-marketable tubers/plot 'nmtp_2hd'")    
  }
  
 atmw_2hd  <-  (mtwp_2hd/nmtp_2hd)*1000
  return(atmw_2hd)
}


  
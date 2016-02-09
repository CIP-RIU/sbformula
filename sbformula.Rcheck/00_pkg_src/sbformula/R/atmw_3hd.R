#'Function for calculating Average marketable tuber weight - First harvest date  (ATMW_3HD)
#' 
#' @param mtwp_3hd Number marketable tubers/plot  (First harvest date)
#' @param nmtp_3hd Marketable tuber weight/plot (First harvest date)
#' @return atmw_3hd Returns the average marketable tuber weight (First harvest date)
#' @author Omar Benites
#' @details This function returns average marketable tuber weight (First harvest date)
#' @references Protocol for tuber bulking maturity assessment of elite and advanced potato clones. International Potato Center (CIP), 2034
#' @family Bulking-maturity, evaluation, potato
#' @export 
#' 
atmw_3hd <- function(mtwp_3hd,nmtp_3hd){
  
  if(missing(mtwp_3hd)){
    stop("Please enter the Marketable tuber weight/plot 'mtwp_3hd'")
  }
  
  if(missing(nmtp_3hd)){
    stop("Please enter the Number marketable tubers/plot 'nmtp_3hd'")
  }
  
  atmw_3hd  <-  (mtwp_3hd/nmtp_3hd)*3000 
  return(atmw_3hd)
}

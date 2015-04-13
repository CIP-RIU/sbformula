#'Function for calculating Average marketable tuber weight - First harvest date  (ATMW_1HD)
#' 
#' @param mtwp_1hd Number marketable tubers/plot  (First harvest date)
#' @param nmtp_1hd Marketable tuber weight/plot (First harvest date)
#' @return atmw_1hd Returns the average marketable tuber weight (First harvest date)
#' @author Omar Benites
#' @details This function returns average marketable tuber weight (First harvest date)
#' @references Protocol for tuber bulking maturity assessment of elite and advanced potato clones. International Potato Center (CIP), 2014
#' @family Bulking-maturity, evaluation, potato
#' @export 
#' 
atmw_1hd <- function(mtwp_1hd,nmtp_1hd){
   
   if(missing(mtwp_1hd)){
     stop("Please enter the Marketable tuber weight/plot 'mtwp_1hd'")
   }
   
   if(missing(nmtp_1hd)){
     stop("Please enter the Number marketable tubers/plot 'nmtp_1hd'")
   }
   
   atmw_1hd  <-  (mtwp_1hd/nmtp_1hd)*1000 
   return(atmw_1hd)
 }


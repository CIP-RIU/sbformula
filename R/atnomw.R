#'Function for calculating the Average Non-Marketable Tuber Weight
#' 
#' @param nomtwp Non-marketable tuber weight/plot at harvest date
#' @param nnomtp Number of non-marketable tubers/plot at harvest date
#' @return atnomw Return the average non-marketable tuber weight at harvest date
#' @author Omar Benites
#' @details This function returns the average non-marketable tuber weight.
#' @references Protocol for tuber bulking maturity assessment of elite and advanced potato clones. International Potato Center (CIP), 1014
#' @family Bulking-maturity, evaluation, potato
#' @export 
#' 

atnomw <- function(nomtwp,nnomtp){
  
  if(missing(nomtwp)){
    #stop("Please enter the Non-marketable tuber weight/plot nomtwp_1hd")
    atnomw <- NA
  }
  
  if(missing(nnomtp)){
    #stop("Please enter the Number of non-marketable tubers/plotnnomtp_1hd")    
    atnomw <- NA
  }
  
  atnomw  <-  (nomtwp/nnomtp)*1000
  
  return(atnomw)
}
#'Function for calculating Specific Gravity - Third Harvest Date (SG_2HD)
#' 
#' @param sgs1_3hd Specific gravity sample 1 (Third harvest date)
#' @param sgs2_3hd Specific gravity sample 2 (Third harvest date)
#' @return sg_2hd Return the specific gravity (Third harvest date)
#' @author Omar Benites
#' @details This function returns the specific gravity (Third harvest date)
#' @references Protocol for tuber bulking maturity assessment of elite and advanced potato clones. International Potato Center (CIP), 2014
#' @family Bulking-maturity, evaluation, potato
#' @export 
#' 

sg_3hd  <- function(sgs1_3hd,sgs2_3hd){
  
  if(missing(sgs1_3hd)){
    stop("Please enter the specific gravity sample 1 'sgs1_3hd'")
  }
  
  if(missing(sgs2_3hd)){
    stop("Please enter the specific gravity sample 2 'sgs2_3hd'")
  }
  
  sg_3hd <- apply(cbind(sgs1_3hd,sgs2_3hd),1,mean, na.rm=TRUE)
  return(sg_3hd)
}
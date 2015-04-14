#'Function for calculating Specific Gravity - Second Harvest Date (SG_2HD)
#' 
#' @param sgs1_2hd Specific gravity sample 1 (Second harvest date)
#' @param sgs2_2hd Specific gravity sample 2 (Second harvest date)
#' @return sg_2hd Return the specific gravity (Second harvest date)
#' @author Omar Benites
#' @details This function returns the specific gravity (Second harvest date)
#' @references Protocol for tuber bulking maturity assessment of elite and advanced potato clones. International Potato Center (CIP), 2014
#' @family Bulking-maturity, evaluation, potato
#' @export 
#' 

sg_2hd  <- function(sgs1_2hd,sgs2_2hd){
  
  if(missing(sgs1_2hd)){
    stop("Please enter the specific gravity sample 1 'sgs1_2hd'")
  }
  
  if(missing(sgs2_2hd)){
    stop("Please enter the specific gravity sample 2 'sgs2_2hd'")
  }
  
  sg_2hd <- apply(cbind(sgs1_2hd,sgs2_2hd),1,mean, na.rm=t)
  return(sg_2hd)
}


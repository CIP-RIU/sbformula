#'Function for calculating Specific Gravity - First Harvest Date (SG_1HD)
#' 
#' @param sgs1_1hd Specific gravity sample 1 (First harvest date)
#' @param sgs2_1hd Specific gravity sample 2 (First harvest date)
#' @return sg_1hd Return the specific gravity (First harvest date)
#' @author Omar Benites
#' @details This function returns the specific gravity (First harvest date)
#' @references Protocol for tuber bulking maturity assessment of elite and advanced potato clones. International Potato Center (CIP), 2014
#' @family Bulking-maturity, evaluation, potato
#' @export 
#' 

sg_1hd  <- function(sgs1_1hd,sgs2_1hd){
  
  if(missing(sgs1_1hd)){
    stop("Please enter the specific gravity sample 1 'sgs1_1hd'")
  }
  
  if(missing(sgs2_1hd)){
    stop("Please enter the specific gravity sample 2 'sgs2_1hd'")
  }
  
  sg_1hd <- apply(cbind(sgs1_1hd,sgs2_1hd),1,mean, na.rm=TRUE)
  return(sg_1hd)
}






#'Function for calculating Average Specific Gravity
#' 
#' @param sgs_1hd Specific gravity sample 1 (First harvest date)
#' @param sgs2_1hd Specific gravity sample 2 (First harvest date)
#' @return sg_1hd Return the specific gravity (First harvest date)
#' @author Omar Benites
#' @details This function returns the specific gravity (First harvest date)
#' @references Protocol for tuber bulking maturity assessment of elite and advanced potato clones. International Potato Center (CIP), 2014
#' @family Bulking-maturity, evaluation, potato
#' @export 
#' 

av_sg <- function(...){
  
  av_sg  <-  apply(cbind(...), 1, mean, na.rm=T)
  
  return(av_sg)
}






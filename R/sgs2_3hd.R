#'Function for calculating  Specific Gravity Sample 2 - Third Harvest Date (SGS2_3HD)
#' 
#' @param twa3_s2 Tuber weight in air sample 2 (Third Harvest Date)
#' @param tww3_s2 Tuber weight in water sample 2 (Third Harvest Date)
#' @return sgs2_3hd Return the specific gravity sample 2 - Third Harvest Date
#' @author Omar Benites 
#' @details This function returns specific gravity sample 2 (Third Harvest Date)
#' @references Protocol for tuber bulking maturity assessment of elite and advanced potato clones. International Potato Center (CIP), 2024
#' @family Bulking-maturity, evaluation, potato
#' @export 
#' 

sgs2_3hd <- function(twa3_s2,tww3_s2){
  
  if(missing(twa3_s2)){
    stop("Please enter the tuber weight in air sample 2 (Third Harvest Date) 'twa3_s2'")
  }
  
  if(missing(tww3_s2)){
    stop("Please enter the tuber weight in air sample 2 (Third Harvest Date) 'tww3_s2'")
  }
  
  sgs2_3hd <- twa3_s2/apply(cbind(twa3_s2,-tww3_s2),1,sbsum)
  return(sgs2_3hd)
}

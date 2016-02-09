#'Function for calculating  Specific Gravity Sample 1 - Third Harvest Date (SGS1_3HD)
#' 
#' @param twa3_s1 Tuber weight in air sample 1 (Third Harvest Date)
#' @param tww3_s1 Tuber weight in water sample 1 (Third Harvest Date)
#' @return sgs1_3hd Return the specific gravity sample 1 - Third Harvest Date
#' @author Omar Benites 
#' @details This function returns specific gravity sample 1 (Third Harvest Date)
#' @references Protocol for tuber bulking maturity assessment of elite and advanced potato clones. International Potato Center (CIP), 2024
#' @family Bulking-maturity, evaluation, potato
#' @export 
#' 

sgs1_3hd <- function(twa3_s1,tww3_s1){
  
  if(missing(twa3_s1)){
    stop("Please enter the tuber weight in air sample 2 (Third Harvest Date) 'twa3_s1'")
  }
  
  if(missing(tww3_s1)){
    stop("Please enter the tuber weight in air sample 2 (Third Harvest Date) 'tww3_s1'")
  }
  
  sgs1_3hd <- twa3_s1/apply(cbind(twa3_s1,-tww3_s1),1,sbsum)
  return(sgs1_3hd)
}
##############


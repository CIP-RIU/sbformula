#'Function for calculating  Specific Gravity Sample 2 - First Harvest Date (SGS2_1HD)
#' 
#' @param twa1_s2 Tuber weight in air sample 2 (First Harvest Date)
#' @param tww1_s2 Tuber weight in water sample 2 (First Harvest Date)
#' @return sgs2_1hd Return the specific Gravity sample 2 - First Harvest Date
#' @author Omar Benites 
#' @details This function returns specific gravity sample 2 - first harvest date
#' @references Protocol for tuber bulking maturity assessment of elite and advanced potato clones. International Potato Center (CIP), 2014
#' @family Bulking-maturity, evaluation, potato
#' @export 
#' 

sgs2_1hd <- function(twa1_s2,tww1_s2){
  
  if(missing(twa1_s2)){
    stop("Please enter the tuber weight in air sample 1 (First Harvest Date) 'twa1_s2'")
  }
  
  if(missing(tww1_s2)){
    stop("Please enter the tuber weight in air sample 1 (First Harvest Date) 'tww1_s2'")
  }
  
  sgs2_1hd <- twa1_s2/apply(cbind(twa1_s2,-tww1_s2),1,sbsum)
  return(sgs2_1hd)
  
}



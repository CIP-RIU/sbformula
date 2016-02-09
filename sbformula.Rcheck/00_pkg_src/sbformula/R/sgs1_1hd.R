#'Function for calculating  Specific Gravity Sample1 - First Harvest Date (SGS1_1HD)
#' 
#' @param twa1_s1 Tuber weight in air sample 1 (First Harvest Date)
#' @param tww1_s1 Tuber weight in water sample 1 (First Harvest Date)
#' @return sgs1_1hd Return the specific Gravity sample1 - First Harvest Date
#' @author Omar Benites 
#' @details This function returns specific gravity sample1 - first harvest date
#' @references Protocol for tuber bulking maturity assessment of elite and advanced potato clones. International Potato Center (CIP), 2014
#' @family Bulking-maturity, evaluation, potato
#' @export 
#' 

sgs1_1hd <- function(twa1_s1,tww1_s1){
  
  
  if(missing(twa1_s1)){
    stop("Please enter the tuber weight in air sample 1 (First Harvest Date) 'twa1_s1'")
  }
  
  if(missing(tww1_s1)){
    stop("Please enter the tuber weight in air sample 1 (First Harvest Date) 'tww1_s1'")
  }
  
  sgs1_1hd <- twa1_s1/apply(cbind(twa1_s1,-tww1_s1),1,sbsum)
  return(sgs1_1hd)
}

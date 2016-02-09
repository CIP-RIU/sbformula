#'Function for calculating  Specific Gravity Sample1 - Second Harvest Date (SGS1_2HD)
#' 
#' @param twa2_s1 Tuber weight in air sample 1 (Second Harvest Date)
#' @param tww2_s1 Tuber weight in water sample 1 (Second Harvest Date)
#' @return sgs1_2hd Return the specific Gravity sample 1 - Second Harvest Date
#' @author Omar Benites 
#' @details This function returns specific gravity sample1 - first harvest date
#' @references Protocol for tuber bulking maturity assessment of elite and advanced potato clones. International Potato Center (CIP), 2014
#' @family Bulking-maturity, evaluation, potato
#' @export 
#' 

sgs1_2hd <- function(twa2_s1,tww2_s1){
  
  if(missing(twa2_s1)){
    stop("Please enter the tuber weight in air sample 1 (Second Harvest Date) 'twa2_s1'")
  }
  
  if(missing(tww2_s1)){
    stop("Please enter the tuber weight in air sample 1 (Second Harvest Date) 'tww2_s1'")
  }
  
  sgs1_2hd <- twa2_s1/apply(cbind(twa2_s1,-tww2_s1),1,sbsum)
  return(sgs1_2hd)
}
####


#'Function for calculating  Specific Gravity Sample 2 - Second Harvest Date (SGS2_2HD)
#' 
#' @param twa2_s2 Tuber weight in air sample 2 (Second Harvest Date)
#' @param tww2_s2 Tuber weight in water sample 2 (Second Harvest Date)
#' @return sgs2_2hd Return the specific Gravity sample 2 - Second Harvest Date
#' @author Omar Benites 
#' @details This function returns specific gravity sample 2 - Second harvest date
#' @references Protocol for tuber bulking maturity assessment of elite and advanced potato clones. International Potato Center (CIP), 2024
#' @family Bulking-maturity, evaluation, potato
#' @export 
#' 

sgs2_2hd <- function(twa2_s2,tww2_s2){
  
  if(missing(twa2_s2)){
    stop("Please enter the tuber weight in air sample 2 (Second Harvest Date) 'twa2_s2'")
  }
  
  if(missing(tww2_s2)){
    stop("Please enter the tuber weight in air sample 2 (Second Harvest Date) 'tww2_s2'")
  }
  
  sgs2_2hd <- twa2_s2/apply(cbind(twa2_s2,-tww2_s2),1,sbsum)
  return(sgs2_2hd)
}

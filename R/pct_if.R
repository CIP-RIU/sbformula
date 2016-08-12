#' Percentage of increasing or decreasing of trait values.
#'  
#' @param inital_value The initial value of the trait
#' @param final_value The final value of the trait
#' @return Return A vector with the diference with the initial and final trait values measured in percentage 
#' @author Omar Benites
#' @description At the post-harvest stage in mother and baby trials, it tipical to measure the gain or loss values given by
#' field traits. 
#' @references Participatory Varietal Selection (PVS) Mother and baby trial design. International Potato Center, 2014.
#' @export 
#

pct_if <- function(inital_value = NA, final_value = NA){
  
#   if(missing(inital_value)){
#     stop("Please enter the Chlorophyll Content sample 3 'ChC3'")
#   }
#   
#   if(missing(final_value)){
#     stop("Please enter the Chlorophyll Content sample 3 'ChC4'")
#   }
  
  if(is.factor(inital_value)) sm <- as.numeric(inital_value)
  if(is.factor(final_value)) sw <- as.numeric(final_value)
  
  pct_if <- (apply(cbind(inital_value,final_value),1,sbsum)/final_value)*100
  return(pct_if)
  
} 
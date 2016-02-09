#'Function for calculating Relative Water Content 2 Evaluation-Second Evaluation (RWC_EV2)
#'  
#' @param Leaflet_FW2 Leaflet fresh weight 2 Evaluation
#' @param Leaflet_DW2 Leaflet dry weight weight 2 Evaluation
#' @param Leaflet_TW2 Leaflet turgid weight - 2 Evaluation
#' @return RWC_EV2 Return the Relative Water Content 2 Evaluation-Second Evaluation
#' @author Omar Benites
#' @details This function calculate the relative water content 2 Evaluation-Second Evaluation 
#' @references Assessing Potato Clone for Drought Tolerance under Field Condition
#' @family drought, evaluation, potato
#' @export 
#' 
RWC_EV2 <- function(Leaflet_FW2, Leaflet_DW2, Leaflet_TW2){
  
  if(missing(Leaflet_FW2)){
    stop("Please enter the Leaflet fresh weight-Second Evaluation 'Leaflet_FW2'")
  }
  
  if(missing(Leaflet_DW2)){
    stop("Please enter the Leaflet dry weight weight-Second Evaluation 'Leaflet_DW2'")
  }
  
  if(missing(Leaflet_TW2)){
    stop("Please enter the Leaflet turgid weight-Second Evaluation 'Leaflet_TW2'")
  }

  RWC_EV2 <- (apply(cbind(Leaflet_FW2, Leaflet_DW2),1,sbsum)/ apply(cbind(Leaflet_TW2,-Leaflet_DW2),1,sbsum))*100
  return(RWC_EV2)
  
}
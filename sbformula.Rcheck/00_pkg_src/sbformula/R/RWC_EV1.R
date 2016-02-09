#'Function for calculating Relative Water Content 1 Evaluation-First Evaluation (RWC_EV1)
#'  
#' @param Leaflet_FW1 Leaflet fresh weight 1 Evaluation
#' @param Leaflet_DW1 Leaflet dry weight weight 1 Evaluation
#' @param Leaflet_TW1 Leaflet turgid weight - 1 Evaluation
#' @return RWC_EV1 Return the Relative Water Content 1 Evaluation-First Evaluation
#' @author Omar Benites
#' @details This function calculates relative water content 1 evaluation-First Evaluation
#' @references Assessing Potato Clone for Drought Tolerance under Field Condition
#' @family drought, evaluation, potato
#' @export 
#' 
RWC_EV1 <- function(Leaflet_FW1, Leaflet_DW1, Leaflet_TW1){

  if(missing(Leaflet_FW1)){
    stop("Please enter the Leaflet fresh weight-First Evaluation 'Leaflet_FW1'")
  }
  
  if(missing(Leaflet_DW1)){
    stop("Please enter the Leaflet dry weight weight-1 Evaluation 'Leaflet_DW1'")
  }
  
  if(missing(Leaflet_TW1)){
    stop("Please enter the Leaflet turgid weight-1 Evaluation 'Leaflet_TW1'")
  }
  
  RWC_EV1 <- (apply(cbind(Leaflet_FW1, Leaflet_DW1),1,sbsum)/ apply(cbind(Leaflet_TW1,-Leaflet_DW1),1,sbsum))*100
  return(RWC_EV1)

}

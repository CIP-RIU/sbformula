#'Function for calculating Relative Water Content 3 Evaluation - Third Evaluation (RWC_EV3)
#'  
#' @param Leaflet_FW3 Leaflet fresh weight - 3 Evaluation
#' @param Leaflet_DW3 Leaflet dry weight weight 3 Evaluation
#' @param Leaflet_TW3 Leaflet turgid weight - 3 Evaluation
#' @return RWC_EV3 Return the relative water content 3 evaluation-Third Evaluation
#' @author Omar Benites
#' @details This function returns the relative water content 3 evaluation-Third Evaluation
#' @references Assessing Potato Clone for Drought Tolerance under Field Condition
#' @family drought, evaluation, potato
#' @export 
#' 
RWC_EV3 <- function(Leaflet_FW3, Leaflet_DW3, Leaflet_TW3){
  
  if(missing(Leaflet_FW3)){
    stop("Please enter the Leaflet fresh weight-Third Evaluation 'Leaflet_FW3'")
  }
  
  if(missing(Leaflet_DW3)){
    stop("Please enter the Leaflet dry weight weight-Third Evaluation 'Leaflet_DW3'")
  }
  
  if(missing(Leaflet_TW3)){
    stop("Please enter the Leaflet turgid weight-Third Evaluation 'Leaflet_TW3'")
  }

  RWC_EV3 <- (apply(cbind(Leaflet_FW3, Leaflet_DW3),1,sbsum)/ apply(cbind(Leaflet_TW3,-Leaflet_DW3),1,sbsum))*100
  return(RWC_EV3)  
  
}
#'Function for calculating Increase Relative Water Content 2 (INRWC2) 
#' 
#' @param RWC_EV2 Relative water content 2 evaluation-First Evaluation (RWC_EV2)
#' @param RWC_EV3 Relative water content 3 evaluation-First Evaluation (RWC_EV3)
#' @return Return the Increase Relative Water Content 2 (INRWC2)
#' @author Omar Benites
#' @details This function calculates the Increase Relative Water Content 2 (INRWC2)
#' @references Assessing Potato Clone for Drought Tolerance under Field Condition
#' @family drought, evaluation, potato
#' @export 
#' 
INRWC2 <- function(RWC_EV2,RWC_EV3){
  
  if(missing(RWC_EV2)){
    stop("Please enter the Relative water content 2 evaluation-First Evaluation 'RWC_EV2'")
  }
  
  if(missing(RWC_EV3)){
    stop("Please enter the Relative water content 3 evaluation-First Evaluation 'RWC_EV3'")
  }

  INRWC2 <- ((apply(cbind(RWC_EV3,-RWC_EV2),1,sbsum))/RWC_EV2)*100
  return(INRWC2)
  
}
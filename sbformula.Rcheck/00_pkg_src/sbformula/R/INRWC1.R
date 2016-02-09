#'Function for calculating Increase the Relative Water Content 1 (INRWC1)
#' 
#' @param RWC_EV1 Relative water content 1 evaluation-First Evaluation (RWC_EV1)
#' @param RWC_EV2 Relative water content 2 evaluation-First Evaluation (RWC_EV2)
#' @return Return the Increase Relative Water Content 1 (INRWC1)
#' @author Omar Benites
#' @details This function calculates the Increase Relative Water Content 1 (INRWC1)
#' @references Assessing Potato Clone for Drought Tolerance under Field Condition
#' @family drought, evaluation, potato
#' @export 
#' 
INRWC1 <- function(RWC_EV1,RWC_EV2){
  
  if(missing(RWC_EV1)){
    stop("Please enter the Relative water content 1 evaluation-First Evaluation 'RWC_EV1'")
  }
  
  if(missing(RWC_EV2)){
    stop("Please enter the Relative water content 2 evaluation-First Evaluation 'RWC_EV2'")
  }
  
  INRWC1 <- ((apply(cbind(RWC_EV2,-RWC_EV1),1,sbsum))/RWC_EV1)*100
  return(INRWC1) 
 
}
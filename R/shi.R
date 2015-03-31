#'Formula for calculating the Harvest sowing index - Survival (SHI)
#'
#'@param noph Number of plants harvested
#'@param nops Number of plants planted
#'@return A vector that contains the harvest sowing index - Survival 'shi'
#'@author Omar Benites 
#'@details Formula for calculating the harvest sowing index - Survival
#'@export
#'@keywords potato, agronomy,harvest,quantitative-continuous,percentage,Yield; Late Blight
#'@family potato,Yield; Late Blight
#'
shi <- function(noph,nops){
  
      if (missing(noph)){
        stop("Please enter the Number of Plants Harvested 'nph'")
      }
      if (missing(nops)){
        stop("Please enter the Number of Tubers Planted 'ntp'")
      }
      shi  <- (noph/nops)*100
      return(shi)
} 

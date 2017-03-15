#'Formula for calculating the Harvest sowing index - Survival (SHI)
#'
#'@param noph Number of plants harvested
#'@param nops Number of plants planted
#'@return A vector that contains the harvest sowing index - Survival 'shi'
#'@author Omar Benites, Raul Eyzaguirre 
#'@details Formula for calculating the harvest sowing index - Survival
#'@export
#'@keywords potato, agronomy,harvest,quantitative-continuous,percentage,Yield; Late Blight
#'@family potato,Yield; Late Blight
#'
#'
# https://github.com/omarbenites/sbformula/issues/1
#SHI: Cuidar division por 0 en NOPH/NOPS*100. Si NOPS es 0 la funcion debe devolver NA.

shi <- function(noph,nops){
  
#       if (missing(noph)){
#         stop("Please enter the Number of Plants Harvested 'nph'")
#       }
#       if (missing(nops)){
#         stop("Please enter the Number of Tubers Planted 'ntp'")
#       }
        if(sum(nops,na.rm = TRUE)==0){shi <- NA}
        else{
        shi  <- (noph/nops)*100
        }
        return(shi)
} 

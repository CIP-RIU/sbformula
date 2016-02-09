#'Formula for calculating the Total Number of Tubers/Plant (TNTPL)
#'
#'@param tntp Total Number of tubers/plot  
#'@param nph Number of plants harvested
#'@return Return the Total number of tubers/plant
#'@author Omar Benites
#'@details Formula for calculating the total number of tubers/plant
#'@references International Potato Center (CIP).2014: Procedures for Standard Evaluation and Data Management of Advanced Potato Clones
#'Module 2. Healthy Tuber Yield Trials International Cooperators Guide.
#'@keywords potato, agronomy,harvest,quantitative-continuous,yield, late blight,plant
#'@family potato,yield,late blight,harvest
#'@export 
#'

tntpl <- function(tntp,nph){
  
  if (missing(tntp)){
    stop("Please enter the Total Number of tubers/plot 'tntp'")
  }
  if (missing(nph)){
    stop("Please enter Number of plants harvested 'nph'")
  }
  tntpl <- tntp/nph
  return(tntpl)
} 

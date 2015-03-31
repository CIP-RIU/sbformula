#'Formula for calculating the Percentage of Plants Harvested (PPH)
#'
#'Formula to calculate the Percentage of Plants Harvested.
#'@param nph  Number of plants harvested
#'@param ntp  Number of tubers planted
#'@return A vector that contains the percentage of plants harvested 'pph'
#'@author Omar Benites 
#'@details Formula to calculate the percentage of plants harvested
#'@references International Potato Center (CIP).2014: Procedures for Standard Evaluation and Data Management of Advanced Potato Clones
#'Module 2. Healthy Tuber Yield Trials International Cooperators Guide.
#'@keywords potato, agronomy,harvest,quantitative-continuous,percentage,Yield; Late Blight
#'@family potato,Yield; Late Blight
#'@export
pph <- function(nph,ntp){
  
  if (missing(nph)){
    stop("Please enter the Number of Plants Harvested 'nph'")
  }
  if (missing(ntp)){
    stop("Please enter the Number of Tubers Planted 'ntp'")
  }
  pph  <- (nph*100)/ntp
  return(pph)
} 

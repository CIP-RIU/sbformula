#'Formula for calculating the Number of Marketable Tubers/Plant (NMTPL)
#'@param nmtp Number of marketable tubers/plot
#'@param nph Number of plants harvested
#'@return Return the 
#'@author Omar Benites
#'@details Formula for calculating the number marketable tubers/plant
#'@references International Potato Center (CIP).2014: Procedures for Standard Evaluation and Data Management of Advanced Potato Clones
#'Module 2. Healthy Tuber Yield Trials International Cooperators Guide.
#'@keywords potato, agronomy,harvest,quantitative-continuous,yield,late blight,plant
#'@family potato,yield,late blight,harvest,plant
#'@export 
nmtpl <- function(nmtp, nph = NA){
  
#      if (missing(nmtp)){
#        stop("Please enter the Number of marketable tubers/plot 'nmtp'")
#      }
     if (missing(nph)) nph <- NA 
     #if (nph == 0)     nph <- NA 
     nmtpl <- nmtp/nph
     return(nmtpl)
} 

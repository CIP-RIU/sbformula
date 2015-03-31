#'Formula for calculating the Oil Content Sample 2 Percentage (OCS2)
#'
#'@param iws2 Initial weight sample 2
#'@param fws2 Fresh weight of tuber sample 2
#'@return Return the Oil content sample 2 percentage
#'@author Omar Benites
#'@details Formula for calculating the Oil content sample 2 percentage
#'@references International Potato Center (CIP).2014: Procedures for Standard Evaluation and Data Management of Advanced Potato Clones
#'Module 2. Healthy Tuber Yield Trials International Cooperators Guide.
#'@keywords potato, agronomy,post-harvest,quantitative-continuous,yield,late blight,plant
#'@family potato,yield,late blight,post-harvest,plant
#'@export 
#'
ocs2 <- function(iws2,fws2){
  
  if (missing(iws2)){
    stop("Please enter the Initial weight sample 2 'iws2'")
  }
  if (missing(fws2)){
    stop("Please enter the Fresh weight of tuber sample 2 'fws2'")
  }
  ocs2  = 200 - ((iws2/fws2)*200)
  return(ocs2)
  
}
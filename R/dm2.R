#'Formula for calculating the Dry Matter Content Sample 2 (DM2)
#'
#'@param dwts2 Dry weight of tuber sample 2
#'@param fwts2 Fresh weight of tuber sample 2
#'@return Return the dry matter content sample 2
#'@author Omar Benites
#'@details Formula for calculating the dry matter content sample 2
#'@references International Potato Center (CIP).2014: Procedures for Standard Evaluation and Data Management of Advanced Potato Clones
#'Module 2. Healthy Tuber Yield Trials International Cooperators Guide.
#'@keywords potato, agronomy,post-harvest,quantitative-continuous,yield,late blight,plant
#'@family potato,yield,late blight,post-harvest,plant
#'@export 
#'
dm2 <- function(dwts2,fwts2 ){
  
  if (missing(dwts2)){
    stop("Please enter the dry weight of tuber sample 2 'dwts2'")
  }
  if (missing(fwts2)){
    stop("Please enter the fresh weight of tuber sample 2 'fwts2'")
  }
  dm2 <- (dwts2/fwts2)*100
  return(dm2)
  
}
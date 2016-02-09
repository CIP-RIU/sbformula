#'Formula for calculating the Dry Matter Content Sample 1 (DM1)
#'
#'@param dwts1 Dry weight of tuber sample 1
#'@param fwts1 Fresh weight of tuber sample 1
#'@return Return the dry matter content sample 1
#'@author Omar Benites
#'@details Formula for calculating the dry matter content sample 1
#'@references International Potato Center (CIP).2014: Procedures for Standard Evaluation and Data Management of Advanced Potato Clones
#'Module 2. Healthy Tuber Yield Trials International Cooperators Guide.
#'@keywords potato, agronomy,post-harvest,quantitative-continuous,yield,late blight,plant
#'@family potato,yield,late blight,post-harvest,plant
#'@export 
#'
dm1 <- function(dwts1,fwts1){
  
  if (missing(dwts1)){
    stop("Please enter the dry weight of tuber sample 1 'dwts1'")
  }
  if (missing(fwts1)){
    stop("Please enter the fresh weight of tuber sample 1 'fwts1'")
  }
  dm1 <- (dwts1/fwts1)*100
  return(dm1)
}

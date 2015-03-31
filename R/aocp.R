#'Formula for calculating the Average Oil Content Percentage (AOCP)
#'
#'@param ocs1 Oil content sample1 percentage
#'@param ocs2 Oil content sample1 percentage
#'@return Return the average oil content percentage 'aocp'
#'@author Omar Benites
#'@details Formula for calculating the average oil content percentage
#'@references International Potato Center (CIP).2014: Procedures for Standard Evaluation and Data Management of Advanced Potato Clones
#'Module 2. Healthy Tuber Yield Trials International Cooperators Guide.
#'@keywords potato, agronomy,post-harvest,quantitative-continuous,yield,late blight,plant
#'@family potato,yield,late blight,post-harvest,plant
#'@export 
#'
aocp <- function(ocs1,ocs2){
  if(!missing(ocs1) && missing(ocs2) ) {aocp <- ocs1}  
  if(missing(ocs1)  && !missing(ocs2)) {aocp <- ocs2}        
  if(!missing(ocs1) && !missing(ocs2)) {aocp <- apply(cbind(ocs1,ocs2),1,sbsum)/2}        
  return(aocp)
}

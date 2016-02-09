#'Formula for calculating the Specific gravity (SG)
#'
#'@param twa Tuber weight in air
#'@param tww Tuber weight in water
#'@return Return the specific gravity
#'@author Omar Benites
#'@details Formula for calculating the specific gravity
#'@references International Potato Center (CIP).2014: Procedures for Standard Evaluation and Data Management of Advanced Potato Clones
#'Module 2. Healthy Tuber Yield Trials International Cooperators Guide.
#'@keywords potato, agronomy,post-harvest,quantitative-continuous,yield,late blight,plant
#'@family potato,yield,late blight,post-harvest,plant
#'@export 
#'
sg <- function(twa,tww){
  
      if(missing(twa)){
        stop("Please enter the Tuber weight in air 'twa'")
      }     
      if(missing(tww)){
        stop("Please enter the Tuber weight in water 'tww'")
      }
      
      sg = twa/apply(cbind(twa,-tww),1,sbsum)
      return(sg)
}

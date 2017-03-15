#'Formula for calculating the Average Tuber Weight (ATW)
#'
#'@param ttwp Total tuber weight/plot
#'@param tntp Total number of tubers/plot
#'@return Return the average tuber weight 'atw'
#'@author Omar Benites
#'@details Formula for calculating average tuber weight
#'@references International Potato Center (CIP).2014: Procedures for Standard Evaluation and Data Management of Advanced Potato Clones
#'Module 2. Healthy Tuber Yield Trials International Cooperators Guide.
#'@keywords potato, agronomy,post-harvest,quantitative-continuous,yield,late blight,plant
#'@family potato,yield,late blight,post-harvest,plant
#'@export 
#'
atw <- function(ttwp,tntp){
      
      if (missing(ttwp)){
        #stop("Please enter the Total tuber weight/plot 'ttwp'")
        atw <- NA
      }
      if (missing(tntp)){
        #stop("Please enter the Total number of tubers/plot 'tntp'")
        atw <- NA
      }
      atw <- (ttwp/tntp)*1000
      return(atw)
}

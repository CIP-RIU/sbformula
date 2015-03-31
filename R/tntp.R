#'This function allows sum columns and avoid NA values at the same time
#'
#'@param x vector of measurements
#'@return A vector contains sum of values that avoid NA's
#'@keywords sum, NA's
#'
sbsum <- function(x){
  z <- x[!is.na(x)]; 
  ifelse(length(z), sum(z), NA)
} 

#'Formula for calculating the Total Number of Tubers/Plot (TNTP)
#'
#'@param nmtp Number marketable tubers/plot
#'@param nnomtp Number of non-marketable tubers/plot
#'@return A vector that contains the Total number of tubers/plot
#'@author Omar Benites 
#'@details Formula for calculating the Total number of tubers/plot TNTP
#'@keywords potato, agronomy,harvest,quantitative-continuous,yield, late blight
#'@references International Potato Center (CIP).2014: Procedures for Standard Evaluation and Data Management of Advanced Potato Clones
#'Module 2. Healthy Tuber Yield Trials International Cooperators Guide.
#'@family potato,yield,late blight,harvest
#'
#'@export
#'
tntp <- function(nmtp,nnomtp){
        
                 
        if(!missing(nmtp)>0 && missing(nnomtp)>0)  {tntp <- nmtp}
        if(missing(nmtp)>0 && !missing(nnomtp)>0) {tntp <- nnomtp}
        if(!missing(nnomtp)>0 && !missing(nmtp)>0) {tntp <- apply(cbind(nnomtp,nmtp),1,sbsum)}
               
        return(tntp)     
        }

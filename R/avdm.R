#'Formula for calculating the Average Dry Matter (AVDM)
#'
#'@param dm1 Dry matter content sample 1
#'@param dm2 Dry matter content sample 2
#'@return Return the average dry matter
#'@author Omar Benites
#'@details Formula for calculating the average dry matter
#'@references International Potato Center (CIP).2014: Procedures for Standard Evaluation and Data Management of Advanced Potato Clones
#'Module 2. Healthy Tuber Yield Trials International Cooperators Guide.
#'@keywords potato, agronomy,post-harvest,quantitative-continuous,yield,late blight,plant
#'@family potato,yield,late blight,post-harvest,plant
#'@export 
#'
avdm <- function(dm1=NULL, dm2 = NULL){
  
         if(is.null(dm1) && is.null(dm2))  {avdm <- NULL}  
         if(!is.null(dm1) && is.null(dm2))  {avdm <- dm1}  
         if(is.null(dm1)  && !is.null(dm2)) {avdm <- dm2}  
  
         if(!is.null(dm1) && !is.null(dm2)) {
           
           res <- dplyr::data_frame(dm1,dm2)
           avdm <- rowMeans(res, na.rm = TRUE)
           #avdm <- apply(cbind(dm1,dm2),1,sbsum)/2
         }        
         return(avdm)
}

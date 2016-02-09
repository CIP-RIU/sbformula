#'Formula for calculating the Marketable Tuber Weight/Plot (MTWP)
#'
#'@param mtwci Marketable tuber weight category I/plot 
#'@param mtwcii Marketable tuber weight category II/plot  
#'@return Return the Marketable Tuber Weight/Plot (MTWP)
#'@author Omar Benites
#'@details Formula for calculating the marketable tuber weight/plot
#'@references International Potato Center (CIP).2014: Procedures for Standard Evaluation and Data Management of Advanced Potato Clones
#'Module 2. Healthy Tuber Yield Trials International Cooperators Guide.
#'@keywords potato, agronomy,harvest,quantitative-continuous,yield,late blight,plot
#'@family potato,yield,late blight,harvest,plant.
#'@export 
#'
mtwp <- function(mtwci,mtwcii){
  
        if(!missing(mtwci) && missing(mtwcii) ) {mtwp <- mtwci}  
        if(missing(mtwci)  && !missing(mtwcii)) {mtwp <- mtwcii}        
        if(!missing(mtwci) && !missing(mtwcii)) {mtwp <- apply(cbind(mtwci,mtwcii),1,sbsum)}
        return(mtwp)
}

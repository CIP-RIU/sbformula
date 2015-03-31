#'Formula for calculating the Number of Marketable Tubers/Plot (NMTP)
#'
#'@param nmtci Number marketable tubers category I/plot 
#'@param nmtcii Number marketable tubers category II/plot
#'@param nmtciii Number marketable tubers category III/plot
#'@return Return the Number of Marketable Tubers/Plot
#'@author Omar Benites
#'@details Formula for calculating the Number Marketable Tubers/Plot NMTP
#'@references International Potato Center (CIP).2014: Procedures for Standard Evaluation and Data Management of Advanced Potato Clones
#'Module 2. Healthy Tuber Yield Trials International Cooperators Guide.
#'@keywords potato, agronomy,harvest,quantitative-continuous,yield, late blight,plot
#'@family potato,yield,Late blight,harvest
#'@export 
#'
nmtp <- function(nmtci,nmtcii,nmtciii){
        
           if(!missing(nmtci) && missing(nmtcii)  && missing(nmtciii))  {nmtp <- nmtci}  
           if(missing(nmtci)  && !missing(nmtcii) && missing(nmtciii))  {nmtp <- nmtcii}        
           if(missing(nmtci)  && missing(nmtcii)  && !missing(nmtciii)) {nmtp <- nmtciii}
           if(missing(nmtci)  && !missing(nmtcii) && !missing(nmtciii)) {nmtp <- apply(cbind(nmtcii,nmtciii),1,sbsum)}
           if(!missing(nmtci) && missing(nmtcii)  && !missing(nmtciii)) {nmtp <- apply(cbind(nmtci,nmtciii),1,sbsum)}
           if(!missing(nmtci) && !missing(nmtcii) && missing(nmtciii))  {nmtp <- apply(cbind(nmtci,nmtcii),1,sbsum)}
           if(!missing(nmtci) && !missing(nmtcii) && !missing(nmtciii)) {nmtp <- apply(cbind(nmtci,nmtcii,nmtciii),1,sbsum)}  
        return(nmtp)  
}

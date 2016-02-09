#'Formula for calculating the Total Number of Tubers/Plot (TNTP)
#'
#'@param nmtp Number marketable tubers/plot
#'@param nnomtp Number of non-marketable tubers/plot
#'@param nmtci Number marketable tubers comercial I
#'@param nmtcii Number marketable tubers comercial II
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
tntp <- function(nmtp,nnomtp,nmtci,nmtcii){
                 
        if(!missing(nmtp)>0 && missing(nnomtp)>0)    {tntp <- nmtp}
        if(missing(nmtp)>0 && !missing(nnomtp)>0)    {tntp <- nnomtp}
        if(!missing(nnomtp)>0 && !missing(nmtp)>0)   {tntp <- apply(cbind(nnomtp,nmtp),1,sbsum)}
    
        if(!missing(nmtci)>0 && !missing(nmtcii)>0)  {tntp <- apply(cbind(nmtci,nmtcii),1,sbsum)}
        if(!missing(nmtci)>0 && !missing(nnomtp)>0)  {tntp <- apply(cbind(nmtci,nnomtp),1,sbsum)}   
        if(!missing(nmtcii)>0 && !missing(nnomtp)>0) {tntp <- apply(cbind(nmtcii,nnomtp),1,sbsum)}      
        
        if(!missing(nmtci)>0 && !missing(nmtcii)>0 && !missing(nnomtp)>0) {tntp <- apply(cbind(nmtci,nmtcii,nnomtp),1,sbsum)}
            
        return(tntp)     
        }

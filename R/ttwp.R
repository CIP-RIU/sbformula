#'Formula for Total Tuber Weight/Plot (TTWP)
#'
#'@param mtwci Marketable tuber weight category I/plot 
#'@param mtwcii Marketable tuber weight category II/plot 
#'@param mtwp Markeable tuber weight/plot
#'@param nomtwp Non-marketable tuber weight/plot
#'@return Return the Total tuber weight/plot 'ttwp'
#'@author Omar Benites
#'@details This functions calculate Total tuber weight/plot
#'@references International Potato Center (CIP).2014: Procedures for Standard Evaluation and Data Management of Advanced Potato Clones
#'Module 2. Healthy Tuber Yield Trials International Cooperators Guide.
#'@keywords potato, agronomy,harvest,quantitative-continuous,yield,late blight,plot
#'@family potato,yield,late blight,harvest
#'@export 
#'
ttwp <- function(mtwci,mtwcii,mtwp,nomtwp){
  
  if(!missing(mtwci) && missing(mtwcii)  && missing(nomtwp))  {ttwp <- mtwci}  
  if(missing(mtwci)  && !missing(mtwcii) && missing(nomtwp))  {ttwp <- mtwcii}        
  if(missing(mtwci)  && missing(mtwcii)  && !missing(nomtwp)) {ttwp <- nomtwp}
  if(!missing(mtwp)  && missing(nomtwp)) {ttwp <- apply(cbind(mtwp),1,sbsum)}
  if(missing(mtwp)  && !missing(nomtwp)) {ttwp <- apply(cbind(nomtwp),1,sbsum)}
  if(!missing(mtwp)  && !missing(nomtwp)) {ttwp <- apply(cbind(mtwp,nomtwp),1,sbsum)}
  if(missing(mtwci)  && !missing(mtwcii) && !missing(nomtwp)) {ttwp <- apply(cbind(mtwcii,nomtwp),1,sbsum)}
  if(!missing(mtwci) && missing(mtwcii)  && !missing(nomtwp)) {ttwp <- apply(cbind(mtwci,nomtwp),1,sbsum)}
  if(!missing(mtwci) && !missing(mtwcii) && missing(nomtwp))  {ttwp <- apply(cbind(mtwci,mtwcii),1,sbsum)}
  if(!missing(mtwci) && !missing(mtwcii) && !missing(nomtwp)) {ttwp <- apply(cbind(mtwci,mtwcii,nomtwp),1,sbsum)}  
  
  return(ttwp)  
}

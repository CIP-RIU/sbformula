#'Formula for calculating the Total number of root counting per plot (TNRPLOT)
#'
#'@param nonc Number of non commercial roots
#'@param nocr Number of commercial roots
#'@return Return the number of roots per plant 'tnrplot'
#'@author Omar Benites, Raul Eyzaguirre
#'@details Formula for calculating total number of root counting per plot
#'@keywords sweetpotato, agronomy, harvest, quantitative-continuous, yield
#'@family sweetpotato, quality, harvest
#'@export 
#'

tnrplot <- function(nonc,nocr){

    if(!missing(nocr) &&  missing(nonc)) {tnrplot <- nocr}  
    if( missing(nocr) && !missing(nonc)) {tnrplot <- nonc}       
    if(!missing(nocr) && !missing(nonc)) {tnrplot <- apply(cbind(nocr,nonc), 1, sbsum)}

  return(tnrplot)
}

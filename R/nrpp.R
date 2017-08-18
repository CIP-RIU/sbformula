#'Formula for calculating the Number of Roots per Plant (NRPP)
#'
#'@param nonc Number of non commercial roots
#'@param nocr Number of commercial roots
#'@param noph Number of plants harvested
#'@return Return the number of roots per plant 'nrpp'
#'@author Omar Benites, Raul Eyzaguirre
#'@details Formula for calculating the number of roots per plant
#'@keywords sweetpotato, agronomy,harvest,quantitative-continuous,yield
#'@family sweetpotato,quality,harvest
#'@export 
#'
#NRPP: Tener cuidado cuando NOPH = 0. En esos casos NRPP debe ser NA y no Inf.
#https://github.com/omarbenites/sbformula/issues/1

nrpp <- function(nonc,nocr,noph){
#         if(missing(noph)){
#           stop("Please enter Number of plants harvested 'noph'")
#         }
        if(sum(noph,na.rm = TRUE)==0){ nrpp <- NA }
        else { 
        if(!missing(nocr) && missing(nonc))  {nrpp <- nocr/noph}  
        if(missing(nocr)  && !missing(nonc)) {nrpp <- nonc/noph}       
        if(!missing(nocr) && !missing(nonc)) {nrpp <- apply(cbind(nonc,nocr), 1, sbsum)/noph}
          
        }
        return(nrpp)
}

#'Formula for calculating the Root Foliage Ratio (RFR)
#'
#'@param crw Commercial root weight
#'@param ncrw Non commercial root weight
#'@param vw Vine weight
#'@param dmvd Dry weight of DMVD samples
#'@param dmvf Fresh weight vines for dry matter assessment
#'@return Return the root foliage ratio 'rfr' 
#'@author Omar Benites, Raul Eyzaguirre
#'@details Formula for calculating the root foliage ratio
#'@keywords sweetpotato, agronomy,harvest,quantitative-continuous,yield
#'@family sweetpotato,quality,harvest
#'@export 
#'
#This formula was improved using sbformula issue #1
# RFR: Para calcular esto necesariamente tiene que haber VW, DMVD, DMVF, DMD, y DMF. Luego de verificar que estas 5 variables no son ni NA ni 0, calcular:
# Igual a 0 si no hay ni CRW ni NCRW.
# Con CRW si solo hay CRW.
# Con NCRW si solo hay NCRW.
# Con CRW + NCRW si hay las dos.

rfr <- function(crw,ncrw,vw,dmvd,dmvf){
#     if(missing(crw)){
#        stop("Please enter the commercial root weight 'crw'")
#     }
#     if(missing(ncrw)){
#        stop("Please enter the non commercial root weight 'ncrw'")
#     }
#     if(missing(vw)){
#        stop("Please enter the vine weight 'vw'")
#     }    
#     if(missing(dmvd)){
#       stop("Please enter the dry weight of dmvd samples 'dmvd'")
#     } 
#     if(missing(dmvf)){
#       stop("Please enter the fresh weight vines for dry matter assessment 'dmvf'")
#     }   
    
    #  
    if(missing(crw)  && missing(ncrw))  {rfr <- 0}
    if(!missing(crw) && missing(ncrw))  {rfr <- crw*(dmvd/dmvf)/(vw*dmvd/dmvf)}
    if(missing(crw)  && !missing(ncrw)) {rfr <- ncrw*(dmvd/dmvf)/(vw*dmvd/dmvf)}       
    if(!missing(crw) && !missing(ncrw)) {rfr <- apply(cbind(crw, ncrw), 1, sbsum)*(dmvd/dmvf)/(vw*dmvd/dmvf)}
    return(rfr)
}

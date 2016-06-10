#' Plot Mother and Baby Score Global (SGLO)
#' 
#' @param sm Plot Mother/Baby Score by Women. (Number of corn beans)
#' @param swm Plot Mother/Baby Score by Men. (Number of corn kernels)
#' @author Omar Benites
#' @importFrom dplyr mutate
#' @description In Participatory Varietal Selection (PVS) is quite common calculate Plot Mother Score global which is the sum of 
#' Plot Mother/Baby Score by Men (sm) and Plot Mother/Baby Score by Women (sw). All these scores are provided by agricultors using the
#' number of corn kernels or beans for every genotype in the field.
#' @references Participatory Varietal Selection (PVS): Mother and baby trial design. International Potato Center. 2014
#' @examples  
#' \dontrun{
#' #Participatory data example
#' sdata <- data.frame(genotypes= c("cip1","cip2","cip3","cip4"), score_women= c(11,6,8,4), score_men = c(10,5,10,1))
#' #The global score : score_women + score_men
#' sglo  <-  sglo(sdata$score_women,sdata$score_men)
#' }
#'  

sglo <- function(sm = NULL, sw = NULL){
  
  if(is.factor(sm)) sm <- as.numeric(sm)
  if(is.factor(sw)) sw <- as.numeric(sw)
  
  sglo <- apply(cbind(sm,sw),1,sbsum)
  sglo
} 


#' Mother and Baby scores calculations to select best clones by Parcel.
#' 
#' @param data A data frame
#' @description With the group of farmers, all of the (previously) harvested fields are visited in order to select 
#' the best clones, taking into account the previously identified criteria. In the case of the Mother 
#' trial, the evaluation is performed for each repetition. 
#' @importFrom dplyr mutate
#' @export
#' 

calculate_form_sclones <- function(data){
  
  fieldbook <- data
  
  if("MSM" %in% names(fieldbook)){
        fieldbook <- mutate(fieldbook, 
                       MSGLO = sglo(sm = MSM, sw = NULL))
  }
  if("MSWM" %in% names(fieldbook)){
    fieldbook <- mutate(fieldbook, 
                        MSGLO = sglo(sm =  NULL, sw = MSWM))
  }
  if(all(c("MSM","MSWM") %in% names(fieldbook))){
    fieldbook <- mutate(fieldbook, 
                        MSGLO = sglo(sm =  MSM, sw = MSWM))
  }
  
  if("BSM" %in% names(fieldbook)){
    fieldbook <- mutate(fieldbook, 
                        BSGLO = sglo(sm =  BSM, sw = NULL))
  }

  if("BSWM" %in% names(fieldbook)){
    fieldbook <- mutate(fieldbook,
                        BSGLO = sglo(sm = NULL, sw = BSWM))
  }
  
  if(all(c("BSM","BSWM") %in% names(fieldbook))){
    fieldbook <- mutate(fieldbook, 
                        BSGLO = sglo(sm =  BSM, sw = BSWM))
  }
  fieldbook
  
}

#' Calculation of standard evaluation of yield variables under Participatory Varietal Selection Methodology
#' 
#' @param data A data frame with the fieldbook data
#' @param plot.size The plot size in square meters
#' @param plant.den Plant density 
#' @description During the harvest period, agricultors and researchers make direct observations or measurements by counting and weighing
#' yield traits in the field. This evaluation is carried out for each clone and plot (each repetition of the Mother trial / each Baby trial).
#' The amount of harvested plants and the number and weight of the tubers are recorded
#' @importFrom dplyr mutate
#' @export
#' 

calculate_form_harvest <- function(data ,plot.size=NA, plant.den=NA){
  
   fieldbook <- data
  
   if(length(fieldbook$NTP)>0 & length(fieldbook$NPH)>0 ) { 
    
      fieldbook <- mutate(fieldbook, 
                        PPH 	= pph(nph = NPH, ntp = NTP))	
    }		  
  
    if(length(fieldbook$NMTP)>0 & length(fieldbook$NPH)>0  ) {  
    
      fieldbook <- mutate(fieldbook, 
                        NMTPL	= nmtpl(nmtp =  NMTP, nph = NPH))
    }		
    
    if(length(fieldbook$NMTP)>0 & length(fieldbook$NNoMTP)>0 ) {  
      
      fieldbook <- mutate(fieldbook, 
                          TNTP = tntp(nmtp = NMTP, nnomtp = NNoMTP))
    }
    
    if(length(fieldbook$NMTP)>0){  
      fieldbook <- mutate(fieldbook, 
                          TNTP = tntp(nmtp = NMTP))
    }
    
    if(length(fieldbook$NNoMTP)>0 ) {  
      fieldbook <- mutate(fieldbook, 
                          TNTP = tntp(nnomtp =  NNoMTP))
    }
    
    if(length(fieldbook$TNTP)>0 & length(fieldbook$NPH)>0){  
      
      fieldbook <- mutate(fieldbook, 
                          TNTPL = tntpl(tntp = TNTP,nph = NPH))
    }	
   
    if(length(fieldbook$NoMTWP)>0 ) {  
        fieldbook <- mutate(fieldbook, 
                            TTWP = ttwp(nomtwp = NoMTWP))
    }
      
    if(length(fieldbook$MTWP)>0 ) {  
        
        fieldbook <- mutate(fieldbook, 
                            TTWP = ttwp(mtwp = MTWP))
    }
   
    if(length(fieldbook$MTWP)>0 & length(fieldbook$NoMTWP)>0 ) {  
      
      fieldbook <- mutate(fieldbook, 
                          TTWP = ttwp(mtwp = MTWP,nomtwp = NoMTWP))
    }
 
    if(length(fieldbook$TTWP)>0 & length(fieldbook$NPH)>0 ) {  
      
      fieldbook <- mutate(fieldbook, 
                          TTWPL	= ttwpl(ttwp = TTWP,nph = NPH))	
    }	
    
    if(length(fieldbook$TTWP)>0) {  
      	
      fieldbook <- mutate(fieldbook, 
                          TTYNA = ttyna(ttwp = TTWP, pls = plot.size))
    }
 
    if(length(fieldbook$TTWPL)>0  ) {  
      #TTYA	= (TTWPL*plant.den)/1000}) # GTDM-45 for m&b	1
      fieldbook <- mutate(fieldbook, 
                          TTYA	= ttya(ttwpl = TTWPL, plantden = plant.den))
    }
    
    if(length(fieldbook$MTWP)>0 & length(fieldbook$NPH)>0 ) {  
      
      fieldbook <- mutate(fieldbook, 
                          MTWPL = mtwpl(mtwp = MTWP, nph = NPH))
    } 

    if(length(fieldbook$MTWP)>0 ) {  
      #MTYNA	= (MTWP/plot.size)*10
      fieldbook <- mutate(fieldbook, 
                          MTYNA = mtyna(mtwp = MTWP, pls = plot.size))
    
    }	#GTDM-39	
    
    if(length(fieldbook$MTWPL)>0) {  
      #MTYA	= (MTWPL*plant.den)/1000
      fieldbook <- mutate(fieldbook, 
                          MTYA = mtya(mtwpl = MTWPL, plantden = plant.den))
    }#GTDM-39		  
    
    if(length(fieldbook$TTWP)>0 & length(fieldbook$TNTP)>0) {  
      #ATW		= (TTWP/TNTP)*1000
      fieldbook <- mutate(fieldbook, 
                          ATW = atw(ttwp = TTWP, tntp = TNTP))
     }
  
  fieldbook
  
}



#res <- load("D:/HIDAP_DOCUMENTATION_AND_EXAMPLES/FIELDBOOK EXAMPLES/PVS/pvs_example.rda")
#pvs_data <- pvs_example

#calculate_form_sclones()

# #if(!has.data(fb$MSGLO) & length(fb$MSM)>0 ) fb=within(fb,{  
#   MSGLO	= MSM
# #})	
# 
# #if(!has.data(fb$MSGLO) & length(fb$MSWM)>0) fb=within(fb,{  
#   MSGLO  = MSWM
# #})	
# 
# #if(!has.data(fb$MSGLO) & length(fb$MSWM)>0 & length(fb$MSM)>0 ) fb=within(fb,{  
#   MSGLO <- apply(cbind(MSM,MSWM),1,sbsum)
#  # })

#MSGLO	= MSM+MSWM	


# if(!has.data(fb$BSGLO) & length(fb$BSM)>0 & length(fb$BSWM)>0 ) fb=within(fb,{  
#   #BSGLO	= BSM+BSWM})	
#   BSGLO <- apply(cbind(BSM,BSWM),1,sbsum)
#   })
# 
# if(!has.data(fb$BSGLO) & length(fb$BSM)>0  ) fb=within(fb,{  
#   BSGLO  = BSM
#   })	
# 
# if(!has.data(fb$BSGLO) & length(fb$BSWM)>0 ) fb=within(fb,{  
#   BSGLO  = BSWM
#   })  





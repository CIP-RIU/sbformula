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


#' Percentages and scores by Plant Development
#' 
#' @param phase Phase or Stage of the plant development. There are 3 options: Flowering, Harvesting and Storage
#' @param data A data frame with the field data
#' 
scriteria_phase <- function (phase, data) {
  
  fieldbook <- as.data.frame(data)
  phase <- as.character(phase)
  fieldbook <- fieldbook[fieldbook[,"PHASE"]==phase,]
  
  #fphase$PMEN=round(100*fphase$SCMEN/sum(fphase$SCMEN,na.rm = TRUE),2)
  
  #if(is_contained("SCORE_MEN", set = fb_names)) fieldbook=within(fieldbook,{  
  #SCORE_MEN <- NULL
  #SCORE_WOMEN <- NULL
  #SCORE_GLOBAL <- NULL
  
  fieldbook <- mutate(fieldbook, 
                        PCT_MEN = round(100*SCORE_MEN/sum(SCORE_MEN,na.rm = TRUE) , 2)
  )
    
  fieldbook <- mutate(fieldbook, 
                        PCT_WOMAN = round(100*SCORE_WOMEN/sum(SCORE_WOMEN, na.rm = TRUE), 2)
  )
  
  fieldbook <- mutate(fieldbook, 
                        PCT_GLOBAL = round(100*SCORE_GLOBAL/sum(SCORE_GLOBAL, na.rm = TRUE), 2)
  )
    
  fieldbook

} 



#' Percetages and global scores by Selection Criteria
#' 
#' @param data A data frame
#' @description Gathering and Ranking of Criteria at Flowering, harvest and post-harvest stage. 
#' The group is gathered and the objectives of the trial and the evaluation are briefly explained.
#' The group is asked: What is it that you look for in a new variety of potato, taking into account
#' its foliage? In other words: When do we say that a variety is good, while evaluating only its
#' foliage?.
#' The greatest amount of possible answers are encouraged and a list is compiled of all the
#' criteria and reasons mentioned by the farmers.
#' For example:
#' (a) Resistance to late blight,
#' (b) Enough foliage to feed my cow,
#' (c) That the plant does not extend and touch the floor (size when erect),
#' (d) That it is resistant to moths,
#' (e) That the foliage is very green,
#' (f) That the plant shows vigor, etc. (according to what is indicated by the farmers).
#' Each of the mentioned criteria is written on a paper bag or on a cardboard tray. In other
#' words, if the farmers have identified 6 criteria, then we also have 6 bags or cardboard trays. 
#' 
#' @importFrom dplyr mutate
#' @export
#' 

calculate_form_scriteria <- function(data){

  fieldbook <- as.data.frame(data)
  fb_names <- names(fieldbook)
  
  
  if(is_contained("SCORE_MEN", set = fb_names)){  
                    fieldbook <- mutate(fieldbook, 
                                 SCORE_GLOBAL = sglo(sm = SCORE_MEN,sw = NULL))
  }
  
  if(is_contained("SCORE_WOMEN", set = fb_names)) {  
                     fieldbook <- mutate(fieldbook, 
                                  SCORE_GLOBAL  = sglo(sm = NULL, sw = SCORE_WOMEN))
  }
  
  if(is_contained("SCORE_MEN", "SCORE_WOMEN", set = fb_names)) {  
                      fieldbook <- mutate(fieldbook, 
                                   SCORE_GLOBAL  = sglo(sm = SCORE_MEN, sw = SCORE_WOMEN))
  }  
  
  #fieldbook <- rbind(crit.phase("Flowering",temp6),crit.phase("Harvest",temp6),crit.phase("Storage",temp6)) 
  fieldbook <- rbind(scriteria_phase("Flowering", fieldbook), scriteria_phase("Harvest", fieldbook), scriteria_phase("Storage", fieldbook))   
  
  #ToDo
  fieldbook 
  #fieldbook <- fieldbook[, fb_names]
  
  
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
  fb_names <- names(data)
  
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
  
  fieldbook <- fieldbook[,fb_names]
  return(fieldbook)
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

calculate_form_harvest <- function(data, plot_size=NA, plant_den=NA){
  
   fieldbook <- data
   fb_names <- names(fieldbook)
   #if(length(fieldbook$NTP)>0 & length(fieldbook$NPH)>0 ) { 
   if(is_contained("NTP","NPH", set = fb_names)) {    
   
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
                          TTYNA = ttyna(ttwp = TTWP, pls = plot_size))
    }
 
    if(length(fieldbook$TTWPL)>0  ) {  
      #TTYA	= (TTWPL*plant.den)/1000}) # GTDM-45 for m&b	1
      fieldbook <- mutate(fieldbook, 
                          TTYA	= ttya(ttwpl = TTWPL, plantden = plant_den))
    }
    
    if(length(fieldbook$MTWP)>0 & length(fieldbook$NPH)>0 ) {  
      
      fieldbook <- mutate(fieldbook, 
                          MTWPL = mtwpl(mtwp = MTWP, nph = NPH))
    } 

    if(length(fieldbook$MTWP)>0 ) {  
      #MTYNA	= (MTWP/plot.size)*10
      fieldbook <- mutate(fieldbook, 
                          MTYNA = mtyna(mtwp = MTWP, pls = plot_size))
    
    }	#GTDM-39	
    
    if(length(fieldbook$MTWPL)>0) {  
      #MTYA	= (MTWPL*plant.den)/1000
      fieldbook <- mutate(fieldbook, 
                          MTYA = mtya(mtwpl = MTWPL, plantden = plant_den))
    }#GTDM-39		  
    
    if(length(fieldbook$TTWP)>0 & length(fieldbook$TNTP)>0) {  
      #ATW		= (TTWP/TNTP)*1000
      fieldbook <- mutate(fieldbook, 
                          ATW = atw(ttwp = TTWP, tntp = TNTP))
     }
  
  #fieldbook <- fieldbook[,fb_names]
  fieldbook
}


#' Calculation of standard evaluation values in Dormancy Test in Potato Seed Tubers
#'
#' @param  data A dataframe with the fieldbook data
#' @description To determine the dormancy period and the initiation of sprouting of advanced
#' clones for its use as seed under storage conditions of diffuse light or rustic storage.
#' @importFrom dplyr mutate
#' @export
#'

calculate_form_dormancy <- function(data){
  
  fieldbook <- data
  fb_names <- names(fieldbook)
  
  if(is_contained("ITW","FTWSPT", set = fb_names)) {  
    
   fieldbook <- mutate(fieldbook, 
                         PWL_SPT = pct_if(inital_value = ITW, final_value = -FTWSPT))
  }
  
  if(is_contained("ITW","FTWRSPT", set = fb_names)) {  
   # PWL_RSPT = ((ITW-FTWRSPT)/ITW)*100
    
    fieldbook <- mutate(fieldbook, 
                         PWL_RSPT = pct_if(inital_value = ITW, final_value = -FTWRSPT))
    
    } 
  
  #fieldbook <- fieldbook[,fb_names]
  fieldbook

}

#' Percetages and Global scores by Selection Criteria at Post-Harvest Stage
#' 
#' @param data A dataframe with the fieldbook data
#' @description With the group of farmers, the storage facilities of each one of the families is visited in order to
#' select the best clones according to storage characteristics, taking into account the previously
#' identified criteria. If desirable, the clones / varieties in each of the 3 farmer stores can also be
#' ranked by different groups of evaluators
#' @export
#' 

calculate_form_postharvest <- function(data){

    fieldbook <- data
    fb_names <- names(fieldbook)
    
    
    if(is_contained("SCORE_MEN", set = fb_names)) {  
      fieldbook <- mutate(fieldbook, 
                          SCORE_GLOBAL = sglo(sm = SCORE_MEN,sw = NULL))
    }
    
    
    if(is_contained("SCORE_WOMEN", set = fb_names)) {  
      fieldbook <- mutate(fieldbook, 
                          SCORE_GLOBAL  = sglo(sm = NULL,sw = SCORE_WOMEN))
    }
    
    if(is_contained("SCORE_MEN", "SCORE_WOMEN", set = fb_names)) {  
      fieldbook <- mutate(fieldbook, 
                          SCORE_GLOBAL  = sglo(sm = SCORE_MEN, sw = SCORE_WOMEN))
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





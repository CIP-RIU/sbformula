#'Calculation of measured variables on potato and sweetpotato
# 
#' @param fb A data frame with the fieldbook data
#' @param plot.size The plot size in square meters
#' @param plant.den Plant density 
#' @return A data frame with the calculated variables from the fieldbook
#' @details This function allows to calculated several measured trait in one go.
#' @references Progress in developing a potato ontology for breeders. Reinhard Simon, Vilma Hualla, Raul Eyzaguirre, Raul Cordova, Robert O M Mwanga, Genoveva Rossel
#' Progress in developing a sweetpotato ontology for breeders. Reinhard Simon, Vilma Hualla, E. Salas, Rene Gomez, Raul Cordova, Stef de Haan
#' @details All the variables calculated
#' \itemize{
#'  \item	NTP	Number of tubers planted
#' 	\item	NPE	Number of plants emerged
#' 	\item	Plant_Unif	Plant uniformity
#' 	\item	PGH	Plant growth habit
#' 	\item	Plant_Vigor	Plant vigor
#' 	\item	FLOWERING	Degree of Flowering
#' 	\item	SE	Senescence
#' 	\item	PPE	Percentage plants emerged
#' 	\item	NPH	Number of plants harvested
#' 	\item	PPH	Percentage of plants harvested
#' 	\item	NMTCI	Number marketable tubers category I/plot  
#' 	\item	NMTCII	Number marketable tubers category II/plot 
#' 	\item	NNoMTP	Number of non-marketable tubers/plot
#' 	\item	NMTP	Number marketable tubers/plot
#' 	\item	NMTPL	Number marketable tubers/plant
#' 	\item	TNTP	Total number of tubers/plot
#' 	\item	TNTPL	Total number of tuber/plant
#' 	\item	MTWCI	Marketable tuber weight category I /plot 
#' 	\item	MTWCII	Marketable tuber weight category II/plot 
#' 	\item	NoMTWP	Non-marketable tuber weight/plot
#' 	\item	TTWP	Total tuber weight/plot
#' 	\item	TTWPL	Total tuber weight/plant
#' 	\item	TTYNA	Total tuber yield no adjusted
#' 	\item	TTYA	Total tuber yield adjusted
#' 	\item	MTWP	Marketable tuber weight/plot
#' 	\item	MTWPL	Marketable tuber weight/plant
#' 	\item	MTYA	Marketable tuber yield adjusted
#' 	\item	MTYNA	Marketable tuber yield no adjusted
#' 	\item	Num_Stolon	Number of Stolons
#' 	\item	Leng_Stolon	Length of the stolon
#' 	\item	Tuber_Apper	Tuber Appearance
#' 	\item	Tub_Unif	Tuber Uniformity 
#' 	\item	Tub_Size	Tuber Size
#' 	\item	ATW	Average tuber weight
#' 	\item	ATMW	Average marketable tuber weight
#' 	\item	FWTS1	Fresh weight of tuber sample 1
#' 	\item	FWTS2	Fresh weight of tuber sample 2
#' 	\item	DWTS1	Dry weight of tuber sample 1
#' 	\item	DWTS2	Dry weight of tuber sample 2
#' 	\item	DM1	Dry Matter Content Sample1
#' 	\item	DM2	Dry Matter Content Sample2
#' 	\item	AVDM	Average Dry Matter
#' 	\item	TWA	Tuber weight in air
#' 	\item	TWW	Tuber weight in water
#' 	\item	SG	Specific Gravity
#' 	\item	IWS1	Initial weight sample 1
#' 	\item	IWS2	Initial weight sample 2
#' 	\item	FWS1	Final weight sample 1
#' 	\item	FWS2	Final weight sample 2
#' 	\item	OCS1	Oil Content Sample1 Percentage
#' 	\item	OCS2	Oil Content Sample2 Percentage
#' 	\item	AOCP	Oil Absorption Rate
#' 	\item	Chip_Color	Chipping color
#'}
#'@export

sbcalculate <- function(fb,plot.size=NA,plant.den=NA){   
  #potato variables
  
  fb_names <- names(fb)
  
  #BEGIN PPE
  #if(length(fb$NPE)>0 & length(fb$NTP)>0 ) fb=within(fb,{  
  if(is_contained("NPE", "NTP", set = fb_names)) fb=within(fb,{
    PPE <- sbformula::ppe(npe = NPE, ntp = NTP)
  })  		
  #END PPE
  
  #BEIGN PPH
  #if(length(fb$NTP)>0 & length(fb$NPH)>0 ) fb=within(fb,{	
  if(is_contained("NPH", "NTP", set = fb_names)) fb=within(fb,{
    PPH <- sbformula::pph(nph = NPH, ntp = NTP)
  })			
  #END PPH
  
  #if(length(fb$NPE)>0 & length(fb$NTP)>0 ) fb=within(fb,{  
  if(is_contained("NPE", "NTP", set = fb_names)) fb=within(fb,{
    PPE <- sbformula::ppe(npe = NPE, ntp = NTP)  
  })  		  
  
  #BEGIN TNTP
  #if( length(fb$NNoMTP)>0 &  length(fb$NMTP)>0) fb=within(fb,{ 
  if(is_contained("NNoMTP", "NMTP", set = fb_names)) fb=within(fb,{
    TNTP <- sbformula::tntp(nnomtp = NNoMTP, nmtp = NMTP)
  })
  
  #if(length(fb$NMTCI)>0 & length(fb$NMTCII)>0) fb=within(fb,{
  if(is_contained("NMTCI", "NMTCII", set = fb_names)) fb=within(fb,{
    #temp <- sbformula::nmtp(nmtci = NMTCI, nmtcii = NMTCII )
    #TNTP <- temp
    TNTP <- sbformula::tntp(nmtci = NMTCI, nmtcii = NMTCII)
  })
  
  #if(length(fb$NMTCI)>0 & length(fb$NNoMTP)>0 ) fb=within(fb,{  
  if(is_contained("NMTCI", "NNoMTP", set = fb_names)) fb=within(fb,{
#     temp <- sbformula::nmtp(nmtci = NMTCI)
#     TNTP <- sbformula::tntp(nmtp = temp,nnomtp = NNoMTP)
      TNTP <- sbformula::tntp(nmtci= NMTCI,nnomtp = NNoMTP) 
  })			
  
  #if(length(fb$NMTCII)>0 & length(fb$NNoMTP)>0 ) fb=within(fb,{  
  if(is_contained("NMTCII", "NNoMTP", set = fb_names)) fb=within(fb,{
#     temp <- sbformula::nmtp(nmtcii = NMTCII)
#     TNTP <- sbformula::tntp(nmtp = temp,nnomtp = NNoMTP)
      TNTP <- sbformula::tntp(nmtcii = NMTCII, nnomtp = NNoMTP)
  })
  
  #if(length(fb$NMTCI)>0 & length(fb$NMTCII)>0 & length(fb$NNoMTP)>0 ) fb=within(fb,{  
  if(is_contained("NMTCI", "NMTCII", "NNoMTP", set = fb_names)) fb=within(fb,{
#     temp <- sbformula::nmtp(nmtci = NMTCI,nmtcii = NMTCII)
#     TNTP <- sbformula::tntp(nmtp = temp,nnomtp = NNoMTP) 
      TNTP <- sbformula::tntp(nmtci = NMTCI, nmtcii = NMTCII, nnomtp = NNoMTP)   
  })
  ###END TNTP  
  
  #BEGIN TNTPL
  #if(length(fb$TNTP)>0 & length(fb$NPH)>0  ) fb=within(fb,{	
  if(is_contained("TNTP", "NPH", set = fb_names)) fb=within(fb,{
    TNTPL <- sbformula::tntpl(tntp = TNTP,nph = NPH)
  })
  #END TNTPL
  
  #BEGIN NMTP
  #if(length(fb$NMTCI)>0 & length(fb$NMTCII)>0) fb=within(fb,{	
  if(is_contained("NMTCI", "NMTCII", set = fb_names)) fb=within(fb,{
    NMTP <- sbformula::nmtp(nmtci = NMTCI,nmtcii = NMTCII) #original
  })	
  #END NMTP
  
  #BEGIN NMTPL
  #if(length(fb$NMTP)>0 & length(fb$NPH)>0  ) fb=within(fb,{	
  if(is_contained("NMTP", "NPH", set = fb_names)) fb=within(fb,{
    NMTPL <- sbformula::nmtpl(nmtp = NMTP, nph = NPH)	
  })			
  #END NMTPL
  
  ###BEGIN TTWP  
  #if(length(fb$MTWP)>0 & length(fb$NoMTWP)>0 ) fb=within(fb,{ 
  if(is_contained("MTWP", "NoMTWP", set = fb_names)) fb=within(fb,{
    TTWP <- sbformula::ttwp(mtwp= MTWP, nomtwp = NoMTWP) 
  })
  
  #if(length(fb$MTWCI)>0 & length(fb$MTWCII)>0) fb=within(fb,{   
  if(is_contained("MTWCI", "MTWCII", set = fb_names)) fb=within(fb,{
    TTWP <- sbformula::ttwp(mtwci = MTWCI, mtwcii = MTWCII)
  })
  
  #if(length(fb$MTWCI)>0 & length(fb$MTWCII)>0 & length(fb$NoMTWP)>0 ) fb=within(fb,{  
  if(is_contained("MTWCI", "MTWCII", "NoMTWP", set = fb_names)) fb=within(fb,{
    #temp <- sbformula::nmtp(nmtci = MTWCI,nmtcii = MTWCII)
    TTWP <- sbformula::ttwp(mtwci=MTWCI, mtwcii=MTWCII, nomtwp = NoMTWP)
  })
  ##end TTWP
  
  #BEGIN TTWPL
  #if(length(fb$TTWP)>0 & length(fb$NPH)>0 ) fb=within(fb,{	
  if(is_contained("TTWP", "NPH", set = fb_names)) fb=within(fb,{
    TTWPL <- sbformula::ttwpl(ttwp = TTWP, nph = NPH)
  })
  #END TTWPL
  
  #BEGIN TTYNA
  #if(length(fb$TTWP)>0) fb=within(fb,{	
  if(is_contained("TTWP",set = fb_names)) fb=within(fb,{
    TTYNA <- sbformula::ttyna(ttwp = TTWP,pls = plot.size)	#GTDM-39	
  })
  #END TTYNA
  
  
  #BEGIN TTYA
  #if(length(fb$TTWPL)>0 &  length(plant.den)) fb=within(fb,{	
  if(is_contained("TTWPL",set = fb_names)) fb=within(fb,{
    TTYA <- sbformula::ttya(ttwpl = TTWPL,plantden = plant.den) # GTDM-45*			
  })
  #END TTYA
  
  #BEGIN MTWP
  #if(length(fb$MTWCI)>0 & length(fb$MTWCII)>0 ) fb=within(fb,{  
  if(is_contained("MTWCI", "MTWCII", set = fb_names)) fb=within(fb,{
    MTWP <-  sbformula::mtwp(mtwci = MTWCI,mtwcii = MTWCII)
  })	
  #END MTWP
  
  #BEGIN MTWPL
  #if(length(fb$MTWP)>0 & length(fb$NPH)>0 ) fb=within(fb,{
  if(is_contained("MTWP","NPH", set = fb_names)) fb=within(fb,{
    MTWPL <- sbformula::mtwpl(mtwp = MTWP,nph = NPH)
  #})
  })
  #END MTWPL
  
  #BEGIN MTYNA
  if(is_contained("MTWP", set = fb_names)) fb=within(fb,{	
    MTYNA <- sbformula::mtyna(mtwp = MTWP,pls = plot.size)
  })	#GTDM-39
  #END MTYNA
  
  #BEGIN MTYA
  if(is_contained("MTWPL", "NMTP", set = fb_names)) fb=within(fb,{	
    MTYA <- sbformula::mtya(mtwpl = MTWPL,plantden = plant.den)
  })#GTDM-39		
  #END MTYA
  
  
  if(is_contained("TTWP", "TNTP", set = fb_names)) fb=within(fb,{	
    ATW <- sbformula::atw(ttwp = TTWP,tntp = TNTP)
  })	
  
  if(is_contained("MTWP", "NMTP",set = fb_names))  fb=within(fb,{	
    ATMW <- sbformula::atmw(mtwp = MTWP,nmtp = NMTP)
  })	
  
  if(is_contained("DWTS1", "FWTS1",set = fb_names)) fb=within(fb,{	
    DM1 <- sbformula::dm1(DWTS1,FWTS1)
  })	
  
  #if(length(fb$DWTS2)>0 & length(fb$FWTS2)>0) fb=within(fb,{	
  if(is_contained("DWTS2", "FWTS2",set = fb_names)) fb=within(fb,{	
    DM2 <- sbformula::dm2(dwts2 = DWTS2,fwts2 = FWTS2)
  })	
  
  #if(length(fb$DM1)>0) fb=within(fb,{  
  if(is_contained("DM1",set = fb_names)) fb=within(fb,{
    AVDM <- sbformula::avdm(dm1 = DM1)
  })
  
  if(is_contained("DM2",set = fb_names)) fb=within(fb,{
    AVDM <- sbformula::avdm(dm1 = DM2)
  })
  
  #if(length(fb$DM1)>0 & length(fb$DM2)>0) fb=within(fb,{	
  if(is_contained("DM1", "DM2",set = fb_names)) fb=within(fb,{
    AVDM <- sbformula::avdm(dm1 = DM1,dm2 = DM2)
  })
  ##
  
  #if(length(fb$TWA)>0 & length(fb$TWA)>0 & length(fb$TWW)>0) fb=within(fb,{  
  if(is_contained("TWA", "TWW",set = fb_names)) fb=within(fb,{
    SG <- sbformula::sg(twa = TWA, tww = TWW)
  })
  ##
  
  #if(length(fb$IWS1)>0 & length(fb$FWS1)>0 ) fb=within(fb,{	
  if(is_contained("IWS1", "FWS1", set = fb_names)) fb=within(fb,{
    OCS1 <- sbformula::ocs1(iws1 = IWS1, fws1 = FWS1)
  })	
  
  #if(length(fb$IWS2)>0 & length(fb$FWS2)>0 ) fb=within(fb,{	
  if(is_contained("IWS2", "FWS2", set = fb_names)) fb=within(fb,{
    OCS2 <- sbformula::ocs2(iws2 = IWS2, fws2 = FWS2)
  })			
  
  #if(length(fb$OCS1)>0 & length(fb$OCS2)>0 ) fb=within(fb,{	
  if(is_contained("OCS1", "OCS2", set = fb_names)) fb=within(fb,{
    AOCP <- sbformula::aocp(ocs1 = OCS1,ocs2 = OCS2)
  })			
  

  # Bulking Formulas --------------------------------------------------------
  
  if(is_contained("MTWP_1HD", "NMTP_1HD",set = fb_names))  fb=within(fb,{	
    ATMW_1HD <- sbformula::atmw(mtwp = MTWP_1HD, nmtp = NMTP_1HD)
  })	

  if(is_contained("MTWP_2HD", "NMTP_2HD",set = fb_names))  fb=within(fb,{	
    ATMW_2HD <- sbformula::atmw(mtwp = MTWP_2HD, nmtp = NMTP_2HD)
  })	
  
  if(is_contained("MTWP_3HD", "NMTP_3HD",set = fb_names))  fb=within(fb,{	
    ATMW_3HD <- sbformula::atmw(mtwp = MTWP_3HD, nmtp = NMTP_3HD)
  })	
  
  if(is_contained("NoMTWP_1HD", "NNoMTP_1HD",set = fb_names))  fb=within(fb,{	
    ATNoMW_1HD <- sbformula::atnomw(nomtwp= NoMTWP_1HD ,nnomtp = NNoMTP_1HD)
  })	
  
  if(is_contained("NoMTWP_2HD", "NNoMTP_2HD",set = fb_names))  fb=within(fb,{	
    ATNoMW_2HD <- sbformula::atnomw(nomtwp = NoMTWP_2HD , nnomtp= NNoMTP_2HD)
  })	
  
  if(is_contained("NoMTWP_3HD", "NNoMTP_3HD", set = fb_names))  fb=within(fb,{	
    ATNoMW_3HD <- sbformula::atnomw( nomtwp= NoMTWP_3HD , nnomtp= NNoMTP_3HD)
  })	

  ## first harvest date
  if(is_contained("TWA1_S1", "TWW1_S1",set = fb_names)) fb=within(fb,{
    SGS1_1HD <- sbformula::sg(twa = TWA1_S1, tww = TWW1_S1)
  })
  
  if(is_contained("TWA1_S2", "TWW1_S2",set = fb_names)) fb=within(fb,{
    SGS2_1HD <- sbformula::sg(twa = TWA1_S2, tww = TWW1_S2)
  })
  
  ## Second harvest date
  if(is_contained("TWA2_S1", "TWW2_S1",set = fb_names)) fb=within(fb,{
    SGS1_2HD <- sbformula::sg(twa = TWA2_S1, tww = TWW2_S1)
  })
  
  if(is_contained("TWA2_S2", "TWW2_S2",set = fb_names)) fb=within(fb,{
    SGS2_2HD <- sbformula::sg(twa = TWA2_S2, tww = TWW2_S2)
  })

  ## Third harvest date
  if(is_contained("TWA3_S1", "TWW3_S1",set = fb_names)) fb=within(fb,{
    SGS1_3HD <- sbformula::sg(twa = TWA3_S1, tww = TWW3_S1)
  })
  
  if(is_contained("TWA3_S2", "TWA3_S2",set = fb_names)) fb=within(fb,{
    SGS2_3HD <- sbformula::sg(twa = TWA3_S2, tww = TWW3_S2)
  })
  
  if(is_contained("SGS1_1HD","SGS2_1HD", set = fb_names)) fb=within(fb,{
    SG_1HD <- sbformula::av_sg(SGS1_1HD	,SGS2_1HD)
  })
  
  if(is_contained("SGS1_2HD",	"SGS2_2HD", set = fb_names)) fb=within(fb,{
    SG_2HD <- sbformula::av_sg(SGS1_2HD, SGS2_2HD)
  })
  
  if(is_contained("SGS1_3HD",	"SGS2_3HD", set = fb_names)) fb=within(fb,{
    SG_3HD <- sbformula::av_sg(SGS1_3HD,SGS2_3HD)
  })
  
  #we have to complete specific formulas for bulking
  
  
  # Dormancy Formulas --------------------------------------------------------
  if(is_contained("LGLATSP1","LGLATSP2", "AVLGLATSP", set = fb_names))  fb=within(fb,{	
    AVLGLATSP <- sbformula::avlglatsp(LGLATSP1, LGLATSP2)
  })	
  
  if(is_contained("LGLATSP1","LGLATSP2","LGLATSP3", "AVLGLATSP",set = fb_names))  fb=within(fb,{	
    AVLGLATSP <- sbformula::avlglatsp(LGLATSP1, LGLATSP2, LGLATSP3)
  })	
  
  if(is_contained("LGLATSP1","LGLATSP2","LGLATSP3","LGLATSP4","AVLGLATSP", set = fb_names))  fb=within(fb,{	
    AVLGLATSP <- sbformula::avlglatsp(LGLATSP1, LGLATSP2, LGLATSP3, LGLATSP4)
  })	
  
  if(is_contained("LGLATSP1","LGLATSP2","LGLATSP3","LGLATSP4","LGLATSP5","AVLGLATSP",set = fb_names))  fb=within(fb,{	
    AVLGLATSP <- sbformula::avlglatsp(LGLATSP1, LGLATSP2, LGLATSP3, LGLATSP4, LGLATSP5)
  })	
  
  if(is_contained("LGLATSP1","LGLATSP2","LGLATSP3","LGLATSP4","LGLATSP5","LGLATSP6", "AVLGLATSP", set = fb_names))  fb=within(fb,{	
    AVLGLATSP <- sbformula::avlglatsp(LGLATSP1, LGLATSP2, LGLATSP3, LGLATSP4, LGLATSP5, LGLATSP6)
  })	

  if(is_contained("THLSP1", "THLSP2", "AVTHSP", set = fb_names))  fb=within(fb,{	
      AVTHSP <- sbformula::avthsp(THLSP1, THLSP2)
  })
  
  if(is_contained("ITW", "FTW", "PW_SPT", set = fb_names))  fb=within(fb,{	
    PW_SPT <- sbformula::pw_spt(ITW, FTW)
  })
  
  if(is_contained("ITW", "INTW", "PW_USPT", set = fb_names))  fb=within(fb,{	
    PW_USPT <- sbformula::pw_uspt(ITW, INTW)
  })
  
  #Droguth Variables
  
  if(length(fb$PLAHE_EV1)>0 & length(fb$PLAHE_EV2)>0) fb=within(fb,{
    INPLAHE1= (apply(cbind(PLAHE_EV2,-PLAHE_EV1),1,sbsum)/PLAHE_EV1)*100
  })
  
  if(length(fb$PLAHE_EV3)>0 & length(fb$PLAHE_EV2)>0) fb=within(fb,{
    INPLAHE2 = (apply(cbind(PLAHE_EV3,-PLAHE_EV2),1,sbsum)/PLAHE_EV2)*100
  })
  
  if(length(fb$PLAHE_EV4)>0 & length(fb$PLAHE_EV3)>0) fb=within(fb,{
    INPLAHE3 = (apply(cbind(PLAHE_EV4,-PLAHE_EV3),1,sbsum)/PLAHE_EV3)*100
  })
  
  if(length(fb$SNPP_EV2)>0 & length(fb$SNPP_EV1)>0) fb=within(fb,{
    INSNPP1 = (apply(cbind(SNPP_EV2,-SNPP_EV1),1,sbsum)/SNPP_EV1)*100
  }) 
  
  if(length(fb$SNPP_EV3)>0 & length(fb$SNPP_EV2)>0) fb=within(fb,{
    INSNPP2 = (apply(cbind(SNPP_EV3,-SNPP_EV2),1,sbsum)/SNPP_EV2)*100
  })
  
  if(length(fb$SD_EV2)>0 & length(fb$SD_EV1)>0) fb=within(fb,{
    INSD1 = (apply(cbind(SD_EV2,-SD_EV1),1,sbsum)/SD_EV1)*100
  })
  
  if(length(fb$SD_EV3)>0 & length(fb$SD_EV2)>0) fb=within(fb,{
    INSD2 = (apply(cbind(SD_EV3,-SD_EV2),1,sbsum)/SD_EV2)*100
  })
  
  if(length(fb$ChC2)>0 & length(fb$ChC1)>0) fb=within(fb,{
    INChC1 = (apply(cbind(ChC2,-ChC1),1,sbsum)/ChC1)*100
  })
  
  if(length(fb$ChC3)>0 & length(fb$ChC2)>0) fb=within(fb,{
    INChC2 = (apply(cbind(ChC3,-ChC2),1,sbsum)/ChC2)*100
  })
  
  if(length(fb$ChC4)>0 & length(fb$ChC3)>0) fb=within(fb,{
    INChC3 = (apply(cbind(ChC4,-ChC3),1,sbsum)/ChC3)*100
  })
  
  if(length(fb$ChC5)>0 & length(fb$ChC4)>0) fb=within(fb,{
    INChC4 = (apply(cbind(ChC5,-ChC4),1,sbsum)/ChC4)*100
  })
  
  if(length(fb$Leaflet_FW1)>0 & length(fb$Leaflet_DW1)>0 & length(fb$Leaflet_TW1)>0) fb=within(fb,{
    RWC_EV1 = (apply(cbind(Leaflet_FW1, Leaflet_DW1),1,sbsum)/ apply(cbind(Leaflet_TW1,-Leaflet_DW1),1,sbsum))*100
  }) #Leaflet_TW1
  
  if(length(fb$Leaflet_FW2)>0 & length(fb$Leaflet_DW2)>0 & length(fb$Leaflet_TW2)>0) fb=within(fb,{
    RWC_EV2 = (apply(cbind(Leaflet_FW2, Leaflet_DW2),1,sbsum)/ apply(cbind(Leaflet_TW2,-Leaflet_DW2),1,sbsum))*100
  })#Leaflet_TW2
  
  if(length(fb$Leaflet_FW3)>0 & length(fb$Leaflet_DW3)>0 & length(fb$Leaflet_TW3)>0) fb=within(fb,{
    RWC_EV3 = (apply(cbind(Leaflet_FW3, Leaflet_DW3),1,sbsum)/ apply(cbind(Leaflet_TW3,-Leaflet_DW3),1,sbsum))*100
  })#Leaflet_TW3
  
  if(length(fb$RWC_EV2)>0 & length(fb$RWC_EV1)>0) fb=within(fb,{
    INRWC1 = ((apply(cbind(RWC_EV2,-RWC_EV1),1,sbsum))/RWC_EV1)*100
  })
  
  if(length(fb$RWC_EV3)>0 & length(fb$RWC_EV2)>0) fb=within(fb,{
    INRWC2 = ((apply(cbind(RWC_EV3,-RWC_EV2),1,sbsum))/RWC_EV2)*100
  })
  
  if(length(fb$LFA_Ev1)>0 & length(fb$Leaflet_DW1)>0) fb=within(fb,{
    SLA1=(LFA_Ev1/Leaflet_DW1)
  })
  
  if(length(fb$LFA_Ev2)>0 & length(fb$Leaflet_DW2)>0) fb=within(fb,{
    SLA2= (LFA_Ev2/Leaflet_DW2)
  })
  
  if(length(fb$LFA_Ev3)>0 & length(fb$Leaflet_DW3)>0) fb=within(fb,{
    SLA3= (LFA_Ev3/Leaflet_DW3)
  })
  
  if(length(fb$LEAFSD1)>0 & length(fb$LEAFSD2)>0 & length(fb$LEAFSD3)>0) fb=within(fb,{
    AV_LEAFSD = apply(cbind(LEAFSD1,LEAFSD2,LEAFSD3),1,mean, na.rm=T)
  })
  
  if(length(fb$LFW)>0 & length(fb$TFW)>0 & length(fb$RFW)>0 ) fb=within(fb,{
    TBFW = apply(cbind(LFW,TFW,RFW),1,sbsum)  
  })
  
  if(length(fb$LFW)>0 & length(fb$SFW)>0 & length(fb$TFW)>0 & length(fb$RFW)>0) fb=within(fb,{
    TBFW= apply(cbind(LFW,SFW,TFW,RFW),1,sbsum)
  })
  
  if(length(fb$LFW)>0 & length(fb$SFW)>0 & length(fb$TFW)>0 & length(fb$RFW)>0 & length(fb$STLFW)>0 ) fb=within(fb,{
    TBFW= apply(cbind(LFW, SFW, TFW, RFW, STLFW),1,sbsum)
  })
  
  if(length(fb$LDW)>0 & length(fb$TDW)>0 & length(fb$RDW)>0) fb=within(fb,{
    TBDW= apply(cbind(LDW, TDW, RDW),1,sbsum)
  })
  
  if(length(fb$LDW)>0 & length(fb$SDW)>0 & length(fb$TDW)>0 & length(fb$RDW)>0) fb=within(fb,{
    TBDW= apply(cbind(LDW, SDW, TDW, RDW),1,sbsum)
  })
  
  if(length(fb$LDW)>0 & length(fb$SDW)>0 & length(fb$TDW)>0 & length(fb$RDW)>0 & length(fb$STLDW)>0) fb=within(fb,{
    TBDW= apply(cbind(LDW, SDW, TDW, RDW, STLDW),1,sbsum)
  })
  
  if(length(fb$TFW)>0 & length(fb$TBFW)>0) fb=within(fb,{
    HI_FW= (TFW/TBFW)*100})
  
  if(length(fb$TDW)>0 & length(fb$TBDW)>0) fb=within(fb,{
    HI_DW= (TDW/TBDW)*100
  })
  
  if(length(fb$Ta)>0  & length(fb$Tc_EV1)>0) fb=within(fb,{
    CTD= apply(cbind(Ta,-Tc_EV1),1,sbsum)
  })
  
  if(length(fb$Ta)>0  & length(fb$Tc_EV2)>0) fb=within(fb,{
    CTD= apply(cbind(Ta,-Tc_EV2),1,sbsum)
  })
  

  
  ###############################################################################
  # SWEETPOTATO VARIABLES ---------------------------------------------------
  
  #if(length(fb$CRW)>0 & length(fb$NCRW)>0 ) fb=within(fb,{	
  if(is_contained("CRW", "NCRW", "TRW", set = fb_names))  fb=within(fb,{	
    TRW	 <-  sbformula::trw(crw = CRW,ncrw = NCRW)
  })
  
  #if(length(fb$CRW)>0) fb=within(fb,{	
  if(is_contained("CRW", "CYTHA", set = fb_names))  fb=within(fb,{	
    CYTHA	 <-  sbformula::cytha(CRW, pls = plot.size)
  })
  
  #if(length(fb$CRW)>0 & length(fb$NCRW)>0) fb=within(fb,{	
  if(is_contained("CRW", "NCRW", "RYTHA", set = fb_names))  fb=within(fb,{	
    RYTHA	= apply(cbind(CRW, NCRW), 1, sum, na.rm=T)*10/plot.size #this formulas is pending
  })
  
  #if(length(fb$CRW)>0 & length(fb$NOCR)>0) fb=within(fb,{	
  if(is_contained("CRW", "NOCR", "ACRW", set = fb_names))  fb=within(fb,{	
    ACRW	= sbformula::acrw(crw = CRW, nocr = NOCR)
  })
  
  
  if(is_contained("NONC", "NOCR", "TNRPLOT" ,set = fb_names))  fb=within(fb,{	
    TNRPLOT	= sbformula::tnrplot(nonc = NONC, nocr = NOCR)
  })
  
  
  #if(length(fb$NOCR)>0 & length(fb$NONC)>0 & length(fb$NOPH)>0) fb=within(fb,{	
  if(is_contained("NONC", "NOCR", "NOPH", "NRPP",set = fb_names))  fb=within(fb,{	
    NRPP	= sbformula::nrpp(nonc = NONC, nocr = NOCR, noph = NOPH)
  })
  

  #if(length(fb$CRW)>0 & length(fb$NCRW)>0 & length(fb$NOPH)>0) fb=within(fb,{	
  if(is_contained("CRW", "NCRW", "NOPH", "YPP", set = fb_names))  fb=within(fb,{	
    YPP	= sbformula::ypp(crw = CRW, ncrw = NCRW, noph = NOPH)
  })
  
  #if(length(fb$NOCR)>0 & length(fb$NONC)>0) fb=within(fb,{	
  if(is_contained("NCRW", "NONC","CI", set = fb_names))  fb=within(fb,{	
    CI	= sbformula::ci(nocr = NOCR, nonc = NONC)
  })
  
  #if(length(fb$CRW)>0 & length(fb$NCRW)>0 & length(fb$VW)>0) fb=within(fb,{	
  if(is_contained("CRW", "NCRW", "VW", "HI", set = fb_names))  fb=within(fb,{
    HI	= sbformula::hi(crw = CRW, ncrw = NCRW, vw = VW)
  })
  
  #if(length(fb$NOPH)>0 & length(fb$NOPS)>0) fb=within(fb,{	
  if(is_contained("NOPH", "NOPS", "SHI",set = fb_names))  fb=within(fb,{ 
    SHI	= sbformula::shi(noph = NOPH, nops = NOPS)
  })
  
  #if(length(fb$CRW)>0 & length(fb$NCRW)>0 & length(fb$VW)>0) fb=within(fb,{	
  if(is_contained("CRW", "NCRW", "VW", "BIOM",set = fb_names))  fb=within(fb,{ 
    BIOM	= sbformula::biom(crw = CRW, ncrw = NCRW, vw = VW, pls = plot.size)
  })
  
  if(is_contained("VW", "FYTHA", set = fb_names))  fb=within(fb,{ 
    FYTHA	= sbformula::fytha(vw = VW, pls = plot.size)
  })
  
  #if(length(fb$DMD)>0 & length(fb$DMF)>0) fb=within(fb,{	
  if(is_contained("DMD", "DMF", "DM" ,set = fb_names))  fb=within(fb,{ 
    DM	= sbformula::dm(dmd = DMD, dmf = DMF)
  })
  
  #if(length(fb$VW)>0 & length(fb$DMVD)>0 & length(fb$DMVF)>0) fb=within(fb,{	
  if(is_contained("VW", "DMVD", "DMVF", "DMFY", set = fb_names))  fb=within(fb,{ 
    DMFY	= sbformula::dmfy(vw = VW, dmvd = DMVD, dmvf = DMVF, pls = plot.size)
  })
  
  #if(length(fb$DMRY)>0 & length(fb$CRW)>0 & length(fb$NCRW)>0 & length(fb$DMD)>0 & length(fb$DMF)>0) fb=within(fb,{	
  if(is_contained("CRW", "NCRW", "DMF", "DMD", "DMRY",set = fb_names))  fb=within(fb,{ 
    DMRY	=  sbformula::dmry(crw = CRW, ncrw = NCRW, dmf = DMF, pls = plot.size, dmd = DMD)
  })
  
  #if(length(fb$RFR)>0 & length(fb$CRW)>0 & length(fb$NCRW)>0 & length(fb$DMD)>0 & length(fb$DMF)>0 & length(fb$DMVD)>0 & length(fb$DMVF)>0) fb=within(fb,{	
  if(is_contained("CRW", "NCRW", "VW", "DMVD", "DMVF","RFR", set = fb_names))  fb=within(fb,{ 
    RFR <- sbformula::rfr(crw = CRW, ncrw = NCRW, vw = VW, dmvd = DMVD, dmvf = DMVF)
  })
  
  
  fieldbook <- fb[,fb_names]
    
  return(fieldbook)
  
  #fieldbook[,fbn] # make sure not any variables were attached
}

# fp <- file.choose()
# fp <- "C:\\Users\\fanny\\Desktop\\PTDT201409_STRSIGUAS_VHT.xlsx"
# datos <- xlsx::read.xlsx(fp,"Fieldbook")
# 
# 
#  plot.size <- 15
#  plant.den <- 14






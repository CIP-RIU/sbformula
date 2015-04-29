#'Calculation of measured variables on potato and sweetpotato
#'
#'@param fb A data frame with the fieldbook data
#'@return A data frame with the calculated variables from the fieldbook
#'@details This function allows to calculated several measured trait in one go.
#'@references Progress in developing a potato ontology for breeders. Reinhard Simon, Vilma Hualla, Raúl Eyzaguirre, Raúl Cordova, Robert O M Mwanga, Genoveva Rossel
#'Progress in developing a sweetpotato ontology for breeders. Reinhard Simon, Vilma Hualla, E. Salas, Rene Gomez, Raúl Cordova, Stef de Haan
#'@export
#'
sfbcalculate <- function(fb){   
  #potato variables
  
  #BEGIN PPE
  if(length(fb$NPE)>0 & length(fb$NTP)>0 ) fb=within(fb,{  
    PPE <- sbformula::ppe(npe = NPE,ntp = NTP)
  })  		
  #END PPE
  
  #BEIGN PPH
  if(length(fb$NTP)>0 & length(fb$NPH)>0 ) fb=within(fb,{	
    PPH <- sbformula::pph(nph = NPH,ntp = NTP)
  })			
  #END PPH
  
  if(length(fb$NPE)>0 & length(fb$NTP)>0 ) fb=within(fb,{  
    PPE <- sbformula::ppe(npe = NPE,ntp = NTP)  
  })  		  
  
  #BEGIN TNTP
  if( length(fb$NNoMTP)>0 &  length(fb$NMTP)>0) fb=within(fb,{ 
    TNTP <- sbformula::tntp(nnomtp = NNoMTP,nmtp = NMTP)
  })
  
  if(length(fb$NMTCI)>0 & length(fb$NMTCII)>0) fb=within(fb,{ 
    temp <- sbformula::nmtp(nmtci = NMTCI, nmtcii = NMTCII )
    TNTP <- temp
  })
  
  if(length(fb$NMTCI)>0 & length(fb$NNoMTP)>0 ) fb=within(fb,{  
    temp <- sbformula::nmtp(nmtci = NMTCI)
    TNTP <- sbformula::tntp(nmtp = temp,nnomtp = NNoMTP)
  })			
  
  if(length(fb$NMTCII)>0 & length(fb$NNoMTP)>0 ) fb=within(fb,{  
    temp <- sbformula::nmtp(nmtcii = NMTCII)
    TNTP <- sbformula::tntp(nmtp = temp,nnomtp = NNoMTP)
  })
  
  if(length(fb$NMTCI)>0 & length(fb$NMTCII)>0 & length(fb$NNoMTP)>0 ) fb=within(fb,{  
    temp <- sbformula::nmtp(nmtci = NMTCI,nmtcii = NMTCII)
    TNTP <- sbformula::tntp(nmtp = temp,nnomtp = NNoMTP) 
  })
  ###END TNTP  
  
  #BEGIN TNTPL
  if(length(fb$TNTP)>0 & length(fb$NPH)>0  ) fb=within(fb,{	
    TNTPL <- sbformula::tntpl(tntp = TNTP,nph = NPH)
  })
  #END TNTPL
  
  #BEGIN NMTP
  if(length(fb$NMTCI)>0 & length(fb$NMTCII)>0) fb=within(fb,{	
    NMTP <- sbformula::nmtp(nmtci = NMTCI,nmtcii = NMTCII) #original
  })	
  #END NMTP
  
  #BEGIN NMTPL
  if(length(fb$NMTP)>0 & length(fb$NPH)>0  ) fb=within(fb,{	
    NMTPL <- sbformula::nmtpl(nmtp = NMTP,nph = NPH)	
  })			
  #END NMTPL
  
  ###BEGIN TTWP  
  if(length(fb$MTWP)>0 & length(fb$NoMTWP)>0 ) fb=within(fb,{ 
    TTWP <- sbformula::ttwp(mtwp= MTWP,nomtwp = NoMTWP) #in this case we consider mtcwi=MTWP, especial case
  })
  
  if(length(fb$MTWCI)>0 & length(fb$MTWCII)>0) fb=within(fb,{   
    TTWP <- sbformula::ttwp(mtwci = MTWCI,mtwcii = MTWCII)
  })
  
  if(length(fb$MTWCI)>0 & length(fb$MTWCII)>0 & length(fb$NoMTWP)>0 ) fb=within(fb,{  
    temp <- sbformula::nmtp(nmtci = MTWCI,nmtcii = MTWCII)
    TTWP <- sbformula::ttwp(temp,nomtwp = NoMTWP)
  })
  ##end TTWP
  
  #BEGIN TTWPL
  if(length(fb$TTWP)>0 & length(fb$NPH)>0 ) fb=within(fb,{	
    TTWPL <- sbformula::ttwpl(ttwp = TTWP,nph = NPH)
  })
  #END TTWPL
  
  #BEGIN TTYNA
  if(length(fb$TTWP)>0  ) fb=within(fb,{	
    TTYNA <- sbformula::ttyna(ttwp = TTWP,pls = plot.size)	#GTDM-39	
  })
  #END TTYNA
  
  
  #BEGIN TTYA
  if(length(fb$TTWPL)>0 &  length(plant.den)) fb=within(fb,{	
    TTYA <- sbformula::ttya(ttwpl = TTWPL,plantden = plant.den) # GTDM-45*			
  })
  #END TTYA
  
  #BEGIN MTWP
  if(length(fb$MTWCI)>0 & length(fb$MTWCII)>0 ) fb=within(fb,{  
    MTWP <-  sbformula::mtwp(mtwci = MTWCI,mtwcii = MTWCII)
  })	
  #END MTWP
  
  #BEGIN MTWPL
  if(length(fb$MTWP)>0 & length(fb$NPH)>0 ) fb=within(fb,{	
    MTWPL <- sbformula::mtwpl(mtwp = MTWP,nph = NPH)
  })		
  #END MTWPL
  
  #BEGIN MTYNA
  if(length(fb$MTWP)>0 ) fb=within(fb,{	
    MTYNA <- sbformula::mtyna(mtwp = MTWP,pls = plot.size)
  })	#GTDM-39
  #END MTYNA
  
  #BEGIN MTYA
  if(length(fb$MTWPL)>0 & length(plant.den)>0) fb=within(fb,{	
    MTYA <- sbformula::mtya(mtwpl = MTWPL,plantden = plant.den)
  })#GTDM-39		
  #END MTYA
  
  
  if(length(fb$TTWP)>0 & length(fb$TNTP)>0) fb=within(fb,{	
    ATW <- sbformula::atw(ttwp = TTWP,tntp = TNTP)
  })	
  
  if(length(fb$MTWP)>0 & length(fb$NMTP)>0) fb=within(fb,{	
    ATMW <- sbformula::atmw(mtwp = MTWP,nmtp = NMTP)
  })	
  
  if(length(fb$DWTS1)>0 & length(fb$FWTS1)>0) fb=within(fb,{	
    DM1 <- sbformula::dm1(DWTS1,FWTS1)
  })	
  
  if(length(fb$DWTS2)>0 & length(fb$FWTS2)>0) fb=within(fb,{	
    DM2 <- sbformula::dm2(dwts2 = DWTS2,fwts2 = FWTS2)
  })	
  
  if(length(fb$DM1)>0) fb=within(fb,{  
    AVDM <- sbformula::avdm(dm1 = DM1)
  })
  
  if(length(fb$DM2)>0) fb=within(fb,{  
    AVDM <- sbformula::avdm(dm2 = DM2)
  })
  
  if(length(fb$DM1)>0 & length(fb$DM2)>0) fb=within(fb,{	
    AVDM <- sbformula::avdm(dm1 = DM1,dm2 = DM2)
  })
  ##
  
  if(length(fb$TWA)>0 & length(fb$TWA)>0 & length(fb$TWW)>0) fb=within(fb,{  
    SG <- sbformula::sg(twa = TWA,tww = TWW)
  })
  ##
  
  if(length(fb$IWS1)>0 & length(fb$FWS1)>0 ) fb=within(fb,{	
    OCS1 <- sbformula::ocs1(iws1 = IWS1,fws1 = FWS1)
  })	
  
  if(length(fb$IWS2)>0 & length(fb$FWS2)>0 ) fb=within(fb,{	
    OCS2 <- sbformula::ocs2(iws2 = IWS2,fws2 = FWS2)
  })			
  
  if(length(fb$OCS1)>0 & length(fb$OCS2)>0 ) fb=within(fb,{	
    AOCP <- sbformula::aocp(ocs1 = OCS1,ocs2 = OCS2)
  })			
  
  ###############################################################################
  # Start related variables for sweetpotato
  
  if(length(fb$CRW)>0 & length(fb$NCRW)>0 ) fb=within(fb,{	
    TRW	 <-  sbformula::trw(crw = CRW,ncrw = NCRW)
  })
  
  if(length(fb$CRW)>0) fb=within(fb,{	
    CYTHA	 <-  sbformula::cytha(CRW,pls = plot.size)
  })
  
  if(length(fb$CRW)>0 & length(fb$NCRW)>0) fb=within(fb,{	
    RYTHA	= apply(cbind(CRW,NCRW), 1, sum, na.rm=T)*10/plot.size
  })
  
  if(length(fb$CRW)>0 & length(fb$NOCR)>0) fb=within(fb,{	
    ACRW	= sbformula::acrw(crw = CRW,nocr = NOCR)
  })
  
  if(length(fb$NOCR)>0 & length(fb$NONC)>0 & length(fb$NOPH)>0) fb=within(fb,{	
    NRPP	= sbformula::nrpp(nonc = NONC,nocr = NOCR,noph = NOPH)
  })
  
  if(length(fb$CRW)>0 & length(fb$NCRW)>0 & length(fb$NOPH)>0) fb=within(fb,{	
    YPP	= sbformula::ypp(crw = CRW, ncrw = NCRW,noph = NOPH)
  })
  
  if(length(fb$NOCR)>0 & length(fb$NONC)>0) fb=within(fb,{	
    CI	= sbformula::ci(nocr = NOCR,nonc = NONC)
  })
  
  if(length(fb$CRW)>0 & length(fb$NCRW)>0 & length(fb$VW)>0) fb=within(fb,{	
    HI	= sbformula::hi(crw = CRW, ncrw = NCRW,vw = VW)
  })
  
  if(length(fb$NOPH)>0 & length(fb$NOPS)>0) fb=within(fb,{	
    SHI	= sbformula::shi(noph = NOPH,nops = NOPS)
  })
  
  if(length(fb$CRW)>0 & length(fb$NCRW)>0 & length(fb$VW)>0) fb=within(fb,{	
    BIOM	= sbformula::biom(vw = VW, crw = CRW, ncrw = NCRW,pls = plot.size)
  })
  
  if(length(fb$VW)>0) fb=within(fb,{	
    FYTHA	= sbformula::fytha(vw = VW,pls = plot.size)
  })
  
  if(length(fb$DMD)>0 & length(fb$DMF)>0) fb=within(fb,{	
    DM	= sbformula::dm(dmd = DMD,dmf = DMF)
  })
  
  if(length(fb$VW)>0 & length(fb$DMVD)>0 & length(fb$DMVF)>0) fb=within(fb,{	
    DMFY	= sbformula::dmfy(vw = VW,pls = plot.size,dmvd = DMVD,dmvf = DMVF)
  })
  
  if(length(fb$DMRY)>0 & length(fb$CRW)>0 & length(fb$NCRW)>0 & length(fb$DMD)>0 & length(fb$DMF)>0) fb=within(fb,{	
    DMRY	=  sbformula::dmry(crw = CRW, ncrw = NCRW,pls = plot.size,dmd = DMD,dmf = DMF)
  })
  
  if(length(fb$RFR)>0 & length(fb$CRW)>0 & length(fb$NCRW)>0 & length(fb$DMD)>0 & length(fb$DMF)>0 & length(fb$DMVD)>0 & length(fb$DMVF)>0) fb=within(fb,{	
    RFR <- sbformula::rfr(crw = CRW, ncrw = NCRW,vw = VW,dmvd = DMVD,dmvf = DMVF)
  })
  
  
  fieldbook <- fb
  
  return(fieldbook)
  
  #fieldbook[,fbn] # make sure not any variables were attached
}



# fp <- file.choose()
# fp <- "C:\\Users\\fanny\\Desktop\\PTDT201409_STRSIGUAS_VHT.xlsx"
# datos <- xlsx::read.xlsx(fp,"Fieldbook")
# 
# 
# plot.size <- 15
# plant.den <- 14

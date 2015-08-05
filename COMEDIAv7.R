#rm(list = ls());
#lUCIATEST
#Pour le modèle G, il faudrait regarder les modifs que j'ai faite mais le mieux est surement de repartir de W corriger et d'y remettre les modif
#lim par nutri?

#Il faut comprendre pourquoi A13_CO2atm *diff_CO2+(C13_DIC_inc/(12.01 * H*10^(-6))) différent de 0

#J AI UN PROBLEME DE DISSOLUTION DU 13C_CO2 QUAND ON AUGMENTE LA TEMP, c'est due à mes calculs de fraction entre CO2aq et HCO3

#Models of C cycle in aquatic ecosystem

#The models aims at representing a simple but complete C cycle in a simplified aquatic ecosystems (without predators or herbivores, viral lysis, etc.).
#The orginilaity of the models is to take represent implicetly the priming effect based on the equations of Guenet et al., (2013) in GMD (model G) or Wutlzer and Reichstein (2008) in BG (model W) or only a classical first order kinetic (F).
# A another alternative is to represent the POC decomposition based on 1st order kinetics only (model F)
#The model G is simplest with less pools (5) and less parameters (8) comparted to model W (6 pools and 9 parameters)
# The model represents explicitely a 13C tracer.
# The time step of the model is daily.
# To calculate the Henry constant we used the eq. 18 of Sander ACP 2014
# The phytoplankton growth rate is based on the PROTECH model (Elliot et al 2001, Freshwater biology) where we applied a scalar based on Schippers et al., 2004 (Ecosystems) to take into account the limitation by the DIC. 


#For the moment, the parameters are based on litterature for some of them or just dummy for the others

#Open the parameters

#PARAM=read.table('run_optim_p0.def',header=FALSE)


########################################################Model forcings########################################################
#What is the model you want to use(F,G,W)?
model="G"

#Do you want to use the full model with plankton module (FALSE/TRUE)?
full=FALSE

#Water and air temperature  [K]
Temperature=273.15+20

#Water pH
pH=6.4

#Surface Volume ratio of the phytoplankton (median of the Lewis data published in Science in 1976)
sv_ratio=1.44

#the number of daylight hours from sunrise to sunset.
tp=12.0

#Atmospheric CO2 concentration and isotopic abundance
co2atm=375
A13_co2atm= 0.01102441 #(corresponding to -8‰)

# Half saturation for Stichococcus bacillaris # Why there is a name species here?????????
HCO2aq=7.5e-6
HHCO3moins=300e-6   

########################################################Model Parameters#####################################################

#DOC decomposition rate [d-1] (1/turnover time)
Kdoc=1./60

#Carbon use efficiency for phytoplankton [no units] 
CUEphyto=0.00005

#Exudation rates for phytoplankton [fraction of C fixed] 
Exud= 0.1

#Carbon use efficiency for decomposers [no units] (based on Sinsabaug et al., 2013 Ecology Letters)
CUEdecomp=0.3

#Mortality rate of the phyto [d-1] 
Deathphyto=0.00005

#Mortality rate of the decomposers [d-1] 
Deathdecomp=0.005

#POC decomposition rate [d-1] (1/turnover time)
Kpoc=1./120

#Production of POC by DOC [no units] (through microbial biomass turnover)
e=0.5

#Priming parameters for POC [no units]  (based on Guenet et al., 2012 BG)
c=47.57

#Carrying capacity (gC l-1)
K=1
        
     

        
#duration of the simulation
time<-365
days<-c(1:time)
time_exp_begin<-10000

########################################################initialisation#####################################################
CO2ATM<-c(co2atm,rep(0,time-1))
#For the moment the pH is not influenced by the CO2
PH<-c(pH,rep(pH,time-1))
DIC<-c(rep(0,time))
DIC_inc<-c(rep(0,time))
CO2aq<-c(rep(0,time))
HCO3moins<-c(rep(0,time))
CO3_2moins<-c(rep(0,time))
PHYTO<-c(10^-3,rep(0,time-1))
#DOC<-c(rep(0,time))
#POC<-c(rep(0,time))
#DECOMP<-c(rep(0,time))
#DOC<-c(10^-3,rep(0,time-1))
#POC<-c(10^-3,rep(0,time-1))
#DECOMP<-c(10^-3,rep(0,time-1))
DOC<-c(3,rep(0,time-1))
POC<-c(3,rep(0,time-1))
DECOMP<-c(3,rep(0,time-1))

fc<-c(rep(0,time))
diff_CO2<-c(rep(0,time))
Input_CO2<-c(rep(0,time))
FrCO2<-c(rep(0,time))
FrHCO3<-c(rep(0,time))
Hplus<-c(10^-pH,rep(0,time-1))

#For 13C pools
C13_CO2ATM<-c(A13_co2atm* co2atm,rep(0,time-1))
C13_DIC<-c(rep(0,time))
C13_DIC_inc<-c(rep(0,time))
C13_CO2aq<-c(rep(0,time))
C13_HCO3moins<-c(rep(0,time))
C13_CO3_2moins<-c(rep(0,time))
C13_PHYTO<-c(0.01083336*PHYTO[1],rep(0,time-1))
C13_DOC<-c(0.01083336 *DOC[1],rep(0,time-1))
C13_POC<-c(0.01083336*POC[1], rep(0,time-1))
C13_DECOMP<-c(0.01083336*DECOMP[1], rep(0,time-1))
C13_diff_CO2<-c(rep(0,time)-1)
C13_Input_CO2<-c(rep(0,time)-1)
C13_FrCO2<-c(rep(0,time))
C13_FrHCO3<-c(rep(0,time))
C13_fc<-c(rep(0,time))


        pho<-c(rep(0,time))
        C13_pho<-c(rep(0,time))
############Model W####################
if (model=="W") {
	print("We are using the W model")
for (t in 1:(time-1) ) {

        #increase temperature
        Temp<-Temperature
#       if (t<time_exp_begin) {Temp=Temp} else {Temp=Temp+10}

        #doubling CO2
       CO2atm<-co2atm
       A13_CO2atm<-A13_co2atm
#        if (t<time_exp_begin) {CO2atm=CO2atm} else {CO2atm=CO2atm*2}
#        if (t<time_exp_begin) {C13_CO2ATM=C13_CO2ATM} else {C13_CO2ATM=A13_co2atm*CO2ATM}

        #DOC inputs
#       if (t>time_exp_begin) {DOC[t]<-DOC[t]+DOC[time_exp_begin]*0.02}
#        if (t>time_exp_begin) {C13_DOC[t]<-C13_DOC[t]+C13_DOC[time_exp_begin]*0.04}       
       
       #K1 and K2 parameter calculation based on Harned and Davis, 1943 for pK1 and Harned and Scholes, 1941 for pK2
       pK1= 3404.71/Temp + 0.032786*Temp- 14.8435
       pK2=2902.39/Temp + 0.02379*Temp-6.4980
       
       K1=10^-pK1
       K2=10^-pK2

              
        #Classical Q10 function coming from ORCHIDEE
        temp_ctrl<-pmin(1,exp(0.69*(Temp-(273.15+30))/10))	
        	 	
        #Henry's constant calculation
        H = (3.4*10^(-2))*exp(2400*((1/Temp)-(1/298.15)))  
        #H = 0.8317 #fixed for 25°C for the moment but must be temperature dependent

        # Phytoplankton growth rate
        beta = 3.378-2.505 * log(sv_ratio)
        r20 = 1.142 * (sv_ratio)^0.325
        rtheta = r20 * exp(beta * ((1000/295.15) - (1000/Temp)))
        rtheta_I = rtheta * (tp/24)
        	 	
         #H+ concentrion
        Hplus[t]=10^-PH[t]
               	 	
        DIC[t] = CO2ATM[t] * 10^(-6) * H * 12.01 
        # We multiplied by 12.01, the molar mass of C to convert from mol.l-1 of CO2 to gC.l-1
        CO2aq[t] = ((Hplus[t]^2)/(Hplus[t]^2+K1*Hplus[t]+K1*K2))*DIC[t]
        HCO3moins[t] = ((Hplus[t]*K1)/(Hplus[t]^2+K1*Hplus[t]+K1*K2))*DIC[t]
        CO3_2moins[t] = ((K1*K2)/(Hplus[t]^2+K1*Hplus[t]+K1*K2))*DIC[t]
                
        # Scalar defining the limitation effect of DIC availability 
        FrCO2[t]=CO2aq[t]/(HCO3moins[t] + CO2aq[t])
        FrHCO3[t]=HCO3moins[t]/(HCO3moins[t] + CO2aq[t])
                
        fc[t] =  ((CO2aq[t]/44.01)+(HCO3moins[t]/61.0168))/(FrCO2[t]*HCO2aq+ FrHCO3[t]*HHCO3moins + (CO2aq[t]/44.01)+(HCO3moins[t]/61.0168))



        
        #flux calculation for total C	
        mineralization_poc = Kpoc*POC[t]*(1-exp(-c*DECOMP[t]))* temp_ctrl
        mineralization_doc = Kdoc*DOC[t]* temp_ctrl
#mineralization_poc =0
#mineralization_doc =0


if (full) {      photosynthesis = (( rtheta_I  * fc[t])*(1-(PHYTO[t])/K))*(PHYTO[t]) } else {photosynthesis = 0}

pho[t]<-photosynthesis


        #When the population is growing exudation is just a fraction of photosynthesis but once the maximum
        #carrying capacity is reached all the C fixed is exudated to take into account the fact that algae can't 
        #regulate the photosynthesis activity 
        if (PHYTO[t] < K-K/100) {
        exudation = photosynthesis*Exud        	
        }
        else {
        exudation = (( rtheta_I  * fc[t])*(PHYTO[t]))        	
        }
        aut_resp = CUEphyto* PHYTO[t]	
        #aut_resp=0
        phyto_death=Deathphyto * (PHYTO[t])
        #phyto_death=0
        decomp_death = Deathdecomp*DECOMP[t]
        het_resp = (mineralization_poc+ mineralization_doc)*CUEdecomp



        #update of the pool
        DIC_inc[t+1] = aut_resp + het_resp - photosynthesis 
        PHYTO[t+1] = PHYTO[t] + photosynthesis - phyto_death -exudation - aut_resp
        DECOMP[t+1] = DECOMP[t]+ mineralization_doc + mineralization_poc - decomp_death - het_resp
        DOC[t+1] = DOC[t]+ exudation - mineralization_doc
        POC[t+1] = POC[t]+phyto_death + decomp_death - mineralization_poc
        CO2ATM[t+1] = CO2ATM[t] + DIC_inc[t+1]/(12.01 * H*10^(-6))
        diff_CO2[t+1]=CO2ATM[t+1]-CO2atm
        CO2ATM[t+1] = CO2atm
  
        
        #Whereas all is gC l-1 every *CO2* variables are in ppm
        Input_CO2[t+1]= CO2atm - abs(diff_CO2[t+1])
        
        #flux of 13C
        #Definition of some parameters 
        # The three following equation come from Mook et al., 1974 Earth and Planetary Science Letters
        # In the litterature, the discriminination are given in delta but should be converted in Abundance for calculation
        discrimination_CO2g_CO2aq=((-0.373*1e3/Temp +0.19)/1000 +1)*0.0112372
        discrimination_HCO3_CO2aq = ((-9.866*1e3/Temp +24.12)/1000 +1)*0.0112372
        discrimination_HCO3_CO2g = ((-9.483*1e3/Temp +23.89)/1000 +1)*0.0112372
        discrimination_CO2g_DIC = FrCO2[t]* discrimination_CO2g_CO2aq + FrHCO3[t]*(-discrimination_HCO3_CO2g) # here we have a ' - ' because the discrimination is calculated for HCO3 that goes to CO2g.



        #DUMMY PARAMETERS
        discrimination_HCO3_phyto = 0.00
        discrimination_CO2aq_phyto = 0.00
        discrimination_exudation = 0.00
        discrimination_aut_resp = 0.00
        discrimination_photosynthesis = FrCO2[t] * discrimination_CO2aq_phyto + FrHCO3[t] * discrimination_HCO3_phyto
         #It is generally assumed that heterotrophic organisms do not discriminate when they consume a subsrate (Ekblad et al., 2002 Oecologia)
        discrimination_DOC_mine = 0.0
        discrimination_POC_mine = 0.0
        discrimination_het_resp = 0.0
        
       #We assume no discrimintation due to death
       discrimination_phyto_death = 0.0
       discrimination_decomp_death = 0.0


        C13_DIC[t] = C13_CO2ATM[t] * 10^(-6) * H * 12.01 + (discrimination_CO2g_DIC*CO2ATM[t] * 10^(-6) * H * 12.01)
        # We multiplied by 13.01, the molar mass of C to convert from mol.l-1 of 13CO2 to g13C.l-1
        C13_CO2aq[t] = ((Hplus[t]^2)/(Hplus[t]^2+K1*Hplus[t]+K1*K2))*C13_DIC[t]
        C13_HCO3moins[t] = ((Hplus[t]*K1)/(Hplus[t]^2+K1*Hplus[t]+K1*K2))*C13_DIC[t]
        C13_CO3_2moins[t] = ((K1*K2)/(Hplus[t]^2+K1*Hplus[t]+K1*K2))*C13_DIC[t]
                
        # Scalar defining the limitation effect of DIC availability ASSUMING FOR THE MOMENT THAT HALF IS CO2 AND HALF IS HCO3-
        C13_FrCO2[t]=C13_CO2aq[t]/(C13_HCO3moins[t] + C13_CO2aq[t])
        C13_FrHCO3[t]=C13_HCO3moins[t]/(C13_HCO3moins[t] + C13_CO2aq[t])
        C13_fc[t] = ((C13_CO2aq[t]/45.01)+(C13_HCO3moins[t]/62.0168)+ 
        					discrimination_CO2aq_phyto*(C13_CO2aq[t]/45.01)+ discrimination_HCO3_phyto*(C13_HCO3moins[t]/62.0168))/
     	  					(C13_FrCO2[t]*HCO2aq+ C13_FrHCO3[t]*HHCO3moins + (C13_CO2aq[t]/45.01)+(C13_HCO3moins[t]/62.0168)+
     	  					discrimination_CO2aq_phyto*(C13_CO2aq[t]/45.01)+ discrimination_HCO3_phyto*(C13_HCO3moins[t]/62.0168))
         
                 
         #flux calculation for 13 C	
        C13_mineralization_poc = mineralization_poc*(C13_POC[t]/POC[t]) 
        C13_mineralization_doc = mineralization_doc*(C13_DOC[t]/DOC[t]) 
#C13_mineralization_poc =0
#C13_mineralization_doc =0


if (full)  {        C13_photosynthesis = (( rtheta_I  * C13_fc[t])*(1-(PHYTO[t])/K))*PHYTO[t] } else {C13_photosynthesis = 0}

        #When the population is growing exudation is just a fraction of photosynthesis but once the maximum
        #carrying capacity is reached all the C fixed is exudated to take into account the fact that algae can't 
        #regulate the photosynthesis activity 
        if (PHYTO[t] < K-K/100) {
        C13_exudation = C13_photosynthesis*Exud        	
        }
        else {
        C13_exudation = ( rtheta_I  * C13_fc[t])* PHYTO[t]
        }
        C13_aut_resp = CUEphyto* C13_PHYTO[t]
        #C13_aut_resp=0
        C13_phyto_death=Deathphyto * (C13_PHYTO[t])
        #C13_phyto_death=0
        C13_decomp_death = Deathdecomp*C13_DECOMP[t]
        C13_het_resp = (C13_mineralization_poc+ C13_mineralization_doc)*CUEdecomp
        
        C13_pho[t]<-C13_photosynthesis 
                       
        #update of the 13C pool        
        C13_DIC_inc[t+1] =  C13_aut_resp + discrimination_aut_resp*PHYTO[t] + 
                                   C13_het_resp + discrimination_het_resp*DECOMP[t] - 
                                   C13_photosynthesis 
                                                       
        C13_PHYTO[t+1] = C13_PHYTO[t] +  C13_photosynthesis  - 
                                                                    C13_phyto_death - discrimination_phyto_death*PHYTO[t] - 
                                                                    C13_exudation  - discrimination_exudation*PHYTO[t] -
                                                                    C13_aut_resp -  discrimination_aut_resp*PHYTO[t] 
                                                                   
        C13_DECOMP[t+1] = C13_DECOMP[t]+ C13_mineralization_doc + discrimination_DOC_mine*DOC[t] + 
                                                                        C13_mineralization_poc+ discrimination_POC_mine*POC[t] - 
                                                                        C13_decomp_death - discrimination_decomp_death*DECOMP[t] - 
                                                                        C13_het_resp - discrimination_het_resp*DECOMP[t]
                                                                       
        C13_DOC[t+1] = C13_DOC[t]+ C13_exudation + discrimination_exudation*PHYTO[t] - 
                                                         C13_mineralization_doc  - discrimination_DOC_mine*DOC[t]
                                                         
        C13_POC[t+1] = C13_POC[t]+ C13_phyto_death + discrimination_phyto_death*PHYTO[t] + 
                                                       C13_decomp_death + discrimination_decomp_death*DECOMP[t] - 
                                                       C13_mineralization_poc - discrimination_POC_mine*POC[t]
                                                       
        C13_CO2ATM[t+1] = C13_CO2ATM[t]+ (C13_DIC_inc[t+1]/(12.01 * H*10^(-6)))  + 
                                          discrimination_CO2g_DIC*(DIC_inc[t+1]/(12.01 * H*10^(-6))) + 
                                          A13_CO2atm *abs(diff_CO2[t]) 
        
        #C13_diff_CO2[t+1]=C13_CO2ATM[t+1] - C13_CO2ATM[t]

#The following line assumes that the flux of 13C_CO2 coming from the water is negligible
        C13_CO2ATM[t+1] = A13_CO2atm*CO2ATM[t]
        
	                                                 }

        # conversion from ppm to gC l-1
        diff_CO2 = diff_CO2*12.01*10^-6
                           }

############Model G####################                           
if (model=="G")   {
	print("We are using the G model")
for (t in 1:(time-1) ) {

        #increase temperature
        Temp<-Temperature
#       if (t<time_exp_begin) {Temp=Temp} else {Temp=Temp+10}

        #doubling CO2
       CO2atm<-co2atm
       A13_CO2atm<-A13_co2atm
#        if (t<time_exp_begin) {CO2atm=CO2atm} else {CO2atm=CO2atm*2}
#        if (t<time_exp_begin) {C13_CO2ATM=C13_CO2ATM} else {C13_CO2ATM=A13_co2atm*CO2ATM}

        #DOC inputs
#       if (t>time_exp_begin) {DOC[t]<-DOC[t]+DOC[time_exp_begin]*0.02}
#        if (t>time_exp_begin) {C13_DOC[t]<-C13_DOC[t]+C13_DOC[time_exp_begin]*0.04}       
       
       #K1 and K2 parameter calculation based on Harned and Davis, 1943 for pK1 and Harned and Scholes, 1941 for pK2
       pK1= 3404.71/Temp + 0.032786*Temp- 14.8435
       pK2=2902.39/Temp + 0.02379*Temp-6.4980
       
       K1=10^-pK1
       K2=10^-pK2

              
        #Classical Q10 function coming from ORCHIDEE
        temp_ctrl<-pmin(1,exp(0.69*(Temp-(273.15+30))/10))	
        	 	
        #Henry's constant calculation
        H = (3.4*10^(-2))*exp(2400*((1/Temp)-(1/298.15)))  
        #H = 0.8317 #fixed for 25°C for the moment but must be temperature dependent

        # Phytoplankton growth rate
        beta = 3.378-2.505 * log(sv_ratio)
        r20 = 1.142 * (sv_ratio)^0.325
        rtheta = r20 * exp(beta * ((1000/295.15) - (1000/Temp)))
        rtheta_I = rtheta * (tp/24)
        	 	
         #H+ concentrion
        Hplus[t]=10^-PH[t]
               	 	
        DIC[t] = CO2ATM[t] * 10^(-6) * H * 12.01 
        # We multiplied by 12.01, the molar mass of C to convert from mol.l-1 of CO2 to gC.l-1
        CO2aq[t] = ((Hplus[t]^2)/(Hplus[t]^2+K1*Hplus[t]+K1*K2))*DIC[t]
        HCO3moins[t] = ((Hplus[t]*K1)/(Hplus[t]^2+K1*Hplus[t]+K1*K2))*DIC[t]
        CO3_2moins[t] = ((K1*K2)/(Hplus[t]^2+K1*Hplus[t]+K1*K2))*DIC[t]
                
        # Scalar defining the limitation effect of DIC availability 
        FrCO2[t]=CO2aq[t]/(HCO3moins[t] + CO2aq[t])
        FrHCO3[t]=HCO3moins[t]/(HCO3moins[t] + CO2aq[t])
                
        fc[t] =  ((CO2aq[t]/44.01)+(HCO3moins[t]/61.0168))/(FrCO2[t]*HCO2aq+ FrHCO3[t]*HHCO3moins + (CO2aq[t]/44.01)+(HCO3moins[t]/61.0168))



        
        #flux calculation for total C	
        mineralization_poc = Kpoc*POC[t]*(1-exp(-c*DOC[t]))* temp_ctrl
        mineralization_doc = Kdoc*DOC[t]* temp_ctrl
#mineralization_poc =0
#mineralization_doc =0


if (full) {      photosynthesis = (( rtheta_I  * fc[t])*(1-(PHYTO[t])/K))*(PHYTO[t]) } else {photosynthesis = 0}

pho[t]<-photosynthesis


        #When the population is growing exudation is just a fraction of photosynthesis but once the maximum
        #carrying capacity is reached all the C fixed is exudated to take into account the fact that algae can't 
        #regulate the photosynthesis activity 
        if (PHYTO[t] < K-K/100) {
        exudation = photosynthesis*Exud        	
        }
        else {
        exudation = (( rtheta_I  * fc[t])*(PHYTO[t]))        	
        }
        aut_resp = CUEphyto* PHYTO[t]	
        #aut_resp=0
        phyto_death=Deathphyto * (PHYTO[t])
        #phyto_death=0
        decomp_death = Deathdecomp*DECOMP[t]
        het_resp = (mineralization_poc+ mineralization_doc)*CUEdecomp



        #update of the pool
        DIC_inc[t+1] = aut_resp + het_resp - photosynthesis 
        PHYTO[t+1] = PHYTO[t] + photosynthesis - phyto_death -exudation - aut_resp
        DECOMP[t+1] = DECOMP[t]+ mineralization_doc + mineralization_poc - decomp_death - het_resp
        DOC[t+1] = DOC[t]+ exudation - mineralization_doc
        POC[t+1] = POC[t]+phyto_death + decomp_death - mineralization_poc
        CO2ATM[t+1] = CO2ATM[t] + DIC_inc[t+1]/(12.01 * H*10^(-6))
        diff_CO2[t+1]=CO2ATM[t+1]-CO2atm
        CO2ATM[t+1] = CO2atm
  
        
        #Whereas all is gC l-1 every *CO2* variables are in ppm
        Input_CO2[t+1]= CO2atm - abs(diff_CO2[t+1])
        
        #flux of 13C
        #Definition of some parameters 
        # The three following equation come from Mook et al., 1974 Earth and Planetary Science Letters
        # In the litterature, the discriminination are given in delta but should be converted in Abundance for calculation
        discrimination_CO2g_CO2aq=((-0.373*1e3/Temp +0.19)/1000 +1)*0.0112372
        discrimination_HCO3_CO2aq = ((-9.866*1e3/Temp +24.12)/1000 +1)*0.0112372
        discrimination_HCO3_CO2g = ((-9.483*1e3/Temp +23.89)/1000 +1)*0.0112372
        discrimination_CO2g_DIC = FrCO2[t]* discrimination_CO2g_CO2aq + FrHCO3[t]*(-discrimination_HCO3_CO2g) # here we have a ' - ' because the discrimination is calculated for HCO3 that goes to CO2g.



        #DUMMY PARAMETERS
        discrimination_HCO3_phyto = 0.00
        discrimination_CO2aq_phyto = 0.00
        discrimination_exudation = 0.00
        discrimination_aut_resp = 0.00
        discrimination_photosynthesis = FrCO2[t] * discrimination_CO2aq_phyto + FrHCO3[t] * discrimination_HCO3_phyto
         #It is generally assumed that heterotrophic organisms do not discriminate when they consume a subsrate (Ekblad et al., 2002 Oecologia)
        discrimination_DOC_mine = 0.0
        discrimination_POC_mine = 0.0
        discrimination_het_resp = 0.0
        
       #We assume no discrimintation due to death
       discrimination_phyto_death = 0.0
       discrimination_decomp_death = 0.0


        C13_DIC[t] = C13_CO2ATM[t] * 10^(-6) * H * 12.01 + (discrimination_CO2g_DIC*CO2ATM[t] * 10^(-6) * H * 12.01)
        # We multiplied by 13.01, the molar mass of C to convert from mol.l-1 of 13CO2 to g13C.l-1
        C13_CO2aq[t] = ((Hplus[t]^2)/(Hplus[t]^2+K1*Hplus[t]+K1*K2))*C13_DIC[t]
        C13_HCO3moins[t] = ((Hplus[t]*K1)/(Hplus[t]^2+K1*Hplus[t]+K1*K2))*C13_DIC[t]
        C13_CO3_2moins[t] = ((K1*K2)/(Hplus[t]^2+K1*Hplus[t]+K1*K2))*C13_DIC[t]
                
        # Scalar defining the limitation effect of DIC availability ASSUMING FOR THE MOMENT THAT HALF IS CO2 AND HALF IS HCO3-
        C13_FrCO2[t]=C13_CO2aq[t]/(C13_HCO3moins[t] + C13_CO2aq[t])
        C13_FrHCO3[t]=C13_HCO3moins[t]/(C13_HCO3moins[t] + C13_CO2aq[t])
        C13_fc[t] = ((C13_CO2aq[t]/45.01)+(C13_HCO3moins[t]/62.0168)+ 
        					discrimination_CO2aq_phyto*(C13_CO2aq[t]/45.01)+ discrimination_HCO3_phyto*(C13_HCO3moins[t]/62.0168))/
     	  					(C13_FrCO2[t]*HCO2aq+ C13_FrHCO3[t]*HHCO3moins + (C13_CO2aq[t]/45.01)+(C13_HCO3moins[t]/62.0168)+
     	  					discrimination_CO2aq_phyto*(C13_CO2aq[t]/45.01)+ discrimination_HCO3_phyto*(C13_HCO3moins[t]/62.0168))
         
                 
         #flux calculation for 13 C	
        C13_mineralization_poc = mineralization_poc*(C13_POC[t]/POC[t]) 
        C13_mineralization_doc = mineralization_doc*(C13_DOC[t]/DOC[t]) 
#C13_mineralization_poc =0
#C13_mineralization_doc =0


if (full)  {        C13_photosynthesis = (( rtheta_I  * C13_fc[t])*(1-(PHYTO[t])/K))*PHYTO[t] } else {C13_photosynthesis = 0}

        #When the population is growing exudation is just a fraction of photosynthesis but once the maximum
        #carrying capacity is reached all the C fixed is exudated to take into account the fact that algae can't 
        #regulate the photosynthesis activity 
        if (PHYTO[t] < K-K/100) {
        C13_exudation = C13_photosynthesis*Exud        	
        }
        else {
        C13_exudation = ( rtheta_I  * C13_fc[t])* PHYTO[t]
        }
        C13_aut_resp = CUEphyto* C13_PHYTO[t]
        #C13_aut_resp=0
        C13_phyto_death=Deathphyto * (C13_PHYTO[t])
        #C13_phyto_death=0
        C13_decomp_death = Deathdecomp*C13_DECOMP[t]
        C13_het_resp = (C13_mineralization_poc+ C13_mineralization_doc)*CUEdecomp
        
        C13_pho[t]<-C13_photosynthesis 
                       
        #update of the 13C pool        
        C13_DIC_inc[t+1] =  C13_aut_resp + discrimination_aut_resp*PHYTO[t] + 
                                   C13_het_resp + discrimination_het_resp*DECOMP[t] - 
                                   C13_photosynthesis 
                                                       
        C13_PHYTO[t+1] = C13_PHYTO[t] +  C13_photosynthesis  - 
                                                                    C13_phyto_death - discrimination_phyto_death*PHYTO[t] - 
                                                                    C13_exudation  - discrimination_exudation*PHYTO[t] -
                                                                    C13_aut_resp -  discrimination_aut_resp*PHYTO[t] 
                                                                   
        C13_DECOMP[t+1] = C13_DECOMP[t]+ C13_mineralization_doc + discrimination_DOC_mine*DOC[t] + 
                                                                        C13_mineralization_poc+ discrimination_POC_mine*POC[t] - 
                                                                        C13_decomp_death - discrimination_decomp_death*DECOMP[t] - 
                                                                        C13_het_resp - discrimination_het_resp*DECOMP[t]
                                                                       
        C13_DOC[t+1] = C13_DOC[t]+ C13_exudation + discrimination_exudation*PHYTO[t] - 
                                                         C13_mineralization_doc  - discrimination_DOC_mine*DOC[t]
                                                         
        C13_POC[t+1] = C13_POC[t]+ C13_phyto_death + discrimination_phyto_death*PHYTO[t] + 
                                                       C13_decomp_death + discrimination_decomp_death*DECOMP[t] - 
                                                       C13_mineralization_poc - discrimination_POC_mine*POC[t]
                                                       
        C13_CO2ATM[t+1] = C13_CO2ATM[t]+ (C13_DIC_inc[t+1]/(12.01 * H*10^(-6)))  + 
                                          discrimination_CO2g_DIC*(DIC_inc[t+1]/(12.01 * H*10^(-6))) + 
                                          A13_CO2atm *abs(diff_CO2[t]) 
        
        #C13_diff_CO2[t+1]=C13_CO2ATM[t+1] - C13_CO2ATM[t]

#The following line assumes that the flux of 13C_CO2 coming from the water is negligible
        C13_CO2ATM[t+1] = A13_CO2atm*CO2ATM[t]
        
	                                                 }

        # conversion from ppm to gC l-1
        diff_CO2 = diff_CO2*12.01*10^-6                           }	
                                              
############Model F####################                           
if (model=="F")   {
	print("We are using the F model")
for (t in 1:(time-1) ) {

        #increase temperature
        Temp<-Temperature
#       if (t<time_exp_begin) {Temp=Temp} else {Temp=Temp+10}

        #doubling CO2
       CO2atm<-co2atm
       A13_CO2atm<-A13_co2atm
#        if (t<time_exp_begin) {CO2atm=CO2atm} else {CO2atm=CO2atm*2}
#        if (t<time_exp_begin) {C13_CO2ATM=C13_CO2ATM} else {C13_CO2ATM=A13_co2atm*CO2ATM}

        #DOC inputs
#       if (t>time_exp_begin) {DOC[t]<-DOC[t]+DOC[time_exp_begin]*0.02}
#        if (t>time_exp_begin) {C13_DOC[t]<-C13_DOC[t]+C13_DOC[time_exp_begin]*0.04}       
       
       #K1 and K2 parameter calculation based on Harned and Davis, 1943 for pK1 and Harned and Scholes, 1941 for pK2
       pK1= 3404.71/Temp + 0.032786*Temp- 14.8435
       pK2=2902.39/Temp + 0.02379*Temp-6.4980
       
       K1=10^-pK1
       K2=10^-pK2

              
        #Classical Q10 function coming from ORCHIDEE
        temp_ctrl<-pmin(1,exp(0.69*(Temp-(273.15+30))/10))	
        	 	
        #Henry's constant calculation
        H = (3.4*10^(-2))*exp(2400*((1/Temp)-(1/298.15)))  
        #H = 0.8317 #fixed for 25°C for the moment but must be temperature dependent

        # Phytoplankton growth rate
        beta = 3.378-2.505 * log(sv_ratio)
        r20 = 1.142 * (sv_ratio)^0.325
        rtheta = r20 * exp(beta * ((1000/295.15) - (1000/Temp)))
        rtheta_I = rtheta * (tp/24)
        	 	
         #H+ concentrion
        Hplus[t]=10^-PH[t]
               	 	
        DIC[t] = CO2ATM[t] * 10^(-6) * H * 12.01 
        # We multiplied by 12.01, the molar mass of C to convert from mol.l-1 of CO2 to gC.l-1
        CO2aq[t] = ((Hplus[t]^2)/(Hplus[t]^2+K1*Hplus[t]+K1*K2))*DIC[t]
        HCO3moins[t] = ((Hplus[t]*K1)/(Hplus[t]^2+K1*Hplus[t]+K1*K2))*DIC[t]
        CO3_2moins[t] = ((K1*K2)/(Hplus[t]^2+K1*Hplus[t]+K1*K2))*DIC[t]
                
        # Scalar defining the limitation effect of DIC availability 
        FrCO2[t]=CO2aq[t]/(HCO3moins[t] + CO2aq[t])
        FrHCO3[t]=HCO3moins[t]/(HCO3moins[t] + CO2aq[t])
                
        fc[t] =  ((CO2aq[t]/44.01)+(HCO3moins[t]/61.0168))/(FrCO2[t]*HCO2aq+ FrHCO3[t]*HHCO3moins + (CO2aq[t]/44.01)+(HCO3moins[t]/61.0168))



        
        #flux calculation for total C	
        mineralization_poc = Kpoc*POC[t]* temp_ctrl
        mineralization_doc = Kdoc*DOC[t]* temp_ctrl
#mineralization_poc =0
#mineralization_doc =0


if (full) {      photosynthesis = (( rtheta_I  * fc[t])*(1-(PHYTO[t])/K))*(PHYTO[t]) } else {photosynthesis = 0}

pho[t]<-photosynthesis


        #When the population is growing exudation is just a fraction of photosynthesis but once the maximum
        #carrying capacity is reached all the C fixed is exudated to take into account the fact that algae can't 
        #regulate the photosynthesis activity 
        if (PHYTO[t] < K-K/100) {
        exudation = photosynthesis*Exud        	
        }
        else {
        exudation = (( rtheta_I  * fc[t])*(PHYTO[t]))        	
        }
        aut_resp = CUEphyto* PHYTO[t]	
        #aut_resp=0
        phyto_death=Deathphyto * (PHYTO[t])
        #phyto_death=0
        decomp_death = Deathdecomp*DECOMP[t]
        het_resp = (mineralization_poc+ mineralization_doc)*CUEdecomp



        #update of the pool
        DIC_inc[t+1] = aut_resp + het_resp - photosynthesis 
        PHYTO[t+1] = PHYTO[t] + photosynthesis - phyto_death -exudation - aut_resp
        DECOMP[t+1] = DECOMP[t]+ mineralization_doc + mineralization_poc - decomp_death - het_resp
        DOC[t+1] = DOC[t]+ exudation - mineralization_doc
        POC[t+1] = POC[t]+phyto_death + decomp_death - mineralization_poc
        CO2ATM[t+1] = CO2ATM[t] + DIC_inc[t+1]/(12.01 * H*10^(-6))
        diff_CO2[t+1]=CO2ATM[t+1]-CO2atm
        CO2ATM[t+1] = CO2atm
  
        
        #Whereas all is gC l-1 every *CO2* variables are in ppm
        Input_CO2[t+1]= CO2atm - abs(diff_CO2[t+1])
        
        #flux of 13C
        #Definition of some parameters 
        # The three following equation come from Mook et al., 1974 Earth and Planetary Science Letters
        # In the litterature, the discriminination are given in delta but should be converted in Abundance for calculation
        discrimination_CO2g_CO2aq=((-0.373*1e3/Temp +0.19)/1000 +1)*0.0112372
        discrimination_HCO3_CO2aq = ((-9.866*1e3/Temp +24.12)/1000 +1)*0.0112372
        discrimination_HCO3_CO2g = ((-9.483*1e3/Temp +23.89)/1000 +1)*0.0112372
        discrimination_CO2g_DIC = FrCO2[t]* discrimination_CO2g_CO2aq + FrHCO3[t]*(-discrimination_HCO3_CO2g) # here we have a ' - ' because the discrimination is calculated for HCO3 that goes to CO2g.



        #DUMMY PARAMETERS
        discrimination_HCO3_phyto = 0.00
        discrimination_CO2aq_phyto = 0.00
        discrimination_exudation = 0.00
        discrimination_aut_resp = 0.00
        discrimination_photosynthesis = FrCO2[t] * discrimination_CO2aq_phyto + FrHCO3[t] * discrimination_HCO3_phyto
         #It is generally assumed that heterotrophic organisms do not discriminate when they consume a subsrate (Ekblad et al., 2002 Oecologia)
        discrimination_DOC_mine = 0.0
        discrimination_POC_mine = 0.0
        discrimination_het_resp = 0.0
        
       #We assume no discrimintation due to death
       discrimination_phyto_death = 0.0
       discrimination_decomp_death = 0.0


        C13_DIC[t] = C13_CO2ATM[t] * 10^(-6) * H * 12.01 + (discrimination_CO2g_DIC*CO2ATM[t] * 10^(-6) * H * 12.01)
        # We multiplied by 13.01, the molar mass of C to convert from mol.l-1 of 13CO2 to g13C.l-1
        C13_CO2aq[t] = ((Hplus[t]^2)/(Hplus[t]^2+K1*Hplus[t]+K1*K2))*C13_DIC[t]
        C13_HCO3moins[t] = ((Hplus[t]*K1)/(Hplus[t]^2+K1*Hplus[t]+K1*K2))*C13_DIC[t]
        C13_CO3_2moins[t] = ((K1*K2)/(Hplus[t]^2+K1*Hplus[t]+K1*K2))*C13_DIC[t]
                
        # Scalar defining the limitation effect of DIC availability ASSUMING FOR THE MOMENT THAT HALF IS CO2 AND HALF IS HCO3-
        C13_FrCO2[t]=C13_CO2aq[t]/(C13_HCO3moins[t] + C13_CO2aq[t])
        C13_FrHCO3[t]=C13_HCO3moins[t]/(C13_HCO3moins[t] + C13_CO2aq[t])
        C13_fc[t] = ((C13_CO2aq[t]/45.01)+(C13_HCO3moins[t]/62.0168)+ 
        					discrimination_CO2aq_phyto*(C13_CO2aq[t]/45.01)+ discrimination_HCO3_phyto*(C13_HCO3moins[t]/62.0168))/
     	  					(C13_FrCO2[t]*HCO2aq+ C13_FrHCO3[t]*HHCO3moins + (C13_CO2aq[t]/45.01)+(C13_HCO3moins[t]/62.0168)+
     	  					discrimination_CO2aq_phyto*(C13_CO2aq[t]/45.01)+ discrimination_HCO3_phyto*(C13_HCO3moins[t]/62.0168))
         
                 
         #flux calculation for 13 C	
        C13_mineralization_poc = mineralization_poc*(C13_POC[t]/POC[t]) 
        C13_mineralization_doc = mineralization_doc*(C13_DOC[t]/DOC[t]) 
#C13_mineralization_poc =0
#C13_mineralization_doc =0


if (full)  {        C13_photosynthesis = (( rtheta_I  * C13_fc[t])*(1-(PHYTO[t])/K))*PHYTO[t] } else {C13_photosynthesis = 0}

        #When the population is growing exudation is just a fraction of photosynthesis but once the maximum
        #carrying capacity is reached all the C fixed is exudated to take into account the fact that algae can't 
        #regulate the photosynthesis activity 
        if (PHYTO[t] < K-K/100) {
        C13_exudation = C13_photosynthesis*Exud        	
        }
        else {
        C13_exudation = ( rtheta_I  * C13_fc[t])* PHYTO[t]
        }
        C13_aut_resp = CUEphyto* C13_PHYTO[t]
        #C13_aut_resp=0
        C13_phyto_death=Deathphyto * (C13_PHYTO[t])
        #C13_phyto_death=0
        C13_decomp_death = Deathdecomp*C13_DECOMP[t]
        C13_het_resp = (C13_mineralization_poc+ C13_mineralization_doc)*CUEdecomp
        
        C13_pho[t]<-C13_photosynthesis 
                       
        #update of the 13C pool        
        C13_DIC_inc[t+1] =  C13_aut_resp + discrimination_aut_resp*PHYTO[t] + 
                                   C13_het_resp + discrimination_het_resp*DECOMP[t] - 
                                   C13_photosynthesis 
                                                       
        C13_PHYTO[t+1] = C13_PHYTO[t] +  C13_photosynthesis  - 
                                                                    C13_phyto_death - discrimination_phyto_death*PHYTO[t] - 
                                                                    C13_exudation  - discrimination_exudation*PHYTO[t] -
                                                                    C13_aut_resp -  discrimination_aut_resp*PHYTO[t] 
                                                                   
        C13_DECOMP[t+1] = C13_DECOMP[t]+ C13_mineralization_doc + discrimination_DOC_mine*DOC[t] + 
                                                                        C13_mineralization_poc+ discrimination_POC_mine*POC[t] - 
                                                                        C13_decomp_death - discrimination_decomp_death*DECOMP[t] - 
                                                                        C13_het_resp - discrimination_het_resp*DECOMP[t]
                                                                       
        C13_DOC[t+1] = C13_DOC[t]+ C13_exudation + discrimination_exudation*PHYTO[t] - 
                                                         C13_mineralization_doc  - discrimination_DOC_mine*DOC[t]
                                                         
        C13_POC[t+1] = C13_POC[t]+ C13_phyto_death + discrimination_phyto_death*PHYTO[t] + 
                                                       C13_decomp_death + discrimination_decomp_death*DECOMP[t] - 
                                                       C13_mineralization_poc - discrimination_POC_mine*POC[t]
                                                       
        C13_CO2ATM[t+1] = C13_CO2ATM[t]+ (C13_DIC_inc[t+1]/(12.01 * H*10^(-6)))  + 
                                          discrimination_CO2g_DIC*(DIC_inc[t+1]/(12.01 * H*10^(-6))) + 
                                          A13_CO2atm *abs(diff_CO2[t]) 
        
        #C13_diff_CO2[t+1]=C13_CO2ATM[t+1] - C13_CO2ATM[t]

#The following line assumes that the flux of 13C_CO2 coming from the water is negligible
        C13_CO2ATM[t+1] = A13_CO2atm*CO2ATM[t]
        
	                                                 }

        # conversion from ppm to gC l-1
        diff_CO2 = diff_CO2*12.01*10^-6                           }	
        
                                                                                             
###########################################################Plot the results###################################################
        
nf<-layout (matrix (1:12,4,3), widths=c(lcm(10), lcm(10), lcm(10)), heights=c(lcm(4),lcm(4),lcm(4),lcm(4)))
 layout.show(nf)
 par(mar=c(0,9,0,0), oma=c(2,0,0,0), las=1, mex=1)
 
plot(PHYTO~days, col=0,ylab="",  cex.axis=0.8, xaxt="n")
lines(PHYTO[1: time_exp_begin]~days[1: time_exp_begin], col=1)
lines(PHYTO[time_exp_begin:time] ~days[time_exp_begin:time], col=2)
#lines(C13_PHYTO[1: time_exp_begin]*100~days[1: time_exp_begin], col=3)
#lines(C13_PHYTO[time_exp_begin:time]*100 ~days[time_exp_begin:time], col=4)
mtext("Phytoplankton biomass (gC.l-1)", side=2,line=4,las=0, cex=0.7)

plot(DECOMP~days, col=0,ylab="",  cex.axis=0.8, xaxt="n")
lines(DECOMP[1: time_exp_begin]~days[1: time_exp_begin], col=1)
lines(DECOMP[time_exp_begin:time] ~days[time_exp_begin:time], col=2)
mtext("Decomposers biomass (gC.l-1)", side=2,line=4,las=0, cex=0.7)

plot(DOC~days, col=0,ylab="",  cex.axis=0.8, xaxt="n")
lines(DOC[1: time_exp_begin]~days[1: time_exp_begin], col=1)
lines(DOC[time_exp_begin:time] ~days[time_exp_begin:time], col=2)
mtext("DOC (gC.l-1)", side=2,line=4,las=0, cex=0.7)

plot(POC~days, col=0,ylab="",  cex.axis=0.8, xlab="days")
lines(POC[1: time_exp_begin]~days[1: time_exp_begin], col=1)
lines(POC[time_exp_begin:time] ~days[time_exp_begin:time], col=2)
mtext("POC (gC.l-1)", side=2,line=4,las=0,cex=0.7)

plot(diff_CO2 ~days, col=0,ylab="",  cex.axis=0.8, xaxt="n")
lines(diff_CO2[1: time_exp_begin] ~days[1: time_exp_begin], col=1)
lines(diff_CO2[time_exp_begin:time] ~days[time_exp_begin:time], col=2)
mtext("CO2 consumption rate (gC.l-1.d-1)", side=2,line=4,las=0, cex=0.7)

#plot(CO2ATM ~days, col=0,ylab="",  cex.axis=0.8, xaxt="n")
#lines(CO2ATM[1: time_exp_begin] ~days[1: time_exp_begin], col=1)
#lines(CO2ATM[time_exp_begin:time] ~days[time_exp_begin:time], col=2)
#mtext("CO2 in the atmosphere (ppm)", side=2,line=4,las=0, cex=0.7)

plot(DIC[2:time-1] ~days[2:time-1], col=0,ylab="",  cex.axis=0.8, xaxt="n")
lines(DIC[2: time_exp_begin]~days[2: time_exp_begin], col=1)
lines(DIC[time_exp_begin:time-1] ~days[time_exp_begin:time-1], col=2)
mtext("DIC (gC.l-1)", side=2,line=4,las=0,cex=0.7)

A<-C13_CO2ATM/CO2ATM
delta = (((A/(1-A))/0.0112372)-1)*1000
#delta=C13_CO2ATM
plot(delta ~days, col=0,ylab="",  cex.axis=0.8, xaxt="n")
lines(delta[1: time_exp_begin] ~days[1: time_exp_begin], col=1)
lines(delta[time_exp_begin:time] ~days[time_exp_begin:time], col=2)
mtext("∂13C-CO2", side=2,line=4,las=0,cex=0.7)

A<-C13_DIC/DIC
delta = (((A/(1-A))/0.0112372)-1)*1000
#delta= C13_DIC
plot(delta ~days, col=0,ylab="",  cex.axis=0.8)
lines(delta[1: time_exp_begin] ~days[1: time_exp_begin], col=1)
lines(delta[time_exp_begin:time-1] ~days[time_exp_begin:time-1], col=2)
mtext("∂13C-DIC", side=2,line=4,las=0, cex=0.7)

A<-C13_DOC/DOC
delta = (((A/(1-A))/0.0112372)-1)*1000
#delta=C13_DOC
plot(delta ~days, col=0,ylab="",  cex.axis=0.8, , xaxt="n")
lines(delta[1: time_exp_begin] ~days[1: time_exp_begin], col=1)
lines(delta[time_exp_begin:time-1] ~days[time_exp_begin:time-1], col=2)
mtext("∂13C-DOC", side=2,line=4,las=0, cex=0.7)

A<-C13_POC/POC
delta = (((A/(1-A))/0.0112372)-1)*1000
#delta=C13_POC
plot(delta ~days, col=0,ylab="",  cex.axis=0.8,, xaxt="n")
lines(delta[1: time_exp_begin] ~days[1: time_exp_begin], col=1)
lines(delta[time_exp_begin:time-1] ~days[time_exp_begin:time-1], col=2)
mtext("∂13C-POC", side=2,line=4,las=0, cex=0.7)

A<-C13_DECOMP/DECOMP
delta = (((A/(1-A))/0.0112372)-1)*1000
#delta=C13_DECOMP
plot(delta ~days, col=0,ylab="",  cex.axis=0.8, , xaxt="n")
lines(delta[1: time_exp_begin] ~days[1: time_exp_begin], col=1)
lines(delta[time_exp_begin:time-1] ~days[time_exp_begin:time-1], col=2)
mtext("∂13C-DECOMP", side=2,line=4,las=0, cex=0.7)

A<-C13_PHYTO/PHYTO
delta = (((A/(1-A))/0.0112372)-1)*1000
#delta=C13_PHYTO
plot(delta ~days, col=0,ylab="",  cex.axis=0.8, xlab="days")
lines(delta[2: time_exp_begin] ~days[2: time_exp_begin], col=1)
lines(delta[time_exp_begin:time-1] ~days[time_exp_begin:time-1], col=2)
mtext("∂13C-PHYTO", side=2,line=4,las=0, cex=0.7)

###########################################################Export the results#####################################################

#write.table(out_model_G,'out_modelG.txt',row.names=FALSE,col.names=FALSE)
#write.table(out_model_W,'out_modelW.txt',row.names=FALSE,col.names=FALSE)

#write.table(out_model_G_temp_inc ,'out_modelG.txt',row.names=FALSE,col.names=FALSE)
#write.table(out_model_W_temp_inc ,'out_modelW.txt',row.names=FALSE,col.names=FALSE)

#write.table(out_model_G_CO2_inc ,'out_modelG.txt',row.names=FALSE,col.names=FALSE)
#write.table(out_model_W_CO2_inc ,'out_modelW.txt',row.names=FALSE,col.names=FALSE)

#write.table(out_model_G_CO2_and_temp_inc ,'out_modelG.txt',row.names=FALSE,col.names=FALSE)
#write.table(out_model_W_CO2_and_temp_inc ,'out_modelW.txt',row.names=FALSE,col.names=FALSE)

###########################################################Some metrics calculated#################################################
#n<-48
#msd_all_data<-sum((ABCDout-OBS1234)^2)/n
##Kobayshi Approach (Kobayashi and Salam, 2000)
#sb_all_data<-(mean(ABCDout)-mean(OBS1234))^2
#MSV<-sum(((ABCDout-mean(ABCDout))-(OBS1234-mean(OBS1234)))^2)/n
#SDS<-sqrt(sum((ABCDout-mean(ABCDout))^2)/n)
#SDM<-sqrt(sum((OBS1234-mean(OBS1234))^2)/n)
#sdsd_all_data<-(SDS-SDM)^2
#r<-(sum((ABCDout-mean(ABCDout))*(OBS1234-mean(OBS1234)))/n)/(SDS*SDM)
#MSV_2<-(SDS-SDM)^2+2*SDS*SDM*(1-r)
#cs_all_data<-2*SDS*SDM*(1-r)
#
##Gauch et al (2003) approach based on Kobayashi and Salam (2000), SB is the same in both cases
#b<-(sum((ABCDout-mean(ABCDout))*(OBS1234-mean(OBS1234))))/(sum((ABCDout-mean(ABCDout))^2))
#nu_all_data<-((1-b)^2)*(1/n)*(sum((ABCDout-mean(ABCDout))^2))
#r_Gauch<-((sum((ABCDout-mean(ABCDout))*(OBS1234-mean(OBS1234))))^2)/((sum((ABCDout-mean(ABCDout))^2))*(sum((OBS1234-mean(OBS1234))^2)))
#lc_all_data<-(1-r_Gauch)*(1/n)*(sum((OBS1234-mean(OBS1234))^2))
#
#print("All data")
#print("MSD")
#print(msd_all_data)
#print("SB")
#print(sb_all_data)
#print("NU")
#print(nu_all_data)
#print("LC")
#print(lc_all_data)
#print("BIC")
#BIC<-log(msd_all_data)*n+log(n)*nb_param
#print(BIC)



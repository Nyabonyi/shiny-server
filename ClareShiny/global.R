# library(rsconnect)
library(readxl)
library(shiny)
library(lubridate)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(curl)



Raw_Data<-read_excel("FP Mar-Sept 2018.xlsx",sheet="Raw Data1")
Raw_Data$reported<-as.Date(Raw_Data$reported,format = "%m,%d,%y")
#newData <- subset(Raw_Data, (reported >= as.Date(start) & reported <= as.Date(end)))

#Eligible_women<-summarise(newData,  Eligible_Women = n_distinct(patient_id))
#Raw_Data2<-subset(Raw_Data, (in_the_household == 'yes' & pregnant == F ))
#FP_clients<-summarise(Raw_Data2,  FP_Clients = n_distinct(patient_id))

#Preg_Status <- Raw_Data %>%
# group_by(pregnant) %>% summarise(value = n_distinct(patient_id))
# 
# Referral_Status <- Raw_Data %>% group_by(is_referral_case) %>% summarise(value = n_distinct(patient_id)) ##requires filtering
# Raw_Data4<-subset(Raw_Data, (in_the_household == 'yes' & is_pregnant == F))
# Modern_FP <- Raw_Data4 %>% group_by(using_modern_fp) %>% summarise(value = n_distinct(patient_id)) 
# ####Unmet need for FP
# Raw_Data5<-subset(Raw_Data, (in_the_household == 'yes' & is_pregnant == F & using_modern_fp=='no'))
# Unmet_Need <- Raw_Data5 %>% group_by(take_on_switch) %>% summarise(value = n_distinct(patient_id))%>% na.omit()
# 
# ####Reasons not take/switch
# # Raw_Data6<-subset(Raw_Data, (in_the_household == 'yes' & is_pregnant == F & Using_Modern_FP=='no' & take_on_switch=='no'))
# # Reasons <- Raw_Data6 %>% group_by(g_reason_not_switch) %>% summarise(value = n_distinct(patient_id))
# 
# 
# #####Referral cases with no risk factors
# ####Reasons not take/switch
# # Raw_Data7<-subset(Raw_Data, (in_the_household == 'yes' & is_pregnant == F & Using_Modern_FP=='no' & take_on_switch=='yes' & Referral_case ==T))
# # Referrals <- Raw_Data7  %>% summarise(value = n_distinct(patient_id))
# 
# ##########Methods currently used at point of registration
# Raw_Data7<-subset(Raw_Data, (in_the_household == 'yes' & is_pregnant == F & using_modern_fp=='yes'))
Methods_Used <- subset(Raw_Data, 
                       patient_age_in_years >= 15 & patient_age_in_years <=49 &
                         in_the_household == TRUE & is_pregnant == FALSE & using_modern_fp == TRUE) %>% 
  group_by(current_method) %>% summarise(value = n_distinct(patient_id))

Methods_Administered <- subset(Raw_Data, 
                               patient_age_in_years >= 15 & patient_age_in_years <=49 &
                                 in_the_household == TRUE & is_pregnant == FALSE & take_on_switch == TRUE) %>% 
  group_by(fp_method_administered) %>% summarise(value = n_distinct(patient_id))



###short term users
Short_Term<-read_excel("FP Mar-Sept 2018.xlsx",sheet="FP Short Term Users1")
str(Short_Term)
####Reasons not take/switch
Short_Term1<-subset(Short_Term, (pregnant == F))
Short_Term1_No<-summarise(Short_Term1,  Short_Term1_No = n_distinct(patient_id))
Short_Users <- Short_Term1 %>% group_by(has_side_effects ) %>% summarise(value = n_distinct(patient_id))
Short_Term1<-subset(Short_Term, (pregnant == F & has_side_effects =='yes'))%>% group_by(side_effects ) %>% summarise(value = n_distinct(patient_id))

Short_Users$value = Short_Users$value / sum(Short_Users$value)

Short_Users$ymax = cumsum(round(Short_Users$value,1))
Short_Users$ymin = c(0, head(Short_Users$ymax, n=-1))


###Prospective users
Prospective<-read_excel("FP Mar-Sept 2018.xlsx",sheet="Prospective User Raw1")
str(Prospective)
####Reasons not take/switch
Prospective1<-subset(Prospective, (pregnant == F))
Prospective_No<-summarise(Prospective1,  Prospective_No = n_distinct(patient_id)) %>% na.omit()

Prospective_Users <- Prospective1 %>% group_by(take_on_switch ) %>% summarise(value = n_distinct(patient_id)) %>% na.omit()

Method1<-subset(Prospective, (pregnant == F & take_on_switch == 'yes'))%>% group_by(new_fp_method ) %>% summarise(value = n_distinct(patient_id))%>% na.omit()

Prospective_Users$value = Prospective_Users$value / sum(Prospective_Users$value)

Prospective_Users$ymax = cumsum(round(Prospective_Users$value,1))
Prospective_Users$ymin = c(0, head(Prospective_Users$ymax, n=-1))

# ###Pregnancy Registration
Preg_Reg<-read_excel("FP Mar-Sept 2018.xlsx",sheet="Pregnancy registration")
Preg_Reg$reported1<-as.Date(Preg_Reg$reported1,format = "%m,%d,%y")
####Reasons not take/switch
Preg_Reg1<-subset(Preg_Reg, (pregnant == T))
Preg_Reg1_No<-summarise(Preg_Reg1,  Preg_Reg1_No= n_distinct(patient_id))
LLIN_Users <- Preg_Reg1 %>% group_by(llin ) %>% summarise(value = n_distinct(patient_id))
Gone_ANC <- Preg_Reg1 %>% group_by(gone_for_anc) %>% summarise(value = n_distinct(patient_id))%>% na.omit()
TT <- Preg_Reg1 %>% group_by(tt_immunization) %>% summarise(value = n_distinct(patient_id))%>% na.omit()


blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )
# 
# ######Pregnancy status of all eligible women
Preg_Status <- subset(Raw_Data,
                      patient_age_in_years >= 15 & patient_age_in_years <=49) %>%  group_by(is_pregnant) %>% summarise(value = n_distinct(patient_id))
Preg_Status$value =Preg_Status$value / sum( Preg_Status$value)

Preg_Status$ymax = cumsum(round( Preg_Status$value,1))
Preg_Status$ymin = c(0, head(Preg_Status$ymax, n=-1))

#####Using or not using modern FP of all eligible women in HH
Modern_FP <- subset(Raw_Data, 
                    patient_age_in_years >= 15 & patient_age_in_years <=49 &
                      in_the_household == TRUE & is_pregnant == FALSE & used_modern_fp == TRUE) %>% 
  group_by(using_modern_fp) %>% summarise(value = n_distinct(patient_id)) 
Modern_FP$value = Modern_FP$value / sum(Modern_FP$value)

Modern_FP$ymax = cumsum(round(Modern_FP$value,1))
Modern_FP$ymin = c(0, head(Modern_FP$ymax, n=-1))

# ########Unmet need for FP
# Unmet_Need$value = Unmet_Need$value / sum(Unmet_Need$value)
# 
# Unmet_Need$ymax = cumsum(round(Unmet_Need$value,1))
# Unmet_Need$ymin = c(0, head(Unmet_Need$ymax, n=-1))
# 
# #########Pregnancy Registration
LLIN_Users$value = LLIN_Users$value / sum(LLIN_Users$value)

LLIN_Users$ymax = cumsum(round(LLIN_Users$value,1))
LLIN_Users$ymin = c(0, head(LLIN_Users$ymax, n=-1))

Gone_ANC$value = Gone_ANC$value / sum(Gone_ANC$value)

Gone_ANC$ymax = cumsum(round(Gone_ANC$value,1))
Gone_ANC$ymin = c(0, head(Gone_ANC$ymax, n=-1))

TT$value = TT$value / sum(TT$value)

TT$ymax = cumsum(round(TT$value,1))
TT$ymin = c(0, head(TT$ymax, n=-1))


# ###Posnatal Care
PNC<-read_excel("FP Mar-Sept 2018.xlsx",sheet="PNC1")
# ####Reasons not take/switch
# ####Mothers Information
Followed_Preg<-subset(PNC, (follow_up_count == 1))
Mothers<-summarise(Followed_Preg,  Mothers= n_distinct(patient_id))

Delivery_Outcome <- Followed_Preg %>% group_by(display_delivery_outcome ) %>% summarise(value = n_distinct(patient_id)) %>% na.omit()
Delivery_OutcomeNo<-summarise(Followed_Preg,  Prospective_No = n_distinct(patient_id))  

Del_Outcome <- Followed_Preg %>%
  group_by(display_delivery_outcome ) %>%
  count() %>% na.omit() %>%
  ungroup() %>%
  mutate(per=`n`/sum(`n`)) %>%
  arrange(desc(display_delivery_outcome ))
Del_Outcome$label <- scales::percent(Del_Outcome$per)


HFDelivery<-subset(Followed_Preg, (display_delivery_outcome != 'Miscarriage'))
HF_Delivery1 <- HFDelivery %>% group_by(health_facility_delivery) %>% summarise(value = n_distinct(patient_id))
HF_Delivery1$value = HF_Delivery1$value / sum(HF_Delivery1$value)
HF_Delivery1$ymax = cumsum(round(HF_Delivery1$value,1))
HF_Delivery1$ymin = c(0, head(HF_Delivery1$ymax, n=-1))

ANC_Visit <- Followed_Preg %>% group_by(anc_visits_count) %>% summarise(value = n_distinct(patient_id))

Delivery_Breastfed<-subset(Followed_Preg, (display_delivery_outcome == 'Live Birth'))
Breast_Fed <-Delivery_Breastfed %>% group_by(delivery_breastfed) %>% summarise(value = n_distinct(patient_id))
HF_Delivery1$value = HF_Delivery1$value / sum(HF_Delivery1$value)
HF_Delivery1$ymax = cumsum(round(HF_Delivery1$value,1))
HF_Delivery1$ymin = c(0, head(HF_Delivery1$ymax, n=-1))

# ##Immunization
# ZeroWeeks<-subset(PNC, (reported1 == "Sep 2018" & child_age_in_weeks==0 & display_delivery_outcome == 'Live Birth'))
# Zeroweeks1<-summarise(ZeroWeeks,  Zeroweeks1= n_distinct(patient_id))
# Received_Vaccine<-ZeroWeeks %>% group_by(g_received_vaccines) %>% summarise(value = n_distinct(patient_id))
# Received_Vaccine$value = Received_Vaccine$value / sum(Received_Vaccine$value)
# Received_Vaccine$ymax = cumsum(round(Received_Vaccine$value,1))
# Received_Vaccine$ymin = c(0, head(Received_Vaccine$ymax, n=-1))
# 
# Immun_Updated<-subset(PNC, (reported1 == "Sep 2018" & child_age_in_weeks==0 & 
#                               display_delivery_outcome == 'Live Birth' & g_received_vaccines == 'yes'))
# Immun_Updated1<-Immun_Updated %>% group_by(g_immunization_updated) %>% summarise(value = n_distinct(patient_id))
# Immun_Updated1$value = Immun_Updated1$value / sum(Immun_Updated1$value)
# Immun_Updated1$ymax = cumsum(round(Immun_Updated1$value,1))
# Immun_Updated1$ymin = c(0, head(Immun_Updated1$ymax, n=-1))
# 
# Immun<-read_excel("FP Mar-Sept 2018.xlsx",sheet="U1 Immun Follow Up")
# 
# Three_months<-subset(Immun, (reported1 == "Sep 2018" & patient_age_in_months==3))
# Three_months1<-summarise(Three_months,  Three_months= n_distinct(patient_id))
# Received_Vaccine3<-Three_months %>% group_by(g_received_vaccines) %>% summarise(value = n_distinct(patient_id))
# Received_Vaccine3$value = Received_Vaccine3$value / sum(Received_Vaccine3$value)
# Received_Vaccine3$ymax = cumsum(round(Received_Vaccine3$value,1))
# Received_Vaccine3$ymin = c(0, head(Received_Vaccine3$ymax, n=-1))
# 
# Immun_Updated3<-Three_months %>% group_by(g_immunization_updated) %>% summarise(value = n_distinct(patient_id))
# Immun_Updated3$value = Immun_Updated3$value / sum(Immun_Updated3$value)
# Immun_Updated3$ymax = cumsum(round(Immun_Updated3$value,1))
# Immun_Updated3$ymin = c(0, head(Immun_Updated3$ymax, n=-1))
# 
# six_months<-subset(Immun, (reported1 == "Sep 2018" & patient_age_in_months==6))
# six_months1<-summarise(six_months,  six_months= n_distinct(patient_id))
# Received_Vaccine6<-six_months %>% group_by(g_received_vaccines) %>% summarise(value = n_distinct(patient_id))
# Received_Vaccine6$value = Received_Vaccine6$value / sum(Received_Vaccine6$value)
# Received_Vaccine6$ymax = cumsum(round(Received_Vaccine6$value,1))
# Received_Vaccine6$ymin = c(0, head(Received_Vaccine6$ymax, n=-1))
# 
# Immun_Updated6<-six_months %>% group_by(g_immunization_updated) %>% summarise(value = n_distinct(patient_id))
# Immun_Updated6$value = Immun_Updated6$value / sum(Immun_Updated6$value)
# Immun_Updated6$ymax = cumsum(round(Immun_Updated6$value,1))
# Immun_Updated6$ymin = c(0, head(Immun_Updated6$ymax, n=-1))
# 
# 
# nine_months<-subset(Immun, (reported1 == "Sep 2018" & patient_age_in_months==9))
# nine_months1<-summarise(nine_months,  nine_months= n_distinct(patient_id))
# Received_Vaccine9<-nine_months %>% group_by(g_received_vaccines) %>% summarise(value = n_distinct(patient_id))
# Received_Vaccine9$value = Received_Vaccine9$value / sum(Received_Vaccine9$value)
# Received_Vaccine9$ymax = cumsum(round(Received_Vaccine9$value,1))
# Received_Vaccine9$ymin = c(0, head(Received_Vaccine9$ymax, n=-1))
# 
# nine_months1<-subset(Immun, (reported1 == "Sep 2018" & patient_age_in_months==9 & g_received_vaccines == 'yes'))
# Immun_Updated9<-nine_months1 %>% group_by(g_immunization_updated) %>% summarise(value = n_distinct(patient_id))
# Immun_Updated9$value = Immun_Updated9$value / sum(Immun_Updated9$value)
# Immun_Updated9$ymax = cumsum(round(Immun_Updated9$value,1))
# Immun_Updated9$ymin = c(0, head(Immun_Updated9$ymax, n=-1))
# 
# eighteen_months<-subset(Immun, (reported1 == "Sep 2018" & patient_age_in_months==18))
# eighteen_months1<-summarise(eighteen_months,  eighteen_months= n_distinct(patient_id))
# Received_Vaccine18<-eighteen_months %>% group_by(g_received_vaccines) %>% summarise(value = n_distinct(patient_id))
# Received_Vaccine18$value = Received_Vaccine18$value / sum(Received_Vaccine9$value)
# Received_Vaccine18$ymax = cumsum(round(Received_Vaccine18$value,1))
# Received_Vaccine18$ymin = c(0, head(Received_Vaccine18$ymax, n=-1))
# 
# eighteen_months1<-subset(Immun, (reported1 == "Sep 2018" & patient_age_in_months==18 & g_received_vaccines == 'yes'))
# Immun_Updated18<-eighteen_months1 %>% group_by(g_immunization_updated) %>% summarise(value = n_distinct(patient_id))
# Immun_Updated18$value = Immun_Updated18$value / sum(Immun_Updated18$value)
# Immun_Updated18$ymax = cumsum(round(Immun_Updated18$value,1))
# Immun_Updated18$ymin = c(0, head(Immun_Updated18$ymax, n=-1))
# 
# 
# 

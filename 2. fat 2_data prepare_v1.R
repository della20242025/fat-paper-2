setwd("/labs/DiabetesPopulationHealth/data/dwang/data")
library(tidyverse)
library(purrr)


#############    Import data #######################################
load ("/labs/DiabetesPopulationHealth/data/dwang/data/ukb_sub_mri_20251023.rda",v=TRUE)
ukb_sub_mri
df<-ukb_sub_mri
var<-dput(names(df))
var[which(grepl("f23284",var)==TRUE)]

#############    Define basic info #######################################
df<-df %>% mutate(date_death=date_of_death_f40000_0_0,
                  date_loss=date_lost_to_followup_f191_0_0,
                  sex=sex_f31_0_0,
                  dob=year_of_birth_f34_0_0,
                  eth=if_else(is.na(ethnic_background_f21000_2_0)==TRUE,ethnic_background_f21000_0_0,ethnic_background_f21000_2_0),
                  
                  date=date_of_attending_assessment_centre_f53_2_0,
                  center=uk_biobank_assessment_centre_f54_2_0,
                  age=age_when_attended_assessment_centre_f21003_2_0,
                  age_r1=age_when_attended_assessment_centre_f21003_3_0,
                  preg=pregnant_f3140_2_0,
                  
                  condi_count=number_of_selfreported_noncancer_illnesses_f135_2_0,
                  health_sr=overall_health_rating_f2178_2_0,
                  
                  #menopause
                  mpause=case_when(had_menopause_f2724_2_0=="Yes"~1,had_menopause_f2724_2_0=="No"~0),
                  age_mpause=if_else(age_at_menopause_last_menstrual_period_f3581_2_0>0,age_at_menopause_last_menstrual_period_f3581_2_0,NA),
                  age_mnar=if_else(age_when_periods_started_menarche_f2714_2_0>0,age_when_periods_started_menarche_f2714_2_0,NA)
)

df$age_cal<- as.numeric(substr(df$"date_of_attending_assessment_centre_f53_2_0",0,4))-as.numeric(df$"year_of_birth_f34_0_0")#similiar to self-report age
df$age_cal_r1<- as.numeric(substr(df$"date_of_attending_assessment_centre_f53_3_0",0,4))-as.numeric(df$"year_of_birth_f34_0_0")#similiar to self-report age
df$year<-as.numeric(substring(df$date,1,4))

df<-df%>% mutate( 
  ##(NB this makes some assumptions i.e. British is white European, and does not include mixed, Chinese or other Asian)
  eth_4=case_when(eth %in% c("White","British","Irish","Any other white background")~"White",
                  eth %in% c("Asian or Asian British","Indian","Pakistani","Bangladeshi",
                             "Any other Asian background","Chinese")~"Asian",   
                  eth %in% c("Black or Black British","Caribbean","African","Any other Black background")~"African",  
                  eth %in% c("Mixed","White and Black Caribbean","White and Black African","White and Asian","Any other mixed background",
                             "Other ethnic group")~"Mixed"),
  
  eth_2=case_when(eth %in% c("White","British","Irish","Any other white background")~"White",
                  eth %in% c("Asian or Asian British","Indian","Pakistani","Bangladeshi",
                             "Black or Black British","Caribbean","African","Any other Black background",
                             "Mixed",
                             "White and Black Caribbean","White and Black African","White and Asian","Any other mixed background",
                             "Any other Asian background","Chinese","Other ethnic group")~"Non-white"))

df$dbp<-rowMeans(df[, c("diastolic_blood_pressure_automated_reading_f4079_2_0", "diastolic_blood_pressure_automated_reading_f4079_2_1")], na.rm = TRUE)
df$sbp<-rowMeans(df[, c("systolic_blood_pressure_automated_reading_f4080_2_0", "systolic_blood_pressure_automated_reading_f4080_2_1")], na.rm = TRUE)



#############    Define family history##########
var[which(grepl("f20107_2|20110_2|20111_2",var)==TRUE)]
table(df$illnesses_of_father_f20107_2_2)

df$dia_fh_f<-0
for (i in 0:9) {
  df[grepl("Prefer not to answer|Do not know",df[, paste0("illnesses_of_father_f20107_2_", i)])== TRUE,"dia_fh_f"]<-NA
  df[which(df[, paste0("illnesses_of_father_f20107_2_", i)]== "Diabetes"),"dia_fh_f"]<-1
}

df$dia_fh_m<-0
for (i in 0:10) {
  df[grepl("Prefer not to answer|Do not know",df[, paste0("illnesses_of_mother_f20110_2_", i)])== TRUE,"dia_fh_m"]<-NA
  df[which(df[, paste0("illnesses_of_mother_f20110_2_", i)]== "Diabetes"),"dia_fh_m"]<-1
}

df$dia_fh_s<-0
for (i in 0:11) {
  df[grepl("Prefer not to answer|Do not know",df[, paste0("illnesses_of_siblings_f20111_2_", i)])== TRUE,"dia_fh_s"]<-NA
  df[which(df[, paste0("illnesses_of_siblings_f20111_2_", i)]== "Diabetes"),"dia_fh_s"]<-1
}

df <- df %>% mutate(dia_fh = case_when(dia_fh_f==1 | dia_fh_m==1 | dia_fh_s==1~1,
                                       dia_fh_f==0 | dia_fh_m==0 | dia_fh_s==0~0))

#############    Define SES##############
df<-df %>% mutate(inc=average_total_household_income_before_tax_f738_2_0,
                  mdi_eng=cut(index_of_multiple_deprivation_england_f26410_0_0,
                              breaks=quantile(index_of_multiple_deprivation_england_f26410_0_0,probs=seq(0,1,1/4),na.rm=TRUE),
                              include.lowest = TRUE,
                              labels=c("q1","q2","q3","q4")),
                  mdi_wal=cut(index_of_multiple_deprivation_wales_f26426_0_0,
                              breaks=quantile(index_of_multiple_deprivation_wales_f26426_0_0,probs=seq(0,1,1/4),na.rm=TRUE),
                              include.lowest = TRUE,
                              labels=c("q1","q2","q3","q4")),
                  mdi_sco=cut(index_of_multiple_deprivation_scotland_f26427_0_0,
                              breaks=quantile(index_of_multiple_deprivation_scotland_f26427_0_0,probs=seq(0,1,1/4),na.rm=TRUE),
                              include.lowest = TRUE,
                              labels=c("q1","q2","q3","q4")))

df$mdi<-df$mdi_eng
df$mdi<-if_else(is.na(df$mdi)==TRUE,df$mdi_wal,df$mdi)
df$mdi<-if_else(is.na(df$mdi)==TRUE,df$mdi_sco,df$mdi)


#############    Define lifestyle##############
#diet
temp<-var[grepl("f1289_2|f1299_2|f1309_2|f1319_2|f1438_2|f1458_2",var)==TRUE]
for (i in 1:length(temp)){
  df[,temp[i]]<-if_else(df[,temp[i]]==-10,0.5, df[,temp[i]])
  df[,temp[i]]<-if_else(df[,temp[i]]<0,NA, df[,temp[i]])
}

df$veg<-df$salad_raw_vegetable_intake_f1299_2_0+df$cooked_vegetable_intake_f1289_2_0
df$veg<-if_else(is.na(df$veg)==TRUE & is.na(df$salad_raw_vegetable_intake_f1299_2_0)==FALSE,df$salad_raw_vegetable_intake_f1299_2_0,df$veg)
df$veg<-if_else(is.na(df$veg)==TRUE & is.na(df$cooked_vegetable_intake_f1289_2_0)==FALSE,df$cooked_vegetable_intake_f1289_2_0,df$veg)
median(df$veg,na.rm=TRUE)#4

df$frt<-df$fresh_fruit_intake_f1309_2_0+df$dried_fruit_intake_f1319_2_0
df$frt<-if_else(is.na(df$frt)==TRUE & is.na(df$fresh_fruit_intake_f1309_2_0)==FALSE,df$fresh_fruit_intake_f1309_2_0,df$frt)
df$frt<-if_else(is.na(df$frt)==TRUE & is.na(df$dried_fruit_intake_f1319_2_0)==FALSE,df$dried_fruit_intake_f1319_2_0,df$frt)
median(df$frt,na.rm=TRUE)#3

df<-df %>% mutate(diet_1=if_else(veg>=9, 1,0),#VEG
                  diet_2=if_else(frt>=3, 1,0),#FRT
                  diet_3=if_else(oily_fish_intake_f1329_2_0 %in% c("2-4 times a week","5-6 times a week","Once or more daily")|
                                   nonoily_fish_intake_f1339_2_0 %in% c("2-4 times a week","5-6 times a week","Once or more daily")|
                                   oily_fish_intake_f1329_2_0 == "Once a week"|nonoily_fish_intake_f1339_2_0== "Once a week",1,0),#FISH>=2/WK
                  diet_4=if_else(processed_meat_intake_f1349_2_0 %in% c("Never","Less than once a week"," Once a week"),1,0),#processed meat <=1/WK
                  diet_5=if_else(beef_intake_f1369_2_0 %in% c("Never","Less than once a week"," Once a week")|
                                   lambmutton_intake_f1379_2_0 %in% c("Never","Less than once a week"," Once a week")|
                                   pork_intake_f1389_2_0 %in% c("Never","Less than once a week"," Once a week"),1,0),#red meat <=2/WK
                  diet_6=if_else(bread_type_f1448_2_0=="Wholemeal or wholegrain" & bread_intake_f1438_2_0>=3|
                                   grepl("Bran cereal|Oat cereal|Muesli",cereal_type_f1468_2_0)==TRUE & cereal_intake_f1458_2_0>=3,1,0),
                  diet_7=if_else(bread_type_f1448_2_0 %in% c("White","Brown","Other type of bread") & bread_intake_f1438_2_0<=2|
                                   grepl("Biscuit|Other",cereal_type_f1468_2_0)==TRUE & cereal_intake_f1458_2_0<=2,1,0))

lapply(df[,c("diet_1","diet_2","diet_3","diet_4","diet_5","diet_6","diet_7")],table)                 
nrow(df[which(df$beef_intake_f1369_2_0 =="Once a week" & df$lambmutton_intake_f1379_2_0 ==" Once a week" & df$pork_intake_f1389_2_0 ==" Once a week"),])

df<-df %>% mutate(diet_score=diet_1+diet_2+diet_3+diet_4+diet_5+diet_6+diet_7,
                  
                  smo=smoking_status_f20116_2_0,
                  alc=alcohol_drinker_status_f20117_2_0,
                  
                  pa=ipaq_activity_group_f22032_2_0,
                  pa_bl=ipaq_activity_group_f22032_0_0,
                  pa_m_dur=if_else(duration_of_moderate_activity_f894_2_0>0,duration_of_moderate_activity_f894_2_0,NA),
                  pa_v_dur=if_else(duration_of_vigorous_activity_f914_2_0>0,duration_of_vigorous_activity_f914_2_0,NA),
                  pa_m_day=if_else(number_of_daysweek_of_moderate_physical_activity_10_minutes_f884_2_0>0,number_of_daysweek_of_moderate_physical_activity_10_minutes_f884_2_0,NA),
                  pa_v_day=if_else(number_of_daysweek_of_vigorous_physical_activity_10_minutes_f904_2_0>0,number_of_daysweek_of_vigorous_physical_activity_10_minutes_f904_2_0,NA),
                  pa_walk=usual_walking_pace_f924_2_0)

df<-df%>% mutate(smo_b=case_when(smo=="Never"~"0",grepl("Previous|Current",smo)==TRUE~"1"),
                 alc_b=case_when(alc=="Never"~"0",grepl("Previous|Current",alc)==TRUE~"1"))



#############    Define body size and fat #######################################
var[which(grepl("f23289",var)==TRUE)]
df<-df %>% mutate(hei=if_else(is.na(standing_height_f50_2_0)==TRUE,standing_height_f50_0_0,standing_height_f50_2_0),
                  wei=weight_f21002_2_0,
                  wc=waist_circumference_f48_2_0,
                  hc=hip_circumference_f49_2_0, 
                  
                  wei_bl=weight_f21002_0_0,
                  bmi_bl=body_mass_index_bmi_f21001_0_0,
                  bmi_fl=body_mass_index_bmi_f21001_2_0,
                  
                  #BIA
                  fp_bia=body_fat_percentage_f23099_2_0,
                  tf_bia=whole_body_fat_mass_f23100_2_0 ,
                  trunk_bia=trunk_fat_mass_f23128_2_0 ,
                  leg_bia_l=leg_fat_mass_left_f23116_2_0 ,
                  leg_bia_r=leg_fat_mass_right_f23112_2_0 ,
                  arm_bia_l=arm_fat_mass_left_f23124_2_0 ,
                  arm_bia_r=arm_fat_mass_right_f23120_2_0,
                  
                  #DXA
                  fp_dxa=total_tissue_fat_percentage_f23281_2_0,
                  tf_dxa=total_fat_mass_f23278_2_0 /1000,
                  trunk_dxa=trunk_fat_mass_f23284_2_0/1000,
                  and_dxa=android_fat_mass_f23245_2_0/1000,
                  gyn_dxa=gynoid_fat_mass_f23262_2_0/1000,
                  leg_dxa=legs_fat_mass_f23274_2_0/2000,
                  arm_dxa=arms_fat_mass_f23257_2_0/2000,
                  vat_dxa_m=vat_visceral_adipose_tissue_mass_f23288_2_0/1000,
                  vat_dxa_v=vat_visceral_adipose_tissue_volume_f23289_2_0/1000,
                  
                  
                  #MRI,cat 149
                  tf_mri=total_adipose_tissue_volume_f22415_2_0,
                  vat_mri=visceral_adipose_tissue_volume_vat_f22407_2_0,
                  asat_mri=abdominal_subcutaneous_adipose_tissue_volume_asat_f22408_2_0,
                  livf_mri=fr_liver_pdff_mean_f24352_2_0,
                  
                  mfi_mri=muscle_fat_infiltration_f22435_2_0,
                  mfi_mri_al=anterior_thigh_muscle_fat_infiltration_mfi_left_f24353_2_0,
                  mfi_mri_ar=anterior_thigh_muscle_fat_infiltration_mfi_right_f24354_2_0,
                  mfi_mri_pl=posterior_thigh_muscle_fat_infiltration_mfi_left_f23355_2_0,
                  mfi_mri_pr=posterior_thigh_muscle_fat_infiltration_mfi_right_f23356_2_0,
                  
                  tm_mri=total_thigh_fatfree_muscle_volume_f22409_2_0,
                  tm_mri_al=anterior_thigh_fatfree_muscle_volume_left_f22405_2_0,
                  tm_mri_ar=anterior_thigh_fatfree_muscle_volume_right_f22403_2_0,
                  tm_mri_pl=posterior_thigh_fatfree_muscle_volume_left_f22406_2_0,
                  tm_mri_pr=posterior_thigh_fatfree_muscle_volume_right_f22404_2_0,
                  
                  #MRI,cat 158
                  vat_mri_c1=visceral_fat_volume_f21085_2_0,
                  asat_mri_c1=subcutaneous_fat_volume_f21086_2_0,
                  livf_mri_c1=liver_pdff_fat_fraction_f21088_2_0,
                  panf_mri_c1=pancreas_pdff_fat_fraction_f21090_2_0
)

df<-df %>% mutate(hei_r1=if_else(is.na(standing_height_f50_3_0)==TRUE,standing_height_f50_0_0,standing_height_f50_3_0),
                  wei_r1=weight_f21002_3_0,
                  bmi_r1=body_mass_index_bmi_f21001_3_0,
                  wc_r1=waist_circumference_f48_3_0,
                  hc_r1=hip_circumference_f49_3_0,
                  
                  #BIA
                  fp_bia_r1=body_fat_percentage_f23099_3_0,
                  tf_bia_r1=whole_body_fat_mass_f23100_3_0 ,
                  trunk_bia_r1=trunk_fat_mass_f23128_3_0 ,
                  leg_bia_l_r1=leg_fat_mass_left_f23116_3_0 ,
                  leg_bia_r_r1=leg_fat_mass_right_f23112_3_0 ,
                  arm_bia_l_r1=arm_fat_mass_left_f23124_3_0 ,
                  arm_bia_r_r1=arm_fat_mass_right_f23120_3_0,
                  
                  #DXA
                  fp_dxa_r1=total_tissue_fat_percentage_f23281_3_0,
                  tf_dxa_r1=total_fat_mass_f23278_3_0 /1000,
                  trunk_dxa_r1=trunk_fat_mass_f23284_3_0/1000,
                  and_dxa_r1=android_fat_mass_f23245_3_0/1000,
                  gyn_dxa_r1=gynoid_fat_mass_f23262_3_0/1000,
                  leg_dxa_r1=legs_fat_mass_f23274_3_0/2000,
                  arm_dxa_r1=arms_fat_mass_f23257_3_0/2000,
                  vat_dxa_r1=vat_visceral_adipose_tissue_mass_f23288_3_0/1000,
                  
                  #MRI,cat 158
                  vat_mri_c1_r1=visceral_fat_volume_f21085_3_0,
                  asat_mri_c1_r1=subcutaneous_fat_volume_f21086_3_0,
                  livf_mri_c1_r1=liver_pdff_fat_fraction_f21088_3_0,
                  panf_mri_c1_r1=pancreas_pdff_fat_fraction_f21090_3_0
)

lapply(df[,c("sex","eth","eth_2","eth_4","inc","mdi","pa","smo","alc","pa_walk","health_sr")],table)
df[,c("inc","smo","alc","pa_walk","health_sr")]<-
  sapply(df[,c("inc","smo","alc","pa_walk","health_sr")],function(x) gsub("Prefer not to answer|Do not know|None of the above",NA,x))

#############    Define medication by NI#######################
col_names<-colnames(df)
new_col_names<-gsub("^(.*?)_(f[1-9])","\\n_\\2",col_names)
new_col_names<-gsub("n_f","n_",new_col_names)
colnames(df)<-new_col_names
var1<-dput(names(df))   

#########################################################nurse interview
med<-read.csv("med_ni_atc.csv")
unique(med[grepl("B01A",med$atc),]$atc)

med_ins<-"1140883066"
med_met<-unique(c(med[grepl("A10BA",med$atc),]$coding,
                  med[grepl("Metformin",med$drug_name),]$coding,
                  "1140884600","1140874686","1141189090","1141189094","1140921964","1141153138"))
med_su<-unique(c(med[grepl("A10BB",med$atc),]$coding,
                 # med[grepl("A10BD01|A10BD02",med$atc),]$coding,
                 "1140874718","1140874744","1140874746","1141152590","1141156984","1140874646",
                 "1141157284","1140874652","1140874674","1140910566","1140874658","1140874728"))
med_tzd<-unique(c(med[grepl("A10BG|A10BD03",med$atc),]$coding,
                  # med[grepl("A10BD03|A10BD04|A10BD05|A10BD06|A10BD09|A10BD12",med$atc),]$coding,
                  "1141171646","1141171652","1141153254","1141177600","1141177606","1141189090", "1141189094"))
med_glm<-unique(c(med_ins,med_met,med_su,med_tzd,
                  med[grepl("\\|A10|^A10",med$atc),]$coding,
                  "1140868902","1140868908","1140857508","1141173882","1141173786","1141168660"))
med_others<-unique(c(med[grepl("A10BF|A10BG|A10BH|A10BJ|A10BK|A10BX",med$atc),]$coding,
                     "1140868902","1140868908","1140857508","1141173882","1141173786","1141168660"))


med_ane<-med[grepl("\\|B03|^B03",med$atc),]$coding 
med_ppi<-unique(c(med[grepl("A02BC",med$atc),]$coding,
                  1140929012, 1141184174, 1141180462, 1141168822, 1141177526, 1141168824, 
                  1141184176, 1140864752, 1140909578, 1141177532, 1140865634, 1141187060, 
                  1141168590, 1141164616, 1141168584, 1140850960, 1141190552, 1140923688))
med_statin<-c(med[grepl("C10AA",med$atc),]$coding ,
              1141146234, 1141192414, 1140910632, 1140888594, 1140864592, 1141146138, 
              1140861970, 1140888648, 1141195196, 1141192410, 1141188146, 1140861958, 
              1140910652, 1140910654, 1140881748, 1141200040)
med_ace<-med[grepl("C09A",med$atc),]$coding 
med_arb<-med[grepl("C09C",med$atc),]$coding 
med_antihyp<-med[grepl("C09A|C09C",med$atc),]$coding 
med_antipla<-unique(c(med[grepl("\\|B01|^B01",med$atc),]$coding,
                      1140909770, 1140861704, 1141164760, 1140861696, 1140864122, 
                      1140861702, 1140861698, 1140910832, 1140888266,1140861776, 
                      1141167848, 1140861790, 1141168318, 1140861778, 1141167844, 
                      1140911710, 1140861780, 1141168322,1141163328, 1141163324, 1140851930))
med_nsaid<-med[grepl("M01A",med$atc),]$coding 

#ppi, statins
df$med_ins<- "0"
df$med_met<- "0"
df$med_su<- "0"
df$med_glm<- "0"
df$med_others<- "0"

df$med_ane<- "0"

df$med_tzd<- "0"
df$med_ppi<- "0"
df$med_statin<- "0"
df$med_antihyp<- "0"
df$med_antipla<- "0"
df$med_nsaid<- "0"

for (i in 0:47 ){
  
  ins<- df[, paste0("n_20003_2_", i)]%in% med_ins==TRUE
  met <- df[, paste0("n_20003_2_", i)]%in% med_met==TRUE
  su <- df[, paste0("n_20003_2_", i)]%in% med_su==TRUE
  tzd <- df[, paste0("n_20003_2_", i)]%in% med_tzd==TRUE
  glm<- df[, paste0("n_20003_2_", i)]%in% med_glm==TRUE
  others<- df[, paste0("n_20003_2_", i)]%in% med_others==TRUE
  
  ane <- df[, paste0("n_20003_2_", i)]%in% med_ane==TRUE
  
  ppi <- df[, paste0("n_20003_2_", i)]%in% med_ppi==TRUE
  statin <- df[, paste0("n_20003_2_", i)]%in% med_statin==TRUE
  antihyp <- df[, paste0("n_20003_2_", i)]%in% med_antihyp==TRUE
  antipla <- df[, paste0("n_20003_2_", i)]%in% med_antipla==TRUE
  nsaid <- df[, paste0("n_20003_2_", i)]%in% med_nsaid==TRUE
  
  
  df$med_ins[ins] <- "1"
  df$med_met[met] <- "1"
  df$med_su[su] <- "1"
  df$med_tzd[tzd] <- "1"
  df$med_glm[glm] <- "1"
  df$med_others[others] <- "1"
  
  df$med_ane[ane] <- "1"
  
  df$med_ppi[ppi] <- "1"
  df$med_statin[statin] <- "1"
  df$med_antihyp[antihyp] <- "1"
  df$med_antipla[antipla] <- "1"
  df$med_nsaid[nsaid] <- "1"
}
lapply(df[,c("med_ins","med_met","med_su","med_tzd","med_glm","med_others")],table)
lapply(df[,c("med_ane","med_ppi","med_statin","med_antihyp","med_antipla","med_nsaid")],table)#10487,45427,74351,62025,68918,97371


#########################################################touch screen 
#Currentinsuli receipt
df$dm_drug_ins_men<-"0"
df[which(df$n_6177_2_0== "Insulin"|df$n_6177_2_1== "Insulin"|df$n_6177_2_2== "Insulin"),]$dm_drug_ins_men<-"1"
df[which(df$n_6177_2_0 %in% c("Prefer not to answer","Do not know",NA)==TRUE),]$dm_drug_ins_men<-NA

df$dm_drug_ins_women<-"0"
df[rowSums(df[,var1[which(grepl("n_6153_2_",var1)==TRUE)]] == "Insulin", na.rm = TRUE) >0,]$dm_drug_ins_women<-"1"
df[which(df$n_6153_2_0 %in% c("Prefer not to answer","Do not know",NA)==TRUE),]$dm_drug_ins_women<-NA

df$dm_drug_ins_bl_sr<-NA
df[which(df$dm_drug_ins_men=="1" | df$dm_drug_ins_women=="1"),]$dm_drug_ins_bl_sr<-"1"
df[which(df$dm_drug_ins_men=="0" | df$dm_drug_ins_women=="0"),]$dm_drug_ins_bl_sr<-"0"
lapply(df[,c("dm_drug_ins_men", "dm_drug_ins_women", "dm_drug_ins_bl_sr"),],table)

table(df$med_glm,df$dm_drug_ins_bl_sr)
nrow(filter(df,dm_drug_ins_bl_sr=="1" & med_glm=="0"))
df$med_ins[df$dm_drug_ins_bl_sr=="1"]<-"1"
df$med_glm[df$dm_drug_ins_bl_sr=="1"]<-"1"








#############    Define dia by NI########
df$dm_alldm_ni_bl <- NA
df$dm_agediag_alldm_ni_bl <- NA

#####Gestational diabetes 
df$dm_gdm_ni_bl <- NA
df$dm_agediag_gdm_ni_bl <- NA

#####Type 1 diabetes 
df$dm_t1dm_ni_bl <- NA
df$dm_agediag_t1dm_ni_bl <- NA

#####Type 2 diabetes
df$dm_t2dm_ni_bl <- NA
df$dm_agediag_t2dm_ni_bl <- NA

for (i in 0:33) {
  df[which(df[, paste0("n_20002_2_", i)]==1220),"dm_alldm_ni_bl"]<-1
  df[which(df[, paste0("n_20002_2_", i)]==1220),"dm_agediag_alldm_ni_bl"]<- df[which(df[, paste0("n_20002_2_", i)]==1220),paste0("n_20009_0_", i)]
  
  df[which(df[, paste0("n_20002_2_", i)]==1221),"dm_gdm_ni_bl"]<-1
  df[which(df[, paste0("n_20002_2_", i)]==1221),"dm_agediag_gdm_ni_bl"]<- df[which(df[, paste0("n_20002_2_", i)]==1221),paste0("n_20009_0_", i)]
  
  df[which(df[, paste0("n_20002_2_", i)]==1222),"dm_t1dm_ni_bl"]<- 1
  df[which(df[, paste0("n_20002_2_", i)]==1222),"dm_agediag_t1dm_ni_bl"] <- df[which(df[, paste0("n_20002_2_", i)]==1222),paste0("n_20009_0_", i)]
  
  df[which(df[, paste0("n_20002_2_", i)]==1223),"dm_t2dm_ni_bl"] <- 1
  df[which(df[, paste0("n_20002_2_", i)]==1223),"dm_agediag_t2dm_ni_bl"] <- df[which(df[, paste0("n_20002_2_", i)]==1223),paste0("n_20009_0_", i)]
}


#####Non-type-specific / gestational/ type 1/ type 2 diabetes self-report  - from nurse interview
df$dm_anynsgt1t2_ni_bl <-NA
df[which(df$dm_alldm_ni_bl == 1 | df$dm_gdm_ni_bl == 1 | df$dm_t1dm_ni_bl == 1 | df$dm_t2dm_ni_bl == 1),]$dm_anynsgt1t2_ni_bl <-"1"
table(df$dm_anynsgt1t2_ni_bl)
lapply(df[,c("dm_anynsgt1t2_ni_bl","dm_alldm_ni_bl","dm_gdm_ni_bl","dm_t1dm_ni_bl","dm_t2dm_ni_bl")],table)


df$dia_ni<-df$dm_anynsgt1t2_ni_bl
table(df$dia_ni)

table(df$n_4041_0_0)#gdm in ts
df$gdm<-NA
df[which(is.na(df$"n_132202_0_0")==FALSE|df$dm_gdm_ni_bl=="1"|df$n_4041_0_0=="Yes"),]$gdm<-1  #1457
table(df$gdm)


#####Age at diabetes (all types) diagnosis - touchscreen or nurse interview (nurse interview supercedes)
df<-df %>% mutate(dm_agediag_sr_bl=if_else(n_2976_2_0>=0,n_2976_2_0,NA))

df$dm_agedm_ts_or_ni<-df$dm_agediag_alldm_ni_bl
df[which(is.na(df$dm_agediag_alldm_ni_bl) == TRUE & 
           is.na(df$dm_agediag_t1dm_ni_bl) ==TRUE & 
           is.na(df$dm_agediag_t2dm_ni_bl) == TRUE & 
           df$dm_gdmonly_sr_bl %in% c("0",NA)==TRUE),]$dm_agedm_ts_or_ni<-df[which(is.na(df$dm_agediag_alldm_ni_bl) == TRUE & 
                                                                                     is.na(df$dm_agediag_t1dm_ni_bl) ==TRUE & 
                                                                                     is.na(df$dm_agediag_t2dm_ni_bl) == TRUE & 
                                                                                     df$dm_gdmonly_sr_bl %in% c("0",NA)==TRUE),]$dm_agediag_sr_bl
df[which(df$dm_t1dm_ni_bl== "1"),]$dm_agedm_ts_or_ni<-df[which(df$dm_t1dm_ni_bl== "1"),]$dm_agediag_t1dm_ni_bl
df[which(df$dm_t2dm_ni_bl== "1"),]$dm_agedm_ts_or_ni<-df[which(df$dm_t2dm_ni_bl== "1"),]$dm_agediag_t2dm_ni_bl

df[which(is.na(df$dm_agediag_alldm_ni_bl) == TRUE &
           is.na(df$dm_agediag_t1dm_ni_bl) ==TRUE &
           is.na(df$dm_agediag_t2dm_ni_bl) == TRUE &
           is.na(df$dm_agediag_sr_bl)==TRUE),]$dm_agedm_ts_or_ni<-NA

df[which(df$dm_agedm_ts_or_ni<0),]$dm_agedm_ts_or_ni<-NA

range(df$dm_agedm_ts_or_ni,na.rm=TRUE)
sum(!is.na(df$dm_agedm_ts_or_ni))
df$age_diag<-df$dm_agedm_ts_or_ni


#############    Save data#########
dia<-read.csv("dia_sr_mri.csv") 
dia<-dia %>% select("eid", "sr_prob_t2_diabetes_mri",  "sr_prob_t1_diabetes_mri") 

dput(names(df))
df1<-df %>% dplyr::select("center","date","year","date_death","date_loss","preg","eid",
                          "age","age_cal","age_cal_r1","dob","sex", "eth","eth_2","eth_4","mdi","inc","dbp", "sbp","mpause","age_mpause",
                          
                          
                          "diet_score",
                          "smo","smo_b","alc","alc_b",
                          "pa","pa_bl","pa_m_dur","pa_v_dur","pa_m_day","pa_v_day","pa_walk",
                          
                          #medication
                          "med_ppi","med_statin","med_antihyp","med_antipla","med_nsaid",
                          "med_ins","med_met","med_su","med_tzd","med_glm","med_others",
                          
                          #body size and comp
                          "hei", "wei", "wei_bl", "bmi_fl", "bmi_bl", "wc", "hc",
                          "fp_bia", "tf_bia", "trunk_bia","leg_bia_l", "leg_bia_r","arm_bia_l", "arm_bia_r", 
                          "fp_dxa", "tf_dxa", "trunk_dxa","leg_dxa","arm_dxa", "and_dxa","gyn_dxa", "vat_dxa_m", "vat_dxa_v", 
                          "tf_mri", "vat_mri",   "asat_mri",   "livf_mri", "mfi_mri_al", "mfi_mri_ar", "mfi_mri_pl", "mfi_mri_pr",
                          "vat_mri_c1", "asat_mri_c1", "livf_mri_c1", "panf_mri_c1", 
                          
                          #repeated
                          "hei_r1", "wei_r1", "bmi_r1", "wc_r1", "hc_r1", 
                          "fp_bia_r1", "tf_bia_r1", "trunk_bia_r1", "leg_bia_l_r1", "leg_bia_r_r1",  "arm_bia_l_r1", "arm_bia_r_r1", 
                          "fp_dxa_r1", "tf_dxa_r1", "trunk_dxa_r1", "and_dxa_r1", "gyn_dxa_r1", "leg_dxa_r1", "arm_dxa_r1", "vat_dxa_r1", 
                          "vat_mri_c1_r1", "asat_mri_c1_r1", "livf_mri_c1_r1", "panf_mri_c1_r1", 
                          
                          
                          #condi
                          "health_sr", "condi_count",
                          
                          #diabetes related
                          "gdm","dia_ni","age_diag","dia_fh")


df2 <- left_join(df1, dia, by = "eid")
write.csv(df2,file="ukb_mri_for analysis_fat2.csv")#76603
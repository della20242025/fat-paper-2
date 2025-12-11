####run terminal
# srun --cpus-per-task=8 --mem=480000 --pty bash -i
# module avail R
# module load R/openblas/4.2.3
# R

setwd("/labs/DiabetesPopulationHealth/data/dwang/data")
library(tidyverse)

#############    Import UKB#################################
load ("/labs/datasets/ukb55469/R_workspace/240112_ukb_data.rda")
var<-dput(names(ukb_data))
dim(ukb_data)
var[which(grepl("f74_",var)==TRUE)]

#############    Select variables #######################################
df<-ukb_data %>% select(c(
  #basic info
  "eid","sex_f31_0_0","year_of_birth_f34_0_0",
  var[which(grepl("f190_|f191_",var)==TRUE)],#follow up status
  var[which(grepl("f53_|f54_|f21003_|f21000_|f3140_",var)==TRUE)],#center,date,age,eth,pregnant
  
  #SES 
  var[which(grepl("f26410_|f26426_|f26427_|f6138_|f738_",var)==TRUE)],#index,edu,inc
  
  #lifestyle 
  var[grepl("f4079_|f4080_",var)==TRUE],#bp
  var[grepl("f1289_|f1299_|f1309_|f1319_|f1329_|f1339_|f1349_|f1359_|f1369_|f1379_|f1389_|f1438_|f1448_|f1458_|f1468_|f3680_|f1498_|f1508_",var)==TRUE],#diet 
  var[which(grepl("f22032_|f864_|f874_|f884_|f894_|f904_|f914_|f924_",var)==TRUE)],#pa
  var[which(grepl("f20116_|f20160_|f1239_|f1249_|f1259_|f3446_",var)==TRUE)],#smo
  var[which(grepl("f1558_|f1628_|f20117_|f3731",var)==TRUE)],#alc
  var[which(grepl("f1160_|f1170_|f1180_|f1190_|f1200_|f1210_|f1220_",var)==TRUE)],#sleep
  
  #blood and urine biomarkers
  var[which(grepl(paste(paste0("f",  seq(30000, 30300, by = 10), "_"), collapse = "|"),var)==TRUE)],#blood count
  var[which(grepl(paste(paste0("f", 30601:30897, "_"), collapse = "|"),var)==TRUE)],#blood chemistry
  var[which(grepl("f30510_|f30500_|f30505_|f30515_|f30520_|f30525_|f30530_|f30535_|f74_",var)==TRUE)],#urine,fasting 
 
  # body size and composition
  var[which(grepl("f48_|f49_|f50_|f21002_|f21001_",var)==TRUE)],#wc,hc,hei,wei,bmi
  # var[grepl(paste(paste0("f", 21110:21135, "_"), collapse = "|"),var)],#DXA,no variables in current dataset
  var[which(grepl(paste(paste0("f", 23244:23289, "_"), collapse = "|"),var)==TRUE)],#DXA
  var[which(grepl(paste(paste0("f", 23098:23130, "_"), collapse = "|"),var)==TRUE)],#impedance

  var[which(grepl(paste(paste0("f", 21080:21094, "_"), collapse = "|"),var)==TRUE)],#MRI,cat 158
  # var[which(grepl(paste(paste0("f", 21170:21174, "_"), collapse = "|"),var)==TRUE)],#MRI,cat 158,no variables in current dataset
  var[which(grepl(paste(paste0("f", 22403:22436, "_"), collapse = "|"),var)==TRUE)],#MRI,cat 149
  var[which(grepl(paste(paste0("f", 23355:23364, "_"), collapse = "|"),var)==TRUE)],#MRI,cat 149
  var[which(grepl(paste(paste0("f", 24352:24354, "_"), collapse = "|"),var)==TRUE)],#MRI,cat 149
  # var[which(grepl("f40061_",var)==TRUE)],#cat 126, no variables in current dataset
  
  
  # conditions related
  var[which(grepl("f134_|f135_|f2178_|f26413_|f26430_|f26420_|f20107_|f20110_|f20111_",var)==TRUE)],#no.of disease,overall health, health score,fh
  var[grepl("f20002_0_|f20003_0_|f20009_0_|f20002_2_|f20003_2_|f20009_2_",var)==TRUE],#self-report condi,medi
  var[which(grepl("f40000_|f40001_|f40002_|f40007|f40010_",var)==TRUE)],#death 
  var[which(grepl("f40011_|f40012_|f40006_|f40013_|f40005_|f40008_|f40009_",var)==TRUE)],#cancer
  var[which(grepl("f42014_|f42016_|f42026_|f42000_|f42006_|f42008_|f42010_|f42012_|f42018_|f42020_|f42022_|f42024_",var)==TRUE)],#algorithm, asthma,copd,kid,mi,stroke,dementia by alg
  
  var[grepl("f41270_|f41271_|f41280_|f41281_|f41272_|f41273_|f41282_|f41283_",var)==TRUE],#inpatient 
  var[which(grepl("date_n.*_first_reported|date_i.*_first_reported|date_k.*first_reported",var)==TRUE)],#kidney,heart,digest
  var[which(grepl("date.*first_reported.*anaemia|f130632_|f130634_|f130640_",var)==TRUE)],#anemia
  var[which(grepl("date_h.*first_reported|date_g.*first_reported|date_f.*_first_reported.*dementia",var)==TRUE)],#eye,nervous,dementia
  var[which(grepl("f130706_|f130708_|f130710_|f130712_|f130714_|f132202_",var)==TRUE)],#diabetes

  # Diabetes 
  var[which(grepl("f2443_|f4041_|f2976_|f2986_",var)==TRUE)],#ever,gdm,onset age,insulin within one year
  var[which(grepl("f6153_|f6177_",var)==TRUE)],#female, male
  
  #touch screen
  var[grepl("f6179_|f6155_|f2714_|f2724_|f3581_|f2814_",var)==TRUE]#iron related, mineral,vitamin,hormone-related
))

#############    Save data#######################################
ukb_sub_mri<-df%>% filter(is.na(df$"date_of_attending_assessment_centre_f53_2_0")==FALSE)#76603
dim(ukb_sub_mri)
save(ukb_sub_mri, file = "ukb_sub_mri_fat2.rda")







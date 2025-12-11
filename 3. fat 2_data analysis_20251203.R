# Date:2025-12-02

library(tidyverse)
library(tableone)
library(ppcor)
library(rsq)
library(ggpubr)
library(pROC)
library(ggpmisc)
library(irr)

rm(list=ls())
print(R.Version())

setwd("/labs/DiabetesPopulationHealth/data/dwang/data")
data<-read.csv("ukb_mri_for analysis_fat2.csv")#76603
data$leg_bia<-rowMeans(data[,c("leg_bia_l","leg_bia_r")],na.rm = TRUE)
data$arm_bia<-rowMeans(data[,c("arm_bia_l","arm_bia_r")],na.rm = TRUE)


##################################  Data cleaning#############################
#  select rows#########
data1<-data %>%filter(preg %in% c("No",NA))#76593
data1<-data1 %>%filter(eth_2=="White")#73840
data1<-data1 %>% filter(is.na(age_cal)==FALSE & is.na(sex)==FALSE )#73840

data1<-data1 %>% 
  filter(wc>0 & hc>0 & wei>0 & hei>0) %>% 
  filter(fp_bia>0 & tf_bia>0 & trunk_bia>0 & arm_bia>0 & leg_bia>0) #53788
 
data1<-data1 %>% 
  filter(vat_mri_c1>0 & asat_mri_c1>0 & livf_mri_c1>0 & panf_mri_c1>0) %>%
  filter(vat_dxa_m>0 & vat_dxa_v>0) %>%
  filter(fp_dxa>0 & tf_dxa>0 & trunk_dxa>0 & and_dxa>0 & gyn_dxa>0 & arm_dxa>0 & leg_dxa>0)#18803

data1<-data1 %>% filter(hei<=190 & wei<=204)#18622



#  define fat indices#########
#anthropometrics
##identify outliers
temp<-c("wc", "hc","wei","hei")
par(mfrow=c(1,4))
for (i in 1:length(temp)){hist<-hist(data1[,temp[i]],main=temp[i])}#normal dist

range(data1$wc,na.rm=TRUE)
head(sort(data$wc),10)
head(sort(data$wc,decreasing=TRUE),10)

range(data1$hc,na.rm=TRUE)
head(sort(data$hc),10)
head(sort(data$hc,decreasing=TRUE),10)# continuous, no outliers

##define 
data1<-data1 %>% mutate(sex_num=case_when(sex=="Female"~1,sex=="Male"~0),
                        bmi=wei/((hei/100)^2))%>% 
                 mutate( bmi_check=bmi_fl,
                         whr=wc/hc,
                         whtr=wc/hei,
                         
                         wwi=wc/sqrt(wei),
                       
                         bsi=(wc/100)/(bmi^(2/3) * sqrt(hei/100)),
                         bsi_check=(wc/100)* wei^(-2/3) * (hei/100)^(5/6),
                         hi=(hc/100)*wei^(-0.482)*(hei/100)^0.310,
                         whi=whr*(wei/hei^2)^(-0.25),
                         whi_check=whr*(bmi)^(-0.25),
                         
                         bai=(hc/((hei/100)^1.5))-18,
                         bai_check=hc*(hei/100)^(-1.5)-18,
                         rfm=64-(20*(hei/wc))+12*sex_num,
                         
                         avi=(2*wc^2+0.7*(wc-hc)^2)/1000,
                         bri=364.2-365.5*sqrt(1-((wc/3.1415926)/hei)^2),
                         bri_check=364.2-365.5*sqrt(1-((wc/100)/(2*3.1415926))^2 / (0.5*hei/100)^2),
                         ci=(wc/100)/(0.109*sqrt(wei/(hei/100))))
head(data1 %>% dplyr::select("bmi","bmi_check","bsi","bsi_check","whi","whi_check","bai","bai_check","bri","bri_check"))


temp<-c("bmi","whr", "whtr","wwi","bsi","hi","whi","bai","rfm","avi","bri","ci")
par(mfrow=c(3,4))
for (i in 1:length(temp)){hist<-hist(data1[,temp[i]],main=temp[i])}#normal dist


#BIA
temp<-c("fp_bia","tf_bia","trunk_bia","arm_bia","leg_bia")
par(mfrow=c(2,3))
for (i in 1:length(temp)){hist<-hist(data1[,temp[i]],main=temp[i])}#normal

range(data1$leg_bia,na.rm=TRUE)
head(sort(data$leg_bia),10)
head(sort(data$leg_bia,decreasing=TRUE),10)# continuous, no outliers

#DXA
data1$agr_dxa<-data1$and_dxa/data1$gyn_dxa
data1$fp_dxa<-data1$fp_dxa*100
data1$fp_dxa_check<-data1$tf_dxa/data1$wei *100
head(data1[,c("fp_dxa","fp_dxa_check")])

temp<-c("fp_dxa","tf_dxa","and_dxa","agr_dxa","vat_dxa_m","gyn_dxa","arm_dxa","leg_dxa")
par(mfrow=c(2,4))
for (i in 1:length(temp)){hist<-hist(data1[,temp[i]],main=temp[i])}#skewed for vat

range(data1$leg_dxa,na.rm=TRUE)
head(sort(data$leg_dxa),10)
head(sort(data$leg_dxa,decreasing=TRUE),10)# continuous, no outliers

#MRI
temp<-c("asat_mri_c1", "vat_mri_c1","livf_mri_c1","panf_mri_c1")
par(mfrow=c(1,4))
for (i in 1:length(temp)){hist<-hist(data1[,temp[i]],main=temp[i])}#skewed for vat, liver and pancreas fat 

range(data1$panf_mri_c1,na.rm=TRUE)
head(sort(data$panf_mri_c1),10)
head(sort(data$panf_mri_c1,decreasing=TRUE),10)# continuous, no outliers

#log 
temp<-c("vat_dxa_m","vat_dxa_v",
        "asat_mri","vat_mri","livf_mri",
        "asat_mri_c1","vat_mri_c1","livf_mri_c1","panf_mri_c1")
for(i in 1:length(temp)){
  var<-paste0(temp[i],"_log")
  data1[,var]<-log(data1[,temp[i]])
}

temp<-c("vat_dxa_m_log","vat_dxa_v_log", "asat_mri_log", "vat_mri_log", "livf_mri_log", 
        "asat_mri_c1_log", "vat_mri_c1_log", "livf_mri_c1_log", "panf_mri_c1_log")
par(mfrow=c(3,3))
for (i in 1:length(temp)){hist<-hist(data1[,temp[i]],main=temp[i])}#more normal after log-transformed

#  format variable################
#check NA
sapply(data1[,c("age_cal","sex","eth_2",
                "mdi","smo","pa","condi_count",
                "tf_bia","trunk_bia","arm_bia","leg_bia",
                "tf_dxa","trunk_dxa","and_dxa","gyn_dxa","arm_dxa","leg_dxa", 
                "asat_mri_c1","vat_mri_c1",  "livf_mri_c1", "panf_mri_c1",
                "bmi","wei","hei","wc","hc","whr","whtr")],function(x) sum(is.na(x)))

# basic
data1<-data1 %>% mutate(age_c=cut(age_cal,breaks = c(0,60,70,Inf),right=FALSE),
                        age_b=cut(age_cal,breaks = c(0,65,Inf),right=FALSE),
                        pa_b=case_when(pa=="high"~"1",
                                       pa %in% c("low","moderate")~"0"),
                        bmi_c=cut(bmi,breaks = c(0,25,30,Inf),right=FALSE))

table(data1$age_c)

#standardization
temp1<-c( "wei","hei","bmi", "wc", "hc", "whr", "whtr","wwi","whi","hi","rfm","bai","bsi","bri","ci","avi",
          "fp_bia","tf_bia","trunk_bia","arm_bia","leg_bia",
          "fp_dxa","tf_dxa","trunk_dxa","arm_dxa","leg_dxa", "and_dxa","gyn_dxa","vat_dxa_v",
          "asat_mri",    "vat_mri",     "livf_mri",
          "asat_mri_c1", "vat_mri_c1",  "livf_mri_c1","panf_mri_c1")
temp2<-c("vat_dxa_v_log",
         "asat_mri_log","vat_mri_log", "livf_mri_log",
         "asat_mri_c1_log", "vat_mri_c1_log","livf_mri_c1_log","panf_mri_c1_log")
temp_sum<-c(temp1,temp2)
i<-1
for(i in 1:length(temp_sum)){
  var1<-paste0(temp_sum[i],"_z")
  data1[,var1]<-scale(data1[,temp_sum[i]])
}

##################################  Data analysis#############################
#  Table 1,      baseline table###############
data1 %>% group_by(age_c) %>% summarize(age1=min(age_cal),
                                        age2=max(age_cal))

data1$health_sr <- factor (data1$health_sr, levels=c("Excellent","Good","Fair","Poor"))


#table
factorVars<-c("sex","mdi",
              "smo_b","alc_b","pa_b","mpause","health_sr")

vars<- c("age_cal","sex","mdi",
         "smo_b","alc_b","pa_b","mpause","health_sr",
         
         "bmi", "wc", "whtr","whr","hc",
         "wwi","bsi","hi","whi",
         "bai","rfm",
         "avi","bri","ci",
         
         
         "fp_bia","tf_bia","trunk_bia","arm_bia","leg_bia",
         "fp_dxa","tf_dxa","trunk_dxa","and_dxa","vat_dxa_v","gyn_dxa","arm_dxa","leg_dxa",
         "asat_mri_c1", "vat_mri_c1","livf_mri_c1","panf_mri_c1")


tb<-CreateTableOne(vars = vars,
                   factorVars=factorVars,
                   strata="sex",
                   data=data1,
                   includeNA=FALSE,
                   addOverall=TRUE)

tb_out<-print(tb,showAllLevels = TRUE,contDigits = 1, catDigits = 1,noSpaces = TRUE,
              nonnormal=c("vat_dxa_v","asat_mri_c1", "vat_mri_c1","livf_mri_c1","panf_mri_c1")) %>% as.data.frame()


#  vector or function for loop use##############
# vector
temp_sex <- c("Female","Male")
temp_age <- c("[0,60)","[60,70)","[70,Inf)")
temp_mri<-c("asat_mri_c1_log_z", "vat_mri_c1_log_z", "livf_mri_c1_log_z", "panf_mri_c1_log_z")
temp_exp <-c("bmi_z","wc_z", "whtr_z","whr_z", "hc_z",
          
              "wwi_z","bsi_z","hi_z","whi_z",
              "bai_z","rfm_z", 
              "avi_z","bri_z","ci_z", 
              
              "fp_bia_z","tf_bia_z","trunk_bia_z","arm_bia_z","leg_bia_z",
              "fp_dxa_z","tf_dxa_z","trunk_dxa_z","and_dxa_z","vat_dxa_v_log_z","gyn_dxa_z","arm_dxa_z","leg_dxa_z")

# function 
extract_rsq_fun <- function(model, group) {
  est<-summary(model)$coefficients[,"Estimate"]
  sd<-summary(model)$coefficients[,"Std. Error"]
  
  
  df<-data.frame(beta=est,
                 lower=est-1.96*sd,
                 upper=est+1.96*sd,
                 r_square=summary(model)$r.squared,
                 r_square_p=rsq.partial(model)$partial.rsq[1],
                 gp=group)
  
  df <- df %>% filter(grepl("age|sex|Intercept",rownames(df))==FALSE) 
  
  df
}

format_exp_fun <- function(df_before) {
  df_after <- df_before %>% mutate(
                            exp_label=case_when(exp == "bmi_z"          ~ "BMI",
                                                exp == "wc_z"           ~ "WC",
                                                exp == "whtr_z"         ~ "WHtR",
                                                exp == "whr_z"          ~ "WHR",
                                                exp == "hc_z"           ~ "HC",
                                                exp == "wwi_z"          ~ "WWI",
                                                exp == "bsi_z"          ~ "BSI",
                                                exp == "whi_z"          ~ "WHI",
                                                exp == "hi_z"           ~ "HI",
                                                exp == "bai_z"          ~ "BAI",
                                                exp == "rfm_z"          ~ "RFM",
                                                exp == "avi_z"          ~ "AVI",
                                                exp == "bri_z"          ~ "BRI",
                                                exp == "ci_z"           ~ "CI",
                                                
                                                exp == "fp_bia_z"       ~ "BIA_Total, pct",
                                                exp == "tf_bia_z"       ~ "BIA_Total",
                                                exp == "trunk_bia_z"    ~ "BIA_Trunk",
                                                exp == "arm_bia_z"      ~ "BIA_Arm",
                                                exp == "leg_bia_z"      ~ "BIA_Leg",
                                                
                                                exp == "fp_dxa_z"       ~ "DXA_Total, pct",
                                                exp == "tf_dxa_z"       ~ "DXA_Total",
                                                exp == "trunk_dxa_z"    ~ "DXA_Trunk",
                                                exp == "and_dxa_z"      ~ "DXA_Android",
                                                exp == "vat_dxa_v_log_z"  ~ "DXA_VAT",
                                                exp == "gyn_dxa_z"      ~ "DXA_Gynoid",
                                                exp == "arm_dxa_z"      ~ "DXA_Arm",
                                                exp == "leg_dxa_z"      ~ "DXA_Leg")) %>% 
    mutate(exp_label= factor(exp_label,level=c( "BMI","WC", "WHtR","WHR","HC",
                                                "WWI","BSI","WHI","HI",
                                                "BAI","RFM",
                                                "AVI","BRI","CI",
                                                "BIA_Total, pct","BIA_Total","BIA_Trunk","BIA_Arm","BIA_Leg",
                                                "DXA_Total, pct","DXA_Total","DXA_Trunk","DXA_Android","DXA_VAT",
                                                "DXA_Gynoid","DXA_Arm","DXA_Leg")))
  df_after
}
     
# plot details
my_colors <- c(
  colorRampPalette(c("#9ACD32", "#F0FFF0"))(5),
  colorRampPalette(c("#C28800", "#FFFACD"))(9),
  colorRampPalette(c("#00008B", "#CCCCFF"))(5),
  colorRampPalette(c("#8B0000", "#FFCCCC"))(8))

#  Figure 1 & 2, cor and icc, by age and sex#########
# # exp
# sub <- data1 %>% filter(sex == "Female" & age_c=="[0,60)")
# cor(sub$bmi_z, sub$wc_z,method="pearson")
# icc(sub[,c("wc_z","bmi_z")], model = "twoway", type = "agreement", unit = "single")
# icc(sub[,c("wc_z","bmi_z")], model = "oneway", type = "agreement", unit = "single")
# library(psych)
# ICC(as.matrix(sub[,c("wc_z","bmi_z")]), lmer=FALSE)  #just use the aov procedure
# loop
list1<-list()
for (m in 1:length(temp_sex))
{
  for (n in 1:length(temp_age))
  {
    for (i in 1:length(temp_mri))
    {
      for (j in 1:length(temp_exp))
      {
        
        sub <- data1 %>% filter(sex == temp_sex[m] & age_c==temp_age[n]) 
        r1<-cor(sub[temp_mri[i]], sub[temp_exp[j]],method="pearson")
        r2<-cor(sub[temp_mri[i]], sub[temp_exp[j]],method="spearman")
        
        icc_result <- icc(sub[,c(temp_mri[i],temp_exp[j])], model = "oneway", type = "agreement", unit = "single")
        
        a <- data.frame(cor = as.numeric(r1),
                        rsq = (as.numeric(r1))^2,
                        r_spm = as.numeric(r2),
                        icc=icc_result$value,
                        sex = temp_sex[m],
                        age = temp_age[n],
                        out = temp_mri[i],
                        exp = temp_exp[j])
        
        list1[[length(list1) + 1]]<-a
        
      }
    }
  }
}

# format
cor_sa<-do.call(rbind,list1)

cor_sa <- cor_sa %>% 
  format_exp_fun() %>% 
  mutate(
    sex_label=case_when(sex == "Female" ~ "Women",
                      sex == "Male" ~ "Men"),
    age_label=case_when(age == "[0,60)" ~ "< 60 years",
                        age == "[60,70)" ~ "60-69",
                        age == "[70,Inf)" ~ ">=70"),
    out_label=case_when(out == "asat_mri_c1_log_z" ~ "MRI_ASAT",
                        out == "vat_mri_c1_log_z"  ~ "MRI_VAT",
                        out == "livf_mri_c1_log_z" ~ "MRI_Liver",
                        out == "panf_mri_c1_log_z" ~ "MRI_Pancreas")) %>% 
  mutate(
    sex_label= factor(sex_label,level=c("Women", "Men")),
    age_label= factor(age_label,level=c("< 60 years", "60-69", ">=70")),
    out_label= factor(out_label,level=c("MRI_ASAT", "MRI_VAT", "MRI_Liver","MRI_Pancreas")))

# plot
cor_sa %>% 
  ggplot(aes(x=out_label,y=cor,fill=exp_label)) +
  geom_col(position = 'dodge',width=0.8, color = "black")+
  scale_fill_manual(values = my_colors)+
  labs(y="Pearson correlation r")+
  scale_y_continuous(limits=c(-0.32,1),
                     breaks=c(-0.2, 0.0, 0.2, 0.4, 0.6,0.8,1.0),
                     labels=c(-0.2, 0.0, 0.2, 0.4, 0.6,0.8,1.0))+
  facet_grid(age_label~sex_label)+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=15),
        axis.text.x =  element_text(size=15),
        legend.title = element_blank(),
        strip.text=element_text(size=15),
        legend.position = "right")

cor_sa %>% 
  ggplot(aes(x=out_label,y=r_spm,fill=exp_label)) +
  geom_col(position = 'dodge',width=0.8, color = "black")+
  scale_fill_manual(values = my_colors)+
  labs(y="Spearman rank correlation r")+
  facet_grid(age_label~sex_label)+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=15),
        axis.text.x =  element_text(size=15),
        legend.title = element_blank(),
        strip.text=element_text(size=15),
        legend.position = "right")

cor_sa %>% 
  ggplot(aes(x=out_label,y=icc,fill=exp_label)) +
  geom_col(position = 'dodge',width=0.8, color = "black")+
  # scale_y_continuous(limits=c(-0.22,0.9),
  #                    breaks=c(-0.2, 0.0, 0.2, 0.4, 0.6,0.8),
  #                    labels=c(-0.2, 0.0, 0.2, 0.4, 0.6,0.8))+
  scale_fill_manual(values = my_colors)+
  labs(y="ICC")+
  facet_grid(age_label~sex_label)+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=15),
        axis.text.x =  element_text(size=15),
        legend.title = element_blank(),
        strip.text=element_text(size=15),
        legend.position = "right")

# range
cor_sa %>% 
  filter(exp %in% c("vat_dxa_v_log_z")) %>%
  # filter(exp %in% c("fp_dxa_z","tf_dxa_z","trunk_dxa_z","and_dxa_z","vat_dxa_v_log_z","gyn_dxa_z","arm_dxa_z","leg_dxa_z")) %>%
  # filter(exp %in% c("fp_bia_z","tf_bia_z","trunk_bia_z","arm_bia_z","leg_bia_z")) %>% 
  filter(out %in% c("vat_mri_c1_log_z"))%>%
  # filter(out %in% c("livf_mri_c1_log_z", "panf_mri_c1_log_z"))%>%
  dplyr::select("icc") %>% 
  range()



#  Table 2.      ROC cutoff#################
#exp
sub <- data1 %>% filter(sex=="Male") %>% 
  mutate(
    vat_mri_90 = ifelse(vat_mri_c1_log > quantile(vat_mri_c1_log, 0.90, na.rm = TRUE), 1, 0),
    vat_dxa_90 = ifelse(vat_dxa_v_log > quantile(vat_dxa_v_log, 0.90, na.rm = TRUE), 1, 0),
    tf_90  = ifelse(tf_dxa  > quantile(tf_dxa, 0.90, na.rm = TRUE), 1, 0),
    
    vat_mri_75 = ifelse(vat_mri_c1_log > quantile(vat_mri_c1_log, 0.75, na.rm = TRUE), 1, 0),
    vat_dxa_75 = ifelse(vat_dxa_v_log > quantile(vat_dxa_v_log, 0.75, na.rm = TRUE), 1, 0),
    tf_75  = ifelse(tf_dxa  > quantile(tf_dxa, 0.75, na.rm = TRUE), 1, 0)
  )

#method 1
m<-glm(vat_mri_90~wc, family=binomial,data=sub)
pred <- predict(m, type = "response")
roc_obj <- roc(sub$vat_mri_90, pred)
cutoff_prob <- coords(roc_obj, "best", ret = "threshold", best.method = "youden")
cutoff <- (log(cutoff_prob / (1 - cutoff_prob)) - coef(m)[1]) / coef(m)[2]
cutoff

#method 2
cf<-roc(vat_mri_90~wc, sub)
b<-coords(cf, "best", ret = "threshold", best.method = "youden")
b$threshold

#loop
# variables to test
vars <- c("wc", "whtr", "whr")
outcomes <- c("vat_mri_90", "vat_mri_75",
              "vat_dxa_90", "vat_dxa_75",
              "tf_90","tf_75")  

# function to compute ROC + cutoff + sensitivity/specificity for one sex and one outcome
get_cutoff_by_sex_outcome <- function(sex_label, outcome_var) {
  
  sub <- data1 %>%
    filter(sex == sex_label) %>%
    dplyr::select(all_of(c("vat_mri_c1_log", "vat_dxa_v_log", "tf_dxa", vars))) %>%
    mutate(
      vat_mri_90 = ifelse(vat_mri_c1_log > quantile(vat_mri_c1_log, 0.90, na.rm = TRUE), 1, 0),
      vat_dxa_90 = ifelse(vat_dxa_v_log > quantile(vat_dxa_v_log, 0.90, na.rm = TRUE), 1, 0),
      tf_90  = ifelse(tf_dxa  > quantile(tf_dxa, 0.90, na.rm = TRUE), 1, 0),
      
      vat_mri_75 = ifelse(vat_mri_c1_log > quantile(vat_mri_c1_log, 0.75, na.rm = TRUE), 1, 0),
      vat_dxa_75 = ifelse(vat_dxa_v_log > quantile(vat_dxa_v_log, 0.75, na.rm = TRUE), 1, 0),
      tf_75  = ifelse(tf_dxa  > quantile(tf_dxa, 0.75, na.rm = TRUE), 1, 0)
    )
  
  results <- list()
  
  for (v in vars) {
    # ROC
    roc_obj <- roc(sub[[outcome_var]], sub[[v]], quiet = TRUE)
    
    # optimal cutoff using Youden
    cutoff_info <- coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"), best.method = "youden")
    
    # store results
    results[[v]] <- data.frame(
      sex = sex_label,
      outcome = outcome_var,
      variable = v,
      cutoff = cutoff_info["threshold"],
      auc = as.numeric(auc(roc_obj)),
      sensitivity = cutoff_info["sensitivity"],
      specificity = cutoff_info["specificity"]
    )
  }
  
  bind_rows(results)
}

# run for all sexes and outcomes
output_list <- list()
for (sex_label in c("Female", "Male")) {
  for (outcome_var in outcomes) {
    output_list[[paste(sex_label, outcome_var, sep = "_")]] <- get_cutoff_by_sex_outcome(sex_label, outcome_var)
  }
}

# combine into one table
cutoff_table <- bind_rows(output_list)
cutoff_table <- cutoff_table %>%
  mutate(
    threshold = round(threshold, 2),
    auc = round(auc, 2),
    sensitivity = round(sensitivity, 2),
    specificity = round(specificity, 2)
  )

cutoff_table




#  Supl, only normal BMI###############
list5<-list()
for (m in 1:length(temp_sex))
{
  for (n in 1:length(temp_age))
  {
    for (j in 1:length(temp_exp))
    {
      
      sub <- data1 %>% 
        filter(bmi<25) %>% 
        filter(sex == temp_sex[m] & age_c==temp_age[n]) 
      
      r1<-cor(sub$vat_mri_c1_log_z, sub[temp_exp[j]],method="pearson",use="complete.obs")
      icc_result <- icc(sub[,c("vat_mri_c1_log_z",temp_exp[j])], model = "oneway", type = "agreement", unit = "single")
      
      a <- data.frame(cor = as.numeric(r1),
                      icc = icc_result$value,
                      sex = temp_sex[m],
                      age = temp_age[n],
                      exp = temp_exp[j])
      
      list5[[length(list5) + 1]]<-a
      
    }
  }
}

sen_3<-rbind(do.call(rbind,list5))

# format
sen_3 <- sen_3 %>% 
  format_exp_fun() %>% 
  mutate(sex_label=case_when(sex == "Female" ~ "Women",
                             sex == "Male" ~ "Men"),
         age_label=case_when(age == "[0,60)" ~ "< 60 years",
                             age == "[60,70)" ~ "60-69",
                             age == "[70,Inf)" ~ ">=70"),
         out_label="MRI_VAT") %>% 
  mutate(sex_label= factor(sex_label,level=c("Women", "Men")),
         age_label= factor(age_label,level=c("< 60 years", "60-69", ">=70"))) 

# plot
p7 <- sen_3 %>% 
  ggplot(aes(x=out_label,y=cor,fill=exp_label)) +
  geom_col(position = 'dodge',width=0.8, color = "black")+
  scale_fill_manual(values = my_colors)+
  labs(y="Pearson correlation r")+
  facet_grid(sex_label~age_label)+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=15),
        axis.text.x =  element_text(size=15),
        legend.title = element_blank(),
        strip.text=element_text(size=15),
        legend.position = "right")

p8 <- sen_3 %>% 
  ggplot(aes(x=out_label,y=icc,fill=exp_label)) +
  geom_col(position = 'dodge',width=0.8, color = "black")+
  scale_fill_manual(values = my_colors)+
  labs(y="ICC")+
  facet_grid(sex_label~age_label)+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=15),
        axis.text.x =  element_text(size=15),
        legend.title = element_blank(),
        strip.text=element_text(size=15),
        legend.position = "right")

ggarrange(p7, p8,nrow=2,
          labels = c("a)", "b)"), 
          common.legend = TRUE,   
          legend = "right")      

#  Supl, only dia#################
sub <- data1 %>% 
  filter(dia_ni==1)
range(sub$bmi)
list4<-list()
for (m in 1:length(temp_sex))
{
  for (n in 1:length(temp_age))
  {
    for (j in 1:length(temp_exp))
    {
      
      sub <- data1 %>% 
        filter(dia_ni==1) %>% 
        filter(sex == temp_sex[m] & age_c==temp_age[n]) 
      
      r1<-cor(sub$vat_mri_c1_log_z, sub[temp_exp[j]],method="pearson",use="complete.obs")
      icc_result <- icc(sub[,c("vat_mri_c1_log_z",temp_exp[j])], model = "oneway", type = "agreement", unit = "single")
      
      a <- data.frame(cor = as.numeric(r1),
                      icc = icc_result$value,
                      sex = temp_sex[m],
                      age = temp_age[n],
                      exp = temp_exp[j])
      
      list4[[length(list4) + 1]]<-a
      
    }
  }
}

sen_2<-rbind(do.call(rbind,list4))

# format
sen_2 <- sen_2 %>% 
  format_exp_fun() %>% 
  mutate(sex_label=case_when(sex == "Female" ~ "Women",
                             sex == "Male" ~ "Men"),
         age_label=case_when(age == "[0,60)" ~ "< 60 years",
                             age == "[60,70)" ~ "60-69",
                             age == "[70,Inf)" ~ ">=70"),
         out_label="MRI_VAT") %>% 
  mutate(sex_label= factor(sex_label,level=c("Women", "Men")),
         age_label= factor(age_label,level=c("< 60 years", "60-69", ">=70"))) 

# plot
p5 <- sen_2 %>% 
  ggplot(aes(x=out_label,y=cor,fill=exp_label)) +
  geom_col(position = 'dodge',width=0.8, color = "black")+
  scale_fill_manual(values = my_colors)+
  labs(y="Pearson correlation r")+
  facet_grid(sex_label~age_label)+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=15),
        axis.text.x =  element_text(size=15),
        legend.title = element_blank(),
        strip.text=element_text(size=15),
        legend.position = "right")

p6 <- sen_2 %>% 
  ggplot(aes(x=out_label,y=icc,fill=exp_label)) +
  geom_col(position = 'dodge',width=0.8, color = "black")+
  scale_fill_manual(values = my_colors)+
  labs(y="ICC")+
  facet_grid(sex_label~age_label)+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=15),
        axis.text.x =  element_text(size=15),
        legend.title = element_blank(),
        strip.text=element_text(size=15),
        legend.position = "right")

ggarrange(p5, p6,nrow=2,
          labels = c("a)", "b)"), 
          common.legend = TRUE,   
          legend = "right")      

#  Supl, change MRI#################
list3<-list()
for (m in 1:length(temp_sex))
{
  for (n in 1:length(temp_age))
  {
      for (j in 1:length(temp_exp))
      {
        
        sub <- data1 %>% filter(sex == temp_sex[m] & age_c==temp_age[n]) 
        r1<-cor(sub$vat_mri_log_z, sub[temp_exp[j]],method="pearson",use="complete.obs")
        icc_result <- icc(sub[,c("vat_mri_log_z",temp_exp[j])], model = "oneway", type = "agreement", unit = "single")
        
        a <- data.frame(cor = as.numeric(r1),
                        icc = icc_result$value,
                        sex = temp_sex[m],
                        age = temp_age[n],
                        exp = temp_exp[j])
        
        list3[[length(list3) + 1]]<-a
      }
  }
}

#format
sen_1<-rbind(do.call(rbind,list3))
sen_1 <- sen_1 %>% 
  format_exp_fun() %>% 
  mutate(sex_label=case_when(sex == "Female" ~ "Women",
                             sex == "Male" ~ "Men"),
         age_label=case_when(age == "[0,60)" ~ "< 60 years",
                             age == "[60,70)" ~ "60-69",
                             age == "[70,Inf)" ~ ">=70"),
                             out_label="MRI_VAT") %>% 
  mutate(sex_label= factor(sex_label,level=c("Women", "Men")),
         age_label= factor(age_label,level=c("< 60 years", "60-69", ">=70"))) 

#plot
p3<-sen_1 %>% 
  ggplot(aes(x=out_label,y=cor,fill=exp_label)) +
  geom_col(position = 'dodge',width=0.8, color = "black")+
  scale_fill_manual(values = my_colors)+
  labs(y="Pearson correlation r")+
  facet_grid(sex_label~age_label)+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=15),
        axis.text.x =  element_text(size=15),
        legend.title = element_blank(),
        strip.text=element_text(size=15),
        legend.position = "right")

p4<-sen_1 %>% 
  ggplot(aes(x=out_label,y=icc,fill=exp_label)) +
  geom_col(position = 'dodge',width=0.8, color = "black")+
  scale_fill_manual(values = my_colors)+
  labs(y="ICC")+
  facet_grid(sex_label~age_label)+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=15),
        axis.text.x =  element_text(size=15),
        legend.title = element_blank(),
        strip.text=element_text(size=15),
        legend.position = "right")

ggarrange(p3, p4,nrow=2,
          labels = c("a)", "b)"), 
          common.legend = TRUE,   
          legend = "right")      



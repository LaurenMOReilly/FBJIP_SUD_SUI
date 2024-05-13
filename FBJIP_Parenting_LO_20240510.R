#-----------------------------------------------
#Project title: Parental Relationship -> Hope -> Child Outcomes
#Date: 5/10/2024
#Author: Lauren O'Reilly, Natalie Guerrero
#Changes: 
#-----------------------------------------------
#Read in FBJIP files
library(haven)

#Dataset from Steve
fbjip = read_sas("C:/Users/loreilly/OneDrive - Indiana University/PostDoc/Projects/Aalsma Lab/FBJIP/fbjip_15aug22.sas7bdat")

#Processed dataset Casey used for AAS 2022 presentation
fbjip_processed = read_spss("C:/Users/loreilly/OneDrive - Indiana University/PostDoc/Projects/Aalsma Lab/FBJIP/CP_AAS_FBJIB_Processed.sav")

#Updated dataset with KCAT processing (i.e., time, # items administered) variables
fbjip_kcat = read_sas("C:/Users/loreilly/OneDrive - Indiana University/PostDoc/Projects/Aalsma Lab/FBJIP/fbjip_06jan24.sas7bdat")

#-----------------------------------------------
#Demographics raw datasets
#-----------------------------------------------
#KCAT processing variables
mean(fbjip_kcat$C_length_t1, na.rm=TRUE) #Exclude NAs
sd(fbjip_kcat$C_length_t1, na.rm=TRUE) #Exclude NAs
max(fbjip_kcat$C_length_t1, na.rm=TRUE) #Exclude NAs
min(fbjip_kcat$C_length_t1, na.rm=TRUE) #Exclude NAs
IQR(fbjip_kcat$C_length_t1, na.rm=TRUE) #Exclude NAs

mean(fbjip_kcat$C_length_t2, na.rm=TRUE) #Exclude NAs
sd(fbjip_kcat$C_length_t2, na.rm=TRUE) #Exclude NAs
max(fbjip_kcat$C_length_t2, na.rm=TRUE) #Exclude NAs
min(fbjip_kcat$C_length_t2, na.rm=TRUE) #Exclude NAs
IQR(fbjip_kcat$C_length_t2, na.rm=TRUE) #Exclude NAs

mean(fbjip_kcat$C_length_t3, na.rm=TRUE) #Exclude NAs
sd(fbjip_kcat$C_length_t3, na.rm=TRUE) #Exclude NAs
max(fbjip_kcat$C_length_t3, na.rm=TRUE) #Exclude NAs
min(fbjip_kcat$C_length_t3, na.rm=TRUE) #Exclude NAs
IQR(fbjip_kcat$C_length_t3, na.rm=TRUE) #Exclude NAs

#Variables of interest
#Peer conflict (PCS_1_YOUt1 PCS_20_YOUt1)
table(fbjip$PCS_1_YOUt1)
sum(is.na(fbjip$PCS_1_YOUt1)) #1

#Alpha scores
library(psych)
fbjip_reduce1 = fbjip_kcat[, c('LS_1_YOUt1', 'LS_2_YOUt1', 'LS_3_YOUt1')]
psych::alpha(x=fbjip_reduce1)

fbjip_reduce2 = fbjip_kcat[, c('hs_1_YOUt1', 'hs_2_YOUt1', 'hs_3_YOUt1')]
psych::alpha(x=fbjip_reduce2)

fbjip_reduce3 = fbjip_kcat[, c('PMS_1_YOUt1', 'PMS_2_YOUt1', 'PMS_3_YOUt1', 'PMS_4_YOUt1', 'PMS_5_YOUt1', 'PMS_6_YOUt1', 'PMS_7_YOUt1', 'PMS_8_YOUt1', 'PMS_9_YOUt1', 'PMS_10_YOUt1')]
psych::alpha(x=fbjip_reduce3)

fbjip_reduce4 = fbjip_kcat[, c('pss_1_r_YOUt1', 'pss_2_r_YOUt1', 'pss_3_r_YOUt1', 'pss_4_r_YOUt1', 'pss_6_r_YOUt1', 'pss_7_rr_YOUt1', 'pss_8_rr_YOUt1', 'pss_9_rr_YOUt1', 'pss_11_r_YOUt1', 'pss_12_r_YOUt1')]
psych::alpha(x=fbjip_reduce4, check.keys=TRUE)


#-----------------------------------------------
#Reduce updated dataset
#-----------------------------------------------

fbjip_kcat1 = fbjip_kcat[, c('dyadid', 'race_1_YOUt1', 'race_2_YOUt1', 'race_3_YOUt1', 'race_4_YOUt1', 'race_6_YOUt1', 'gender_YOUt1', 'age_YOUt1', 'ethnicity_YOUt1', 
                          'C_DEP_t1', 'C_ANX_t1', 'C_SS_t1', 'SUD_t1',
                          'C_DEP_t2', 'C_ANX_t2', 'C_SS_t2', 'SUD_t2',
                          'C_DEP_t3', 'C_ANX_t3', 'C_SS_t3', 'SUD_t3',
                          'C_ADHD_Cat_t1', 'C_ODD_Cat_t1', 'C_CD_Cat_t1', 'C_DEP_Cat_t1', 'C_ANX_Cat_t1', 'C_MANIA_Cat_t1', 'SUD_Cat_t1', 'C_SS_Cat_t1', 
                          'C_ADHD_Cat_t2', 'C_ODD_Cat_t2', 'C_CD_Cat_t2', 'C_DEP_Cat_t2', 'C_ANX_Cat_t2', 'C_MANIA_Cat_t2', 'SUD_Cat_t2', 'C_SS_Cat_t2',
                          'C_ADHD_Cat_t3', 'C_ODD_Cat_t3', 'C_CD_Cat_t3', 'C_DEP_Cat_t3', 'C_ANX_Cat_t3', 'C_MANIA_Cat_t3', 'SUD_Cat_t3', 'C_SS_Cat_t3',
                          'Household_1_YOUt1', 'Household_2_YOUt1', 'Household_3_YOUt1', 'Household_4_YOUt1', 'Household_6_YOUt1', 'Household_7_YOUt1', 'Household_8_YOUt1', 'Household_9_YOUt1', 'HouseTotal_YOUt1',
                          'fas_YOUt1', 'fcu_YOUt1', 'crafft_score_YOUt1', 'cpssv_score_YOUt1', 'pss_mean_YOUt1', 'PMS_altered_YOUt1', 'pcs_score_YOUt1',
                          'ls_score_YOUt1', 'hs_score_YOUt1',
                          'fcu_YOUt2', 'crafft_score_YOUt2', 'PMS_altered_YOUt2', 'pcs_score_YOUt2',
                          'fcu_YOUt3', 'crafft_score_YOUt3', 'PMS_altered_YOUt3', 'pcs_score_YOUt3'
)]


cor.test(fbjip_kcat1$ls_score_YOUt1, fbjip_kcat1$hs_score_YOUt1, method=c("pearson"))

sum(is.na(fbjip_kcat1$age_YOUt1))
sum(is.na(fbjip_kcat1$gender_YOUt1))
sum(is.na(fbjip_kcat1$ethnicity_YOUt1))
sum(is.na(fbjip_kcat1$C_SS_t1)) #2
sum(is.na(fbjip_kcat1$SUD_t1)) #10

sum(is.na(fbjip_kcat1$Household_1_YOUt1)) #25
sum(is.na(fbjip_kcat1$Household_2_YOUt1)) #64
sum(is.na(fbjip_kcat1$Household_3_YOUt1)) #68
sum(is.na(fbjip_kcat1$Household_4_YOUt1)) #60
sum(is.na(fbjip_kcat1$Household_6_YOUt1)) #64
sum(is.na(fbjip_kcat1$Household_7_YOUt1)) #74
sum(is.na(fbjip_kcat1$Household_8_YOUt1)) #69
sum(is.na(fbjip_kcat1$Household_9_YOUt1)) #39
sum(is.na(fbjip_kcat1$HouseTotal_YOUt1)) #1

sum(is.na(fbjip_kcat1$fas_YOUt1)) #0
sum(is.na(fbjip_kcat1$fcu_YOUt1)) #1
sum(is.na(fbjip_kcat1$crafft_score_YOUt1)) #0
sum(is.na(fbjip_kcat1$cpssv_score_YOUt1)) #16
sum(is.na(fbjip_kcat1$pss_mean_YOUt1)) #0
sum(is.na(fbjip_kcat1$PMS_altered_YOUt1)) #0
sum(is.na(fbjip_kcat1$pcs_score_YOUt1)) #1
sum(is.na(fbjip_kcat1$ls_score_YOUt1)) #0
sum(is.na(fbjip_kcat1$hs_score_YOUt1)) #0

sum(is.na(fbjip_kcat1$fcu_YOUt2)) #22
sum(is.na(fbjip_kcat1$crafft_score_YOUt2)) #30
sum(is.na(fbjip_kcat1$PMS_altered_YOUt2)) #21
sum(is.na(fbjip_kcat1$pcs_score_YOUt2)) #21

sum(is.na(fbjip_kcat1$fcu_YOUt3)) #26
sum(is.na(fbjip_kcat1$crafft_score_YOUt3)) #26
sum(is.na(fbjip_kcat1$PMS_altered_YOUt3)) #26
sum(is.na(fbjip_kcat1$pcs_score_YOUt3)) #26
sum(is.na(fbjip_kcat1$fas_YOUt1)) #0

table(fbjip_kcat1$age_YOUt1)
table(fbjip_kcat1$gender_YOUt1)
table(fbjip_kcat1$race_1_YOUt1)
table(fbjip_kcat1$race_2_YOUt1)
table(fbjip_kcat1$race_3_YOUt1)
table(fbjip_kcat1$race_4_YOUt1)
table(fbjip_kcat1$race_6_YOUt1)
table(fbjip_kcat1$ethnicity_YOUt1)
table(fbjip_kcat1$fas_YOUt1)

table(fbjip_kcat1$HouseTotal_YOUt1)
mean(fbjip_kcat1$HouseTotal_YOUt1, na.rm=TRUE)
sd(fbjip_kcat1$HouseTotal_YOUt1, na.rm=TRUE) 

mean(fbjip_kcat1$pss_mean_YOUt1, na.rm=TRUE)
sd(fbjip_kcat1$pss_mean_YOUt1, na.rm=TRUE) 

mean(fbjip_kcat1$PMS_altered_YOUt1, na.rm=TRUE)
sd(fbjip_kcat1$PMS_altered_YOUt1, na.rm=TRUE) 

mean(fbjip_kcat1$hs_score_YOUt1, na.rm=TRUE)
sd(fbjip_kcat1$hs_score_YOUt1, na.rm=TRUE) 

mean(fbjip_kcat1$ls_score_YOUt1, na.rm=TRUE)
sd(fbjip_kcat1$ls_score_YOUt1, na.rm=TRUE) 

mean(fbjip_kcat1$C_SS_t1, na.rm=TRUE)
sd(fbjip_kcat1$C_SS_t1, na.rm=TRUE) 

mean(fbjip_kcat1$SUD_t1, na.rm=TRUE)
sd(fbjip_kcat1$SUD_t1, na.rm=TRUE) 

table(fbjip_kcat1$fas_YOUt1)
table(fbjip_kcat1$fcu_YOUt1)
table(fbjip_kcat1$PMS_altered_YOUt1)
table(fbjip_kcat$C_ADHD_t1)

#Reduce further as some variable have too much missingness
fbjip_kcat2 = fbjip_kcat1[, c('dyadid', 'race_1_YOUt1', 'race_2_YOUt1', 'race_3_YOUt1', 'race_4_YOUt1', 'race_6_YOUt1', 'gender_YOUt1', 'age_YOUt1', 'ethnicity_YOUt1', 
                              'C_DEP_t1', 'C_ANX_t1', 'C_SS_t1', 'SUD_t1',
                              'C_DEP_t2', 'C_ANX_t2', 'C_SS_t2', 'SUD_t2',
                              'C_DEP_t3', 'C_ANX_t3', 'C_SS_t3', 'SUD_t3',
                              'C_ADHD_Cat_t1', 'C_ODD_Cat_t1', 'C_CD_Cat_t1', 'C_DEP_Cat_t1', 'C_ANX_Cat_t1', 'C_MANIA_Cat_t1', 'SUD_Cat_t1', 'C_SS_Cat_t1', 
                              'C_ADHD_Cat_t2', 'C_ODD_Cat_t2', 'C_CD_Cat_t2', 'C_DEP_Cat_t2', 'C_ANX_Cat_t2', 'C_MANIA_Cat_t2', 'SUD_Cat_t2', 'C_SS_Cat_t2',
                              'C_ADHD_Cat_t3', 'C_ODD_Cat_t3', 'C_CD_Cat_t3', 'C_DEP_Cat_t3', 'C_ANX_Cat_t3', 'C_MANIA_Cat_t3', 'SUD_Cat_t3', 'C_SS_Cat_t3',
                              'HouseTotal_YOUt1',
                              'fas_YOUt1', 'fcu_YOUt1', 'crafft_score_YOUt1', 'cpssv_score_YOUt1', 'pss_mean_YOUt1', 'PMS_altered_YOUt1', 'pcs_score_YOUt1',
                              'ls_score_YOUt1', 'hs_score_YOUt1',
                              'fcu_YOUt2', 'crafft_score_YOUt2', 'PMS_altered_YOUt2', 'pcs_score_YOUt2',
                              'fcu_YOUt3', 'crafft_score_YOUt3', 'PMS_altered_YOUt3', 'pcs_score_YOUt3'
)]


#Merge dichotomous race into dataset
fbjip_processed1 = fbjip_processed[, c('dyadid', 'Race', 'raceDi')]

fbjip_kcat3 = dplyr::left_join(fbjip_processed1, fbjip_kcat2, by='dyadid')

sum(is.na(fbjip_kcat3$Race))

#-----------------------------------------------
#Multiple imputation
#-----------------------------------------------
library(mice)

#MICE
md.pattern(fbjip_reduce)
md.pattern(fbjip_kcat1)
md.pattern(fbjip_kcat2)
md.pattern(fbjip_kcat3)

#Change number of multiple imputed datasets to 20
#fbjip_impute = mice::complete(mice(fbjip_reduce, m=25, maxit=500, defaultMethod = 'pmm', seed=1254))
fbjip_impute = mice::complete(mice(fbjip_kcat3, m=25, maxit=500, defaultMethod = 'pmm', seed=1254))

#Save dataset
#saveRDS(fbjip_impute, 'C:/Users/loreilly/OneDrive - Indiana University/PostDoc/Projects/Aalsma Lab/FBJIP/fbjip_impute.rds')
#write_sav(fbjip_impute, 'C:/Users/loreilly/OneDrive - Indiana University/PostDoc/Projects/Aalsma Lab/FBJIP/fbjip_impute.sav')
#saveRDS(fbjip_impute, 'C:/Users/loreilly/OneDrive - Indiana University/PostDoc/Projects/Aalsma Lab/FBJIP/fbjip_impute_.rds')
saveRDS(fbjip_impute, 'C:/Users/loreilly/OneDrive - Indiana University/PostDoc/Projects/Aalsma Lab/FBJIP/fbjip_impute_1.rds') # adding continuous KCAT variables

fbjip_impute = readRDS("C:/Users/loreilly/OneDrive - Indiana University/PostDoc/Projects/Aalsma Lab/FBJIP/fbjip_impute_1.rds")

#Demographics imputed sample
table(fbjip_impute$age_YOUt1)
table(fbjip_impute$gender_YOUt1)
table(fbjip_impute$race_1_YOUt1)
table(fbjip_impute$race_2_YOUt1)
table(fbjip_impute$race_3_YOUt1)
table(fbjip_impute$race_4_YOUt1)
table(fbjip_impute$race_6_YOUt1)
table(fbjip_impute$ethnicity_YOUt1)

var(fbjip_impute$SUD_t1)
table(fbjip_impute$SUD_t1)
var(fbjip_impute$C_SS_t1)
table(fbjip_impute$C_SS_t1)

table(fbjip_impute$HouseTotal_YOUt1)
mean(fbjip_impute$HouseTotal_YOUt1, na.rm=TRUE)
sd(fbjip_impute$HouseTotal_YOUt1, na.rm=TRUE) 

mean(fbjip_impute$pss_mean_YOUt1, na.rm=TRUE)
sd(fbjip_impute$pss_mean_YOUt1, na.rm=TRUE) 

mean(fbjip_impute$PMS_altered_YOUt1, na.rm=TRUE)
sd(fbjip_impute$PMS_altered_YOUt1, na.rm=TRUE) 

mean(fbjip_impute$hs_score_YOUt1, na.rm=TRUE)
sd(fbjip_impute$hs_score_YOUt1, na.rm=TRUE) 

mean(fbjip_impute$ls_score_YOUt1, na.rm=TRUE)
sd(fbjip_impute$ls_score_YOUt1, na.rm=TRUE) 

mean(fbjip_impute$C_SS_t1, na.rm=TRUE)
sd(fbjip_impute$C_SS_t1, na.rm=TRUE) 

mean(fbjip_impute$SUD_t1, na.rm=TRUE)
sd(fbjip_impute$SUD_t1, na.rm=TRUE) 


#-----------------------------------------------
#Logistic regression imputed dataset
#-----------------------------------------------
#Create dummy variable for age
fbjip_impute$age_14 = ifelse(fbjip_impute$age_YOUt1 == 14, 1, 0)
fbjip_impute$age_15 = ifelse(fbjip_impute$age_YOUt1 == 15, 1, 0)
fbjip_impute$age_16 = ifelse(fbjip_impute$age_YOUt1 == 16, 1, 0)
fbjip_impute$age_17 = ifelse(fbjip_impute$age_YOUt1 == 17, 1, 0)

library(dplyr)

fbjip_impute = dplyr::mutate(fbjip_impute, age = case_when(age_YOUt1 == 14 ~ 0,
                                                           age_YOUt1 == 15 ~ 1,
                                                           age_YOUt1 == 16 ~ 2,
                                                           age_YOUt1 == 17 ~ 3))
                                  
#Rescale ethnicity variable
fbjip_impute = dplyr::mutate(fbjip_impute, ethnicity_YOUt1 = case_when(ethnicity_YOUt1 == 1 ~ 0,
                                                                       ethnicity_YOUt1 == 2 ~ 1))

#Dichomotize Household - people living in house
fbjip_impute = dplyr::mutate(fbjip_impute, HouseTotal_d = case_when(HouseTotal_YOUt1 == 1 ~ 0,
                                                                    HouseTotal_YOUt1 == 2 ~ 0,
                                                                    HouseTotal_YOUt1 == 3 ~ 0,
                                                                    HouseTotal_YOUt1 == 4 ~ 0,
                                                                    HouseTotal_YOUt1 == 5 ~ 1,
                                                                    HouseTotal_YOUt1 == 6 ~ 1,
                                                                    HouseTotal_YOUt1 == 7 ~ 1,
                                                                    HouseTotal_YOUt1 == 8 ~ 1,
                                                                    HouseTotal_YOUt1 == 9 ~ 1))

table(fbjip_impute$age)
table(fbjip_impute$age_14)
table(fbjip_impute$age_15)
table(fbjip_impute$age_16)
table(fbjip_impute$age_17)
table(fbjip_impute$ethnicity_YOUt1)
table(fbjip_impute$HouseTotal_d)
  
#-----------------------------------------------
#Logistic regression non-imputed dataset
#-----------------------------------------------
#Create dummy variable for age
fbjip_reduce$age_14 = ifelse(fbjip_reduce$age_YOUt1 == 14, 1, 0)
fbjip_reduce$age_15 = ifelse(fbjip_reduce$age_YOUt1 == 15, 1, 0)
fbjip_reduce$age_16 = ifelse(fbjip_reduce$age_YOUt1 == 16, 1, 0)
fbjip_reduce$age_17 = ifelse(fbjip_reduce$age_YOUt1 == 17, 1, 0)

#-----------------------------------------------
#Correlations among predictors and outcomes
#-----------------------------------------------
cor.test(fbjip_impute$C_SS_Cat_t1, fbjip_impute$ls_score_YOUt1, method=c("pearson", "kendall", "spearman"))
cor.test(fbjip_impute$C_SS_Cat_t1, fbjip_impute$hs_score_YOUt1, method=c("pearson", "kendall", "spearman"))

cor.test(fbjip_impute$C_SS_Cat_t1, fbjip_impute$pcs_score_YOUt1, method=c("pearson", "kendall", "spearman"))
cor.test(fbjip_impute$C_SS_Cat_t1, fbjip_impute$PMS_altered_YOUt1, method=c("pearson", "kendall", "spearman"))

library(qgraph)
cor_auto(fbjip_impute %>%
           dplyr::select(C_SS_Cat_t1, ls_score_YOUt1, pcs_score_YOUt1))


table(fbjip_impute$ls_score_YOUt1)
table(fbjip_impute$hs_score_YOUt1)
table(fbjip_impute$SUD_Cat_t1)
table(fbjip_impute[, c("ls_score_YOUt1","SUD_Cat_t1")])
table(fbjip_impute[, c("ls_score_YOUt1","C_SS_Cat_t1")])


#-----------------------------------------------
#Mediation Models with covariates
#-----------------------------------------------
#---------Model 1: Parental Support --> SUI
#Parental support and youth suicidality - med: life satisfaction
ps_sui_ls_cov = '
#Direct effects
ls_score_YOUt1 ~ a * pss_mean_YOUt1 + raceDi + age_15 + age_16 + age_17 + gender_YOUt1 + ethnicity_YOUt1 + fas_YOUt1 
C_SS_t1 ~ c * pss_mean_YOUt1 + b * ls_score_YOUt1 + raceDi + age_15 + age_16 + age_17 + gender_YOUt1 + ethnicity_YOUt1 + fas_YOUt1 

#Indirect effect
indirect := a * b

#Total effect (c+indirect)
total := c + indirect
'

model_ps_sui_ls_cov = sem(ps_sui_ls_cov, data=fbjip_impute, se="bootstrap", bootstrap=500)
summary(model_ps_sui_ls_cov, standardized=TRUE, fit.measures=TRUE)

semPaths(model_ps_sui_ls_cov, whatLabels = "est", style="lisrel", intercepts = FALSE)


#Parental support and youth substance use - med: life satisfaction
ps_sud_ls_cov = '
#Direct effects
ls_score_YOUt1 ~ a * pss_mean_YOUt1 + raceDi + age_15 + age_16 + age_17 + gender_YOUt1 + ethnicity_YOUt1 + fas_YOUt1 
SUD_t1 ~ c * pss_mean_YOUt1 + b * ls_score_YOUt1 + raceDi + age_15 + age_16 + age_17 + gender_YOUt1 + ethnicity_YOUt1 + fas_YOUt1

#Indirect effect
indirect := a * b

#Total effect (c+indirect)
total := c + indirect
'

model_ps_sud_ls_cov = sem(ps_sud_ls_cov, data=fbjip_impute, se="bootstrap", bootstrap=500)
summary(model_ps_sud_ls_cov, standardized=TRUE, fit.measures=TRUE)

semPaths(model_ps_sud_ls_cov, whatLabels = "est", style="lisrel", intercepts = FALSE)


#Parental support and youth suicidality - med: hopelessness
ps_sui_h_cov = '
#Direct effects
hs_score_YOUt1 ~ a * pss_mean_YOUt1 + raceDi + age_15 + age_16 + age_17 + gender_YOUt1 + ethnicity_YOUt1 + fas_YOUt1 
C_SS_t1 ~ c * pss_mean_YOUt1 + b * hs_score_YOUt1 + raceDi + age_15 + age_16 + age_17 + gender_YOUt1 + ethnicity_YOUt1 + fas_YOUt1 

#Indirect effect
indirect := a * b

#Total effect (c+indirect)
total := c + indirect
'

model_ps_sui_h_cov = sem(ps_sui_h_cov, data=fbjip_impute, se="bootstrap", bootstrap=500)
summary(model_ps_sui_h_cov, standardized=TRUE, fit.measures=TRUE)

semPaths(model_ps_sui_h_cov, whatLabels = "est", style="lisrel", intercepts = FALSE)


#Parental support and youth substance use - med: hopelessness
ps_sud_h_cov = '
#Direct effects
hs_score_YOUt1 ~ a * pss_mean_YOUt1 + raceDi + age_15 + age_16 + age_17 + gender_YOUt1 + ethnicity_YOUt1 + fas_YOUt1 
SUD_t1 ~ c * pss_mean_YOUt1 + b * hs_score_YOUt1 + raceDi + age_15 + age_16 + age_17 + gender_YOUt1 + ethnicity_YOUt1 + fas_YOUt1 

#Indirect effect
indirect := a * b

#Total effect (c+indirect)
total := c + indirect
'

model_ps_sud_h_cov = sem(ps_sud_h_cov, data=fbjip_impute, se="bootstrap", bootstrap=500)
summary(model_ps_sud_h_cov, standardized=TRUE, fit.measures=TRUE)

semPaths(model_ps_sud_h_cov, whatLabels = "est", style="lisrel", intercepts = FALSE)


#---------Model 2: Parental Monitoring --> SUI
#Parental monitoring and youth suicidality - med: life satisfaction
#To help with model fit, combine SUD categories 2 + 3 and SS categories 2 + 3
pm_sui_ls_cov = '
#Direct effects
ls_score_YOUt1 ~ a * PMS_altered_YOUt1 + raceDi + age_15 + age_16 + age_17 + gender_YOUt1 + ethnicity_YOUt1 + fas_YOUt1 
C_SS_t1 ~ c * PMS_altered_YOUt1 + b * ls_score_YOUt1 + raceDi + age_15 + age_16 + age_17 + gender_YOUt1 + ethnicity_YOUt1 + fas_YOUt1 

#Indirect effect
indirect := a * b

#Total effect (c+indirect)
total := c + indirect
'

model_pm_sui_ls_cov = sem(pm_sui_ls_cov, data=fbjip_impute, se="bootstrap", bootstrap=500)
summary(model_pm_sui_ls_cov, standardized=TRUE, fit.measures=TRUE)

semPaths(model_pm_sui_ls_cov, whatLabels = "est", style="lisrel", intercepts = FALSE)


#Parental monitoring and youth substance use - med: life satisfaction
pm_sud_ls_cov = '
#Direct effects
ls_score_YOUt1 ~ a * PMS_altered_YOUt1 + raceDi + age_15 + age_16 + age_17 + gender_YOUt1 + ethnicity_YOUt1 + fas_YOUt1 
SUD_t1 ~ c * PMS_altered_YOUt1 + b * ls_score_YOUt1 + raceDi + age_15 + age_16 + age_17 + gender_YOUt1 + ethnicity_YOUt1 + fas_YOUt1 

#Indirect effect
indirect := a * b

#Total effect (c+indirect)
total := c + indirect
'

model_pm_sud_ls_cov = sem(pm_sud_ls_cov, data=fbjip_impute, se="bootstrap", bootstrap=500)
summary(model_pm_sud_ls_cov, standardized=TRUE, fit.measures=TRUE)

semPaths(model_pm_sud_ls_cov, whatLabels = "est", style="lisrel", intercepts = FALSE)


#Parental monitoring and youth suicidality - med: hopelessness
pm_sui_h_cov = '
#Direct effects
hs_score_YOUt1 ~ a * PMS_altered_YOUt1 + raceDi + age_15 + age_16 + age_17 + gender_YOUt1 + ethnicity_YOUt1 + fas_YOUt1 
C_SS_t1 ~ c * PMS_altered_YOUt1 + b * hs_score_YOUt1 + raceDi + age_15 + age_16 + age_17 + gender_YOUt1 + ethnicity_YOUt1 + fas_YOUt1 

#Indirect effect
indirect := a * b

#Total effect (c+indirect)
total := c + indirect
'

model_pm_sui_h_cov = sem(pm_sui_h_cov, data=fbjip_impute, se="bootstrap", bootstrap=500)
summary(model_pm_sui_h_cov, standardized=TRUE, fit.measures=TRUE)

semPaths(model_pm_sui_h_cov, whatLabels = "est", style="lisrel", intercepts = FALSE)


#Parental monitoring and youth substance use - med: hopelessness
pm_sud_h_cov = '
#Direct effects
hs_score_YOUt1 ~ a * PMS_altered_YOUt1 + raceDi + age_15 + age_16 + age_17 + gender_YOUt1 + ethnicity_YOUt1 + fas_YOUt1 
SUD_t1 ~ c * PMS_altered_YOUt1 + b * hs_score_YOUt1 + raceDi + age_15 + age_16 + age_17 + gender_YOUt1 + ethnicity_YOUt1 + fas_YOUt1 

#Indirect effect
indirect := a * b

#Total effect (c+indirect)
total := c + indirect
'

model_pm_sud_h_cov = sem(pm_sud_h_cov, data=fbjip_impute, se="bootstrap", bootstrap=500)
summary(model_pm_sud_h_cov, standardized=TRUE, fit.measures=TRUE)

semPaths(model_pm_sud_h_cov, whatLabels = "est", style="lisrel", intercepts = FALSE)





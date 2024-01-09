#Load libraries
library(googlesheets4)
library(tidyverse)
library(metafor)
library(robumeta)
library(clubSandwich)
library(weightr)

#Primary Database
NNMA_Data <- read_sheet("https://docs.google.com/spreadsheets/d/1cv5ftm6-XV28pZ_mN43K7HH3C7WhsPMnPsB1HDuRLE4/edit#gid=0")


NNMA_Data_Subset <- subset(NNMA_Data, (measure_type=="Main" | measure_type=="Follow Up (10-14 Days)") &
                           aggregated=="IN" & (wwc_rating=="MWOR" | wwc_rating=="MWR") &
                           comparison_prelim=="BAU" & (NL...48==1 | SE...50==1 | V...53==1 | F...56==1 | BX...63==1 | RS...74==1))

###Create covariance matrix
V_list <- impute_covariance_matrix(vi = NNMA_Data_Subset$variance,  
                                   cluster = NNMA_Data_Subset$record_id,   
                                   r = 0.60)

###Run multivariate meta-regression
NNMA_MVmodel <- rma.mv(yi = effect_size, 
                  V = V_list, 
                  random = ~ 1 | record_id/measure_name/es_id,
                  test =  "t", 
                  data = NNMA_Data_Subset, 
                  method = "REML")
summary(NNMA_MVmodel)

### Use RVE for robustness
mvcf <- coef_test(NNMA_MVmodel,
                  cluster = NNMA_Data_Subset$record_id, 
                  vcov = "CR2")
mvcf

###Find prediction interval
(PI_low <- coef(NNMA_MVmodel) - 1.96*sqrt(NNMA_MVmodel$sigma2[1] + NNMA_MVmodel$sigma2[2]))
(PI_high <- coef(NNMA_MVmodel) + 1.96*sqrt(NNMA_MVmodel$sigma2[1] + NNMA_MVmodel$sigma2[2]))

###Add moderators (as.numeric(unlist(final_domain)))
NNMA_Data_Subset <- NNMA_Data_Subset %>% replace_na(list(NL...48 = 0, SE...50 = 0, V...53 = 0, F...56 = 0,BX...63 = 0,RS...74 = 0))
check <- NNMA_Data_Subset %>% select(NL...48, SE...50, V...53, F...56, BX...63,RS...74)

NNMA_MVregmodel <- rma.mv(yi = effect_size, 
                     V = V_list, 
                     random = ~ 1 | record_id/measure_name/es_id,
                     mods = ~ NL...48 + SE...50 + V...53 + F...56 + BX...63 + RS...74,
                     test =  "t", 
                     data = NNMA_Data_Subset, 
                     method = "REML")
summary(NNMA_MVregmodel)

###Use RVE

NNMA_mvregcf <- coef_test(NNMA_MVregmodel,
                     cluster = NNMA_MVmodel$record_id, 
                     vcov = "CR2")
NNMA_mvregcf

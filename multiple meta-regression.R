#Load libraries
library(googlesheets4)
library(tidyverse)
library(metafor)
library(robumeta)
library(clubSandwich)
library(weightr)

#Primary Database 
NNMA_Data <- read_sheet("https://docs.google.com/spreadsheets/d/1cv5ftm6-XV28pZ_mN43K7HH3C7WhsPMnPsB1HDuRLE4/edit#gid=0")

#This subset is specific to the meta-regression model. It is a preliminary subset that includes only BAU controls.
NNMA_Data_Subset <- subset(NNMA_Data, (measure_type=="Main" | measure_type=="Follow Up (10-14 Days)") &
                           aggregated=="IN" & (wwc_rating=="MWOR" | wwc_rating=="MWR") &
                           comparison_prelim=="BAU")

###Replace all NA values in the moderators with 0 to avoid the "Processing terminated since k <= 1" error
NNMA_Data_Subset <- NNMA_Data_Subset %>% replace_na(list(NL_TX = 0, SE_TX = 0, VF_TX = 0, F_TX = 0, BX_TX = 0, RS_TX = 0, TE_TX = 0))

###Create covariance matrix
V_list <- impute_covariance_matrix(vi = NNMA_Data_Subset$variance,  
                                   cluster = NNMA_Data_Subset$record_id,   
                                   r = 0.60)

###Run multivariate meta-regression
NNMA_MVmodel <- rma.mv(yi = effect_size, 
                  V = V_list, 
                  random = ~ 1 | record_id/es_id,
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
NNMA_MVmod_model <- rma.mv(yi = effect_size, 
                     V = V_list, 
                     random = ~ 1 | record_id/es_id,
                     mods = ~ NL_TX + SE_TX + VF_TX + F_TX + BX_TX + RS_TX - 1,
                     test =  "t", 
                     data = NNMA_Data_Subset, 
                     method = "REML")
summary(NNMA_MVmod_model)

###Use RVE

NNMA_mvMODcf <- coef_test(NNMA_MVmod_model,
                     cluster = NNMA_Data_Subset$record_id, 
                     vcov = "CR2")
NNMA_mvMODcf

#Calculate Correlation Matrix
cor(NNMA_Data_Subset[c("NL_TX", "SE_TX", "VF_TX", "F_TX", "BX_TX", "RS_TX")])

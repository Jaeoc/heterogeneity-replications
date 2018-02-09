##MA heterogeneity project - summary data from Many labs 1 https://osf.io/wx7ck/

#******************************************
#Setup----
#******************************************
if(!require("readxl")){install.packages("readxl")}
library(readxl)

if(!require("dplyr")){install.packages("dplyr")}
library(dplyr)

#set working directory

#extract data on whether in-lab study or not
lab <- read_excel("../../Table_S1_-_Detailed_Site_and_Sample_Characteristics.xlsx") %>% 
  select(`Site identifier`, `Online (O) or Lab (L)`) %>% 
  rename(Site = `Site identifier`, in_lab = `Online (O) or Lab (L)`) %>% 
  mutate(in_lab = ifelse(in_lab == "L", 1, 0), Site = tolower(Site))


#*****************************************
#T-test effects---
#*****************************************
#Effects extracted in this section
#[1] "Sunk Costs"           
#[2] "Anchoring 1 - NYC"    
#[3] "Anchoring 2 - Chicago"
#[4] "Anchoring 3 - Everest"
#[5] "Anchoring 4 - Babies" 
#[6] "Gambler's Fallacy"    
#[7] "Flag Priming"         
#[8] "Quote Attribution"    
#[9] "Money Priming"        
#[10] "Imagined Contact" 
#**************

#sheetnames of effects that use t-tests and have the same data format (Math_art gender does a t-test but has different data format)
tsheets <- excel_sheets("../data/ML-_Summary_Statistics.xlsx")[-c(1:2, 13:18)] 
#extract data for these effects
teffects <- lapply(tsheets, function (X) read_excel("../data/ML-_Summary_Statistics.xlsx", sheet = X)) #read data

tfilt <- c("Overall:", "Mean across samples:", "Overall (sum of samples)", "Overall for US participants:") #for removing summary rows

tnames <- tsheets 
tnames[2:5] <- c("Anchoring 1 - NYC", "Anchoring 2 - Chicago", #Clarify the anchoring effect names
                 "Anchoring 3 - Everest", "Anchoring 4 - Babies")


tnt <- tnc <- tot1 <- toc1 <- tot2 <- toc2 <-  rep(NA, 10) #Vectors for variable names

#loops over the datasets and extracts variable names
for(i in seq_along(tnames)){
  tnt[i] <- names(teffects[[i]])[2] #treatment n variable names
  tnc[i] <- names(teffects[[i]])[3] #control n variable names
  tot1[i] <- names(teffects[[i]])[5] #treatment means variable names
  toc1[i] <- names(teffects[[i]])[6] #control means variable names
  tot2[i] <- names(teffects[[i]])[7] #treatment SD variable names
  toc2[i] <- names(teffects[[i]])[8] #control SD variable names
}



#loop over the 10 different effects (t-tests, same format) and cleans data
for(i in seq_along(tnames)){
  teffects[[i]] <- teffects[[i]] %>% 
    filter(!(Site %in% tfilt)) %>% #remove summary rows
    left_join(., lab, by = "Site") %>% #add online/in lab variable
    rename_at(vars(one_of(tnt)), funs(paste0("ntreatment"))) %>% #'if column name is one of the names in tnt, rename as "ntreatment"'
    rename_at(vars(one_of(tnc)), funs(paste0("ncontrol"))) %>% #These lines will give some warnings, but they are ignorable
    rename_at(vars(one_of(tot1)), funs(paste0("outcome_t1"))) %>%
    rename_at(vars(one_of(toc1)), funs(paste0("outcome_c1"))) %>%
    rename_at(vars(one_of(tot2)), funs(paste0("outcome_t2"))) %>%
    rename_at(vars(one_of(toc2)), funs(paste0("outcome_c2"))) %>%
    rename(effect_size = "ES (from means)")  %>%
    mutate(rs = "ML1", #Add some descriptive information
           effect = tnames[i], 
           B_or_W = "Between", 
           design = "control vs. treatment", 
           or_stat_test = "Equal var t-test", 
           effect_type = "d",
           outcomes1_2 = "mean _ SD", #Describes the content of outcome1 and outcome2 variables
           Ntotal = ntreatment + ncontrol,
           country = c("USA", "BRA", "CZE", "USA", "USA", "MYS", "USA", "USA", "TUR", "CAN", "GBR", #Country of each study site
                       "USA", "USA", "CAN", rep("USA", 8), "POL", "POL", rep("USA", 3), "NLD", "USA",
                       "ITA", rep("USA", 6))) %>% 
    select(rs, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
           ncontrol, ntreatment,outcomes1_2, outcome_c1, outcome_t1, outcome_c2, outcome_t2) #select only variables of interest
}

teffects <- do.call("rbind", teffects) #bind into one dataframe

#******************************************
#Chi-square effects
#******************************************
#Effects extracted in this section
#[1] "Gain vs. loss framing"       
#[2] "Allowed vs. forbidden"       
#[3] "Norm of reciporcity"         
#[4] "Low vs. high category scales"
#*************************

#sheetnames of effects that use chisquare-tests
chisheets <- excel_sheets("../data/ML-_Summary_Statistics.xlsx")[c(15:18)] 
#extract data for these effects
chieffects <- lapply(chisheets, function (X) read_excel("../data/ML-_Summary_Statistics.xlsx", sheet = X)) #read data

chinames <-  c("Gain vs. loss framing", "Allowed vs. forbidden", #Clarify names
                 "Norm of reciprocity", "Low vs. high category scales")


tot1 <- toc1 <- tot2 <- toc2 <-  rep(NA, 4) #Vectors for variable names

#loops over the datasets and extracts variable names
for(i in seq_along(chinames)){
  tot1[i] <- names(chieffects[[i]])[2] #treatment count outcome A
  toc1[i] <- names(chieffects[[i]])[3] #control count outcome A
  tot2[i] <- names(chieffects[[i]])[4] #treatment count outcome B
  toc2[i] <- names(chieffects[[i]])[5] #control count outcome B
}

#Description of outcomes in the shape: "outcome 1 (t, c) _ outcome 2 (t, c)" where (t,c) are the between subjects groups
chioutcomes <- c("count exact (gain, loss) _ count probability (gain, loss)", 
                 "count allow (yes, no) _ count forbid (yes, no)",
                 "count yes (asked first, second) _ count no (asked first, second)",
                 "count < 2.5 hrs (low, high category) _ count > 2.5 hrs (low, high category)")
                 


for(i in seq_along(chinames)){
chieffects[[i]] <- chieffects[[i]] %>% #remove summary rows
  filter(!(Site == "Overall:" | Site == "Sum across samples:")) %>% 
    left_join(., lab, by = "Site") %>% #add online/in lab variable
    rename_at(vars(one_of(tot1)), funs(paste0("outcome_t1"))) %>% #'if column name is one of the names in tot1, rename as "outcome_t1"'
    rename_at(vars(one_of(toc1)), funs(paste0("outcome_c1"))) %>% #These lines will give some warnings, but they are ignorable
    rename_at(vars(one_of(tot2)), funs(paste0("outcome_t2"))) %>%
    rename_at(vars(one_of(toc2)), funs(paste0("outcome_c2"))) %>%
    rename(effect_size = "ES (from means)")  %>%
  mutate(rs = "ML1", #Add some descriptive information
         effect = chinames[i], 
         B_or_W = "Between", 
         design = "Choice of outcome 1 vs. 2 between groups", 
         or_stat_test = "Chisquare", 
         effect_type = "d",
         outcomes1_2 = chioutcomes[i], #Describes the content of outcome1 and outcome2 variables
         ntreatment = outcome_t1 + outcome_t2, #Assign more convenient names
         ncontrol = outcome_c1 + outcome_c2,
         Ntotal = ntreatment + ncontrol,
         country = c("USA", "BRA", "CZE", "USA", "USA", "MYS", "USA", "USA", "TUR", "CAN", "GBR",
                     "USA", "USA", "CAN", rep("USA", 8), "POL", "POL", rep("USA", 3), "NLD", "USA",
                     "ITA", rep("USA", 6))) %>% 
  select(rs, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
         ncontrol, ntreatment,outcomes1_2, outcome_c1, outcome_t1, outcome_c2, outcome_t2) #select only variables of interest
}

chieffects <- do.call("rbind", chieffects) #combine into one dataframe
#******************************************
#Math_Art Gender----
#******************************************
#Effects extracted in this section
#[1] "Gender math attitude"
#*****************

math_art <- read_excel("../data/ML-_Summary_Statistics.xlsx", sheet = "Math_Art Gender")

#Note that original authors remove the sample from the site "qccuny2"  (USA) due to a systematic error in data collection

math_art <- math_art %>% 
  filter(!(Site %in% c("Overall:", "Mean across samples:", "qccuny2"))) %>%  #remove summary rows and qccuny2
  left_join(., lab, by = "Site") %>% #add online/in lab variable
  rename(outcome_t1 = "Mean (Female)", 
         outcome_c1 = "Mean (Male)", 
         outcome_t2 = "SD (Female)", 
         outcome_c2 = "SD (Male)", 
         effect_size = "ES (from means)",
         ntreatment = "N (Female)",
         ncontrol = "N (Male)") %>% 
  mutate(rs = "ML1", #Add some descriptive information
         effect = "Gender math attitude", 
         B_or_W = "Between", 
         design = "control vs. treatment", 
         or_stat_test = "Equal var t-test", 
         effect_type = "d",
         outcomes1_2 = "mean _ SD",  #Describes the content of outcome1 and outcome2 variables
         Ntotal = ntreatment + ncontrol,
         country = c("USA", "BRA", "CZE", "USA", "USA", "MYS", "USA", "USA", "TUR", "CAN", "GBR", 
                     "USA", "USA", "CAN", rep("USA", 7), "POL", "POL", rep("USA", 3), "NLD", "USA", #rep USA 7 instead of 8 because of exclusion of qccuny2
                     "ITA", rep("USA", 6))) %>%
  select(rs, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
         ncontrol, ntreatment,outcomes1_2, outcome_c1, outcome_t1, outcome_c2, outcome_t2) #select only variables of interest

#******************************************
#Math explicit/implicit attitude correlation----
#******************************************
#Effects extracted in this section
#[1] "IAT correlation math"
#*****************

math_cor <- read_excel("../data/ML-_Summary_Statistics.xlsx", sheet = "IAT correlation")

#Note that original authors remove the sample from the site "qccuny2"  (USA) due to a systematic error in data collection

math_cor <- math_cor %>% 
  filter(!(Site %in% c("Overall:", "Sum across samples:", "qccuny2"))) %>%  #remove summary rows and qccuny2
  left_join(., lab, by = "Site") %>% #add online/in lab variable
  rename(Ntotal = "N",
         effect_size = "Correlation (r)") %>% 
  mutate(rs = "ML1", #Add some descriptive information
         effect = "IAT correlation math", 
         B_or_W = "Within", 
         design = "Implicit with explicit attitude", 
         or_stat_test = "Correlation", 
         effect_type = "r",
         outcomes1_2 = NA,  #Describes the content of outcome1 and outcome2 variables
         outcome_t1 = NA, 
         outcome_c1 = NA, 
         outcome_t2 = NA, 
         outcome_c2 = NA, 
         ntreatment = NA,
         ncontrol = NA,
         country = c("USA", "BRA", "CZE", "USA", "USA", "MYS", "USA", "USA", "TUR", "CAN", "GBR", 
                     "USA", "USA", "CAN", rep("USA", 7), "POL", "POL", rep("USA", 3), "NLD", "USA", #rep USA 7 instead of 8 because of exclusion of qccuny2
                     "ITA", rep("USA", 6))) %>%
  select(rs, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
         ncontrol, ntreatment,outcomes1_2, outcome_c1, outcome_t1, outcome_c2, outcome_t2) #select only variables of interest


#******************************************
#Combined dataframe----
#******************************************


ml1 <- rbind(teffects, chieffects, math_art, math_cor)


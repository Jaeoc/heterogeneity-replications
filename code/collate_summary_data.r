#*************************************************************************************************************************

##Project: MA heterogeneity project
##Code: Anton Ohlsson Collentine
##Script purpose: This script extracts, cleans and formats summary data from Many labs 1 & 3 and Registered Replication Reports 1 - 8 

##Script content:----

#[0] Packages
#[1] Many labs 1
#**[1.1] T-test effects
#**[1.2] Chi-square effects
#**[1.3] Math_Art Gender
#**[1.4] Math explicit/implicit attitude correlation
#**[1.5] ML1 combined
#[2] Many Labs 3
#**[2.1] Stroop effect
#**[2.2] T-test effects
#**[2.3] Non-parametric tests
#**[2.4] Interaction effects
#**[2.5] Correlation conscientiousness and persistance
#**[2.6] Ml3 combined
#[3] RRR1 & 2
#[4] RRR3 
#[5] RRR4 
#[6] RRR5 
#[7] RRR6 
#[8] RRR7 
#[9] RRR8 
#[10] Collate and save data

##Additional comments: To be able to run the entire script presumes this script is saved in a folder with the following relationship
#                      to the raw data "../data/" followed by a folder for each origin of data, see code in each section for details. 
#                      It also presumes the location of this script is used as the working directory unless otherwise specified and that 
#                      Rstudio is used

#*************************************************************************************************************************


wd_path <- dirname(rstudioapi::getSourceEditorContext()$path) #used later for correcting working directory


#******************************************
#[0] Packages----
#******************************************
#This section specifies all external packages used in this script. 
#For additional clarity there are commented library() calls in each section specifying the packages used in each section


if(!require("readxl")){install.packages("readxl")}
if(!require("dplyr")){install.packages("dplyr")}
if(!require("tidyr")) {install.packages("tidyr")}
if(!require("ghit")) {install.packages("ghit")} #works similarly to devtools function but more lightweight
if(!require("tabulizer")){ghit::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")} #on 64-bit Windows
#note that tabulizer requires rJava which in turn requires Java, and that windows (if used), R and Java must all
#either be x64 or x32 version (use Sys.info() & sessionInfo() to check windows and R versions). For more details: https://github.com/ropensci/tabulizer
#Also note that tabulizer has the package "png" as a dependency which may have to be installed separately first if the above code gives warnings

library(readxl)
library(dplyr)
library(tidyr)
library(tabulizer)


#******************************************
#[1] Many labs 1----
#******************************************
#summary data from Many labs 1 https://osf.io/wx7ck/
#Data extracted from https://osf.io/dmf62/
#Information on whether in-lab study or not extracted from https://osf.io/g3udn/

#library(readxl)
#library(dplyr)


#extract data on whether in-lab study or not
lab <- read_excel("../data/Ml1/Table_S1_-_Detailed_Site_and_Sample_Characteristics.xlsx") %>% 
  select(`Site identifier`, `Online (O) or Lab (L)`) %>% 
  rename(Site = `Site identifier`, in_lab = `Online (O) or Lab (L)`) %>% 
  mutate(in_lab = ifelse(in_lab == "L", 1, 0), Site = tolower(Site))


#*****************************************
#**[1.1] T-test effects----
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
tsheets <- excel_sheets("../data/Ml1/summary/ML-_Summary_Statistics.xlsx")[-c(1:2, 13:18)] 
#extract data for these effects
teffects <- lapply(tsheets, function (X) read_excel("../data/Ml1/summary/ML-_Summary_Statistics.xlsx", sheet = X)) #read data

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
           or_stat_test = "Independent samples t-test", #Equal var
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
#**[1.2] Chi-square effects----
#******************************************
#Effects extracted in this section
#[1] "Gain vs. loss framing"       
#[2] "Allowed vs. forbidden"       
#[3] "Norm of reciprocity"         
#[4] "Low vs. high category scales"
#*************************

#sheetnames of effects that use chisquare-tests
chisheets <- excel_sheets("../data/Ml1/summary/ML-_Summary_Statistics.xlsx")[c(15:18)] 
#extract data for these effects
chieffects <- lapply(chisheets, function (X) read_excel("../data/Ml1/summary/ML-_Summary_Statistics.xlsx", sheet = X)) #read data

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
chioutcomes <- c("count exact (gain, loss) _ count probability (gain, loss)", #I use the parentheses to clarify here because there is no clear treatment/control division
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
#**[1.3] Math_Art Gender----
#******************************************
#Effects extracted in this section
#[1] "Gender math attitude"
#*****************

math_art <- read_excel("../data/Ml1/summary/ML-_Summary_Statistics.xlsx", sheet = "Math_Art Gender")

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
         or_stat_test = "Independent samples t-test", #Equal var
         effect_type = "d",
         outcomes1_2 = "mean _ SD",  #Describes the content of outcome1 and outcome2 variables
         Ntotal = ntreatment + ncontrol,
         country = c("USA", "BRA", "CZE", "USA", "USA", "MYS", "USA", "USA", "TUR", "CAN", "GBR", 
                     "USA", "USA", "CAN", rep("USA", 7), "POL", "POL", rep("USA", 3), "NLD", "USA", #rep USA 7 instead of 8 because of exclusion of qccuny2
                     "ITA", rep("USA", 6))) %>%
  select(rs, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
         ncontrol, ntreatment,outcomes1_2, outcome_c1, outcome_t1, outcome_c2, outcome_t2) #select only variables of interest

#******************************************
#**[1.4] Math explicit/implicit attitude correlation----
#******************************************
#Effects extracted in this section
#[1] "IAT correlation math"
#*****************

math_cor <- read_excel("../data/Ml1/summary/ML-_Summary_Statistics.xlsx", sheet = "IAT correlation")

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
#**[1.5] ML1 combined----
#******************************************

ml1 <- rbind(teffects, chieffects, math_art, math_cor)


#******************************************
#[2] Many Labs 3----
#******************************************
##summary data from Many labs 3 https://osf.io/ct89g/
#Data extracted from https://osf.io/yhdau/

#library(dplyr)

#set working directory
setwd("../data/Ml3/summary/ML3 Meta") #needs to be set to be able to load all files at once

#All file names in path folder ending with .csv, excluding big five data
files <- list.files(pattern = "*.csv")[-2] 

#read files
ml3 <- lapply(files, read.csv) 
names(ml3) <- gsub(".csv", "", files) #set names


#*****************************************
#**[2.1] Stroop effect----
#*****************************************
#Effects extracted in this section
#[1] "Stroop effect"
#********************

#Additional comments:
#Original authors (see paper and their "ML3 Stroop Script.r", rows 156 - 198) calculated the difference 
#between congruent (t) and incongruent (c) and divided by overall SD. They then performed a one-sample
#t-test against the null hypothesis.

#*********************

ml3_stroop <- ml3[[7]]

ml3_stroop <- ml3_stroop %>% 
  rename(Ntotal = N, #Assign names consistent with other datasets
         effect_size = ES) %>% 
  mutate(rs = "ML3", #Add some descriptive information 
         in_lab = ifelse(Site == "mTurk", 0, 1), #mturk sample is only non-lab sample in this study
         effect = "Stroop effect", 
         B_or_W = "Within", 
         design = "Repeated measurements", 
         or_stat_test = "one sample t-test of std. mean diff", 
         effect_type = "d",
         outcomes1_2 = NA, 
         outcome_c1 = NA, 
         outcome_t1 = NA, 
         outcome_c2 = NA, 
         outcome_t2 = NA,
         ntreatment = NA, 
         ncontrol = NA,
         country = ifelse("mTurk" %in% Site, #if effect includes mturk sample add country label for that sample
                          c("USA","USA", "CAN", rep("USA", 14), "CAN", "USA", "USA", "USA"),
                          c("USA","USA", "CAN", rep("USA", 14), "CAN", "USA", "USA"))) %>%
  select(rs, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
         ncontrol, ntreatment, outcomes1_2, outcome_c1, outcome_t1, outcome_c2, outcome_t2) #select only variables of interest



#******************************************
#**[2.2] T-test effects----
#******************************************
#Effects extracted in this section
#[1]"Power and Perspective"
#[2] "Warmth Perceptions"   
#[3] "Weight Embodiment"
#*******************

#Additional comments:
#For some reason only power has the effect size included. However, in paper authors do t-tests on all 3 effects
#(equal var t-tests according to their R-code: e.g. "t.test(sarcasm~PowerCond,data=PowerData,var.equal=TRUE)")
#They also report cohens d-values for all effects. According to their R-code these are calculated using the 
#function "tes" from the package "compute.es", which computes cohen's d from t-tests.
#Accordingly, using an equivalent formula, I calculate cohen's ds for the warmt and weight effects (see function below)

#*********************

ml3t <- ml3[c("PowerPerspective", "WarmthPerceptions", "WeightEmbodiment")] #subset data for t-test effects
ml3t_names <- c("Power and Perspective", "Warmth Perceptions", "Weight Embodiment") #nicer names

#change the order of variables so that treatment and control group variables are in the same order as for other datframes
ml3t$WarmthPerceptions <- ml3t$WarmthPerceptions %>% #important for the first loop below
  select("Site", "NAgentic", "Ncommunal", "MAgentic", "MCommunal", "SDAgentic", "SDCommunal")


tnc <- tnt <- tot1 <- toc1 <- tot2 <- toc2 <-  rep(NA, 3) #Vectors for variable names

#loops over the datasets and extracts variable names
for(i in seq_along(ml3t_names)){
  tnc[i] <- names(ml3t[[i]])[2] #control n variable names
  tnt[i] <- names(ml3t[[i]])[3] #treatment n variable names
  toc1[i] <- names(ml3t[[i]])[4] #control means variable names
  tot1[i] <- names(ml3t[[i]])[5] #treatment means variable names
  toc2[i] <- names(ml3t[[i]])[6] #control SD variable names
  tot2[i] <- names(ml3t[[i]])[7] #treatment SD variable names
}

#Function to calculate cohen's d, 
#Formula 1 from Lakens (2013). "Calculating and reporting effect sizes to facilitate cumulative science: a prictical primer for t-tests and ANOVAs"
#This is equivalent to the formula used by the function ("tes") used by original authors to calculate cohen's d from a t-test (formula 2 in Lakens)
calc_d <- function(mt, mc, nt, nc, sdt, sdc){
  sp_numerator <- (nt - 1)*sdt^2 + (nc - 1)*sdc^2
  sp_denominator <- nt + nc - 2
  spooled <- sqrt(sp_numerator / sp_denominator)
  d <- (mt - mc) / spooled
  d
}



#loop over the 3 different effects (t-tests, same format) and cleans data
for(i in seq_along(ml3t_names)){
  ml3t[[i]] <- ml3t[[i]] %>% 
    rename_at(vars(one_of(tnt)), funs(paste0("ntreatment"))) %>% #'if column name is one of the names in tnt, rename as "ntreatment"'
    rename_at(vars(one_of(tnc)), funs(paste0("ncontrol"))) %>% #These lines will give some warnings, but they are ignorable
    rename_at(vars(one_of(tot1)), funs(paste0("outcome_t1"))) %>%
    rename_at(vars(one_of(toc1)), funs(paste0("outcome_c1"))) %>%
    rename_at(vars(one_of(tot2)), funs(paste0("outcome_t2"))) %>%
    rename_at(vars(one_of(toc2)), funs(paste0("outcome_c2"))) %>%
    mutate(ES..equal.var. = if(!"ES..equal.var." %in% names(.)) {calc_d(outcome_t1, outcome_c1, ntreatment, ncontrol, outcome_t2, outcome_c2)} 
           else {ES..equal.var.}) %>% #if dataframe does not contain cohen's d (ES..equal.var.), then calculate it for each row using calc_d function
    rename(effect_size = "ES..equal.var.")  %>%
    mutate(rs = "ML3", #Add some descriptive information
           effect = ml3t_names[i], 
           in_lab = ifelse(Site == "mTurk", 0, 1), #mturk sample is only non-lab sample in this study
           B_or_W = "Between", 
           design = "control vs. treatment", 
           or_stat_test = "Independent samples t-test", #equal var
           effect_type = "d",
           outcomes1_2 = "mean _ SD", #Describes the content of outcome1 and outcome2 variables
           Ntotal = ntreatment + ncontrol,
           country = ifelse("mTurk" %in% Site, #if dataframe includes mturk sample add one country label for that sample
                            c("USA","USA", "CAN", rep("USA", 14), "CAN", "USA", "USA", "USA"),
                            c("USA","USA", "CAN", rep("USA", 14), "CAN", "USA", "USA"))) %>%
    select(rs, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
           ncontrol, ntreatment,outcomes1_2, outcome_c1, outcome_t1, outcome_c2, outcome_t2) #select only variables of interest
}

ml3t <- do.call("rbind", ml3t) #bind into dataframe

#******************************************
#**[2.3] Non-parametric tests----
#******************************************
#Effects extracted in this section
#[1] "Availability"
#[2] "Metaphor"
#*********************

##%% Not completely sure if B_or_W should be "Between" or "Within" (or NA?)

nonp_names <- c("Availability", "Metaphor")

nonp <- ml3[names(ml3) %in% nonp_names] #limit data to only these effects

nonp_stat <- c("sign test", "Chisquare") #These variables used in loop below
nonp_outcomes <- c("count letter position (first, third) _ NA", #description of outcome variables used in loop below
                   "count prime (consistent, inconsistent) _ NA")
nonp_t1 <- c("First", "Consistent")
nonp_c1 <- c("Third", "Inconsistent")

#Loop over interaction data and clean it
for(i in seq_along(nonp_names)){
  nonp[[i]] <- nonp[[i]] %>% 
    rename_at(vars(one_of(nonp_c1)), funs(paste0("outcome_c1"))) %>% #"if variable name one_of... rename as paste0(..)"
    rename_at(vars(one_of(nonp_t1)), funs(paste0("outcome_t1"))) %>% #gives some warnings but they are ignorable
    rename(Ntotal = N, 
           effect_size = d) %>% 
    mutate(rs = "ML3", #Add some descriptive information 
           in_lab = ifelse(Site == "mTurk", 0, 1), #mturk sample is only non-lab sample in this study
           effect = nonp_names[i], 
           B_or_W = "Between", 
           design = "Choice of category 1 or 2", 
           or_stat_test = nonp_stat[i], 
           effect_type = "d",
           outcomes1_2 = nonp_outcomes[i],
           outcome_c2 = NA, 
           outcome_t2 = NA,
           ntreatment = NA, #Assign names consistent with other datasets
           ncontrol = NA,
           country = ifelse("mTurk" %in% Site, #if effect includes mturk sample add country label for that sample
                            c("USA","USA", "CAN", rep("USA", 14), "CAN", "USA", "USA", "USA"),
                            c("USA","USA", "CAN", rep("USA", 14), "CAN", "USA", "USA"))) %>%
    select(rs, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
           ncontrol, ntreatment, outcomes1_2, outcome_c1, outcome_t1, outcome_c2, outcome_t2) #select only variables of interest
}

nonp <- do.call("rbind", nonp) #bind together in one dataframe


#******************************************
#**[2.4] Interaction effects----
#******************************************
#Effects extracted in this section
#[1] "Credentials interaction"           
#[2] "Elaboration likelihood interaction"
#[3] "Subjective Distance interaction"
#*********************

inter <- ml3[names(ml3) %in% c("Credentials", "ELM", "SubjectiveDistance")] #limit data to only interactions

internames <- c("Credentials interaction", "Elaboration likelihood interaction", "Subjective Distance interaction")
interdesign <- c(rep("Group x continuous variable", 2), "Group x categorical variable")
interstat <- c(rep("Linear regression with ANOVA", 2), "ANOVA") #These 3 variables used in loop below

#Loop over interaction data and clean it
for(i in seq_along(internames)){
  inter[[i]] <- inter[[i]] %>% 
    rename(ntreatment = N2, #Assign names consistent with other datasets
           ncontrol = N1, 
           Ntotal = NT, 
           effect_size = EtaInter) %>% 
    mutate(rs = "ML3", #Add some descriptive information 
           in_lab = ifelse(Site == "mTurk", 0, 1), #mturk sample is only non-lab sample in this study
           effect = internames[i], 
           B_or_W = "Between", 
           design = interdesign[i], 
           or_stat_test = interstat[i], 
           effect_type = "partial eta2",
           outcomes1_2 = NA, 
           outcome_c1 = NA, 
           outcome_t1 = NA, 
           outcome_c2 = NA, 
           outcome_t2 = NA,
           country = ifelse("mTurk" %in% Site, #if effect includes mturk sample add country label for that sample
                            c("USA","USA", "CAN", rep("USA", 14), "CAN", "USA", "USA", "USA"),
                            c("USA","USA", "CAN", rep("USA", 14), "CAN", "USA", "USA"))) %>%
    select(rs, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
           ncontrol, ntreatment, outcomes1_2, outcome_c1, outcome_t1, outcome_c2, outcome_t2) #select only variables of interest
}

inter <- do.call("rbind", inter) #bind together in one dataframe


#******************************************
#**[2.5] Correlation conscientiousness and persistance----
#******************************************
#Effects extracted in this section
#[1] "Conscientiousness and persistance"
#*************************

cons_cor <- ml3$ConscientiousnessPersistence #limit data to only correlation

cons_cor <- cons_cor %>% 
  rename(Ntotal = N, #Assign names consistent with other datasets
         effect_size = r) %>% 
  mutate(rs = "ML3", #Add some descriptive information 
         in_lab = ifelse(Site == "mTurk", 0, 1), #mturk sample is only non-lab sample in this study
         effect = "Conscientiousness and persistance", 
         B_or_W = "Within", 
         design = "Conscientiousness with persistance", 
         or_stat_test = "correlation", 
         effect_type = "r",
         outcomes1_2 = NA, 
         outcome_c1 = NA, 
         outcome_t1 = NA, 
         outcome_c2 = NA, 
         outcome_t2 = NA,
         ntreatment = NA, 
         ncontrol = NA,
         country = ifelse("mTurk" %in% Site, #if effect includes mturk sample add country label for that sample
                          c("USA","USA", "CAN", rep("USA", 14), "CAN", "USA", "USA", "USA"),
                          c("USA","USA", "CAN", rep("USA", 14), "CAN", "USA", "USA"))) %>%
  select(rs, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
         ncontrol, ntreatment, outcomes1_2, outcome_c1, outcome_t1, outcome_c2, outcome_t2) #select only variables of interest


#******************************************
#**[2.6] Ml3 combined----
#******************************************

ml3 <- rbind(ml3_stroop, ml3t, nonp, inter, cons_cor) %>% 
  mutate(Site = recode(Site, IthacaCollege = "Ithaca", PennStateAbington = "abington", #recode Site names to be consistent with ml1
                       SanDiegoStateUniversity = "SDSU", TexasAandM = "TAMU", UniversityOfFlorida = "UFL",
                       UniversityOfVirginia = "UVA", VirginiaCommonwealthUniversity = "VCU", mTurk = "mturk")) 



#******************************************
#[3] RRR1 & 2 ----
#******************************************
##summary data from Registered Replication Report 1 & 2 https://osf.io/jymhe/
##Data RRR1 https://osf.io/dv5ei/
##Data RRR2 https://osf.io/dy6at/


#library(dplyr)
#library(readxl)

#set working directory
setwd(wd_path)
setwd("../data/RRR1_2") #needs to be set to be able to load all files at once

#All file names in path folder ending with .xlsx
files <- list.files(pattern = "*.xlsx")

rrr1_2 <- lapply(files, read_excel) #read files, tables with summary information for rrr1 & 2
names(rrr1_2) <- c("RRR1", "RRR2") #set names

#Clean and format data
rrr1_2 <- lapply(rrr1_2, function(x) x[-c(1:2),]) #Remove first two rows which are of no interest (old headers and original study data)

for(i in seq_along(files)){
  rrr1_2[[i]] <- rrr1_2[[i]] %>% 
    rename(Site = X__1, #rename variables to consistent names
           country = X__2,
           ntreatment = X__7, #total n treatment group after exclusions
           outcome_t1 = X__8, #n correcttreatment group
           ncontrol = X__14, #Total n control group after exclusions
           outcome_c1 = X__15) %>%  #n correct control group
    mutate(rs = names(rrr1_2)[i], #Add some descriptive information
           Site = ifelse(grepl("MTURK", Site), "mturk", Site), #if Site name contains MTURK recode to mturk
           effect = "Verbal overshadowing", 
           in_lab = ifelse(Site == "mturk", 0, 1), #Only the mturk study was not in the lab
           B_or_W = "Between", 
           design = "Outcome 1 vs. 2 between groups", 
           or_stat_test = "Chisquare", 
           effect_type = "Risk difference",
           outcomes1_2 = "count correct _ count incorrect", #Describes the content of outcome1 and outcome2 variables
           outcome_t2 = as.numeric(X__9) + as.numeric(X__10), #n unsuccessful identifications treatment group
           outcome_c2 = as.numeric(X__16) + as.numeric(X__17), #n unsuccessful identifications control group
           outcome_t1 = as.numeric(outcome_t1), #Necessary to convert these to numeric for the below calculations
           outcome_c1 = as.numeric(outcome_c1), 
           ntreatment = as.numeric(ntreatment),
           ncontrol = as.numeric(ncontrol),
           Ntotal = ntreatment + ncontrol,
           effect_size = outcome_t1  / ntreatment - outcome_c1 / ncontrol, #effect size is proportion correct treatment group - prop correct control group
           country = recode(country, #Convert country names into their official 3 letter acronymns
                            'New Zealand' = "NZL",
                            Canada = "CAN",
                            Italy = "ITA",
                            'United Kingdom' = "GBR",
                            Germany = "DEU",
                            'Czech Republic' = "CZE",
                            Poland = "POL",
                            Australia = "AUS",
                            Netherlands = "NLD")) %>% 
    select(rs, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
           ncontrol, ntreatment,outcomes1_2, outcome_c1, outcome_t1, outcome_c2, outcome_t2) #select only variables of interest
}

rrr1_2 <- do.call("rbind", rrr1_2)



#******************************************
#[4] RRR3 ----
#******************************************
##summary data from Registered Replication Report 3 https://osf.io/d3mw4/
##Data here: https://osf.io/fsmdk/

#library(dplyr)

setwd(wd_path) #reset working directory
rrr3 <- read.csv("../data/RRR3/Intentionality.csv", stringsAsFactors = FALSE)


rrr3 <- rrr3 %>% 
  slice(-1) %>% #Remove first row which contains data from original experiment that was replicated
  rename(Site = Author, #rename to consistent names
         outcome_t1 = mimp,
         outcome_t2 = sdimp,
         ntreatment = nimp,
         outcome_c1 = mperf,
         outcome_c2 = sdperf,
         ncontrol = nperf) %>% 
  mutate(rs = "RRR3", #Add some descriptive information
         effect = "Grammar on intentionality", #Based on curatescience.org I only use one out of the 3 DVs  
         Site = recode(Site, 'ONLINE-Eerland, Sherrill, Magliano, Zwaan' = "mturk"),
         in_lab = ifelse(Site == "mturk", 0, 1), # Only mturk study was online
         B_or_W = "Between", 
         design = "control vs. treatment", 
         or_stat_test = "Independent sample t-test", #No particular test, just looked at the effect and CI
         effect_type = "Raw mean difference",
         outcomes1_2 = "mean _ SD", #Describes the content of outcome1 and outcome2 variables
         Ntotal = ntreatment + ncontrol,
         effect_size = outcome_t1 - outcome_c1,
         country = c(rep("USA", 3), "CAN", "USA", "CAN", rep("USA", 6))) %>% #Country information taken from Table 1 of paper http://journals.sagepub.com/doi/pdf/10.1177/1745691615605826
  select(rs, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
         ncontrol, ntreatment,outcomes1_2, outcome_c1, outcome_t1, outcome_c2, outcome_t2) #select only variables of interest


#******************************************
#[5] RRR4 ----
#******************************************
##summary data from Registered Replication Report 4 https://osf.io/jymhe/
##Data from file named RTV_incl.csv https://osf.io/54nza/

#library(dplyr)

#Extract data
rrr4 <- read.csv("../data/RRR4/RTV_incl.csv", stringsAsFactors = FALSE)
rrr4 <- rrr4[, -which(nchar(names(rrr4)) < 3)] #remove empty columns that were extracted, removes all columns with a name shorter than 3 characters


#Clean and format data
rrr4 <- rrr4 %>% 
  rename(Site = Study.name, #rename variables to consistent names
         outcome_t1 = Ego.Depletion.Mean, #mean treatment group
         outcome_t2 = Ego.Depletion.Std.Dev, #SD treatment group
         ntreatment = Ego.Depletion.Sample.size,
         outcome_c1 = Control.Mean,#mean control group
         outcome_c2 = Control.Std.Dev, #SD control group
         ncontrol = Control.Sample.size,
         effect_size = Std.diff.in.means) %>%  #Cohen's d
  mutate(rs = "RRR4", #Add some descriptive information
         effect = "Ego depletion", 
         in_lab = 1, #Studies were in-lab
         B_or_W = "Between", 
         design = "control vs. treatment", 
         or_stat_test = "Independent samples t-test", 
         effect_type = "d",
         outcomes1_2 = "mean _ SD", #Describes the content of outcome1 and outcome2 variables
         Ntotal = ntreatment + ncontrol,
         country = c("CAN", rep("USA", 4), "CAN", "AUS", "USA", "USA", "NZL", "USA","NLD", #Add country indicators, country info taken from Table 1 in paper
                     "NLD", "DEU", "DEU", "FRA", "NLD", "DEU", "NLD", "BEL", "CHE", "DEU", "IDN")) %>% 
  select(rs, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
         ncontrol, ntreatment,outcomes1_2, outcome_c1, outcome_t1, outcome_c2, outcome_t2) #select only variables of interest





#******************************************
#[6] RRR5 ----
#******************************************
##summary data from Registered Replication Report 5 https://osf.io/s3hfr/
##Data extracted from directly from paper http://journals.sagepub.com/doi/pdf/10.1177/1745691616664694

library(tabulizer)
library(tidyr)
#library(dplyr)

#Data extraction
f <- "http://journals.sagepub.com/doi/pdf/10.1177/1745691616664694"
rrr5 <- extract_tables(f, pages = 3) #Total sample sizes and lab names

#Had to manually extract the means from Figure 2 ("Exit"). Couldn't figure out how to get the values using the r-code on OSF
high <- c(2.37, 1.4, 2.23, 1.52, 1.41, 1.87, 2.12, 1.83, 1.96, 1.87, 1.55, 1.81, 2, 1.59, 1.61, 2.07)
low <- c(2.25, 1.82, 2, 1.49, 1.67, 2.08, 1.96, 1.93, 2, 2.08, 1.79, 1.65, 2.04, 1.3, 1.73, 2.32)

countries <-  c("TUR", "USA", "USA", "USA", "USA", "CAN", "CAN", "USA", "USA", "USA", "USA", 
                "CAN", "CAN", "USA", "CZE", "SGP") #Lab countries from https://osf.io/5e7th/wiki/home/, note that the order on OSF is not the same as in the data and that "Lisa Reddoch" = Hoplock

#Clean and format
rrr5 <- as.data.frame(rrr5[[1]]) #only first list is actually table, keep that
rrr5 <- rrr5[-c(1:5, nrow(rrr5)),c(1, 5)] #keep only first column and exclusion 1 total (which was used in paper and is = Ntotal). Remove superflous rows.

rrr5 <- rrr5 %>% 
  separate(V1, into = c("Site", "N", "Male", "Female", "unreported", "Mean"), sep = " ") %>% 
  mutate(Ntotal = as.numeric(N) - as.numeric(V5)) %>% #total minus excluded total gives us the used total
  select(Site, Ntotal) %>% 
  mutate(rs = "RRR5", #Add some descriptive information, %%FIX EVERYTHING BELOW
         effect = "Commitment on forgiveness", 
         in_lab = 1, # "Participants were tested in-person", p.752 of RRR5
         B_or_W = "Between", 
         design = "control vs. treatment", 
         or_stat_test = "NA", #"The analysis does not focus on null-hypothesis significance testing." p. 755 RRR5 
         effect_type = "Raw mean difference",
         outcome_t1 = high,
         outcome_c1 = low,
         outcome_t2 = NA, #could not find SD or SE
         outcome_c2 = NA,
         ntreatment = NA, #Unable to find treatment group or control group sizes
         ncontrol = NA,
         outcomes1_2 = "mean _ NA", #Describes the content of outcome1 and outcome2 variables
         country = countries,
         effect_size = outcome_t1 - outcome_c1) %>% 
  select(rs, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
         ncontrol, ntreatment,outcomes1_2, outcome_c1, outcome_t1, outcome_c2, outcome_t2) #select only variables of interest




#******************************************
#[7] RRR6 ----
#******************************************
##summary data from Registered Replication Report 6 https://osf.io/hgi2y/
##Download RRR6 data https://osf.io/9j72u/ and run their code https://osf.io/9j72u/ to get a summary data file 

#library(tabulizer)
#library(dplyr)

#Extract data
rrr6 <- read.csv("../data/RRR6/resultsFacialFeedbackReplication.csv", stringsAsFactors = FALSE)
f <- "http://journals.sagepub.com/doi/pdf/10.1177/1745691616674458" #For extracting Table 1 from paper to get info on country for each lab
country <- extract_tables(f, pages = 5, method = "data.frame") #Note that if we do not care about group sample sizes, this table contain all other information

#Clean and format data
country <- as.data.frame(country) %>%
  slice(-1) %>% #remove first row as superflous
  select(Site = X, country = Country.of, Ntotal = Total.1) %>% #change names to those used elsewhere
  mutate(country = recode(country, #recode country names to their official 3 letter acronymns
                          U.S. = "USA",
                          Belgium = "BEL",
                          Canada = "CAN",
                          Italy = "ITA",
                          'United Kingdom' = "GBR",
                          'The Netherlands' = "NLD",
                          Turkey = "TUR",
                          Spain = "ESP"))

country$Site[12] <- "Ozdogru" #Fix badly extracted name
rrr6$studyIDs <- country$Site #Make names consistent over dataframes

rrr6 <- rrr6 %>% 
  select(Site = studyIDs, 
         outcome_t1 = meanSmileEx,
         outcome_c1 = meanPoutEx,
         outcome_t2 = sdSmileEx,
         outcome_c2 = sdPoutEx,
         ntreatment = nSmileEx,
         ncontrol = nPoutEx,
         effect_size = rawMeanDiffEx) %>% 
  left_join(., country) %>% 
  mutate(rs = "RRR6", #Add some descriptive information
         effect = "Facial Feedback hypothesis", 
         in_lab = 1, #In-lab study
         B_or_W = "Between", 
         design = "control vs. treatment", 
         or_stat_test = "NA", #No statistical test performed as part of main analysis, only looked at mean difference. Secondary used BF
         effect_type = "Raw mean difference",
         outcomes1_2 = "mean _ SD") %>% #Describes the content of outcome1 and outcome2 variables
  select(rs, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
         ncontrol, ntreatment,outcomes1_2, outcome_c1, outcome_t1, outcome_c2, outcome_t2) #order variables the same order as elsewhere




#******************************************
#[8] RRR7 ----
#******************************************
##summary data from Registered Replication Report 7 https://osf.io/scu2f/

#library(tabulizer)
#library(tidyr)
#library(dplyr)


##Extract data
f <- "http://journals.sagepub.com/doi/pdf/10.1177/1745691617693624" #temporarily load RRR7 pdf
out <- extract_tables(f, pages = c(8, 9)) #extract summary table (Table 2)
labs <- extract_tables(f, pages = 6) #extract Table 1 with demographic information

##Clean and format data
labs <- as.data.frame(labs, stringsAsFactors = FALSE) %>% 
  filter(!X2 %in% c("", "Country")) %>%  #Remove superfluous rows from country variable
  rename(Site = X1,
         country = X2) %>% 
  select(Site, country) %>% #select only variables of interest
  mutate(country = recode(country, #Recode country names to official 3 letter versions for consistency
                          Hungary = "HUN",
                          France = "FRA",
                          'The Netherlands' = "NLD",
                          'United Kingdom' = "GBR",
                          Portugal = "PRT",
                          Germany = "DEU",
                          'United States' = "USA",
                          'Unites States' = "USA", #misspelt in datafile
                          'Czech Republic' = "CZE",
                          Spain = "ESP",
                          Denmark = "DNK",
                          India = "IND",
                          Sweden = "SWE"))



rrr7 <- lapply(out, function(x) x[x[,2] == "All participants",]) #keep only rows with all participants, also removes headers due to their formatting
rrr7 <- as.data.frame(do.call("rbind", rrr7), stringsAsFactors = FALSE) #bind the two-page table into one dataframe

rrr7$V1[4] <- "Espin" #Name was strangely extracted, had to recode by index. This is for matching Site names with labs dataframe

rrr7 <- rrr7 %>% 
  separate(V4, into = c("drop_DT_mean", "drop_DT_SD", "outcome_t1", "outcome_t2"), sep = " ") %>% #separate columns that include several values into new columns
  separate(V7, into = c("outcome_c1", "outcome_c2"), sep = " ") %>% 
  mutate(outcome_c2 = gsub("\\(|\\)", "", outcome_c2), #remove parentheses from the variables that will be kept
         outcome_t2 = gsub("\\(|\\)", "", outcome_t2))

numcols <-  c("V3", "outcome_c1", "outcome_c2", "outcome_t1", "outcome_t2", "V5") #columns that need to be numeric
rrr7[, numcols] <- sapply(rrr7[,numcols], as.numeric) #change to numeric

rrr7 <- rrr7 %>% 
  rename(ntreatment = V3, #rename to consistent names
         ncontrol = V5,
         Site = V1) %>% 
  mutate(rs = "RRR7", #Add some descriptive information
         effect = "Intuitive-cooperation", 
         in_lab = 1, # Checking the implementation for each lab, they all run the study in qualtrix on lab-computers, https://osf.io/5ciaj/wiki/home/
         B_or_W = "Between", 
         design = "control vs. treatment", 
         or_stat_test = "Confidence Interval", #No particular test, just looked at the effect and CI
         effect_type = "Raw mean difference",
         outcomes1_2 = "mean _ SD", #Describes the content of outcome1 and outcome2 variables
         Ntotal = ntreatment + ncontrol,
         effect_size = outcome_t1 - outcome_c1) %>% ##%Note that because Table 2 of the article contains rounded values these effect sizes are not exactly the same as those reported in Figure 1 in the paper
  left_join(., labs, by = "Site") %>% #add country information for each lab
  select(rs, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
         ncontrol, ntreatment,outcomes1_2, outcome_c1, outcome_t1, outcome_c2, outcome_t2) #select only variables of interest



#******************************************
#[9] RRR8 ----
#******************************************
##summary data from Registered Replication Report 8 https://osf.io/k27hm/
##Data contained in zip-file named final_results.zip: https://osf.io/mxugy/

#library(tabulizer)
#library(dplyr)

#Extract data
f <- "../data/RRR8/final_results/tables/all_raw_effects.pdf" #path to table (which includes country names)
rrr8 <- extract_tables(f)

#Clean and format data
rrr8 <- lapply(rrr8, function(x) x[x[,2] == "Main Effect",]) #remove effects that are not the primary effect, note that this drops the headers which are in row 1 of the first list
rrr8[[1]] <- rrr8[[1]][, -c(4, 6, 8, 10, 18)] #drop a few by extraction incorrectly added columns

rrr8 <- as.data.frame(do.call("rbind", rrr8), stringsAsFactors = FALSE)#conmbine into one dataframe

rrr8[, 3:8] <- sapply(rrr8[,3:8], as.numeric) #change relevant columns to numeric

rrr8 <- rrr8 %>% 
  rename(Site = V1, #rename variables to consistent names
         outcome_t1 = V3, #mean treatment group
         ntreatment = V4,
         outcome_c1 = V5,#mean control group
         ncontrol = V6,
         effect_size = V7, #Mean difference
         outcome_c2 = V8, #standard error
         country = V11) %>% 
  mutate(rs = "RRR8", #Add some descriptive information
         effect = "Professor priming", 
         in_lab = 1, # All participants required to be in individual cubicles or at independent workstations where they could not see each other
         B_or_W = "Between", 
         design = "control vs. treatment", 
         or_stat_test = "Confidence Interval", #No particular test, just looked at the effect and CI
         effect_type = "Raw mean difference",
         outcomes1_2 = "mean _ SE", #Describes the content of outcome1 and outcome2 variables
         Ntotal = ntreatment + ncontrol,
         outcome_t2 = NA, #only have sE of the effect, so nothing for this variable
         country = recode(country, #Recode country names to official three letter acronyms for consistency
                          Hungary = "HUN",
                          'United Arab Emirates' = "ARE",
                          'United States' = "USA",
                          Poland = "POL",
                          Canada = "CAN",
                          Netherlands = "NLD",
                          Belgium = "BEL",
                          Sweden = "SWE",
                          France = "FRA",
                          'United Kingdom' = "GBR",
                          Australia = "AUS",
                          Turkey = "TUR",
                          'New Zealand' = "NZL",
                          Germany = "DEU",
                          Slovakia = "SVK",
                          Switzerland = "CHE",
                          Colombia = "COL",
                          Singapore = "SGP",
                          Spain = "ESP"))  %>% 
  select(rs, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
         ncontrol, ntreatment,outcomes1_2, outcome_c1, outcome_t1, outcome_c2, outcome_t2) #select only variables of interest



#******************************************
#[10] Collate and save data ----
#******************************************

effects <- rbind(ml1, ml3, rrr1_2, rrr3, rrr4, rrr5, rrr6, rrr7, rrr8)

write.csv(effects, "../data/collated_summary_data.csv", row.names = FALSE)

#temporary
library(splitstackshape)

set.seed(43)
eff2 <- effects %>% #stratified sample, one for each effect
  stratified(., "effect", 1) %>% 
  arrange(rs) #Arrange in order by rs

library(xlsx)

write.xlsx(eff2, "../data/overview_summary_data.xlsx", sheetName = "Quick overview", row.names = FALSE)

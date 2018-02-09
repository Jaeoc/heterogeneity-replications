##MA heterogeneity project - summary data from Many labs 3 https://osf.io/ct89g/

#******************************************
#Ml3 setup----
#******************************************

if(!require("dplyr")){install.packages("dplyr")}
library(dplyr)

#set working directory
setwd("../data/ML3 Meta") #needs to be set to be able to load all files at once

#All file names in path folder ending with .csv, excluding big five data
files <- list.files(pattern = "*.csv")[-2] 

#read files
ml3 <- lapply(files, read.csv) 
names(ml3) <- gsub(".csv", "", files) #set names

#mutate Site names to be consistent at the end instead with the full dataframe is more efficient, double check mturk name "mTurk"
         #Site = recode(Site, IthacaCollege = "Ithaca", PennStateAbington = "abington",
          #             SanDiegoStateUniversity = "SDSU", TexasAandM = "TAMU", UniversityOfFlorida = "UFL",
           #            UniversityOfVirginia = "UVA", VirginiaCommonwealthUniversity = "VCU")) 

#*****************************************
#Ml3 Stroop effect---
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
#Ml3 t-tests----
#******************************************
#Effects extracted in this section
#[1] "Power and Perspective"
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
           or_stat_test = "Equal var t-test", 
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
#Ml3 non-parametric tests----
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
#Ml3 interaction effects----
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
#Ml3 correlation conscientiousness and persistance----
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
#Ml3 combined----
#******************************************

ml3 <- rbind(ml3_stroop, ml3t, nonp, inter, cons_cor) %>% 
  mutate(Site = recode(Site, IthacaCollege = "Ithaca", PennStateAbington = "abington", #recode Site names to be consistent with ml1
               SanDiegoStateUniversity = "SDSU", TexasAandM = "TAMU", UniversityOfFlorida = "UFL",
              UniversityOfVirginia = "UVA", VirginiaCommonwealthUniversity = "VCU", mTurk = "mturk")) 
  



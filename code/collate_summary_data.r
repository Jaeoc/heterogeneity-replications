#*************************************************************************************************************************

##Project: Heterogeneity in direct replications in psychology and its association with effect size
##Script purpose: This script extracts, cleans and formats summary data from Many labs 1 & 3 and Registered Replication Reports 1 - 8 
##Code: 

##Script content:----

#[0] Packages
#[1] Many labs 1
#**[1.1] t-test effects
#**[1.2] Chi-square effects
#**[1.3] Math_Art Gender
#**[1.4] Math explicit/implicit attitude correlation
#**[1.5] ML1 combined
#[2] Many Labs 2
#**[2.1] t-test effects
#**[2.2] Chi-square effects (odds ratios)
#**[2.3] Correlation effects
#**[2.4] Cohen's q effects
#**[2.5] Actions are Choices (Savani et al., 2010)
#**[2.5] Choosing or Rejecting (Shafir, 1993)
#**[2.6] ML2 combined
#[3] Many Labs 3
#**[3.1] Stroop effect
#**[3.2] t-test effects
#**[3.3] Non-parametric tests
#**[3.4] Interaction effects
#**[3.5] Correlation conscientiousness and persistence
#**[3.6] ML3 combined
#[4] RRR1 & 2
#[5] RRR3 
#[6] RRR4 
#[7] RRR5 
#[8] RRR6 
#[9] RRR7 
#[10] RRR8 
#[11] RRR9
#[12] RRR10
#[13] Collate and save data

##Additional comments: To be able to run the entire script presumes this script is saved in a folder with the following relationship
#                      to the raw data "../data/" followed by a folder for each origin of data, see code in each section for details. 
#                      It also presumes the location of this script is used as the working directory unless otherwise specified and that 
#                      Rstudio is used

#*************************************************************************************************************************


wd_path <- dirname(rstudioapi::getSourceEditorContext()$path) #used for correcting working directory, requires Rstudio
setwd(wd_path)

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
lab <- read_excel("../data/source/Ml1/Table_S1_-_Detailed_Site_and_Sample_Characteristics.xlsx") %>% 
  select(`Site identifier`, `Online (O) or Lab (L)`) %>% 
  rename(Site = `Site identifier`, in_lab = `Online (O) or Lab (L)`) %>% 
  mutate(in_lab = ifelse(in_lab == "L", 1, 0), Site = tolower(Site))


#*****************************************
#**[1.1] t-test effects----
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
tsheets <- excel_sheets("../data/source/Ml1/summary/ML-_Summary_Statistics.xlsx")[-c(1:2, 13:18)] 
#extract data for these effects
teffects <- lapply(tsheets, function (X) read_excel("../data/source/Ml1/summary/ML-_Summary_Statistics.xlsx", sheet = X)) #read data

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
    mutate(rp = "ML1", #Add some descriptive information
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
    select(rp, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
           ntreatment, ncontrol, outcomes1_2, outcome_t1, outcome_c1, outcome_t2, outcome_c2) #select only variables of interest
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
chisheets <- excel_sheets("../data/source/Ml1/summary/ML-_Summary_Statistics.xlsx")[c(15:18)] 
#extract data for these effects
chieffects <- lapply(chisheets, function (X) read_excel("../data/source/Ml1/summary/ML-_Summary_Statistics.xlsx", sheet = X)) #read data

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
chieffects[[i]] <- chieffects[[i]] %>% 
  filter(!(Site == "Overall:" | Site == "Sum across samples:")) %>% #remove summary rows
    left_join(., lab, by = "Site") %>% #add online/in lab variable
    rename_at(vars(one_of(tot1)), funs(paste0("outcome_t1"))) %>% #'if column name is one of the names in tot1, rename as "outcome_t1"'
    rename_at(vars(one_of(toc1)), funs(paste0("outcome_c1"))) %>% #These lines will give some warnings, but they are ignorable
    rename_at(vars(one_of(tot2)), funs(paste0("outcome_t2"))) %>%
    rename_at(vars(one_of(toc2)), funs(paste0("outcome_c2"))) %>%
    rename(effect_size = "ES (from means)")  %>%
  mutate(rp = "ML1", #Add some descriptive information
         effect = chinames[i], 
         B_or_W = "Between", 
         design = "Choice of outcome 1 vs. 2 between groups", 
         or_stat_test = "Chisquare", 
         effect_type = "d",
         outcomes1_2 = chioutcomes[i], #Describes the content of outcome1 and outcome2 variables
         ntreatment = outcome_t1 + outcome_t2,
         ncontrol = outcome_c1 + outcome_c2,
         Ntotal = ntreatment + ncontrol,
         country = c("USA", "BRA", "CZE", "USA", "USA", "MYS", "USA", "USA", "TUR", "CAN", "GBR",
                     "USA", "USA", "CAN", rep("USA", 8), "POL", "POL", rep("USA", 3), "NLD", "USA",
                     "ITA", rep("USA", 6))) %>% 
  select(rp, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
         ntreatment, ncontrol, outcomes1_2, outcome_t1, outcome_c1, outcome_t2, outcome_c2) #select only variables of interest
}

chieffects <- do.call("rbind", chieffects) #combine into one dataframe
#******************************************
#**[1.3] Math_Art Gender----
#******************************************
#Effects extracted in this section
#[1] "Gender math attitude"
#*****************

math_art <- read_excel("../data/source/Ml1/summary/ML-_Summary_Statistics.xlsx", sheet = "Math_Art Gender")

#Note that original authors remove the sample from the site "qccuny2" (USA) due to a systematic error in data collection (as do I)

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
  mutate(rp = "ML1", #Add some descriptive information
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
  select(rp, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
         ntreatment, ncontrol, outcomes1_2, outcome_t1, outcome_c1, outcome_t2, outcome_c2) #select only variables of interest

#******************************************
#**[1.4] Math explicit/implicit attitude correlation----
#******************************************
#Effects extracted in this section
#[1] "IAT correlation math"
#*****************

math_cor <- read_excel("../data/source/Ml1/summary/ML-_Summary_Statistics.xlsx", sheet = "IAT correlation")

#Note that original authors remove the sample from the site "qccuny2"  (USA) due to a systematic error in data collection (as do I)

math_cor <- math_cor %>% 
  filter(!(Site %in% c("Overall:", "Sum across samples:", "qccuny2"))) %>%  #remove summary rows and qccuny2
  left_join(., lab, by = "Site") %>% #add online/in lab variable
  rename(Ntotal = "N",
         effect_size = "Correlation (r)") %>% 
  mutate(rp = "ML1", #Add some descriptive information
         effect = "IAT correlation math", 
         B_or_W = "Within", 
         design = "Implicit with explicit attitude", 
         or_stat_test = "Correlation", 
         effect_type = "r",
         outcomes1_2 = "NA _ NA",  #Describes the content of outcome1 and outcome2 variables, as character instead of simply NA to facilitate working with data
         outcome_t1 = NA, 
         outcome_c1 = NA, 
         outcome_t2 = NA, 
         outcome_c2 = NA, 
         ntreatment = NA,
         ncontrol = NA,
         country = c("USA", "BRA", "CZE", "USA", "USA", "MYS", "USA", "USA", "TUR", "CAN", "GBR", 
                     "USA", "USA", "CAN", rep("USA", 7), "POL", "POL", rep("USA", 3), "NLD", "USA", #rep USA 7 instead of 8 because of exclusion of qccuny2
                     "ITA", rep("USA", 6))) %>%
  select(rp, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
         ntreatment, ncontrol, outcomes1_2, outcome_t1, outcome_c1, outcome_t2, outcome_c2) #select only variables of interest


#******************************************
#**[1.5] ML1 combined----
#******************************************

ml1 <- rbind(teffects, chieffects, math_art, math_cor)


#******************************************
#[2] Many Labs 2 ----
#******************************************
##summary data from Many Labs 2 https://osf.io/8cd4r/
##Direct link to file with summary data: https://osf.io/j7mhf/
##The relevant data file was identified in the meta-analysis script ("ML2_meta_analyses_simple.R") on line 32: https://osf.io/4akjw/

#Additonal comments: The 'Global effect sizes' reported in ML2 are _not_ meta-analytic effect sizes (rather
#                    disaggregated effect sizes). For tau-values computed in Table 3 of ML2, the effect sizes
#                    converted to correlations are used. In general, ML2 first computed relevant test statistics
#                    for each effect, then fed these to the R-package MBESS to compute non-centrality parameters (ncp)
#                    and ncp confidence intervals. These were then passed to the R-package compute.es to compute
#                    all different types of effect sizes. These converted effect sizes are the ones reported in the 
#                    dataset as e.g., ESCI.r. [personal communication 2019-10-10, Fred Hasselman]. 

#library(dplyr)

#******************************************
#Load data
ml2 <- read.csv("../data/source/Ml2/Data_Figure_NOweird.csv", stringsAsFactors = FALSE)

#Initial clean and format data
ml2$online[ml2$source.Setting%in%c("In a classroom","In a lab")] <- "lab" #from line 35 in ML2_meta_analyses_simple.R https://osf.io/4akjw/
ml2$online[ml2$source.Setting%in%c("Online (at home)")] <- "online" #from line 36 in ML2_meta_analyses_simple.R https://osf.io/4akjw/
#******************************************

#**[2.1] t-test effects----
#*****************************************
#Effects extracted in this section
# [1] "Structure & Goal Pursuit (Kay et al., 2014)"    
# [2] "Priming Consumerism (Bauer et al., 2012)"       
# [3] "Correspondence Bias (Miyamoto & Kitayama, 2002)"
# [4] "Incidental Anchors (Critcher & Gilovich, 2008)" 
# [5] "SMS & Well-Being (Anderson et al., 2012)"       
# [6] "False Consensus 1 (Ross et al., 1977)"          
# [7] "False Consensus 2 (Ross et al., 1977)"          
# [8] "Position & Power (Giessner & Schubert, 2007)"   
# [9] "Intuitive Reasoning (Norenzayan et al., 2002)"  
# [10] "Less is Better (Hsee, 1998)"                    
# [11] "Moral Typecasting (Gray & Wegner, 2009)"        
# [12] "Moral Cleansing (Zhong & Liljenquist, 2006)"    
# [13] "Intentional Side-Effects (Knobe, 2003)"         
# [14] "Direction & Similarity (Tversky & Gati, 1978)"  
# [15] "Direction & SES (Huang et al., 2014)"           
# [16] "Incidental Disfluency (Alter et al., 2007)"     
# [17] "Tempting Fate (Risen & Gilovich, 2008)"         
# [18] "Priming Warmth (Zaval et al., 2014)"   
#**************

##SMD effects
ml2_smd <- ml2 %>% filter(!is.na(stat.cond1.sd)) #drop all effects withouth standard deviation (= 4 odds ratio effects, 1 regression effect)
ml2_smd <- ml2_smd %>% #Drop remaining non-standard SMD effects
  filter(!analysis.name %in% c("Graham.1", #correlation r
                               "Inbar.1a", #difference between correlations q
                               "vanLange.1", #correlation r
                               "Shafir.1", #strange effect 
                               "Schwarz.1a")) #difference between correlations q


ml2_smd <- ml2_smd %>% 
  rename(effect = study.description, #rename variables to consistent names
         Site = study.source, 
         in_lab = online, #
         outcome_t1 = stat.cond1.mean, #mean treatment group
         ntreatment = stat.n1,
         outcome_c1 = stat.cond2.mean,#mean control group
         ncontrol = stat.n2,
         outcome_t2 = stat.cond1.sd,
         outcome_c2 = stat.cond2.sd,
         effect_size = ESCI.r, #NB! ML2 used correlations for computing tau-values in their Table 3. First t-tests were computed (either Welch's or not) and then transformed into correlations uisng compute.es package
         Ntotal = stat.N,
         country = source.Country) %>%  
  mutate(rp = "ML2", #Add some descriptive information
         B_or_W = "Between", 
         design = "control vs. treatment", 
         or_stat_test = "t-test", 
         effect_type = "r",
         outcomes1_2 = "mean _ SD", 
         effect = trimws(gsub("\\(.*", "", .$effect)), #shorten effect names a litle
         country = recode(country, #Recode country names to official three letter acronyms for consistency
                          Hungary = "HUN", #
                          'United Arab Emirates' = "ARE", #
                          `Hong Kong, China` = "HKG",
                          India = "IND",
                          Italy = "ITA",
                          Japan = "JPN",
                          Malaysia = "MYS",
                          Mexico = "MEX",
                          Nigeria = "NGA",
                          Portugal = "PRT",
                          Serbia = "SRB", 
                          `South Africa` = "ZAF",
                          Taiwan = "TWN",
                          Tanzania = "TZA",
                          Uruguay = "URY",
                          Poland = "POL", #
                          Canada = "CAN", #
                          `The Netherlands` = "NLD",
                          Belgium = "BEL", #
                          Sweden = "SWE", #
                          France = "FRA", #
                          UK = "GBR",
                          Australia = "AUS", #
                          Brazil = "BRA",
                          Chile = "CHL",
                          China = "CHN",
                          Turkey = "TUR",
                          'Czech Republic' = "CZE",
                          'New Zealand' = "NZL", #
                          Germany = "DEU", #
                          Switzerland = "CHE", #
                          Colombia = "COL", #
                          `Costa Rica` = "CRI",
                          Spain = "ESP")) %>%
  select(rp, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
         ntreatment, ncontrol, outcomes1_2, outcome_t1, outcome_c1, outcome_t2, outcome_c2) #select only variables of interest

#*****************************************
#**[2.2] Chi-squre effects (odds ratios)----
#*****************************************
#Effects extracted in this section
# [1] "Affect & Risk (Rottenstreich & Hsee, 2001)"
# [2] "Trolley Dilemma 1 (Hauser et al., 2007)"   
# [3] "Framing (Tversky & Kahneman, 1981)"        
# [4] "Trolley Dilemma 2 (Hauser et al., 2007)"   
#**************
ml2_or <- ml2 %>% filter(is.na(stat.cond1.sd)) %>% #Keep only effects without SD (= 4 odds ratio effects, 1 )
  filter(!study.description == "Actions are Choices (Savani et al., 2010)") #hierarchical logistic regression effect, drop from this section

ml2_or <- ml2_or %>% 
  rename(effect = study.description, #rename variables to consistent names
         Site = study.source, 
         in_lab = online, #
         outcome_t1 = stat.cond1.count, #
         outcome_c1 = stat.cond3.count ,#
         outcome_t2 = stat.cond2.count,
         outcome_c2 = stat.cond4.count,
         effect_size = ESCI.r, #NB! ML2 used correlations for computing tau-values in their Table 3. First non-central chi-square values were computen and then transformed into correlations uisng compute.es package (and then OR, which is why computing the OR directly results in slightly different values compared to those in ML2 dataset)
         Ntotal = stat.N,
         country = source.Country) %>%  
  mutate(rp = "ML2", #Add some descriptive information
         B_or_W = "Between", 
         design = "Choice of outcome 1 vs. 2 between groups", 
         ntreatment = outcome_t1 + outcome_t2,
         ncontrol = outcome_c1 + outcome_c2,
         or_stat_test = "Chisquare", 
         effect_type = "r",
         effect = trimws(gsub("\\(.*", "", .$effect)), #shorten effect names a litle
         outcomes1_2 = case_when( #Description of outcomes in the shape: "outcome 1 (t, c) _ outcome 2 (t, c)" where (t,c) are the between subjects groups
           effect == "Affect & Risk" ~ "count moviestar (low, certain) _ count money (low, certain)", #I use the parentheses to clarify here because there is no clear treatment/control division
           effect == "Trolley Dilemma 1" ~ "count yes (side effect, greater good) _ count no (side effect, greater good)",
           effect == "Framing" ~ "count yes (cheap, costly) _ count no (cheap, costly)",
           effect == "Trolley Dilemma  2" ~  "count yes (side effect, greater good) _ count no (side effect, greater good)"),
         country = recode(country, #Recode country names to official three letter acronyms for consistency
                          Hungary = "HUN", #
                          'United Arab Emirates' = "ARE", #
                          `Hong Kong, China` = "HKG",
                          India = "IND",
                          Italy = "ITA",
                          Japan = "JPN",
                          Malaysia = "MYS",
                          Mexico = "MEX",
                          Nigeria = "NGA",
                          Portugal = "PRT",
                          Serbia = "SRB", 
                          `South Africa` = "ZAF",
                          Taiwan = "TWN",
                          Tanzania = "TZA",
                          Uruguay = "URY",
                          Poland = "POL", #
                          Canada = "CAN", #
                          `The Netherlands` = "NLD",
                          Belgium = "BEL", #
                          Sweden = "SWE", #
                          France = "FRA", #
                          UK = "GBR",
                          Australia = "AUS", #
                          Brazil = "BRA",
                          Chile = "CHL",
                          China = "CHN",
                          Turkey = "TUR",
                          'Czech Republic' = "CZE",
                          'New Zealand' = "NZL", #
                          Germany = "DEU", #
                          Switzerland = "CHE", #
                          Colombia = "COL", #
                          `Costa Rica` = "CRI",
                          Spain = "ESP")) %>% 
  select(rp, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
         ntreatment, ncontrol, outcomes1_2, outcome_t1, outcome_c1, outcome_t2, outcome_c2) #select only variables of interest

##NB! Should maybe change the order of outcome_c1 and outcome_t1 in the final select everywhere, right now it makes it confusing for these effects what's what
#with the Odds ratios

#*****************************************
#**[2.3] Correlations----
#*****************************************
#Effects extracted in this section
# [1] "Moral Foundations (Graham et al., 2009)"          
# [2] "Social Value Orientation (Van Lange et al., 1997)" 
#**************

#NB! Check with Marcel about this converting to non-central statistic and then to correlation in this case and what it means

ml2_r <- ml2 %>%  filter(analysis.name %in% c("Graham.1", "vanLange.1")) 

ml2_r <- ml2_r %>% 
  rename(effect = study.description, #rename variables to consistent names
         Site = study.source, 
         in_lab = online, #
         effect_size = ESCI.r, #NB! ML2 computed effect sizes by first computing non-central test statics and then transformed into correlations uisng compute.es package (I believe, although seems strange, check with Marcel)
         Ntotal = stat.N,
         country = source.Country) %>%  
  mutate(rp = "ML2", #Add some descriptive information
         B_or_W = "Within", 
         effect = trimws(gsub("\\(.*", "", .$effect)), #shorten effect names a litle
         design = case_when(
           effect == "Moral Foundations" ~ "moral foundations with political leaning",
           effect == "Social Value Orientation" ~ "SVO with family size"),
         or_stat_test = "Fisher's r-to-z 1-cor", 
         effect_type = "r",
         outcomes1_2 = "NA _ NA",
         outcome_t1 = NA,
         outcome_c1 = NA,
         outcome_t2 = NA,
         outcome_c2 = NA,
         ntreatment = NA,
         ncontrol = NA,
         country = recode(country, #Recode country names to official three letter acronyms for consistency
                          Hungary = "HUN", #
                          'United Arab Emirates' = "ARE", #
                          `Hong Kong, China` = "HKG",
                          India = "IND",
                          Italy = "ITA",
                          Japan = "JPN",
                          Malaysia = "MYS",
                          Mexico = "MEX",
                          Nigeria = "NGA",
                          Portugal = "PRT",
                          Serbia = "SRB", 
                          `South Africa` = "ZAF",
                          Taiwan = "TWN",
                          Tanzania = "TZA",
                          Uruguay = "URY",
                          Poland = "POL", #
                          Canada = "CAN", #
                          `The Netherlands` = "NLD",
                          Belgium = "BEL", #
                          Sweden = "SWE", #
                          France = "FRA", #
                          UK = "GBR",
                          Australia = "AUS", #
                          Brazil = "BRA",
                          Chile = "CHL",
                          China = "CHN",
                          Turkey = "TUR",
                          'Czech Republic' = "CZE",
                          'New Zealand' = "NZL", #
                          Germany = "DEU", #
                          Switzerland = "CHE", #
                          Colombia = "COL", #
                          `Costa Rica` = "CRI",
                          Spain = "ESP")) %>% 
  select(rp, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
         ntreatment, ncontrol, outcomes1_2, outcome_t1, outcome_c1, outcome_t2, outcome_c2) #select only variables of interest


#*****************************************
#**[2.3] Cohen's q effects----
#*****************************************
#Effects extracted in this section
# [1] "Disgust & Homophobia (Inbar et al., 2009)"     
# [2] "Assimilation & Contrast (Schwarz et al., 1991)"
#******************************************
#code to meta-analyze [private communcation] and file "Code_Inbar_Schwarz.R" available at https://github.com/ManyLabsOpenScience/ManyLabs2/tree/master/Script%20-%20Meta%20analyses [2019-10-22]

ml2_q <- ml2 %>% filter(analysis.name %in% c("Inbar.1a", #difference between correlations q
                                             "Schwarz.1a")) #difference between correlations q

ml2_q <- ml2_q %>% 
  rename(effect = study.description, #rename variables to consistent names
         Site = study.source, 
         in_lab = online, #
         effect_size = ESCI.cohensQ, #NB! ML2 computed effect sizes by first computing non-central test statics and then transformed into correlations uisng compute.es package (I believe, although seems strange, check with Marcel)
         Ntotal = stat.N,
         ntreatment = stat.n1,
         ncontrol = stat.n2,
         country = source.Country) %>%  
  mutate(rp = "ML2", #Add some descriptive information
         B_or_W = "Between", 
         effect = trimws(gsub("\\(.*", "", .$effect)), #shorten effect names a litle
         design = case_when(
           effect == "Disgust & Homophobia" ~ "Disgust sensitivity with intentionality group1 vs. disgust sensitivity with intentionality group2",
           effect == "Assimilation & Contrast" ~ "Question1 with question2 vs. question2 with question1"),
         or_stat_test = "Cohen's q", 
         effect_type = "q",
         outcomes1_2 = "t1 = sampling_var(q) _ NA",
         outcome_t1 = 1/(ntreatment-3) + 1/(ncontrol-3), #sampling variance of cohen's q as used by ML2, see "Code_Inbar_Schwarz.r" line 24
         outcome_c1 = NA,
         outcome_t2 = NA,
         outcome_c2 = NA,
         country = recode(country, #Recode country names to official three letter acronyms for consistency
                          Hungary = "HUN", #
                          'United Arab Emirates' = "ARE", #
                          `Hong Kong, China` = "HKG",
                          India = "IND",
                          Italy = "ITA",
                          Japan = "JPN",
                          Malaysia = "MYS",
                          Mexico = "MEX",
                          Nigeria = "NGA",
                          Portugal = "PRT",
                          Serbia = "SRB", 
                          `South Africa` = "ZAF",
                          Taiwan = "TWN",
                          Tanzania = "TZA",
                          Uruguay = "URY",
                          Poland = "POL", #
                          Canada = "CAN", #
                          `The Netherlands` = "NLD",
                          Belgium = "BEL", #
                          Sweden = "SWE", #
                          France = "FRA", #
                          UK = "GBR",
                          Australia = "AUS", #
                          Brazil = "BRA",
                          Chile = "CHL",
                          China = "CHN",
                          Turkey = "TUR",
                          'Czech Republic' = "CZE",
                          'New Zealand' = "NZL", #
                          Germany = "DEU", #
                          Switzerland = "CHE", #
                          Colombia = "COL", #
                          `Costa Rica` = "CRI",
                          Spain = "ESP")) %>% 
  select(rp, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
         ntreatment, ncontrol, outcomes1_2, outcome_t1, outcome_c1, outcome_t2, outcome_c2) #select only variables of interest

#*****************************************
#**[2.4] Actions are Choices (Savani et al., 2010)----
#*****************************************
#Effects extracted in this section
# [1] "Actions are Choices (Savani et al., 2010)" 
#**************

ml2_savani <- ml2 %>% filter(analysis.name == "Savani.3a")
#"The effect of interest was the odds of an action being construed as a choice, 
#depending on the participant's condition, controlling for the reported importance of the action."
#An Hierarchical logistic regression analysis was used.

#NB! This effect is presumably an Odds ratio as well, but the sum of stat.cond1.count etc do not correspond to stat.N
#sum of all counts would be very large for each lab (minimum around 460 ss) which seems unrealistic
#What I can do so far is to recompute the tau from Table 3, this is simply using the r and var r

ml2_savani <- ml2_savani %>% 
  rename(effect = study.description, #rename variables to consistent names
         Site = study.source, 
         in_lab = online, #
         effect_size = ESCI.r, #NB! ML2 computed effect sizes by first computing non-central test statics and then transformed into correlations uisng compute.es package (I believe, although seems strange, check with Marcel)
         Ntotal = stat.N,
         country = source.Country) %>%  
  mutate(rp = "ML2", #Add some descriptive information
         B_or_W = "Between", 
         effect = trimws(gsub("\\(.*", "", .$effect)), #shorten effect names a litle
         design = "Hierarchical logistic regression",
         or_stat_test = "z-test", 
         effect_type = "r",
         outcomes1_2 = "NA _ NA",
         outcome_t1 = NA, 
         outcome_c1 = NA,
         outcome_t2 = NA,
         outcome_c2 = NA,
         ntreatment = NA,
         ncontrol = NA,
         country = recode(country, #Recode country names to official three letter acronyms for consistency
                          Hungary = "HUN", #
                          'United Arab Emirates' = "ARE", #
                          `Hong Kong, China` = "HKG",
                          India = "IND",
                          Italy = "ITA",
                          Japan = "JPN",
                          Malaysia = "MYS",
                          Mexico = "MEX",
                          Nigeria = "NGA",
                          Portugal = "PRT",
                          Serbia = "SRB", 
                          `South Africa` = "ZAF",
                          Taiwan = "TWN",
                          Tanzania = "TZA",
                          Uruguay = "URY",
                          Poland = "POL", #
                          Canada = "CAN", #
                          `The Netherlands` = "NLD",
                          Belgium = "BEL", #
                          Sweden = "SWE", #
                          France = "FRA", #
                          UK = "GBR",
                          Australia = "AUS", #
                          Brazil = "BRA",
                          Chile = "CHL",
                          China = "CHN",
                          Turkey = "TUR",
                          'Czech Republic' = "CZE",
                          'New Zealand' = "NZL", #
                          Germany = "DEU", #
                          Switzerland = "CHE", #
                          Colombia = "COL", #
                          `Costa Rica` = "CRI",
                          Spain = "ESP")) %>% 
  select(rp, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
         ntreatment, ncontrol, outcomes1_2, outcome_t1, outcome_c1, outcome_t2, outcome_c2) #select only variables of interest



#*****************************************
#**[2.5] Choosing or Rejecting (Shafir, 1993) ----
#*****************************************
# [1] "Choosing or Rejecting (Shafir, 1993)"
#******************************************
ml2_shafir <- ml2 %>% filter(analysis.name == "Shafir.1") #strange effect, summed percentage different from 100%

#could not make sense of this one so saved as its correlation data
ml2_shafir <- ml2_shafir %>% 
  rename(effect = study.description, #rename variables to consistent names
         Site = study.source, 
         in_lab = online, #
         effect_size = ESCI.r, #NB! ML2 computed effect sizes by first computing non-central test statics and then transformed into correlations uisng compute.es package (I believe, although seems strange, check with Marcel)
         Ntotal = stat.N,
         country = source.Country) %>%  
  mutate(rp = "ML2", #Add some descriptive information
         B_or_W = "Between", 
         effect = trimws(gsub("\\(.*", "", .$effect)), #shorten effect names a litle
         design = "Sum of % choosing option A across two groups",
         or_stat_test = "1 sided proportion z-test", 
         effect_type = "r",
         outcomes1_2 = "NA _ NA",
         outcome_t1 = NA, 
         outcome_c1 = NA,
         outcome_t2 = NA,
         outcome_c2 = NA,
         ntreatment = NA,
         ncontrol = NA,
         country = recode(country, #Recode country names to official three letter acronyms for consistency
                          Hungary = "HUN", #
                          'United Arab Emirates' = "ARE", #
                          `Hong Kong, China` = "HKG",
                          India = "IND",
                          Italy = "ITA",
                          Japan = "JPN",
                          Malaysia = "MYS",
                          Mexico = "MEX",
                          Nigeria = "NGA",
                          Portugal = "PRT",
                          Serbia = "SRB", 
                          `South Africa` = "ZAF",
                          Taiwan = "TWN",
                          Tanzania = "TZA",
                          Uruguay = "URY",
                          Poland = "POL", #
                          Canada = "CAN", #
                          `The Netherlands` = "NLD",
                          Belgium = "BEL", #
                          Sweden = "SWE", #
                          France = "FRA", #
                          UK = "GBR",
                          Australia = "AUS", #
                          Brazil = "BRA",
                          Chile = "CHL",
                          China = "CHN",
                          Turkey = "TUR",
                          'Czech Republic' = "CZE",
                          'New Zealand' = "NZL", #
                          Germany = "DEU", #
                          Switzerland = "CHE", #
                          Colombia = "COL", #
                          `Costa Rica` = "CRI",
                          Spain = "ESP")) %>% 
  select(rp, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
         ntreatment, ncontrol, outcomes1_2, outcome_t1, outcome_c1, outcome_t2, outcome_c2) #select only variables of interest


#******************************************
#**[2.6] ML2 combined----
#******************************************

ml2 <- rbind(ml2_smd, ml2_or,ml2_r, ml2_q, ml2_savani, ml2_shafir)


#******************************************
#[3] Many Labs 3----
#******************************************
##summary data from Many labs 3 https://osf.io/ct89g/
#Data extracted from https://osf.io/yhdau/

#library(dplyr)

#set working directory
setwd("../data/source/Ml3/ML3 Meta") #needs to be set to be able to load all files at once

#All file names in path folder ending with .csv, excluding big five data
files <- list.files(pattern = "*.csv")[-2] 

#read files
ml3 <- lapply(files, read.csv) 
names(ml3) <- gsub(".csv", "", files) #set names


#*****************************************
#**[3.1] Stroop effect----
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
         effect_size = r) %>% #The correlation used instead of cohen's d because that is what Ml3 use in their meta-analysis (https://osf.io/yhdau/ -> "ML3 Meta Script.R")
  mutate(rp = "ML3", #Add some descriptive information 
         in_lab = ifelse(Site == "mTurk", 0, 1), #mturk sample is only non-lab sample in this study
         effect = "Stroop effect", 
         B_or_W = "Within", 
         design = "Repeated measurements", 
         or_stat_test = "one sample t-test of std. mean diff", 
         effect_type = "r",
         outcomes1_2 = "NA _ NA", #as character instead of simply 'NA' to facilitate later analysis
         outcome_c1 = NA, 
         outcome_t1 = NA, 
         outcome_c2 = NA, 
         outcome_t2 = NA,
         ntreatment = NA, 
         ncontrol = NA,
         country = c("USA","USA", "CAN", rep("USA", 14), "CAN", "USA", "USA", "USA")) %>%
  select(rp, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
         ntreatment, ncontrol, outcomes1_2, outcome_t1, outcome_c1, outcome_t2, outcome_c2) #select only variables of interest



#******************************************
#**[3.2] t-test effects----
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
    mutate(rp = "ML3", #Add some descriptive information
           effect = ml3t_names[i], 
           in_lab = ifelse(Site == "mTurk", 0, 1), #mturk sample is only non-lab sample in this study
           B_or_W = "Between", 
           design = "control vs. treatment", 
           or_stat_test = "Independent samples t-test", #equal var
           effect_type = "d",
           outcomes1_2 = "mean _ SD", #Describes the content of outcome1 and outcome2 variables
           Ntotal = ntreatment + ncontrol,
           country = if("mTurk" %in% Site) {c("USA","USA", "CAN", rep("USA", 14), "CAN", "USA", "USA", "USA")} #if dataframe includes mturk sample add one country label for that sample
                     else{c("USA","USA", "CAN", rep("USA", 14), "CAN", "USA", "USA")}) %>%
    select(rp, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
           ntreatment, ncontrol, outcomes1_2, outcome_t1, outcome_c1, outcome_t2, outcome_c2) #select only variables of interest
}

ml3t <- do.call("rbind", ml3t) #bind into dataframe

#******************************************
#**[3.3] Non-parametric tests----
#******************************************
#Effects extracted in this section
#[1] "Availability"
#[2] "Metaphor"
#*********************

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
           effect_size = r) %>% #ML3 uses r as input for meta-analysis (https://osf.io/yhdau/ -> "ML3 Meta Script.R"), thus so do I
    mutate(rp = "ML3", #Add some descriptive information 
           in_lab = ifelse(Site == "mTurk", 0, 1), #mturk sample is only non-lab sample in this study
           effect = nonp_names[i], 
           B_or_W = "Between", 
           design = "Choice of category 1 or 2", 
           or_stat_test = nonp_stat[i], 
           effect_type = "r",
           outcomes1_2 = nonp_outcomes[i],
           outcome_c2 = NA, 
           outcome_t2 = NA,
           ntreatment = NA, 
           ncontrol = NA,
           country = if("mTurk" %in% Site) {c("USA","USA", "CAN", rep("USA", 14), "CAN", "USA", "USA", "USA")} #if dataframe includes mturk sample add one country label for that sample
                     else{c("USA","USA", "CAN", rep("USA", 14), "CAN", "USA", "USA")}) %>%
    select(rp, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
           ntreatment, ncontrol, outcomes1_2, outcome_t1, outcome_c1, outcome_t2, outcome_c2) #select only variables of interest
}

nonp <- do.call("rbind", nonp) #bind together in one dataframe


#******************************************
#**[3.4] Interaction effects----
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
           effect_size = rInter) %>% #ML3 uses r as input for meta-analysis (https://osf.io/yhdau/ -> "ML3 Meta Script.R"), thus so do I
    mutate(rp = "ML3", #Add some descriptive information 
           in_lab = ifelse(Site == "mTurk", 0, 1), #mturk sample is only non-lab sample in this study
           effect = internames[i], 
           B_or_W = "Between", 
           design = interdesign[i], 
           or_stat_test = interstat[i], 
           effect_type = "r",
           outcomes1_2 = "NA _ NA", #as character instead of simply NA to facilitate working with data
           outcome_c1 = NA, 
           outcome_t1 = NA, 
           outcome_c2 = NA, 
           outcome_t2 = NA,
           country = if("mTurk" %in% Site) {c("USA","USA", "CAN", rep("USA", 14), "CAN", "USA", "USA", "USA")} #if dataframe includes mturk sample add one country label for that sample
                     else{c("USA","USA", "CAN", rep("USA", 14), "CAN", "USA", "USA")}) %>%
    select(rp, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
           ntreatment, ncontrol, outcomes1_2, outcome_t1, outcome_c1, outcome_t2, outcome_c2) #select only variables of interest
}

inter <- do.call("rbind", inter) #bind together in one dataframe


#******************************************
#**[3.5] Correlation conscientiousness and persistence----
#******************************************
#Effects extracted in this section
#[1] "Conscientiousness and persistence"
#*************************

cons_cor <- ml3$ConscientiousnessPersistence #limit data to only correlation

cons_cor <- cons_cor %>% 
  rename(Ntotal = N, #Assign names consistent with other datasets
         effect_size = r) %>% 
  mutate(rp = "ML3", #Add some descriptive information 
         in_lab = ifelse(Site == "mTurk", 0, 1), #mturk sample is only non-lab sample in this study
         effect = "Conscientiousness and persistence", 
         B_or_W = "Within", 
         design = "Conscientiousness with persistence", 
         or_stat_test = "correlation", 
         effect_type = "r",
         outcomes1_2 = "NA _ NA", #as character instead of simply NA to facilitate working with data
         outcome_c1 = NA, 
         outcome_t1 = NA, 
         outcome_c2 = NA, 
         outcome_t2 = NA,
         ntreatment = NA, 
         ncontrol = NA,
         country = c("USA","USA", "CAN", rep("USA", 14), "CAN", "USA", "USA", "USA")) %>%
  select(rp, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
         ntreatment, ncontrol, outcomes1_2, outcome_t1, outcome_c1, outcome_t2, outcome_c2) #select only variables of interest


#******************************************
#**[3.6] ML3 combined----
#******************************************

ml3 <- rbind(ml3_stroop, ml3t, nonp, inter, cons_cor) %>% 
  mutate(Site = recode(Site, mTurk = "mturk")) 

#******************************************
#[3] RRR1 & 2 ----
#******************************************
##summary data from Registered Replication Report 1 & 2 https://osf.io/ybeur/ 
##Data RRR1 https://osf.io/dv5ei/
##Data RRR2 https://osf.io/dy6at/


#library(dplyr)
#library(readxl)

#set working directory
setwd(wd_path)
setwd("../data/source/RRR1_2") #needs to be set to be able to load all files at once

#All file names in path folder ending with .xlsx
files <- list.files(pattern = "*.xlsx")

rrr1_2 <- lapply(files, read_excel) #read files, tables with summary information for rrr1 & 2
names(rrr1_2) <- c("RRR1", "RRR2") #set names

#Clean and format data
rrr1_2 <- lapply(rrr1_2, function(x) x[-c(1:2),]) #Remove first two rows which are of no interest (old headers and original study data)
rrr1_2 <- lapply(rrr1_2, setNames, c("lab", "country", "language", paste0("treatment", 1:8), paste0("control", 1:8))) 

for(i in seq_along(files)){
  rrr1_2[[i]] <- rrr1_2[[i]] %>% 
    rename(Site = lab, #rename variables to consistent names
           country = country,
           ntreatment = treatment5, #total n treatment group after exclusions
           outcome_t1 = treatment6, #n correcttreatment group
           ncontrol = control5, #Total n control group after exclusions
           outcome_c1 = control6) %>%  #n correct control group
    mutate(rp = names(rrr1_2)[i], #Add some descriptive information
           Site = ifelse(grepl("MTURK", Site), "mturk", Site), #if Site name contains MTURK recode to mturk
           effect = paste("Verbal overshadowing", i), 
           in_lab = ifelse(Site == "mturk", 0, 1), #Only the mturk study was not in the lab
           B_or_W = "Between", 
           design = "Outcome 1 vs. 2 between groups", 
           or_stat_test = "Chisquare", 
           effect_type = "Risk difference",
           outcomes1_2 = "count correct _ count incorrect", #Describes the content of outcome1 and outcome2 variables
           outcome_t2 = as.numeric(treatment7) + as.numeric(treatment8), #n unsuccessful identifications treatment group
           outcome_c2 = as.numeric(control7) + as.numeric(control8), #n unsuccessful identifications control group
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
    select(rp, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
           ntreatment, ncontrol, outcomes1_2, outcome_t1, outcome_c1, outcome_t2, outcome_c2) #select only variables of interest
}

rrr1_2 <- do.call("rbind", rrr1_2)



#******************************************
#[4] RRR3 ----
#******************************************
##summary data from Registered Replication Report 3 https://osf.io/d3mw4/
##Imagery (detailed processing) data https://osf.io/be6gd/, intention attribution https://osf.io/7d9yg/ and intentionality https://osf.io/fsmdk/, 

#Effects extracted in this section
#[1] "Grammar on detailed processing"  
#[2] "Grammar on intention attribution"
#[3] "Grammar on intentionality" 
#*********************
#library(dplyr)


setwd(wd_path) #reset working directory
setwd("../data/source/RRR3") #needs to be set to be able to load all files at once

#Extract data
files <- list.files(pattern = "*.csv") #list files
rrr3 <- lapply(files, read.csv, stringsAsFactors = FALSE) #load files

#Clean and format
rrr3_names <- c("Grammar on detailed processing", "Grammar on intention attribution", "Grammar on intentionality")

for(i in seq_along(files)){ #loop over the 3 files and for each file
rrr3[[i]] <- rrr3[[i]] %>% 
  slice(-1) %>% #Remove first row which contains data from original experiment that was replicated
  rename(Site = Author, #rename to consistent names
         outcome_t1 = mimp,
         outcome_t2 = sdimp,
         ntreatment = nimp,
         outcome_c1 = mperf,
         outcome_c2 = sdperf,
         ncontrol = nperf) %>% 
  mutate(rp = "RRR3", #Add some descriptive information
         effect = rrr3_names[i],   
         Site = recode(Site, 'ONLINE-Eerland, Sherrill, Magliano, Zwaan' = "mturk"),
         in_lab = ifelse(Site == "mturk", 0, 1), # Only mturk study was online
         B_or_W = "Between", 
         design = "control vs. treatment", 
         or_stat_test = "Independent sample t-test",
         effect_type = "Raw mean difference",
         outcomes1_2 = "mean _ SD", #Describes the content of outcome1 and outcome2 variables
         Ntotal = ntreatment + ncontrol,
         effect_size = outcome_t1 - outcome_c1,
         country = c(rep("USA", 3), "CAN", "USA", "CAN", rep("USA", 6))) %>% #Country information taken from Table 1 of paper http://journals.sagepub.com/doi/pdf/10.1177/1745691615605826
  select(rp, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
         ntreatment, ncontrol, outcomes1_2, outcome_t1, outcome_c1, outcome_t2, outcome_c2) #select only variables of interest
}

rrr3 <- do.call("rbind", rrr3) #combine into one dataframe

#******************************************
#[5] RRR4 ----
#******************************************
##summary data from Registered Replication Report 4 https://osf.io/jymhe/
##Data from file named RTV_incl.csv https://osf.io/54nza/

#library(dplyr)

setwd(wd_path) #Reset working directory

#Extract data
rrr4 <- read.csv("../data/source/RRR4/RTV_incl.csv", stringsAsFactors = FALSE)
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
  mutate(rp = "RRR4", #Add some descriptive information
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
  select(rp, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
         ntreatment, ncontrol, outcomes1_2, outcome_t1, outcome_c1, outcome_t2, outcome_c2) #select only variables of interest


#******************************************
#[6] RRR5 ----
#******************************************
##summary data from Registered Replication Report 5 https://osf.io/s3hfr/
##Data extracted from raw data files https://osf.io/dvaz7/

#Effects extracted in this section
#[1] "Commitment on exit"   
#[2] "Commitment on neglect"
#*********************

#Additional comment:
#Note that reported participant numbers of Table 1 in source paper (https://osf.io/s3hfr/) are before listwise deletion due to missing values
#In addition, source paper appears to do listwise deletion based on missing values in any of the DVs, not only the primary one (exit/neglect)
#This will result in slight different sample sizes/effects since we only extract and use the stated primary outcomes exit and neglect for listwise deletion

#*********************

#library(dplyr)


#Functions (3)
convert <- function(x){as.numeric(as.character(x))} # "function that converts a column of Qualtrix output to useable form in R", from Finkel_Analysis.R https://osf.io/mp3s7/

extract_rrr5 <- function(loaded_csv){ #function for initial manipulation of raw data
  loaded_csv %>% 
    select(Q7.1, #condition item, {1 = high, 2 = low}
           Exclude, #Exclusion item {0 = include, 1 = exclude, 2 = maybe include}. Only =1 excluded for main analysis.
           !!! exit_items, #Exit items, unquoted by !!! (see item list below)
           !!! neglect_items) %>% #neglect items, unquoted by !!! (see item list below)
    slice(-1) %>% #Remove first row which are alternative column names
    mutate_all(convert) %>%  #transform qualtrix data into usable format in R
    filter(!Exclude == 1) %>% #remove participants that were excluded in main analysis of paper
    rowwise() %>% #ensures we can take the mean across columns in mutate below
    mutate(exit = mean(c(!!! exit_items)), #creation of exit score as the average across items as per MA (Finkel_Analysis.R)
           neglect = mean(c(!!! neglect_items)), #creation of neglect score as the average across items as per MA (Finkel_Analysis.R)
           Q7.1 = ifelse(Q7.1 == 1, "treatment", "control"), #clarification
           group = Q7.1) %>%  #clarification
    ungroup() %>% #remove rowwise grouping
    select(group, exit, neglect) %>% #output is scores for exit and neglect with information on condition where each row represents one participant
    filter(complete.cases(.)) #remove cases with missing scores on exit or neglect, source paper does "listwise deletion" for meta-analysis, see Finkel_Meta.R https://osf.io/2m756/
}

summarizer <- function(extracted){ #Function to transform individual level data to summary statistics
  extracted %>% 
    group_by(group) %>% 
    summarize(n = n(),
              avg = mean(DV),
              SD = sd(DV)) %>% 
    ungroup()
}

#Extract data
f <- list.files("../data/source/RRR5/Data", pattern = "*.csv") #folder with all raw data files for rrr5
setwd("../data/source/RRR5/Data")
rrr5 <- lapply(f, read.csv, stringsAsFactors = FALSE) #read raw data

rrr5_names <- c("Commitment on exit", "Commitment on neglect") #effect names
rrr5_sites <- unlist(lapply(strsplit(f, split = "_"), function(x) x[[1]])) #extracts author names as a vector from listed files
countries <-  c("TUR", "USA", "USA", "USA", "USA", "CAN", "CAN", "USA", "USA", "USA", "USA", 
                "CAN", "CAN", "USA", "CZE", "SGP") #Lab countries from https://osf.io/5e7th/wiki/home/, note that the order on OSF is not the same as in the data (here) and that "Lisa Reddoch" = Hoplock


exit_items <- quos(Q2.2_1,Q2.3_3,Q2.4_1,Q2.5_2,Q2.6_2,Q2.7_4,Q2.8_2,Q2.9_4,Q2.10_1,Q2.11_4,Q2.12_3,Q2.13_4) #quote items for later use in dplyr verbs, list from Finkel_Analysis.R https://osf.io/mp3s7/
neglect_items <- quos(Q2.2_4,Q2.3_2,Q2.4_2,Q2.5_4,Q2.6_1,Q2.7_2,Q2.8_4,Q2.9_2,Q2.10_4,Q2.11_1,Q2.12_1,Q2.13_1) #quote items for later use dplyr verbs, list from Finkel_Analysis.R https://osf.io/mp3s7/

rrr5 <- lapply(rrr5, extract_rrr5) #Extract dependent variables and groupings

#Cleaning and initial formatting of data
exit <- lapply(rrr5, function(x) x %>% select(-neglect, DV = exit)) #separate into exit DV
neglect <- lapply(rrr5, function(x) x %>% select(-exit, DV = neglect)) #separate into neglect DV
#A this point we have individual level data for each DV in separate lists

rrr5 <- list(exit, neglect) #put both DVs into one list for below loop

for(dv in seq_along(rrr5_names)){ #for each DV
  summed <- lapply(rrr5[[dv]], summarizer) #Create summary statistics
  
  high <- lapply(summed, function(x) x[x$group == "treatment",] %>%  #separate by group (high/low)
                   select(-group, #removing grouping variable and clean up the names
                          ntreatment = n,
                          outcome_t1 = avg,
                          outcome_t2 = SD))
  
  low <- lapply(summed, function(x) x[x$group == "control",]%>%
                  select(-group, 
                         ncontrol = n,
                         outcome_c1 = avg,
                         outcome_c2 = SD))
  
  rrr5[[dv]] <- mapply(cbind, high, low, SIMPLIFY = FALSE) #combine high and low columns for each lab
  rrr5[[dv]] <- do.call(rbind, rrr5[[dv]]) #for each DV convert from list into dataframe
  rrr5[[dv]]$Site <- rrr5_sites #clarify origin of each row of summary statistics
} #end loop


#Final formatting of data
for(dv in seq_along(rrr5_names)){ #for each DV
  rrr5[[dv]] <- rrr5[[dv]] %>% 
    mutate(rp = "RRR5", #Add some descriptive information
           effect = rrr5_names[dv], 
           in_lab = 1, # "Participants were tested in-person", p.752 of RRR5
           B_or_W = "Between", 
           design = "control vs. treatment", 
           or_stat_test = "NA", #"The analysis does not focus on null-hypothesis significance testing." p. 755 RRR5 
           effect_type = "Raw mean difference",
           outcomes1_2 = "mean _ SD", #Describes the content of outcome1 and outcome2 variables
           country = countries,
           Ntotal = ncontrol + ntreatment,
           effect_size = outcome_t1 - outcome_c1) %>% 
    select(rp, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
           ntreatment, ncontrol, outcomes1_2, outcome_t1, outcome_c1, outcome_t2, outcome_c2) #select only variables of interest
}

rrr5 <- do.call(rbind, rrr5) #combine into one dataframe

#******************************************
#[7] RRR6 ----
#******************************************
##summary data from Registered Replication Report 6 https://osf.io/hgi2y/
##Download RRR6 data https://osf.io/9j72u/ and run their code https://osf.io/9j72u/ to get a summary data file from which data can be extracted

#library(tabulizer)
#library(dplyr)

setwd(wd_path) #Reset working directory

#Extract data
rrr6 <- read.csv("../data/source/RRR6/resultsFacialFeedbackReplication.csv", stringsAsFactors = FALSE)
f <- "http://journals.sagepub.com/doi/pdf/10.1177/1745691616674458" #For extracting Table 1 from paper to get info on country for each lab
country <- extract_tables(f, pages = 5, output = "data.frame") #Note that if we do not care about group sample sizes, this table contain all other information

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
  mutate(rp = "RRR6", #Add some descriptive information
         effect = "Facial Feedback hypothesis", 
         in_lab = 1, #In-lab study
         B_or_W = "Between", 
         design = "control vs. treatment", 
         or_stat_test = "NA", #No statistical test performed as part of main analysis, only looked at mean difference. Secondary used BF
         effect_type = "Raw mean difference",
         outcomes1_2 = "mean _ SD") %>% #Describes the content of outcome1 and outcome2 variables
  select(rp, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
         ntreatment, ncontrol, outcomes1_2, outcome_t1, outcome_c1, outcome_t2, outcome_c2) #select only variables of interest




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
  mutate(rp = "RRR7", #Add some descriptive information
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
  select(rp, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
         ntreatment, ncontrol, outcomes1_2, outcome_t1, outcome_c1, outcome_t2, outcome_c2) #select only variables of interest



#******************************************
#[9] RRR8 ----
#******************************************
##summary data from Registered Replication Report 8 https://osf.io/k27hm/
##Data contained in zip-file named final_results.zip: https://osf.io/mxugy/

#library(tabulizer)
#library(dplyr)

#Extract data
f <- "../data/source/RRR8/final_results/tables/all_raw_effects.pdf" #path to table (which includes country names)
rrr8 <- extract_tables(f) #note contains data from all labs, not only the 23 used in main analysis of paper

authors_main23 <- c("Schulte-Mecklenbeck", "Baskin", "Braithwaite", "Vazire", "Newell", "O'Donnell", "Tamayo", #23 labs from main analysis in paper
                    "Karpinski", "Klein", "Keller", "Shanks", "Bialobrzeska", "Koppel", "Philipp", "Ropovik", #manually coded from Figure 1 in paper https://www.psychologicalscience.org/publications/replication-dijksterhuis-van-knippenberg
                    "Steele", "Susa", "Steffens", "Aczel", "Saunders", "McLatchie", "Aveyard", "Boot") #also found in final_results/prereg/main.png from OSF


#Clean and format data
rrr8 <- lapply(rrr8, function(x) x[x[,2] == "Main Effect",]) #remove effects that are not the primary effect, note that this drops the headers which are in row 1 of the first list
rrr8[[1]] <- rrr8[[1]][, -c(4, 6, 8, 10, 18)] #drop a few by extraction incorrectly added columns

rrr8 <- as.data.frame(do.call("rbind", rrr8), stringsAsFactors = FALSE)#conmbine into one dataframe

rrr8$V1[22] <- "O'Donnell" #Correct faulty extraction of O'Donnell's name

rrr8[, 3:8] <- sapply(rrr8[,3:8], as.numeric) #change relevant columns to numeric

rrr8 <- rrr8 %>% 
  filter(V1 %in% authors_main23) %>% #limit to the 23 labs from main analysis in paper
  rename(Site = V1, #rename variables to consistent names
         outcome_t1 = V3, #mean treatment group
         ntreatment = V4,
         outcome_c1 = V5,#mean control group
         ncontrol = V6,
         effect_size = V7, #Mean difference
         outcome_c2 = V8, #standard error
         country = V11) %>% 
  mutate(rp = "RRR8", #Add some descriptive information
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
  select(rp, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
         ntreatment, ncontrol, outcomes1_2, outcome_t1, outcome_c1, outcome_t2, outcome_c2) #select only variables of interest




#******************************************
#[10] RRR9 ----
#******************************************
##summary data from Registered Replication Report 10 https://osf.io/k27hm/
##Direct link to .zip file with raw data: https://osf.io/qegfd/
##To extract summary data we ran the RRR9 code in the file above on their raw data

#library(dplyr)

rrr9 <- read.csv("../data/source/rrr9/RRR9_summary_data.csv", stringsAsFactors = FALSE)

rrr9 <- rrr9 %>% 
  rename(Site = lab.name, #rename variables to consistent names
         outcome_t1 = mean.beh.80, #mean treatment group
         ntreatment = n.80,
         outcome_c1 = mean.beh.20,#mean control group
         ncontrol = n.20,
         outcome_t2 = sd.beh.80, 
         outcome_c2 = sd.beh.20) %>% 
  mutate(rp = "RRR9", #Add some descriptive information
         effect = "Hostility priming", 
         in_lab = 1, # All participants in large group of at least 50 people
         B_or_W = "Between", 
         design = "control vs. treatment", 
         effect_size = outcome_t1 - outcome_c1, #Mean difference
         or_stat_test = "ANOVA", #No particular test, just looked at the effect and CI
         effect_type = "Raw mean difference",
         outcomes1_2 = "mean _ SD", #Describes the content of outcome1 and outcome2 variables
         Ntotal = ntreatment + ncontrol,
         country = c("GBR", #Acar, country for each lab from https://osf.io/uskr8/,
                     "HUN", #Aczel
                     "CAN", #Birt
                     "USA", #Evans
                     "PRT", #Ferreira-Santos
                     "GBR", #Iraizoz
                     "AUS", #Holzmeister
                     "ISR", #Rozmann
                     "SWE", #Koppel
                     "FRA", #Laine
                     "GER", #Loschelder
                     "USA", #McCarthy
                     "NLD", #Meijer
                     "TUR", #?zdogru
                     "GBR", #Pennington
                     "BEL", #Roets
                     "GER", #Suchotzki
                     "FRA", #Sutan
                     "NLD", #Vanpaemel
                     "NLD", #Veschuere
                     "USA", #Wick
                     "USA"), #Wiggins
         Site = tolower(Site)) %>% #Make site-names lower-case 
  select(rp, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
         ntreatment, ncontrol, outcomes1_2, outcome_t1, outcome_c1, outcome_t2, outcome_c2) #select only variables of interest

#******************************************
#[11] RRR10 ----
#******************************************
##summary data from Registered Replication Report 9 https://osf.io/k27hm/
##Direct link to .zip file with data: https://osf.io/fwnc2/
##Summary data located at Meta-Analysis_2018-07-09.zip\Meta-Analysis\Results_perMAA\Main\Tables\10 commandments effect_table_data.csv
#Additional comments:  standard error of difference, no SD per group

#library(dplyr)

rrr10 <- read.csv("../data/source/RRR10/10 commandments effect_table_data.csv", stringsAsFactors = FALSE)

rrr10$authors[grep("Iraizoz", rrr10$authors)] <- "Gonzales-Iraizoz" #simplify names not read properly
rrr10$authors[grep("ru", rrr10$authors)] <- "Ozdogru"

rrr10 <- rrr10 %>% 
  slice(-c(1, 21)) %>% #drop original effect and meta-analytic summmary from data
  rename(Site = authors, #rename variables to consistent names
         outcome_t1 = mean1, #mean treatment group
         ntreatment = n1,
         outcome_c1 = mean2,#mean control group
         ncontrol = n2,
         effect_size = means, #Mean difference
         outcome_c2 = se) %>%  #standard error of the difference 
  mutate(rp = "RRR10", #Add some descriptive information
         effect = "Moral reminder", 
         in_lab = 1, # All participants in large group of 50 people
         B_or_W = "Between", 
         design = "control vs. treatment", 
         or_stat_test = "ANOVA", #No particular test, just looked at the effect and CI
         effect_type = "Raw mean difference",
         outcomes1_2 = "mean _ SE", #Describes the content of outcome1 and outcome2 variables
         Ntotal = ntreatment + ncontrol,
         outcome_t2 = NA, #only have sE of the effect (difference of means) and no SDs, so nothing for this variable
         country = c("HUN", #country for each lab from https://osf.io/uskr8/, Aczel
                     "CAN", #Birt
                     "USA", #Evans
                     "PRT", #Ferreira-Santos
                     "GBR", #Iraizoz
                     "AUS", #Holzmeister
                     "ISR", #Rozmann
                     "SWE", #Koppel
                     "FRA", #Laine
                     "GER", #Loschelder
                     "USA", #McCarthy
                     "NLD", #Meijer
                     "TUR", #?zdogru
                     "GER", #Suchotzki
                     "FRA", #Sutan
                     "NLD", #Vanpaemel
                     "NLD", #Veschuere
                     "USA", #Wick
                     "USA")) %>% #Wiggins
  select(rp, effect, Site, country, in_lab, Ntotal, B_or_W, design, or_stat_test, effect_type, effect_size, 
         ntreatment, ncontrol, outcomes1_2, outcome_t1, outcome_c1, outcome_t2, outcome_c2) #select only variables of interest



#******************************************
#[12] Collate and save data ----
#******************************************

effects <- rbind(ml1, ml2, ml3, rrr1_2, rrr3, rrr4, rrr5, rrr6, rrr7, rrr8, rrr9, rrr10)

# write.csv(effects, "../data/collated_summary_data.csv", row.names = FALSE)


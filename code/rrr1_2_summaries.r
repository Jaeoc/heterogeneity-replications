##MA heterogeneity project - summary data from Registered Replication Report 1 & 2 https://osf.io/jymhe/
##Data RRR1 https://osf.io/dv5ei/
##Data RRR2 https://osf.io/dy6at/

#******************************************
#RRR1 & 2 ----
#******************************************
#Setup
library(dplyr)
library(readxl)

#set working directory
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

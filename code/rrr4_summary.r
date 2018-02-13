##MA heterogeneity project - summary data from Registered Replication Report 4 https://osf.io/jymhe/
##Data from file named RTV_incl.csv https://osf.io/54nza/

#******************************************
#RRR4 ----
#******************************************
#Setup
library(dplyr)

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





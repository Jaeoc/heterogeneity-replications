
##MA heterogeneity project - summary data from Registered Replication Report 5 https://osf.io/s3hfr/
## Data extracted from directly from paper http://journals.sagepub.com/doi/pdf/10.1177/1745691616664694

#******************************************
#RRR5 ----
#******************************************

library(tabulizer)
library(tidyr)
library(dplyr)

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

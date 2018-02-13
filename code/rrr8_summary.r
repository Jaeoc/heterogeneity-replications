##MA heterogeneity project - summary data from Registered Replication Report 8 https://osf.io/k27hm/
##Data contained in zip-file named final_results.zip: https://osf.io/mxugy/

#******************************************
#RRR8 ----
#******************************************
#Setup
if (!require("ghit")) {install.packages("ghit")} #works similarly to devtools function but package loads faster
library(tabulizer)
library(dplyr)

#Extract data
f <- "../data/RRR8/final_results/tables/all_raw_effects.pdf" #path to table with country names
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
           effect_type = "Mean difference",
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





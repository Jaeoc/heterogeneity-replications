
##MA heterogeneity project - summary data from Registered Replication Report 7 https://osf.io/scu2f/

#******************************************
#RRR7 ----
#******************************************
##Setup
if (!require("ghit")) {install.packages("ghit")} #works similarly to devtools function but package loads faster

#on 64-bit Windows; https://github.com/ropensci/tabulizer
ghit::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")
#note that tabulizer requires rJava installed, which in turn requires Java, and that windows (if used), R and Java must all
#either be x64 or x32 version (use Sys.info() & sessionInfo() to check windows and R versions)
if(!require("tidyr")) {install.packages("tidyr")}

library(tabulizer)
library(tidyr)
library(dplyr)

##Extract data
f <- "http://journals.sagepub.com/doi/pdf/10.1177/1745691617693624" #temporarily load RRR7 pdf
out <- extract_tables(f, pages = c(8, 9)) #extract summary table (Table 2)
labs <- extract_tables(f, pages = 6) #extract Table 1 with demographic information

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
                          'Unites States' = "USA", #mis-spelt in datafile
                          'Czech Republic' = "CZE",
                          Spain = "ESP",
                          Denmark = "DNK",
                          India = "IND",
                          Sweden = "SWE"))

  
  
rrr7 <- lapply(out, function(x) x[x[,2] == "All participants",]) #keep only rows with all participants, also removes headers due to their formatting
rrr7 <- as.data.frame(do.call("rbind", rrr7), stringsAsFactors = FALSE) #bind the two-page table into one dataframe

##Clean and format data
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



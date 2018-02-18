
##MA heterogeneity project - summary data from Registered Replication Report 6 https://osf.io/hgi2y/
##Download RRR6 data https://osf.io/9j72u/ and run their code https://osf.io/9j72u/ to get a summary data file 

#******************************************
#RRR6 ----
#******************************************

library(tabulizer)

#Extract data
rrr6 <- read.csv("../data/RRR6/resultsFacialFeedbackReplication.csv", stringsAsFactors = FALSE)
f <- "http://journals.sagepub.com/doi/pdf/10.1177/1745691616674458" #For extracting Table 1 from paper to get info on country for each lab
country <- extract_tables(f, pages = 5, method = "data.frame") #Note that if we do not care about group sample sizes, this table contain all other information

#Clean and format data
country <- as.data.frame(country) %>%
  slice(-1) %>% #remove first row as superflous
  select(Site = X, country = Country.of, Ntotal = Total.1) %>% #change names to those used elsewhere
  mutate(Site = recode(Site, 'Ã–zdog Ì† ru' = "Ozdogru"), #Fix badly extracted name
         country = recode(country, #recode country names to their official 3 letter acronymns
                          U.S. = "USA",
                          Belgium = "BEL",
                          Canada = "CAN",
                          Italy = "ITA",
                          'United Kingdom' = "GBR",
                          'The Netherlands' = "NLD",
                          Turkey = "TUR",
                          Spain = "ESP"))

rrr6$studyIDs <- country$lab #Make names consistent over dataframes

rr <- rrr6 %>% 
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
         



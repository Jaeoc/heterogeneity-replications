
##MA heterogeneity project - summary data from Registered Replication Report 3 https://osf.io/d3mw4/
##Data here: https://osf.io/fsmdk/

#******************************************
#RRR3 ----
#******************************************

rrr3 <- read.csv("../data/RRR3/Intentionality.csv", stringsAsFactors = FALSE)


rr <- rrr3 %>% 
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

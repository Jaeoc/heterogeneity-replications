#************************************************

##Project: Limited evidence for widespread heterogeneity in psychology
##Script purpose: This script creates a nice-looking pdf codebook
##Code: Anton Ohlsson Collentine

#************************************************
#Packages and data----
#************************************************
if(!require(readr)){install.packages("readr")}
if(!require(dataMaid)){install.packages("dataMaid")}

library(readr) #load data
library(dataMaid) #create rmd-file that then creates pdf-codebook

dat <- read_csv("../data/collated_summary_data.csv")
#************************************************
#Set variable descriptions and make codebook----
#************************************************

attr(dat$rp, "shortDescription") <- "Replication Project"
attr(dat$effect, "shortDescription") <- "Studied effect"
attr(dat$Site, "shortDescription") <- "Lab-identifier within each replication project"
attr(dat$country, "shortDescription") <- "Country of participants {ISO Alpha-3 code}"
attr(dat$in_lab, "shortDescription") <- "Whether study took place in-lab {1} or not {0} (0 = online)"
attr(dat$Ntotal, "shortDescription") <- "Total study sample size"
attr(dat$B_or_W, "shortDescription") <- "Between or within groups study"
attr(dat$design, "shortDescription") <- "Description of design of research"
attr(dat$or_stat_test, "shortDescription") <- "Statistical test used, if any"
attr(dat$effect_type, "shortDescription") <- "Reported effect type {d, r, Risk difference, Raw mean difference}"
attr(dat$effect_size, "shortDescription") <- "Size of reported effect"
attr(dat$ncontrol, "shortDescription") <- "Sample size control group"
attr(dat$ntreatment, "shortDescription") <- "Sample size treatment group"
attr(dat$outcomes1_2, "shortDescription") <- paste0("Describes content in outcome_1 and outcome_2. For chi-square effects",
                                                     "(group1, group2) indicates 'treatment' and 'control' groups")
attr(dat$outcome_c1, "shortDescription") <- "Control group outcome 1"
attr(dat$outcome_t1, "shortDescription") <- "Treatment group outcome 1"
attr(dat$outcome_c2, "shortDescription") <- "Control group outcome 2"
attr(dat$outcome_t2, "shortDescription") <- "Treatment group outcome 2"

makeCodebook(dat, file = "codebook_collated_summary_data.rmd")
#some changes then made in the resulting .rmd file to create final pdf codebook


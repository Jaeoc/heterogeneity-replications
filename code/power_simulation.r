#******************************************

#Project: Heterogeneity in direct replications
#Purpose: Simulate I2 distribution for different levels of heterogeneity given data weights
#Code: Anton Ohlsson Collentine

#******************************************


#******************************************
#Packages and data
#******************************************
library(readr)
library(metafor)
library(dplyr)
library(purrr) #for 'possibly'
library(ggplot2)

dat <- read_csv("../data/collated_summary_data.csv")


#******************************************
#Functions
#******************************************
#Because we are only interested in heterogeneity, average effect size doesn't matter
#Huedo-Medina I set it to 0.5 (using hedge's g), however, we would then be double-standardizing
#later when running the meta-analysis since each individual's value would be measured already
#in standard deviation units. Simpler to set it to zero.

rma_simulate <- possibly(function(k, tau2, N){ #to use this across all effects I just need to add N_half as an argument
  #I use possibly because REML does not always converge. Non-conversions then -> NA
  N_half <- N%/%2
  #Step 1: draw k mean effect sizes, here homogeneity
  averages <- rnorm(n = k, mean = 0, sd = sqrt(tau2)) #we wish to loop sd = tau over a number of values
  
  #Step 2: draw individual results for each study, control group and treatment group
  #I draw N/2 rounded to the closest integer for each study with equality of variances
  #Following Huedo-Medina I use no effect in control and the full effect in treatment group
  
  #loop because rnorm is not vectorized for n
  treatment <- vector("list", length(N)) #length = k
  control <- vector("list", length(N)) #length = k
  for(Nk in seq_along(N_half)){ 
    treatment_i <- rnorm(n = N_half[Nk], mean = averages[Nk], sd = 1)
    control_i <- rnorm(n = N_half[Nk], mean = 0, sd = 1)
    
    treatment[[Nk]] <- data.frame(avg_t = mean(treatment_i), SD_t = sd(treatment_i), n_t = N_half[Nk])
    control[[Nk]] <- data.frame(avg_c = mean(control_i), SD_c = sd(control_i), n_c = N_half[Nk])
  }
  
  treatment <- do.call(rbind, treatment) 
  control <- do.call(rbind, control) 
  project <- cbind(treatment, control) #Dataframe with means and SDs for each k
  
  rma(measure = "SMD",  m1i = avg_t, m2i = avg_c, sd1i = SD_t, sd2i = SD_c, n1i = n_t, n2i = n_c, 
      data = project, method = "REML")$I2
  
}, otherwise = NA) #If function fails return NA

I2_replicate <- function(reps, tau2, k, N){ 
  out_I2 <- vector("list", length(tau2))
  for(t in seq_along(tau2)){
    I2 <- replicate(reps, rma_simulate(k, tau2[t], N)) #I2 point estimate
    out_I2[[t]] <- data.frame(I2 = mean(I2, na.rm = TRUE), #removs runs where rma did not converge
                              tau2 = tau2[t])
  }
  do.call(rbind, out_I2) #output dataframe
}

#**************************TEST----
datx <- dat %>% 
  filter(effect == "Sunk Costs")

fit <- rma(measure = "SMD", m1i = outcome_t1, m2i = outcome_c1, sd1i = outcome_t2, 
           sd2i = outcome_c2, n1i = ntreatment, n2i = ncontrol, data = datx)
confint(fit)$random[3, 2] #gives us lower bound of I2

#******************************************
#Run simulations
#******************************************

dat2 <- dat %>% #Extract k for each project and N of sub-studies
  split(., .$effect) %>% 
  map(~ list(k = nrow(.), Ntotal = .$Ntotal)) #~ is shorthand for an anonymous function

tau2_values <- seq(0, 0.25, by = 0.005) #tau2 values to loop over

set.seed(112)
res <- vector("list", length(dat2)) 

for(e in seq_along(dat2)){
 res[[e]] <- I2_replicate(1, tau2_values, dat2[[e]]$k, dat2[[e]]$Ntotal)
 cat("...RS",e, "/37") #see progress
 if (e%%5 == 0 | e == 37) saveRDS(res, "temp_sim_results.RDS") #save ocassionally and at finish
}



#******************************************
#Plot results
#******************************************
# dat3 <- readRDS("temp_sim_results.RDS")
dat3 <- res
names(dat3) <- names(dat2)
  
dat3 <- dat3 %>% 
  bind_rows(.id = "effect")

#Point-plot shows clearly that the initial increase is fastest 
#Facet wrap makes it informative but lacks succinctness
ggplot(dat3, aes(x = tau2, y = I2)) + 
  geom_point(alpha = 0.2) +
  facet_wrap(~effect)

#combined line plot gives good overview. Point version of the same is also interesting.
ggplot(dat3, aes(x = tau2, y = I2, group = effect)) +
  geom_line(alpha = 0.2) +
  theme_bw() +
  scale_x_continuous(minor_breaks = NULL) +
  scale_y_continuous(minor_breaks = NULL) +
  coord_cartesian(ylim = c(0, 100))

#******************************************
#Extract tau2 values
#******************************************
#Extract values that correspond best to I2 = zero, small (25%), medium (50%) and large (75%)

dat4 <- dat3 %>% 
  mutate(s = I2 - 25,
         m = I2 - 50,
         l = I2 - 75) %>% 
  group_by(effect) %>% 
  summarize(small = tau2[which.min(abs(s))], #tau2 value for I2 closest to 25
            medium = tau2[which.min(abs(m))], #tau2 value for I2 closest to 50
            large = tau2[which.min(abs(l))]) %>% #tau2 value for I2 closest to 75
  ungroup()

##At this point I have all the tau2-values that I need and can move on
#to the next phase. In th next phase I will simulate power + type I error
#based on the tau2-values that I input





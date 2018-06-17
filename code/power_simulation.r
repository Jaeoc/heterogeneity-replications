#******************************************

#Project: Heterogeneity in direct replications
#Script purpose: Simulate I2 distribution for different levels of heterogeneity given data weights
#Code: Anton Ohlsson Collentine

#******************************************


#******************************************
#Packages and data----
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
      data = project, method = "REML")
  
}, otherwise = NA) #If function fails return NA

#Function to extract average point estimates of I2 for a given tau2 
I2_replicate_point <- function(reps, tau2, k, N){ #where reps = no. replications at a given tau2-value
  out_I2 <- vector("list", length(tau2)) #tau2 is a vector of tau2-values to loop over
  for(t in seq_along(tau2)){ #k is the number of studies in a project and N is a vector of participants in each k
    I2 <- rep(NA, reps)
    
    for(rep in 1:reps){
    fit <- rma_simulate(k, tau2[t], N) 
    I2[rep] <- suppressWarnings(if(is.na(fit)) NA else fit$I2) #I2 point estimate. Suppressing warnings because rma is either NA or a list
    } 
    
    out_I2[[t]] <- data.frame(I2 = mean(I2, na.rm = TRUE), #removes runs where rma did not converge
                              tau2 = tau2[t])
  }
  do.call(rbind, out_I2) #output dataframe
}

#Function to output point estimate of I2 and proportion significant results based on 95% confidence interval
I2_replicate_ci <- function(reps, tau2, k, N){ 
  I2_dist <- vector("list", length(tau2))
  I2_ci <- vector("list", length(tau2))
  for(t in seq_along(tau2)){
    I2 <- rep(NA, reps)
    ci.lb <- rep(NA, reps)
    
    for(rep in 1:reps){
      fit <- rma_simulate(k, tau2[t], N) 
      I2[rep] <- suppressWarnings(if(is.na(fit)) NA else fit$I2) #I2 point estimate
      ci.lb[rep] <- suppressWarnings(if(is.na(fit)) NA else confint(fit)$random[3, 2]) #lower bound I2 CI
    }
    
    I2_dist[[t]] <- data.frame(I2 = I2, tau2 = t)
    I2_ci[[t]] <- data.frame(not_zero = mean(ci.lb > 0, na.rm = TRUE), #removs runs where rma did not converge
                                   tau2 = t) #each input tau2 is output as its index in the tau2 vector
  }
  I2_dist <- do.call(rbind, I2_dist)
  I2_ci <- do.call(rbind, I2_ci)
  list(I2_dist = I2_dist, I2_ci_lb = I2_ci) #outputs two dataframes in a list
}

#**************************TEST----

datx <- dat %>% 
  filter(effect == "Sunk Costs")

fit <- rma(measure = "SMD", m1i = outcome_t1, m2i = outcome_c1, sd1i = outcome_t2, 
           sd2i = outcome_c2, n1i = ntreatment, n2i = ncontrol, data = datx)
confint(fit)$random[3, 2] #gives us lower bound of I2

taus <- list(c(0, 0.01), c(0, 0.02))

for(e in seq_along(dat2)){
  res[[e]] <- I2_replicate_ci(5, c(0, 0.01), dat2[[e]]$k, dat2[[e]]$Ntotal)
  cat("...RS",e, "/37") #see progress
  # if (e%%5 == 0 | e == 37) saveRDS(res, "temp_sim_results.RDS") #save ocassionally and at finish
}



#******************************************
#Run simulations to estimate tau2 values that correspond to small/medium/large I2----
#******************************************

dat2 <- dat %>% #Extract k for each project and N of sub-studies
  split(., .$effect) %>% 
  map(~ list(k = nrow(.), Ntotal = .$Ntotal)) #~ is shorthand for an anonymous function

tau2_values <- seq(0, 0.25, by = 0.005) #tau2 values to loop over

set.seed(112)
res <- vector("list", length(dat2)) 

for(e in seq_along(dat2)){
 res[[e]] <- I2_replicate_point(50, tau2_values, dat2[[e]]$k, dat2[[e]]$Ntotal)
 cat("...RS",e, "/37") #see progress
 if (e%%5 == 0 | e == 37) saveRDS(res, "temp_sim_results.RDS") #save ocassionally and at finish
}

dat3 <- readRDS("temp_sim_results.RDS")
names(dat3) <- names(dat2)

dat3 <- dat3 %>% #create dataframe with identifier
  bind_rows(.id = "effect")


#Extract values that correspond best to I2 = small (25%), medium (50%) and large (75%)
dat4 <- dat3 %>% 
  mutate(s = I2 - 25,
         m = I2 - 50,
         l = I2 - 75) %>% 
  group_by(effect) %>% 
  summarize(small = tau2[which.min(abs(s))], #tau2 value for I2 closest to 25
            medium = tau2[which.min(abs(m))], #tau2 value for I2 closest to 50
            large = tau2[which.min(abs(l))]) %>% #tau2 value for I2 closest to 75
  ungroup()
#******************************************
#Plot tau2 against I2----
#******************************************

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
#Simulation no. 2----
#******************************************
dat5 <- dat4 %>%
  split(.$effect) %>% 
  map(., function(x) x %>% select(-effect) %>% as.numeric(t(.))) %>% 
  map2(dat2, ., list) #add the extracted tau2-values as a vector to each list-element in dat2

set.seed(56)
res2 <- vector("list", length(dat5)) 

system.time(for(e in seq_along(dat5)){
  res2[[e]] <- I2_replicate_ci(1e3, c(0, dat5[[e]][[2]]), dat5[[e]][[1]]$k, dat5[[e]][[1]]$Ntotal)
  cat("...RS",e, "/37") #see progress
  if (e%%5 == 0 | e == 37) saveRDS(res2, "temp_sim_results_2.RDS") #save ocassionally and at finish
})



res2 <- readRDS("temp_sim_results_2.RDS")

I2_dist <- lapply(res2, function(x) x$I2_dist)
I2_ci_lb <- lapply(res2, function(x) x$I2_ci_lb)

names(I2_dist) <- names(I2_ci_lb) <- names(dat2)

library(tidyr) #Used for spread
I2_ci_lb <- I2_ci_lb %>% #power and type 1 error for each effect, ready for tabling
  bind_rows(.id = "effect") %>% 
  tidyr::spread(., key = tau2, value = not_zero) %>% 
  rename(zero = '1', small = '2', medium = '3', large = '4')

#******************************************
#Plot distributions no. 2----
#******************************************
#Prep distribution for plotting
I2_dist <- I2_dist %>% 
  bind_rows(.id = "effect") %>% 
  mutate(tau2 = recode(tau2, '1' = "Zero",
                       '2' = "Small",
                       '3' = "Medium",
                       '4' = "Large"),
         tau2 = as.factor(tau2))

#load function and process from tables.rmd, needs to be fixed
observed <- het %>%
  ungroup() %>% 
  select(effect, I2 = s_I2) %>%
  mutate(tau2 = "Observed")

ggplot(I2_dist, aes(x = I2, group = tau2, fill = tau2, linetype = tau2)) +
  geom_density(alpha = 0.3) +
  theme_classic() +
  coord_cartesian(xlim = c(0, 100)) +
  guides(fill = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE), 
         linetype = guide_legend(reverse = TRUE)) +
  scale_fill_grey()




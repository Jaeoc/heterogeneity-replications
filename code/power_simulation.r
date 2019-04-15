#******************************************

#Project: Heterogeneity in direct replications in psychology and its association with effect size
#Script purpose: Simulate I2 distribution for different levels of heterogeneity given data weights
#Code: 

#******************************************

#******************************************
#Packages and data----
#******************************************
if(!require(readr)){install.packages("readr")}
if(!require(metafor)){install.packages("metafor")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(purrr)){install.packages("purrr")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(tidyr)){install.packages("tidyr")}


library(readr) #load data
library(metafor) #run meta-analyses
library(dplyr) #general data wrangling
library(purrr) #for 'possibly' and iteration with 'map'
library(ggplot2) #plot results from first simulation
library(tidyr) #for 'spread'

dat <- read_csv("../data/collated_summary_data.csv")

source("./helper_functions_tables_figures.r") #for function to transform d to biserial correlation
#******************************************
#Simulation functions----
#******************************************
simulate_I2 <- function(effect, reps, tau, effect_size){ #this function applies to a single list object (effect), see next section
  
  K <- effect$K #Number of studies
  N <- effect$Ntotal #Sample sizes for all K studies
  output <- vector("list", length(tau)) #empty list for output
  
  if(effect$type == "r"){ #If correlation
    
    for(t in seq_along(tau)){ #loop over each tau-value
      
      output[[t]] <- map_dfr(1:reps, possibly(function(x){ #For each tau-value repeat below "reps" times and bind into dataframe
        
        rho <- switch(effect_size, #Draw true correlation rho for each k at the given value of tau and with specified effect size
                      zero = rnorm(n = K, mean = 0, sd = tau[t]),
                      small = rnorm(n = K, mean = 0.1, sd = tau[t]),
                      medium = rnorm(n = K, mean = 0.3, sd = tau[t]),
                      large = rnorm(n = K, mean = 0.5, sd = tau[t]))
        
        fr <- rnorm(n = K, mean = rho, sd = sqrt(1 / (N - 3))) #draw observed correlations (fisher's z) for each k
        
        fit <- rma(yi = fr, vi = 1 / (N - 3), method = "REML") #meta-analysis of fisher's z, each study weighted by its N
        
        data.frame(I2 = fit$I2, Qp = fit$QEp, ci.lb = confint(fit)$random[3, 2], 
                   tau = tau[t], tau_index = t)
        
      }, otherwise = NULL)) #If rma does not converge, drop that iteration ('possibly' function)
    }
    
  }else if(effect$type == "d"){ #That is, for SMD and odds ratios
    
    n_c <- effect$ncontrol #observed control group sizes
    n_t <- effect$ntreatment #observed treatment group sizes
    
    for(t in seq_along(tau)){
      
      output[[t]] <- map_dfr(1:reps, possibly(function(x){
        
        theta <- switch(effect_size,
                        zero = rnorm(n = K, mean = 0, sd = tau[t]), #draw effect sizes for each K at given tau and effect size
                        small = rnorm(n = K, mean = 0.2, sd = tau[t]),
                        medium = rnorm(n = K, mean = 0.5, sd = tau[t]),
                        large = rnorm(n = K, mean = 0.8, sd = tau[t]))
        
        avg_c <- rnorm(n = K, mean = 0, sd = 1 / sqrt(n_c)) #Draw means from sampling distribution control group
        avg_t <- rnorm(n = K, mean = theta, sd = 1 / sqrt(n_t)) #Draw means from sampling distribution treatment group
        var_c <- rchisq(n = K, df = n_c - 1) / (n_c - 1) #draw variances from sampling distribution control group
        var_t <- rchisq(n = K, df = n_t - 1) / (n_t - 1) #draw variances from sampling distribution treatment group
        
        fit <- rma(measure = "SMD",  m1i = avg_t, m2i = avg_c, sd1i = sqrt(var_t), 
                   sd2i = sqrt(var_c), n1i = n_t, n2i = n_c, method = "REML") #fit meta-analysis transforming into standardized mean difference (Hedge's g)
        
        data.frame(I2 = fit$I2, Qp = fit$QEp, ci.lb = confint(fit)$random[3, 2], 
                   tau = tau[t], tau_index = t)
        
      }, otherwise = NULL)) #some repetition in the code because must check if model converges each time
    }
  }else{ #If effect type = "MD", that is for mean differences, risk ratios and odds ratios, see next code section
         #Only difference between this segment and the one above is in fitting the model ("MD" vs. "SMD")
    
    n_c <- effect$ncontrol #observed control group sizes
    n_t <- effect$ntreatment #observed treatment group sizes
    
    for(t in seq_along(tau)){
      
      output[[t]] <- map_dfr(1:reps, possibly(function(x){
        
        theta <- switch(effect_size,
                        zero = rnorm(n = K, mean = 0, sd = tau[t]), #draw effect sizes for each K at given tau and effect size
                        small = rnorm(n = K, mean = 0.2, sd = tau[t]),
                        medium = rnorm(n = K, mean = 0.5, sd = tau[t]),
                        large = rnorm(n = K, mean = 0.8, sd = tau[t]))
        
        avg_c <- rnorm(n = K, mean = 0, sd = 1 / sqrt(n_c)) #Draw means from sampling distribution control group
        avg_t <- rnorm(n = K, mean = theta, sd = 1 / sqrt(n_t)) #Draw means from sampling distribution treatment group
        var_c <- rchisq(n = K, df = n_c - 1) / (n_c - 1) #draw variances from sampling distribution control group
        var_t <- rchisq(n = K, df = n_t - 1) / (n_t - 1) #draw variances from sampling distribution treatment group
        
        fit <- rma(measure = "MD",  m1i = avg_t, m2i = avg_c, sd1i = sqrt(var_t),
                   sd2i = sqrt(var_c), n1i = n_t, n2i = n_c, method = "REML") #fit meta-analysis for mean difference
        
        data.frame(I2 = fit$I2, Qp = fit$QEp, ci.lb = confint(fit)$random[3, 2], 
                   tau = tau[t], tau_index = t)
        
      }, otherwise = NULL))
    }
  }
  
  bind_rows(output) #Combine output across tau-values into one dataframe
}


#*************************
simulate_biserial <- function(effect, reps, tau){ #similar to above, but only for biserial correlations, effect size -> 0
  
  K <- effect$K #Number of studies
  N <- effect$Ntotal #Sample sizes for all K studies
  output <- vector("list", length(tau)) #empty list for output
  
  if(effect$type == "r"){ #If 'correlation'
    
    for(t in seq_along(tau)){ #loop over each tau-value
      
      output[[t]] <- map_dfr(1:reps, possibly(function(x){ #For each tau-value repeat below "reps" times and bind into dataframe
        
        rho <- rnorm(n = K, mean = 0, sd = tau[t]) #draw effect sizes for each K at given tau
        
        fr <- rnorm(n = K, mean = rho, sd = sqrt(1 / (N - 3))) #draw observed correlations (fisher's z) for each k
        
        fit <- rma(yi = fr, vi = 1 / (N - 3), method = "REML") #meta-analysis of fisher's z, each study weighted by its N
        
        data.frame(tau2_hat = fit$tau2, I2 = fit$I2, Qp = fit$QEp, 
                   tau = tau[t], tau_index = t)
        
      }, otherwise = NULL)) #If rma does not converge, drop that iteration ('possibly' function)
    }
    
  }else{ #for all non-'correlations' transform into biserial correlations
    
    n_c <- effect$ncontrol #observed control group sizes
    n_t <- effect$ntreatment #observed treatment group sizes
    
    for(t in seq_along(tau)){
      
      output[[t]] <- map_dfr(1:reps, possibly(function(x){
        
        theta <- rnorm(n = K, mean = 0, sd = tau[t]) #draw effect sizes for each K at given tau
        
        avg_c <- rnorm(n = K, mean = 0, sd = 1 / sqrt(n_c)) #Draw means from sampling distribution control group
        avg_t <- rnorm(n = K, mean = theta, sd = 1 / sqrt(n_t)) #Draw means from sampling distribution treatment group
        var_c <- rchisq(n = K, df = n_c - 1) / (n_c - 1) #draw variances from sampling distribution control group
        var_t <- rchisq(n = K, df = n_t - 1) / (n_t - 1) #draw variances from sampling distribution treatment group
        
        sdpooled <- sqrt(((n_t - 1)*var_t + (n_c - 1)*var_c) / (n_t + n_c - 2)) #Borenstein, M. (2009), p. 226. 
        d <- (avg_t - avg_c) / sdpooled
        
        biserial_r <- transform_d_to_r(d, n_t, n_c) #function sourced from 'helper_functions_tables_figures.r'
        
        fit <- rma(yi = biserial_r$r, vi = biserial_r$vi)
        data.frame(tau2_hat = fit$tau2, I2 = fit$I2, Qp = fit$QEp, 
                   tau = tau[t], tau_index = t)
        
      }, otherwise = NULL))
    }
  }
    
  bind_rows(output)
}

#******************************
#apply biserial simulation function (first run data prep)
tau2 <- c(1/321, 1/107, 3/107)
aa <- lapply(dat2, simulate_biserial, reps = 10, tau = sqrt(tau2))
#There are some effects in ML3 which don't have ntreatment or ncontrol, see str(aa, max.level = 1)
#these thus need to be treated as correlations in the simulations. [actually, we decided that this
#is what we'll do with all these correlations anyway]
# -> fixed

fit <- rma(measure = "UCOR", ri = effect_size, ni = Ntotal,  data = x, vtype = "UB") #model used by ML3
#******************************************
#Simulation 1 -  estimate tau values that correspond to small/medium/large I2----
#******************************************

##Prep data for simulation function

# OR2d <- c('Allowed vs. forbidden', 'Gain vs. loss framing', #effects that were transformed from OR to SMD
#           'Norm of reciprocity', 'Low vs. high category scales') #Not used after all

dat2 <- dat %>% #Extract k, effect type and sample sizes for each effect
  mutate(effect_type = recode(effect_type, 
                              "Risk difference" = "MD",
                              "Raw mean difference" = "MD")) %>% #as mean differences in simulation
  split(.$effect) %>% 
  map(~ list(K = nrow(.), #~ is shorthand for an anonymous function
             Ntotal = .$Ntotal, 
             type = unique(.$effect_type),
             ncontrol = .$ncontrol,
             ntreatment = .$ntreatment)) 

##Simulation
tau_values <- seq(0, 0.5, by = 0.005) #tau values to loop over

set.seed(112)
res <- vector("list", length(dat2)) #output of below loop

system.time(for(e in seq_along(dat2)){ #As loop to be able to see and save progress (lapply otherwise option)
 res[[e]] <- simulate_I2(dat2[[e]], reps = 1e3, tau = tau_values, effect_size = "zero") #NB! 1e3 reps here is about 36 hours on my (fairly slow) machine
 cat("...RS",e, "/37") #see progress
 if (e%%5 == 0 | e == 37) saveRDS(res, "../data/tau_simulation_results.RDS") #save ocassionally and at finish
})

#*********************
#Effect size sensitivity simulation for Appendix A

# set.seed(100)
# res_a <- vector("list", length(dat2)) #output of below loop

# system.time(for(e in seq_along(dat2)){ #As loop to be able to see and save progress (lapply otherwise option)
  # res_a[[e]] <- simulate_I2(dat2[[e]], reps = 1, tau = tau_values, effect_size = "medium") #NB! 1000 reps here is about 24 hours on my (fairly slow) machine
  # cat("...RS",e, "/37") #see progress
  # if (e%%5 == 0 | e == 37) saveRDS(res_a, "../data/AppendixA_tau_simulation_results.RDS") #save ocassionally and at finish
# })
#********************

##Simulation results
dat3 <- readRDS("../data/tau_simulation_results.RDS")
names(dat3) <- names(dat2) #names are lost when looping instead of using lapply

dat3 <- dat3 %>% #create dataframe with identifier
  bind_rows(.id = "effect") %>% 
  select(I2, tau, effect) %>% 
  group_by(tau, effect) %>% 
  summarize(I2 = mean(I2)) %>% #Take mean of I2 at each tau-level and for each effect
  ungroup()

#Extract values that correspond best to I2 = small (25%), medium (50%) and large (75%)
dat4 <- dat3 %>% 
  mutate(s = I2 - 25,
         m = I2 - 50,
         l = I2 - 75) %>% 
  group_by(effect) %>% 
  summarize(small = tau[which.min(abs(s))], #tau value for I2 closest to 25
            medium = tau[which.min(abs(m))], #tau value for I2 closest to 50
            large = tau[which.min(abs(l))]) %>% #tau value for I2 closest to 75
  ungroup()

#Point-plot shows clearly that the initial increase is fastest 
#This plot provides good overview of results but is only an explorative plot
#see figures.rmd for the plots in the paper
ggplot(dat3, aes(x = tau, y = I2)) + 
  geom_point(alpha = 0.2) +
  facet_wrap(~effect)

#******************************************
#Simulation 2 - estimate power/type 1 error at zero/small/medium/large heterogeneity----
#******************************************
##Prep data for simulation function
dat5 <- dat4 %>% #add the extracted tau-values as a vector to each list-element in dat2
  split(.$effect) %>% 
  map(., function(x) x %>% select(-effect) %>% as.numeric(t(.))) %>% 
  map2(dat2, ., list) 

##Simulation
set.seed(56)
res2 <- vector("list", length(dat5)) 

system.time(for(e in seq_along(dat5)){ #As loop to be able to see and save progress (lapply otherwise option)
  res2[[e]] <- simulate_I2(dat5[[e]][[1]], reps = 1e4,
                           tau = c(0, dat5[[e]][[2]]), effect_size = "zero") #NB! 1e4 reps here is about 14 hours on my (fairly slow) machine
  cat("...RS",e, "/37") #see progress
  if (e%%5 == 0 | e == 37) saveRDS(res2, "../data/power_simulation_results.RDS") #save ocassionally and at finish
})

#*********************
#Effect size sensitivity simulation for Appendix A

# set.seed(50)
#  res2_a <- vector("list", length(dat2)) #output of below loop
# 
#  system.time(for(e in seq_along(dat5)){ #As loop to be able to see and save progress (lapply otherwise option)
#    res2_a[[e]] <- simulate_I2(dat5[[e]][[1]], reps = 1e4,
#                             tau = c(0, dat5[[e]][[2]]), effect_size = "medium") #NB! 1e4 reps here is about 9.5 hours on my (fairly slow) machine
#   cat("...RS",e, "/37") #see progress
#    if (e%%5 == 0 | e == 37) saveRDS(res2_a, "../data/AppendixA_power_simulation_results.RDS") #save ocassionally and at finish
#  })
#********************

##Simulation results
dens <- readRDS("../data/power_simulation_results.RDS") #very large if saved as .csv
names(dens) <-  names(dat2)

#Summary of results. These are incorporated into the main table in the paper, see tables.rmd
I2_ci_lb <- dens %>% 
  bind_rows(.id = "effect") %>% 
  group_by(effect, tau_index) %>% 
  summarize(power = mean(Qp <= 0.05)) %>% #Estimate power/type 1 error for each tau level and effect
  ungroup() %>% 
  tidyr::spread(key = tau_index, value = power) %>% #prep for table
  rename(zero = '1', small = '2', medium = '3', large = '4')


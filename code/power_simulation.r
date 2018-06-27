#******************************************

#Project: Heterogeneity in direct replications
#Script purpose: Simulate I2 distribution for different levels of heterogeneity given data weights
#Code: Anton Ohlsson Collentine

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


#******************************************
#Simulation function----
#******************************************
simulate_I2 <- function(effect, reps, tau){ #this function applies to a single list object (effect)
  
  K <- effect$K #Number of studies
  N <- effect$Ntotal #Sample sizes for all K studies
  output <- vector("list", length(tau)) #empty list for output
  
  if(effect$type == "r"){ #If correlation
    
    for(t in seq_along(tau)){ #loop over each tau-value
      
      output[[t]] <- map_dfr(1:reps, possibly(function(x){ #For each tau-value repeat below "reps" times and bind into dataframe
        
        rho <- rnorm(n = K, mean = 0, sd = tau[t]) #Draw true correlation rho for each k at the given value of tau
        fr <- rnorm(n = K, mean = rho, sd = sqrt(1 / (N - 3))) #draw observed correlations (fisher's z) for each k
        
        fit <- rma(yi = fr, vi = 1 / (N - 3), method = "REML") #meta-analysis of fisher's z, each study weighted by its N
        
        data.frame(I2 = fit$I2, ci.lb = confint(fit)$random[3, 2], tau = tau[t], tau_index = t)
        
      }, otherwise = NULL)) #If rma does not converge, drop that iteration ('possibly' function)
    }
    
  }else{ #if not correlation, treat as t-test
    
    N_half <- N%/%2 #divide Ntotal in two equal halves (integer rounded) for treatment and control group
    
    for(t in seq_along(tau)){
      
      output[[t]] <- map_dfr(1:reps, possibly(function(x){
        
        theta <- rnorm(n = K, mean = 0, sd = tau[t]) #draw effect sizes for each study
        avg_c <- rnorm(n = K, mean = 0, sd = 1 / sqrt(N_half)) #Draw means from sampling distribution control group
        avg_t <- rnorm(n = K, mean = theta, sd = 1 / sqrt(N_half)) #Draw means from sampling distribution treatment group
        var_c <- rchisq(n = K, df = N_half - 1) / (N_half - 1) #draw variances from sampling distribution control group
        var_t <- rchisq(n = K, df = N_half - 1) / (N_half - 1) #draw variances from sampling distribution treatment group
        
        fit <- rma(measure = "SMD",  m1i = avg_t, m2i = avg_c, sd1i = sqrt(var_t), sd2i = sqrt(var_c), n1i = N_half,
                   n2i = N_half, method = "REML") #fit meta-analysis transforming into standardized mean difference (Hedge's g)
        
        data.frame(I2 = fit$I2, ci.lb = confint(fit)$random[3, 2], tau = tau[t], tau_index = t)
        
      }, otherwise = NULL))
    }
  }
  
  bind_rows(output) #Combine output across tau-values into one dataframe
}

#******************************************
#Simulation 1 -  estimate tau values that correspond to small/medium/large I2----
#******************************************

dat2 <- dat %>% #Extract k and effect type for each effect and N of each included study
  split(.$effect) %>% 
  map(~ list(K = nrow(.), Ntotal = .$Ntotal, type = unique(.$effect_type))) #~ is shorthand for an anonymous function

tau_values <- seq(0, 0.5, by = 0.005) #tau values to loop over

set.seed(112)
res <- vector("list", length(dat2)) #output of below loop

system.time(for(e in seq_along(dat2)){ #As loop to be able to see and save progress (lapply otherwise option)
 res[[e]] <- simulate_I2(dat2[[e]], reps = 1000, tau = tau_values) #100 reps is about 3.5 hours on my (fairly slow) machine
 cat("...RS",e, "/37") #see progress
 if (e%%5 == 0 | e == 37) saveRDS(res, "../data/tau_simulation_results.RDS") #save ocassionally and at finish
})

dat3 <- readRDS("../data/tau_simulation_results")
names(dat3) <- names(dat2) #names are lost when looping instead of using lapply

dat3 <- dat3 %>% 
  bind_rows(.id = "effect") %>% #create dataframe with identifier
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
dat5 <- dat4 %>% #add the extracted tau2-values as a vector to each list-element in dat2
  split(.$effect) %>% 
  map(., function(x) x %>% select(-effect) %>% as.numeric(t(.))) %>% 
  map2(dat2, ., list) 

set.seed(56)
res2 <- vector("list", length(dat5)) 

system.time(for(e in seq_along(dat5)){ #As loop to be able to see and save progress (lapply otherwise option)
  res2[[e]] <- simulate_I2(dat5[[e]][[1]], reps = 1e4, tau = c(0, dat5[[e]][[2]]))
  cat("...RS",e, "/37") #see progress
  if (e%%5 == 0 | e == 37) saveRDS(res2, "../data/power_simulation_results.RDS") #save ocassionally and at finish
})


dens <- readRDS("../data/power_simulation_results.RDS") #very large if saved as .csv
names(dens) <-  names(dat2)

#Summary of results. These are incorporated into the main table in the paper, see tables.rmd
I2_ci_lb <- dens %>% 
  bind_rows(.id = "effect") %>% 
  group_by(effect, tau_index) %>% 
  summarize(power = mean(ci.lb > 0)) %>% #Estimate power/type 1 error for each tau level and effect
  ungroup() %>% 
  tidyr::spread(key = tau_index, value = power) %>% #prep for table
  rename(zero = '1', small = '2', medium = '3', large = '4')


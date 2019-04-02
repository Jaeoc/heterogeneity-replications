#******************************************

#Project: Heterogeneity in direct replications in psychology and its association with effect size
#Script purpose: Helper functions to prep data for tables and figures
#Code: 
#Additional comments: These functions are intended to be 'sourced' into the 
#                     tables.rmd and figures.rmd files 

#******************************************



#******************************************
#Function to fit a random effects model to all empirical data----
#******************************************
#Each set of input datapoints is meta-analyzed as per the original replication project

est_heterogen_smd_raw <- function(x){
  if(any(x[, "effect_type"] == "Risk difference")){ #without the 'any' we will get warnings because we apply 'if' to a vector
    
    fit <- rma(measure = "RD", ai = outcome_t1, bi = outcome_t2, ci = outcome_c1, di = outcome_c2,  n1i = ntreatment, n2i = ncontrol,  data = x)
    
  } else if(any(x[, "outcomes1_2"] == "mean _ SE")){  
    
    fit <-  rma(yi = effect_size, sei = outcome_c2,  data = x) 
    
  } else if(any(x[, "effect_type"] == "Raw mean difference")){
    if(any(x[, "rp"] %in% c("RRR5", "RRR7"))){ #For these us the Knapp and Hartung adjustment of standard errors
      
      fit <- rma(measure = "MD", m1i = outcome_t1, m2i = outcome_c1, sd1i = outcome_t2, sd2i = outcome_c2, n1i = ntreatment, n2i = ncontrol, test ="knha", data = x)
    } else{
      fit <- rma(measure = "MD", m1i = outcome_t1, m2i = outcome_c1, sd1i = outcome_t2, sd2i = outcome_c2, n1i = ntreatment, n2i = ncontrol, data = x)
    }
    
  } else if(any(x[, "outcomes1_2"] == "mean _ SD")){  
    
    fit <- rma(measure = "SMD", m1i = outcome_t1, m2i = outcome_c1, sd1i = outcome_t2, sd2i = outcome_c2, n1i = ntreatment, n2i = ncontrol, data = x) 
    
  } else if(any(x[, "effect_type"] == "r")){
    if(any(x[, "rp"] == "ML1")){
      fit <- rma(measure = "COR", ri = effect_size, ni = Ntotal,  data = x)
    } else{ #for ML3
      fit <- rma(measure = "UCOR", ri = effect_size, ni = Ntotal,  data = x, vtype = "UB")
      #Note that ML3 specifies measure = "COR" with vtype = "UB", but that this is specified as above in the latest metafor 
    }
    
  } else{ #for all two-group count effects
    
    fit <- rma(measure = "OR2DL", ai = outcome_t1, bi = outcome_t2, ci = outcome_c1, di = outcome_c2, 
               n1i = ntreatment, n2i = ncontrol,  data = x)
    #standardized, ML1 used 'OR2D' which = 'OR2DL' in latest version of metafor
    
  }
  
  hetero <- confint(fit)$random[c(1, 3), ] #Gives us the tau2 and I2 estimates with confidence intervals
  data.frame(eff_size = fit$b[[1]], #effect size (point estimate) 
             s_I2 = hetero[2, 1], s_ci.lb = hetero[2, 2], s_ci.ub = hetero[2, 3],
             tau2 = hetero[1, 1], tau2_ci.lb = hetero[1, 2], tau2_ci.ub = hetero[1, 3]) #I2 + CI
}


#******************************************
#Functions to transform all effect sizes to the same unit of measurement (Fisher's z)----
#******************************************

#Formulas found in: 
#Borenstein, M. (2009). Effect sizes for continuous data. In H. Cooper, L. V. Hedges, & J. C. Valentine (Eds.), 
#The handbook of research synthesis and meta-analysis (2nd ed., pp. 221-235). New York: Russell Sage Foundation.

#For raw mean difference + SE
transform_SE <- function(ES, SE, n1, n2){ 
  sdpooled <- sqrt(SE^2 / (1 / n1 + 1/n2)) #Borenstein, M. (2009). In Cooper & Hedges. p. 224
  d <- ES / sdpooled
  corr <- (n1 + n2)^2 / (n1*n2) #correction factor if unequal sample sizes. p.234
  r <- d / sqrt(d^2 + corr) #p.234
  data.frame(r = r, n = n1 + n2)
}

#For mean differrence + SD
transform_SD <- function(m1, m2, SD1, SD2, n1, n2){ #assumes ES is raw mean difference
  sdpooled <- sqrt(((n1 - 1)*SD1^2 + (n2 - 1)*SD2^2) / (n1 + n2 - 2)) #page 226. 
  d <- (m1 - m2) / sdpooled
  corr <- (n1 + n2)^2 / (n1*n2) #correction factor if unequal sample sizes, p.234
  r <- d / sqrt(d^2 + corr) #p. 234
  data.frame(r = r, n = n1 + n2)
}

#For Risk difference/OR
transform_RD <- function(ai, bi, ci, di){
  ai <- ifelse(ai == 0, 0.5, ai)  #If value is equal to zero add one half so as to not divide by zero
  bi <- ifelse(bi == 0, 0.5, bi)  #This affects 1 study for Allowed vs. forbidden
  ci <- ifelse(ci == 0, 0.5, ci)  #And several studies for Low vs. high category scales
  di <- ifelse(di == 0, 0.5, di)
  logOR <- log((ai*di) / (bi*ci)) #p. 266
  d = logOR * sqrt(3)/pi #p. 232
  n1 = ai + bi
  n2 = ci + di
  corr <- (n1 + n2)^2 / (n1*n2) #correction factor if unequal sample sizes, p. 234
  r <- d / sqrt(d^2 + corr) #p. 234
  data.frame(r = r, n = ai + bi + ci +di)
}

#Function to apply the transformation functions to the data
transform_MA <- function(x){
  if(any(x[, "effect_type"] == "Risk difference")){ #without the 'any' we will get warnings because we apply 'if' to a vector
    
    transformed <- transform_RD(ai = x$outcome_t1, bi = x$outcome_t2, ci = x$outcome_c1, di = x$outcome_c2)
    
    #fit <- rma(measure = "ZCOR", ri = zcor, ni = n, test = "knha", data = transformed)
    
  } else if(any(x[, "outcomes1_2"] == "mean _ SE")){  
    
    transformed <- transform_SE(x$effect_size, x$outcome_c2, x$ntreatment, x$ncontrol)
    
    #fit <- rma(measure = "ZCOR", ri = zcor, ni = n, test = "knha", data = transformed)
    
  } else if(any(x[, "outcomes1_2"] == "mean _ SD")){  
    
    transformed <- transform_SD(x$outcome_t1, x$outcome_c1, x$outcome_t2, x$outcome_c2, x$ntreatment, x$ncontrol)
    
    # fit <- rma(measure = "ZCOR", ri = zcor, ni = n, test = "knha", data = transformed)
    
  } else if(any(x[, "effect_type"] == "r")){
    
    transformed <- data.frame(r = x$effect_size, n = x$Ntotal)
    
    # fit <- rma(measure = "ZCOR", ri = zcor, ni = n, test = "knha", data = transformed)
    
  } else{ #For the many labs OR that were transformed to d [double check]
    
    transformed <- transform_RD(ai = x$outcome_t1, bi = x$outcome_t2, ci = x$outcome_c1, di = x$outcome_c2)
    
  }
  
  # data.frame(b = fit$b[[1]], I2 = fit$I2) #estimate out
  transformed
}





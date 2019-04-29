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
#Functions to transform all effect sizes to the same unit of measurement and meta-analyze----
#******************************************

#Formulas found in: 
#Borenstein, M. (2009). Effect sizes for continuous data. In H. Cooper, L. V. Hedges, & J. C. Valentine (Eds.), 
#The handbook of research synthesis and meta-analysis (2nd ed., pp. 221-235). New York: Russell Sage Foundation.

#Jacobs, P., & Viechtbauer, W. (2017). Estimation of the biserial correlation and its sampling variance for use
#in meta-analysis. Research Synthesis Methods, 8(2), 161-180.
#(provide an exact computation of the point-biserial correlation rather than the approximate [Hunter & Schmitd] in Borenstein)

#Jacobs and Viechtbauer (2017) recommend transforming the point-biserial correlation to a 
#biserial correlation. 

#Brown, M. B., & Benedetti, J. K. (1977). On the mean and variance of the tetrachoric correlation coefficient. 
#Psychometrika, 42(3), 347-355.
#(Equation 9 used to compute the tetrachoric correlation variance)

#Olsson, U. (1979) Maximum likelihood estimation of the polychoric correlation coefficient. 
#Psychometrika 44, 443-460.
#ML approach used by polycor::polychoric to compute the tetrachoric correlations

##For raw mean difference + SE
transform_SE <- function(ES, SE, n1, n2){ 
  sdpooled <- sqrt(SE^2 / (1 / n1 + 1/n2)) #Borenstein, M. (2009). In Cooper & Hedges. p. 224
  d <- ES / sdpooled
  data.frame(d = d, n1 = n1, n2 = n2)
}

##For mean differrence + SD
transform_SD <- function(m1, m2, SD1, SD2, n1, n2){ #assumes ES is raw mean difference
  sdpooled <- sqrt(((n1 - 1)*SD1^2 + (n2 - 1)*SD2^2) / (n1 + n2 - 2)) #Borenstein, M. (2009), p. 226. 
  d <- (m1 - m2) / sdpooled
  data.frame(d = d, n1 = n1, n2 = n2)
}


transform_d_to_r <- function(d, n1, n2){ 
  m <- n1 + n2 - 2 #Jacobs and Viechtbauer (2017), p.164
  h <- m/n1 + m/n2 #p. 164
  rpb <- d / sqrt(d^2 + h) #point-biserial correlation, Jacobs and Viechtbauer (2017), equation 5
  p <- n1 / (n1 + n2)
  q <- n2 / (n1 + n2)
  zp <- qnorm(p)
  fzp <- dnorm(zp)
  rb <- sqrt(p*q) / fzp * rpb #biserial correlation, Jacobs and Viechtbauer (2017), equation 8
  rb_trunc <- ifelse(rb > 1, 1, rb) #truncate for > 1 when computing variance to avoid neg. variance. p. 166-167
  var_rb <- 1/(n1 + n2 -1) * (sqrt(p*q) / fzp - rb_trunc^2)^2 #variance biserial r, approximate formula, eq. 13 
  data.frame(r = rb, vi = var_rb, n = n1 + n2) #I use the approximate formula (eq 13) above to decrease the risk of coding error
}


##For Risk difference/OR [first function no longer used. Instead compute tetrachoric correlations]

#transform_RD <- function(ai, bi, ci, di){
#  ai <- ifelse(ai == 0, 0.5, ai)  #If value is equal to zero add one half so as to not divide by zero
#  bi <- ifelse(bi == 0, 0.5, bi)  #This affects 1 study for Allowed vs. forbidden
#  ci <- ifelse(ci == 0, 0.5, ci)  #And several studies for Low vs. high category scales
#  di <- ifelse(di == 0, 0.5, di)
#  logOR <- log((ai*di) / (bi*ci)) #Borenstein, M. (2009), p. 266
#  d = logOR * sqrt(3)/pi #p. 232
#  n1 = ai + bi
#  n2 = ci + di
#  data.frame(d = d, n1 = n1, n2 = n2)
#}

#bivariate normal density formula (below) from http://mathworld.wolfram.com/BivariateNormalDistribution.html
#Used in the following function
bdnorm <- function(z1, z2, r){ #standardized, i.e, mu1 = mu2 = 0 and sigma1 = sigma2 = 1
  z <- z1^2 - 2*r*z1*z2 - z2^2
  1 / (2*pi*sqrt(1-r^2)) * exp(-z / (2*(1-r^2)))
}
#correctness of bdnorm function checked with online calculator: https://www.easycalculation.com/statistics/bivariate-distribution-calculator.php

#For computing standard errors of the tetrachoric correlations Hamdan 1970 proposed a relatively simple
#formula, which is recommended by Brown and Benedetti 1977, as well as Olsson 1979. 
#[these also all cited by Jacobs and Viechtbauer in regards to tetrachoric correlations]
#Equation 9 in Brown and Benedetti is the Hamdan equation, but presented in a slightly nicer way

var_tetra <- function(ai, bi, ci, di, ri){ #Equation 9 in Brown and Benedetti 1977
  N <- ai+bi+ci+di
  z1 <- (ai + ci) / N
  z2 <- (bi + di) / N
  SE <- 1 / (N*bdnorm(z1, z2, ri)) * (1/ai + 1/bi + 1/ci + 1/di)^-0.5
  data.frame(vi = SE^2, n = N)
}

#Below we use polycor::polychor(q, ML = TRUE) to compute the tetrachoric correlation (where q is a 2x2 matrix)
#this uses Olsson's ML approach


#Function to apply the transformation functions to the data
#Note that this function requires library(dplyr), and that the purrr and polycor packages are installed
transform_MA <- function(x){
  if(any(x[, "effect_type"] == "Risk difference")){ #without the 'any' we will get warnings because we apply 'if' to a vector
    
    ri <- x %>%  #compute tetrachoric correlations, polychor function needs 2x2 table as input
      split(.$Site) %>% 
      purrr::map(~matrix(c(.$outcome_t1, .$outcome_c1, .$outcome_t2, .$outcome_c2), 2, 2)) %>% 
      purrr::map(polycor::polychor, ML = TRUE) %>% 
      purrr::map_dfr(~data.frame(r = .[[1]])) #map_dfr column binds vectors, hence this extra map_dfr step
    vi <- var_tetra(ai = x$outcome_t1, bi = x$outcome_t2, ci = x$outcome_c1, di = x$outcome_c2, ri = ri$r)
    
    out <- data.frame(r = ri$r, vi = vi$vi, n = vi$n)
    
  } else if(any(x[, "outcomes1_2"] == "mean _ SE")){  
    
    d_conversion <- transform_SE(x$effect_size, x$outcome_c2, x$ntreatment, x$ncontrol)
    out <- transform_d_to_r(d_conversion$d, d_conversion$n1, d_conversion$n2)
    
  } else if(any(x[, "outcomes1_2"] == "mean _ SD")){  
    
    d_conversion <- transform_SD(x$outcome_t1, x$outcome_c1, x$outcome_t2, x$outcome_c2, x$ntreatment, x$ncontrol)
    out <- transform_d_to_r(d_conversion$d, d_conversion$n1, d_conversion$n2)
    
  } else if(any(x[, "effect_type"] == "r")){
    
    out <- metafor::escalc(measure = "COR", ri = x$effect_size, ni =x$Ntotal)
    out <- data.frame(r = out$yi, vi = out$vi)
    
  } else{ #For the many labs OR that were transformed to d
    
    ri <- x %>%  #compute tetrachoric correlations, polychor function needs 2x2 table as input
      split(.$Site) %>% 
      purrr::map(~matrix(c(.$outcome_t1, .$outcome_c1, .$outcome_t2, .$outcome_c2), 2, 2)) %>% 
      purrr::map(polycor::polychor, ML = TRUE) %>% 
      purrr::map_dfr(~data.frame(r = .[[1]])) #map_dfr column binds vectors, hence this extra map_dfr step
    vi <- var_tetra(ai = x$outcome_t1, bi = x$outcome_t2, ci = x$outcome_c1, di = x$outcome_c2, ri = ri$r)
    
    out <- data.frame(r = ri$r, vi = vi$vi, n = vi$n)
    
  }
  
  out
}


#function to run random effect meta-analysis on transformed effect sizes
summarizer <- function(x){#Z-transformation not recommended by Jacobs and Viechtbauer (2017) for biserial correlations, p. 176
  fitr <- rma(yi = x$r, vi = x$vi, data = x) #rma for biserial and pearson correlations (distinct per effect!)
  ci <- confint(fitr)$random[c(1, 3), ] #I2 and tau2 confidence intervals
  
  data.frame(r = fitr$b[[1]], tau2 = fitr$tau2, tau2.lb = ci[1, 2], tau2.ub = ci[1, 3],
             I2 = fitr$I2, I2.lb = ci[2, 2], I2.ub = ci[2, 3], H2trunc = fitr$H2, H2 = fitr$QE / (fitr$k - 1)) 
}

#Default method of metafor for calculating H2 is truncated at one. Alternative method for calculating H2 provides information also on excessive homogeneity, i.e, less variability than expected by chance (that is, does not have a lower limit of 1). This method approximates H2 as if we were using the Dersimonian and Laird estimate of tau2, although we use REML. See Higgins & Thompson, 2002 and ?print.rma.uni.

#Higgins, J., & Thompson, S. G. (2002). Quantifying heterogeneity in a meta-analysis. Statistics in medicine, 21(11), 1539-1558.


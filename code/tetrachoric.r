#Code to compute tetrachoric correlations for odds ratios and risk differences
#ie.e, anything where we have 2x2 tables

q <- matrix(c(61661,1610,85,20),2,2)



a <- 61661
b <- 85
c <- 1610
d <- 20
r <- 0.3481
N <- a+b+c+d
z1 <- (a + c) / N
z2 <- (b + d) / N

#bivariate normal density formula (below) from http://mathworld.wolfram.com/BivariateNormalDistribution.html

bdnorm <- function(z1, z2, r){ #standardized, i.e, mu1 = mu2 = 0 and sigma1 = sigma2 = 1
  z <- z1^2 - 2*r*z1*z2 - z2^2
  1 / (2*pi*sqrt(1-r^2)) * exp(-z / (2*(1-r^2)))
}
#correctness of dbnorm function checked with online calculator: https://www.easycalculation.com/statistics/bivariate-distribution-calculator.php

#For computing standard errors of the tetrachoric correlations Hamdan 1970 proposed a relatively simple
#formula, which is recommended by Brown and Benedetti 1977, as well as Olsson 1979
#[these all cited by Jacobs and Viechtbauer]

var_tetra <- function(a, b, c, d, r){ 
  N <- a+b+c+d
  z1 <- (a + c) / N
  z2 <- (b + d) / N
  SE <- 1 / (N*bdnorm(z1, z2, r)) * (1/a + 1/b + 1/c + 1/d)^-0.5
  SE^2
}

#we use polycor::polychor(q, ML = TRUE) to compute the tetrachoric correlation (where q is a 2x2 matrix)
#this uses, I believe, Olsson's ML approach

dat %>% filter(

ri <- a %>% 
  split(.$Site) %>% 
  map(~matrix(c(.$outcome_t1, .$outcome_c1, .$outcome_t2, .$outcome_c2), 2, 2)) %>% 
  map(polycor::polychor, ML = TRUE) %>% 
  map_dfr(~data.frame(r = .[[1]]))

vi <- var_tetra(a$outcome_t1, a$outcome_c1, a$outcome_t2, a$outcome_c2, r = ri$r)



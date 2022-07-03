
rm(list=ls())  #remove list

require(RcppArmadillo)
require(Rcpp)
require(mvtnorm)
require(splines)
require(survival)
library(ggplot2)
library(ggrepel)

Rcpp::sourceCpp("PenalizeStopCpp.cpp")
source("PenalizeStop.R")

load("N5k_single_timevarying_2021.RData")

##########################3333
AR1 <- function(tau, m) {
  if(m==1) {R <- 1}
  if(m > 1) {
    R <- diag(1, m)
    for(i in 1:(m-1)) {
      for(j in (i+1):m) {
        R[i,j] <- R[j,i] <- tau^(abs(i-j))
      }
    }
  }
  return(R)
}
#######################################

library(survival)
library(mvtnorm)

p=1
nfold=5

#beta_true=rnorm(nzc)
K=10
knot=K

nloop=500

new_time_seq    <- seq(0.02,2,0.02)
check_timepoint <- c(0.1, 0.5, 1.0, 1.5, 2.0, 2.5)

llk_training       <- NULL
llk_testing        <- NULL
theta.SGD.all      <- NULL
Time_used_all      <- NULL
Iter_all           <- NULL
ASE_all_appro      <- NULL
ASE_all_true       <- NULL

bias_matrix        <- matrix(0,nloop,p*length(new_time_seq))    # record the bias of beta
ase_matrix_true    <- matrix(0,nloop,p*length(new_time_seq))    # the standard error of 
coverage_prob_true  <- matrix(0,6,p)

model_all <- list()
llk_NR_all <- list()

lambda_all <- c(seq(0.1,1.9,0.1),2:30)

loop=0
pb = txtProgressBar(style = 3)
while (loop<nloop) {
  loop=loop+1
  set.seed(loop)
  if(loop == 74){
    set.seed(loop*100)
  }
  F=1 ###number of facility
  n_f = rep(2000, F)#rpois(F, lambda = 1000)  #sample size for each facility
  N=sum(n_f)
  
  gamma = rep(0, F) #rnorm(F, mean=0, sd=0.5)
  range(gamma)
  gamma_subject=rep(gamma,n_f)
  
  F_pre=1:F
  facility=rep(F_pre, n_f)
  
  ############generate data########################
  Sigma_z1<-AR1(0.6,p)
  
  #z= rmvnorm(N, mean=rep(0,p), sigma=Sigma_z1)
  z = rnorm(N, mean = 0, 1)
  
  z_012_rare=function(x){
    
    U=runif(1, 0.85, 0.95)
    
    x2=quantile(x,prob=U)
    x3=x
    x3[x<x2]=0
    x3[x>x2]=1
    return(x3)
  }
  
  #z=apply(z,2, z_012_rare)
  z = z_012_rare(z)
  
  MAF=mean(z)
  range(MAF) #0.0769688 0.1778108
  
  
  U=runif(N, 0,1)
  
  pre_time=rep(0, N)
  for (i in 1:(N)) {
    f=function(t) {
      integrand <- function(x) {0.5*exp(gamma_subject[i] +sin(3*pi*x/4)*(x<3)*z[i])}
      
      Lambda=integrate(integrand, lower = 0, upper = t)$value
      Lambda+log(1-U[i])
    }
    r1 <- suppressWarnings(try(uniroot(f,  lower = 0, upper = 4), silent=TRUE))
    if (class(r1) == "try-error"){    
      pre_time[i]=4
    }
    else pre_time[i]=uniroot(f,  lower = 0, upper = 4)$root
  }
  
  pre_censoring=runif(N,0,3)
  pre_censoring=pre_censoring*(pre_censoring<3)+3*(pre_censoring>=3)
  tcens=(pre_censoring<pre_time) # censoring indicator
  delta=1-tcens
  time=pre_time*(delta==1)+pre_censoring*(delta==0)
  
  mean(delta)  # 0.8
  
  delta = delta[order(time)]
  facility=facility[order(time)]
  z = z[order(time)]
  time = time[order(time)]
  
  
  #split the data into two halves
  testing_index <- sample(N, floor(N/2))
  testing_index <- sort(testing_index)
  # delta_test    <- delta[-testing_index]
  # facility_test <- facility[-testing_index]
  # z_test        <- z[-testing_index,]
  # time_test     <- time[-testing_index]
  # 
  # knot_set = quantile(time_test[delta_test==1], prob=seq(1:(knot-4))/(knot-3))
  # bs7      = splines::bs(time_test,df=knot, knot=knot_set, intercept=TRUE, degree=3)
  # bs8      = matrix(bs7, nrow=dim(z_test)[1]) # for build beta_t
  # b_spline_test = bs8
  
  #for training data:
  delta     = delta[testing_index]
  facility  = facility[testing_index]
  z         = z[testing_index]
  time      = time[testing_index]
  
  knot_set = quantile(time[delta==1], prob=seq(1:(knot-4))/(knot-3))
  bs7      = splines::bs(time,df=knot, knot=knot_set, intercept=TRUE, degree=3)
  bs8      = matrix(bs7, nrow=length(delta)) # for build beta_t
  b_spline = bs8
  
  
  theta_stratify=matrix( rep(0, knot*p), nrow=p)  #dim P*knot
  
  count_strata <- c(table(facility))
  
  
  ##model fitting:
  
  Time_used = proc.time()
  stratum=rep(1, length(time))
  data_NR <- data.frame(event=delta, time=time, z, strata=stratum, stringsAsFactors=F)
  #Z.char <- paste0("X", 1:p)
  Z.char <- "z"
  fmla <- formula(paste0("Surv(time, event)~",
                         paste(c(paste0("tv(", Z.char, ")"), "strata(strata)"), collapse="+")))
  
  # 
  for(lambda_index in 1:length(lambda_all)){
    if(lambda_all[lambda_index] == 0){
      btr_tmp = "static"
    }else{
      btr_tmp = "static"      
    }
    model1 <- surtiver(fmla, data_NR, nsplines=K, spline ="Smooth-spline", ties="none", tau=0.5, stop="ratch",
                       method = "ProxN", btr = btr_tmp, iter.max = 15, threads = 4, parallel = TRUE,
                       lambda_spline = lambda_all[lambda_index],TIC_prox = FALSE, ord = 4, degree = 3, 
                       fixedstep = FALSE,
                       penalizestop = FALSE,
                       ICLastOnly = TRUE)
    
    record_index <- (loop-1)*length(lambda_all)+lambda_index
    
    model_all[[record_index]] <- model1
    llk_NR   <-  LogPartialTest(event = delta, Z_tv =  as.matrix(z), b_spline, 
                                delta_test, as.matrix(z_test), b_spline_test,
                                theta_list = model1$theta_list[length(model1$theta_list)],
                                parallel = TRUE, threads = 4, TestAll = TRUE)
    llk_NR_all[[record_index]] <- llk_NR
  }
  
  setTxtProgressBar(pb, loop / nloop)
}




save.image(file = "N1000_p1_timevarying_smoothcubic_static_500times_includeHastie.RData")



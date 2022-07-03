
###################################################################################################
######Constant
###################################################################################################
rm(list=ls())

require(RcppArmadillo)
require(Rcpp)
require(mvtnorm)
require(splines)
require(survival)
library(ggplot2)
library(ggrepel)
library(ggplot2)
library(ggpubr)
library(grid)
setwd("C:/Users/lfluo/Dropbox (University of Michigan)/Lingfeng Research/PenalizeStop/simulation/SingleVariable/N1000_new/RData/")
#size 1000
# load("N1000_p1_constant_smoothcubic_from1_500times_includeHastie_covereageprob.RData")
# load("N1000_p1_timevarying_smoothcubic_dynamic_500times_coverageprob_includeHastie.RData")
#load("D:/Projects/PenalizeStop/simulation/CrossValidation/N5000_smoothcubic_penalizestop_crossvalidation.RData")
Rcpp::sourceCpp("D:/Projects/PenalizeStop/PenalizeStopCpp.cpp")
source("D:/Projects/PenalizeStop/PenalizeStop.R")
#size 1000
load("N1000_p1_timevarying_smoothcubic_static_500times_coverageprob.RData")


AIC_VAR_IJI <- coverage_prob_AIC_1/nloop*100
AIC_VAR_I <- coverage_prob_AIC_3/nloop*100
AIC_VAR_III <- coverage_prob_AIC_4/nloop*100
# colnames(AIC_VAR_IJI) <- c("AIC_VAR_IJI_01", "AIC_VAR_IJI_05", "AIC_VAR_IJI_10", "AIC_VAR_IJI_15", "AIC_VAR_IJI_20","AIC_VAR_IJI_25")
# colnames(AIC_VAR_I) <- c("AIC_VAR_I_01", "AIC_VAR_I_05", "AIC_VAR_I_10", "AIC_VAR_I_15", "AIC_VAR_I_20","AIC_VAR_I_25")
# colnames(AIC_VAR_III) <- c("AIC_VAR_III_01", "AIC_VAR_III_05", "AIC_VAR_III_10", "AIC_VAR_III_15", "AIC_VAR_III_20", "AIC_VAR_III_25")

TIC_VAR_IJI <- coverage_prob_TIC_1/nloop*100
TIC_VAR_I <- coverage_prob_TIC_3/nloop*100
TIC_VAR_III <- coverage_prob_TIC_4/nloop*100
# colnames(TIC_VAR_IJI) <- c("TIC_VAR_IJI_01", "TIC_VAR_IJI_05", "TIC_VAR_IJI_10", "TIC_VAR_IJI_15", "TIC_VAR_IJI_20","TIC_VAR_IJI_25")
# colnames(TIC_VAR_I) <- c("TIC_VAR_I_01", "TIC_VAR_I_05", "TIC_VAR_I_10", "TIC_VAR_I_15", "TIC_VAR_I_20","TIC_VAR_I_25")
# colnames(TIC_VAR_III) <- c("TIC_VAR_III_01", "TIC_VAR_III_05", "TIC_VAR_III_10", "TIC_VAR_III_15", "TIC_VAR_III_20", "TIC_VAR_III_25")


GIC_VAR_IJI <- coverage_prob_GIC_1/nloop*100
GIC_VAR_I <- coverage_prob_GIC_3/nloop*100
GIC_VAR_III <- coverage_prob_GIC_4/nloop*100
# colnames(GIC_VAR_IJI) <- c("GIC_VAR_IJI_01", "GIC_VAR_IJI_05", "GIC_VAR_IJI_10", "GIC_VAR_IJI_15", "GIC_VAR_IJI_20","GIC_VAR_IJI_25")
# colnames(GIC_VAR_I) <- c("GIC_VAR_I_01", "GIC_VAR_I_05", "GIC_VAR_I_10", "GIC_VAR_I_15", "GIC_VAR_I_20","GIC_VAR_I_25")
# colnames(GIC_VAR_III) <- c("GIC_VAR_III_01", "GIC_VAR_III_05", "GIC_VAR_III_10", "GIC_VAR_III_15", "GIC_VAR_III_20", "GIC_VAR_III_25")

HIC_VAR_IJI <- coverage_prob_HIC_1/nloop*100
HIC_VAR_I <- coverage_prob_HIC_3/nloop*100
HIC_VAR_III <- coverage_prob_HIC_4/nloop*100

time<-c(0.1,0.5,1.0,1.5,2,2.5)

data_plot_AIC <- data.frame(AIC_VAR_IJI, AIC_VAR_I, AIC_VAR_III, time)
colnames(data_plot_AIC) <- c("IJI", "I", "III", "time")

data_plot_TIC <- data.frame(TIC_VAR_IJI, TIC_VAR_I, TIC_VAR_III, time)
colnames(data_plot_TIC) <- c("IJI", "I", "III", "time")

data_plot_GIC <- data.frame(GIC_VAR_IJI, GIC_VAR_I, GIC_VAR_III, time)
colnames(data_plot_GIC) <- c("IJI", "I", "III", "time")

data_plot_HIC <- data.frame(HIC_VAR_IJI, HIC_VAR_I, HIC_VAR_III, time)
colnames(data_plot_HIC) <- c("IJI", "I", "III", "time")


plot_cp_AIC <- ggplot(data=data_plot_AIC, aes(x = time)) + 
  geom_point(aes(y = IJI, color = "Sandwich"), size = 1) +
  geom_point(aes(y = I, color = "I_inv"), size = 1) +
  geom_point(aes(y = III, color = "Gray"), size = 1) +
  geom_line(aes(y = IJI, color = "Sandwich"), size = 0.5) +
  geom_line(aes(y = I, color = "I_inv"), size = 0.5) +
  geom_line(aes(y = III, color = "Gray"), size = 0.5) +
  scale_x_continuous(name= "time") + 
  scale_y_continuous(name= "coverage probability", limits = c(50,100)) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  #theme(text= element_text(size=14)) + theme(axis.text= element_text(size=14)) +
  theme(axis.title.y = element_text(margin= margin(t=0, r=10, b=0, l=0)))+
  ggtitle("AIC") +
  scale_color_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                     values = c('Sandwich'='red',
                                'I_inv' = 'dark green',
                                'Gray' = 'purple'),
                     labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  theme(legend.title = element_text(size=12), legend.text = element_text(size=11), legend.text.align = 0) +
  geom_hline(yintercept = 95, color = "black",linetype='dotted')
  

plot_cp_TIC <- ggplot(data=data_plot_TIC, aes(x = time)) + 
  geom_point(aes(y = IJI, color = "Sandwich"), size = 1) +
  geom_point(aes(y = I, color = "I_inv"), size = 1) +
  geom_point(aes(y = III, color = "Gray"), size = 1) +
  geom_line(aes(y = IJI, color = "Sandwich"), size = 0.5) +
  geom_line(aes(y = I, color = "I_inv"), size = 0.5) +
  geom_line(aes(y = III, color = "Gray"), size = 0.5) +
  scale_x_continuous(name= "time") + 
  ggtitle("TIC") +
  scale_y_continuous(name= "coverage probability", limits = c(50,100)) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  #theme(text= element_text(size=14)) + theme(axis.text= element_text(size=14)) +
  theme(axis.title.y = element_text(margin= margin(t=0, r=10, b=0, l=0)))+
  scale_color_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                     values = c('Sandwich'='red',
                                'I_inv' = 'dark green',
                                'Gray' = 'purple'),
                     labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  theme(legend.title = element_text(size=16), legend.text = element_text(size=14), legend.text.align = 0) +
  geom_hline(yintercept = 95, color = "black",linetype='dotted')
plot_cp_GIC <- ggplot(data=data_plot_GIC, aes(x = time)) + 
  geom_point(aes(y = IJI, color = "Sandwich"), size = 1) +
  geom_point(aes(y = I, color = "I_inv"), size = 1) +
  geom_point(aes(y = III, color = "Gray"), size = 1) +
  geom_line(aes(y = IJI, color = "Sandwich"), size = 0.5) +
  geom_line(aes(y = I, color = "I_inv"), size = 0.5) +
  geom_line(aes(y = III, color = "Gray"), size = 0.5) +
  scale_x_continuous(name= "time") + 
  scale_y_continuous(name= "coverage probability", limits = c(50,100)) +
  theme_bw() + 
  ggtitle("GIC") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  #theme(text= element_text(size=14)) + theme(axis.text= element_text(size=14)) +
  theme(axis.title.y = element_text(margin= margin(t=0, r=10, b=0, l=0)))+
  scale_color_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                     values = c('Sandwich'='red',
                                'I_inv' = 'dark green',
                                'Gray' = 'purple'),
                     labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  theme(legend.title = element_text(size=16), legend.text = element_text(size=14), legend.text.align = 0) +
  geom_hline(yintercept = 95, color = "black",linetype='dotted')




data_plot_AIC_N1000_timevarying <- data_plot_AIC
data_plot_TIC_N1000_timevarying <- data_plot_TIC
data_plot_GIC_N1000_timevarying <- data_plot_GIC
data_plot_HIC_N1000_timevarying <- data_plot_HIC

save(data_plot_AIC_N1000_timevarying, data_plot_TIC_N1000_timevarying, data_plot_GIC_N1000_timevarying,data_plot_HIC_N1000_timevarying, file = "cpPlots_n1000tv_includeHastie.RData")


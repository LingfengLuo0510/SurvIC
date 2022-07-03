
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
setwd("/Users/luolingfeng/Dropbox (University of Michigan)/Lingfeng Research/PenalizeStop/PaperPlots/")
#size 1000
load("cpPlots_n1000constant_includeHastie.RData")
load("cpPlots_n1000tv_includeHastie.RData")
load("cpPlots_n5000constant_includeHastie.RData")
load("cpPlots_n5000tv_includeHastie.RData")


plot_cp_AIC_N1000_constant <- ggplot(data=data_plot_AIC_N1000_constant, aes(x = time)) + 
  geom_point(aes(y = IJI, color = "Sandwich", shape = "Sandwich"), size = 1) +
  geom_point(aes(y = I, color = "I_inv", shape = "I_inv"), size = 1) +
  geom_point(aes(y = III, color = "Gray", shape = "Gray"), size = 1) +
  geom_line(aes(y = IJI, color = "Sandwich", linetype = "Sandwich"), size = 0.5) +
  geom_line(aes(y = I, color = "I_inv", linetype = "I_inv"), size = 0.5) +
  geom_line(aes(y = III, color = "Gray", linetype = "Gray"), size = 0.5) +
  scale_x_continuous(name= "time") + 
  scale_y_continuous(name= "coverage probability", limits = c(50,100)) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.title.y = element_text(margin= margin(t=0, r=10, b=0, l=0)))+
  ggtitle("AIC") +
  scale_color_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                     values = c('Sandwich'='red',
                                'I_inv' = 'dark green',
                                'Gray' = 'purple'),
                     labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  scale_linetype_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                     values = c('Sandwich'= 1,
                                'I_inv' = 5,
                                'Gray' = 3),
                     labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  scale_shape_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                        values = c('Sandwich'= 16,
                                   'I_inv' = 3,
                                   'Gray' = 17),
                        labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  theme(legend.title = element_text(size=16), legend.text = element_text(size=14), legend.text.align = 0) +
  geom_hline(yintercept = 95, color = "black",linetype='dotted')
plot_cp_AIC_N1000_constant

plot_cp_TIC_N1000_constant <- ggplot(data=data_plot_TIC_N1000_constant, aes(x = time)) + 
  geom_point(aes(y = IJI, color = "Sandwich", shape = "Sandwich"), size = 1) +
  geom_point(aes(y = I, color = "I_inv", shape = "I_inv"), size = 1) +
  geom_point(aes(y = III, color = "Gray", shape = "Gray"), size = 1) +
  geom_line(aes(y = IJI, color = "Sandwich", linetype = "Sandwich"), size = 0.5) +
  geom_line(aes(y = I, color = "I_inv", linetype = "I_inv"), size = 0.5) +
  geom_line(aes(y = III, color = "Gray", linetype = "Gray"), size = 0.5) +
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
  scale_linetype_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                        values = c('Sandwich'= 1,
                                   'I_inv' = 5,
                                   'Gray' = 3),
                        labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  scale_shape_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                     values = c('Sandwich'= 16,
                                'I_inv' = 3,
                                'Gray' = 17),
                     labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  theme(legend.title = element_text(size=16), legend.text = element_text(size=14), legend.text.align = 0) +
  geom_hline(yintercept = 95, color = "black",linetype='dotted')

plot_cp_GIC_N1000_constant <- ggplot(data=data_plot_GIC_N1000_constant, aes(x = time)) + 
  geom_point(aes(y = IJI, color = "Sandwich", shape = "Sandwich"), size = 1) +
  geom_point(aes(y = I, color = "I_inv", shape = "I_inv"), size = 1) +
  geom_point(aes(y = III, color = "Gray", shape = "Gray"), size = 1) +
  geom_line(aes(y = IJI, color = "Sandwich", linetype = "Sandwich"), size = 0.5) +
  geom_line(aes(y = I, color = "I_inv", linetype = "I_inv"), size = 0.5) +
  geom_line(aes(y = III, color = "Gray", linetype = "Gray"), size = 0.5) +
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
  scale_linetype_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                        values = c('Sandwich'= 1,
                                   'I_inv' = 5,
                                   'Gray' = 3),
                        labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  scale_shape_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                     values = c('Sandwich'= 16,
                                'I_inv' = 3,
                                'Gray' = 17),
                     labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  theme(legend.title = element_text(size=16), legend.text = element_text(size=14), legend.text.align = 0) +
  geom_hline(yintercept = 95, color = "black",linetype='dotted')

plot_cp_HIC_N1000_constant <- ggplot(data=data_plot_HIC_N1000_constant, aes(x = time)) + 
  geom_point(aes(y = IJI, color = "Sandwich", shape = "Sandwich"), size = 1) +
  geom_point(aes(y = I, color = "I_inv", shape = "I_inv"), size = 1) +
  geom_point(aes(y = III, color = "Gray", shape = "Gray"), size = 1) +
  geom_line(aes(y = IJI, color = "Sandwich", linetype = "Sandwich"), size = 0.5) +
  geom_line(aes(y = I, color = "I_inv", linetype = "I_inv"), size = 0.5) +
  geom_line(aes(y = III, color = "Gray", linetype = "Gray"), size = 0.5) +
  scale_x_continuous(name= "time") + 
  scale_y_continuous(name= "coverage probability", limits = c(50,100)) +
  theme_bw() + 
  ggtitle("Hastie") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  #theme(text= element_text(size=14)) + theme(axis.text= element_text(size=14)) +
  theme(axis.title.y = element_text(margin= margin(t=0, r=10, b=0, l=0)))+
  scale_color_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                     values = c('Sandwich'='red',
                                'I_inv' = 'dark green',
                                'Gray' = 'purple'),
                     labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  scale_linetype_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                        values = c('Sandwich'= 1,
                                   'I_inv' = 5,
                                   'Gray' = 3),
                        labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  scale_shape_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                     values = c('Sandwich'= 16,
                                'I_inv' = 3,
                                'Gray' = 17),
                     labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  theme(legend.title = element_text(size=16), legend.text = element_text(size=14), legend.text.align = 0) +
  geom_hline(yintercept = 95, color = "black",linetype='dotted')

#tv
plot_cp_AIC_N1000_timevarying <- ggplot(data=data_plot_AIC_N1000_timevarying, aes(x = time)) + 
  geom_point(aes(y = IJI, color = "Sandwich", shape = "Sandwich"), size = 1) +
  geom_point(aes(y = I, color = "I_inv", shape = "I_inv"), size = 1) +
  geom_point(aes(y = III, color = "Gray", shape = "Gray"), size = 1) +
  geom_line(aes(y = IJI, color = "Sandwich", linetype = "Sandwich"), size = 0.5) +
  geom_line(aes(y = I, color = "I_inv", linetype = "I_inv"), size = 0.5) +
  geom_line(aes(y = III, color = "Gray", linetype = "Gray"), size = 0.5) +
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
  scale_linetype_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                        values = c('Sandwich'= 1,
                                   'I_inv' = 5,
                                   'Gray' = 3),
                        labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  scale_shape_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                     values = c('Sandwich'= 16,
                                'I_inv' = 3,
                                'Gray' = 17),
                     labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  theme(legend.title = element_text(size=16), legend.text = element_text(size=14), legend.text.align = 0) +
  geom_hline(yintercept = 95, color = "black",linetype='dotted')

plot_cp_TIC_N1000_timevarying <- ggplot(data=data_plot_TIC_N1000_timevarying, aes(x = time)) + 
  geom_point(aes(y = IJI, color = "Sandwich", shape = "Sandwich"), size = 1) +
  geom_point(aes(y = I, color = "I_inv", shape = "I_inv"), size = 1) +
  geom_point(aes(y = III, color = "Gray", shape = "Gray"), size = 1) +
  geom_line(aes(y = IJI, color = "Sandwich", linetype = "Sandwich"), size = 0.5) +
  geom_line(aes(y = I, color = "I_inv", linetype = "I_inv"), size = 0.5) +
  geom_line(aes(y = III, color = "Gray", linetype = "Gray"), size = 0.5) +
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
  scale_linetype_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                        values = c('Sandwich'= 1,
                                   'I_inv' = 5,
                                   'Gray' = 3),
                        labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  scale_shape_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                     values = c('Sandwich'= 16,
                                'I_inv' = 3,
                                'Gray' = 17),
                     labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  theme(legend.title = element_text(size=16), legend.text = element_text(size=14), legend.text.align = 0) +
  geom_hline(yintercept = 95, color = "black",linetype='dotted')

plot_cp_GIC_N1000_timevarying <- ggplot(data=data_plot_GIC_N1000_timevarying, aes(x = time)) + 
  geom_point(aes(y = IJI, color = "Sandwich", shape = "Sandwich"), size = 1) +
  geom_point(aes(y = I, color = "I_inv", shape = "I_inv"), size = 1) +
  geom_point(aes(y = III, color = "Gray", shape = "Gray"), size = 1) +
  geom_line(aes(y = IJI, color = "Sandwich", linetype = "Sandwich"), size = 0.5) +
  geom_line(aes(y = I, color = "I_inv", linetype = "I_inv"), size = 0.5) +
  geom_line(aes(y = III, color = "Gray", linetype = "Gray"), size = 0.5) +
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
  scale_linetype_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                        values = c('Sandwich'= 1,
                                   'I_inv' = 5,
                                   'Gray' = 3),
                        labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  scale_shape_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                     values = c('Sandwich'= 16,
                                'I_inv' = 3,
                                'Gray' = 17),
                     labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  theme(legend.title = element_text(size=16), legend.text = element_text(size=14), legend.text.align = 0) +
  geom_hline(yintercept = 95, color = "black",linetype='dotted')

plot_cp_HIC_N1000_timevarying <- ggplot(data=data_plot_HIC_N1000_timevarying, aes(x = time)) + 
  geom_point(aes(y = IJI, color = "Sandwich", shape = "Sandwich"), size = 1) +
  geom_point(aes(y = I, color = "I_inv", shape = "I_inv"), size = 1) +
  geom_point(aes(y = III, color = "Gray", shape = "Gray"), size = 1) +
  geom_line(aes(y = IJI, color = "Sandwich", linetype = "Sandwich"), size = 0.5) +
  geom_line(aes(y = I, color = "I_inv", linetype = "I_inv"), size = 0.5) +
  geom_line(aes(y = III, color = "Gray", linetype = "Gray"), size = 0.5) +
  scale_x_continuous(name= "time") + 
  scale_y_continuous(name= "coverage probability", limits = c(50,100)) +
  theme_bw() + 
  ggtitle("Hastie") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  #theme(text= element_text(size=14)) + theme(axis.text= element_text(size=14)) +
  theme(axis.title.y = element_text(margin= margin(t=0, r=10, b=0, l=0)))+
  scale_color_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                     values = c('Sandwich'='red',
                                'I_inv' = 'dark green',
                                'Gray' = 'purple'),
                     labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  scale_linetype_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                        values = c('Sandwich'= 1,
                                   'I_inv' = 5,
                                   'Gray' = 3),
                        labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  scale_shape_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                     values = c('Sandwich'= 16,
                                'I_inv' = 3,
                                'Gray' = 17),
                     labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  theme(legend.title = element_text(size=16), legend.text = element_text(size=14), legend.text.align = 0) +
  geom_hline(yintercept = 95, color = "black",linetype='dotted')

##############n5000:
plot_cp_AIC_N5000_constant <- ggplot(data=data_plot_AIC_N5000_constant, aes(x = time)) + 
  geom_point(aes(y = IJI, color = "Sandwich", shape = "Sandwich"), size = 1) +
  geom_point(aes(y = I, color = "I_inv", shape = "I_inv"), size = 1) +
  geom_point(aes(y = III, color = "Gray", shape = "Gray"), size = 1) +
  geom_line(aes(y = IJI, color = "Sandwich", linetype = "Sandwich"), size = 0.5) +
  geom_line(aes(y = I, color = "I_inv", linetype = "I_inv"), size = 0.5) +
  geom_line(aes(y = III, color = "Gray", linetype = "Gray"), size = 0.5) +
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
  scale_linetype_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                        values = c('Sandwich'= 1,
                                   'I_inv' = 5,
                                   'Gray' = 3),
                        labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  scale_shape_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                     values = c('Sandwich'= 16,
                                'I_inv' = 3,
                                'Gray' = 17),
                     labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  theme(legend.title = element_text(size=16), legend.text = element_text(size=14), legend.text.align = 0) +
  geom_hline(yintercept = 95, color = "black",linetype='dotted')

plot_cp_TIC_N5000_constant <- ggplot(data=data_plot_TIC_N5000_constant, aes(x = time)) + 
  geom_point(aes(y = IJI, color = "Sandwich", shape = "Sandwich"), size = 1) +
  geom_point(aes(y = I, color = "I_inv", shape = "I_inv"), size = 1) +
  geom_point(aes(y = III, color = "Gray", shape = "Gray"), size = 1) +
  geom_line(aes(y = IJI, color = "Sandwich", linetype = "Sandwich"), size = 0.5) +
  geom_line(aes(y = I, color = "I_inv", linetype = "I_inv"), size = 0.5) +
  geom_line(aes(y = III, color = "Gray", linetype = "Gray"), size = 0.5) +
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
  scale_linetype_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                        values = c('Sandwich'= 1,
                                   'I_inv' = 5,
                                   'Gray' = 3),
                        labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  scale_shape_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                     values = c('Sandwich'= 16,
                                'I_inv' = 3,
                                'Gray' = 17),
                     labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  theme(legend.title = element_text(size=16), legend.text = element_text(size=14), legend.text.align = 0) +
  geom_hline(yintercept = 95, color = "black",linetype='dotted')

plot_cp_GIC_N5000_constant <- ggplot(data=data_plot_GIC_N5000_constant, aes(x = time)) + 
  geom_point(aes(y = IJI, color = "Sandwich", shape = "Sandwich"), size = 1) +
  geom_point(aes(y = I, color = "I_inv", shape = "I_inv"), size = 1) +
  geom_point(aes(y = III, color = "Gray", shape = "Gray"), size = 1) +
  geom_line(aes(y = IJI, color = "Sandwich", linetype = "Sandwich"), size = 0.5) +
  geom_line(aes(y = I, color = "I_inv", linetype = "I_inv"), size = 0.5) +
  geom_line(aes(y = III, color = "Gray", linetype = "Gray"), size = 0.5) +
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
  scale_linetype_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                        values = c('Sandwich'= 1,
                                   'I_inv' = 5,
                                   'Gray' = 3),
                        labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  scale_shape_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                     values = c('Sandwich'= 16,
                                'I_inv' = 3,
                                'Gray' = 17),
                     labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  theme(legend.title = element_text(size=16), legend.text = element_text(size=14), legend.text.align = 0) +
  geom_hline(yintercept = 95, color = "black",linetype='dotted')

plot_cp_HIC_N5000_constant <- ggplot(data=data_plot_HIC_N5000_constant, aes(x = time)) + 
  geom_point(aes(y = IJI, color = "Sandwich", shape = "Sandwich"), size = 1) +
  geom_point(aes(y = I, color = "I_inv", shape = "I_inv"), size = 1) +
  geom_point(aes(y = III, color = "Gray", shape = "Gray"), size = 1) +
  geom_line(aes(y = IJI, color = "Sandwich", linetype = "Sandwich"), size = 0.5) +
  geom_line(aes(y = I, color = "I_inv", linetype = "I_inv"), size = 0.5) +
  geom_line(aes(y = III, color = "Gray", linetype = "Gray"), size = 0.5) +
  scale_x_continuous(name= "time") + 
  scale_y_continuous(name= "coverage probability", limits = c(50,100)) +
  theme_bw() + 
  ggtitle("Hastie") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  #theme(text= element_text(size=14)) + theme(axis.text= element_text(size=14)) +
  theme(axis.title.y = element_text(margin= margin(t=0, r=10, b=0, l=0)))+
  scale_color_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                     values = c('Sandwich'='red',
                                'I_inv' = 'dark green',
                                'Gray' = 'purple'),
                     labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  scale_linetype_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                        values = c('Sandwich'= 1,
                                   'I_inv' = 5,
                                   'Gray' = 3),
                        labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  scale_shape_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                     values = c('Sandwich'= 16,
                                'I_inv' = 3,
                                'Gray' = 17),
                     labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  theme(legend.title = element_text(size=16), legend.text = element_text(size=14), legend.text.align = 0) +
  geom_hline(yintercept = 95, color = "black",linetype='dotted')

#tv
plot_cp_AIC_N5000_timevarying <- ggplot(data=data_plot_AIC_N5000_timevarying, aes(x = time)) + 
  geom_point(aes(y = IJI, color = "Sandwich", shape = "Sandwich"), size = 1) +
  geom_point(aes(y = I, color = "I_inv", shape = "I_inv"), size = 1) +
  geom_point(aes(y = III, color = "Gray", shape = "Gray"), size = 1) +
  geom_line(aes(y = IJI, color = "Sandwich", linetype = "Sandwich"), size = 0.5) +
  geom_line(aes(y = I, color = "I_inv", linetype = "I_inv"), size = 0.5) +
  geom_line(aes(y = III, color = "Gray", linetype = "Gray"), size = 0.5) +
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
  scale_linetype_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                        values = c('Sandwich'= 1,
                                   'I_inv' = 5,
                                   'Gray' = 3),
                        labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  scale_shape_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                     values = c('Sandwich'= 16,
                                'I_inv' = 3,
                                'Gray' = 17),
                     labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  theme(legend.title = element_text(size=16), legend.text = element_text(size=14), legend.text.align = 0) +
  geom_hline(yintercept = 95, color = "black",linetype='dotted') 

plot_cp_TIC_N5000_timevarying <- ggplot(data=data_plot_TIC_N5000_timevarying, aes(x = time)) + 
  geom_point(aes(y = IJI, color = "Sandwich", shape = "Sandwich"), size = 1) +
  geom_point(aes(y = I, color = "I_inv", shape = "I_inv"), size = 1) +
  geom_point(aes(y = III, color = "Gray", shape = "Gray"), size = 1) +
  geom_line(aes(y = IJI, color = "Sandwich", linetype = "Sandwich"), size = 0.5) +
  geom_line(aes(y = I, color = "I_inv", linetype = "I_inv"), size = 0.5) +
  geom_line(aes(y = III, color = "Gray", linetype = "Gray"), size = 0.5) +
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
  scale_linetype_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                        values = c('Sandwich'= 1,
                                   'I_inv' = 5,
                                   'Gray' = 3),
                        labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  scale_shape_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                     values = c('Sandwich'= 16,
                                'I_inv' = 3,
                                'Gray' = 17),
                     labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  theme(legend.title = element_text(size=16), legend.text = element_text(size=14), legend.text.align = 0) +
  geom_hline(yintercept = 95, color = "black",linetype='dotted')

plot_cp_GIC_N5000_timevarying <- ggplot(data=data_plot_GIC_N5000_timevarying, aes(x = time)) + 
  geom_point(aes(y = IJI, color = "Sandwich", shape = "Sandwich"), size = 1) +
  geom_point(aes(y = I, color = "I_inv", shape = "I_inv"), size = 1) +
  geom_point(aes(y = III, color = "Gray", shape = "Gray"), size = 1) +
  geom_line(aes(y = IJI, color = "Sandwich", linetype = "Sandwich"), size = 0.5) +
  geom_line(aes(y = I, color = "I_inv", linetype = "I_inv"), size = 0.5) +
  geom_line(aes(y = III, color = "Gray", linetype = "Gray"), size = 0.5) +
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
  scale_linetype_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                        values = c('Sandwich'= 1,
                                   'I_inv' = 5,
                                   'Gray' = 3),
                        labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  scale_shape_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                     values = c('Sandwich'= 16,
                                'I_inv' = 3,
                                'Gray' = 17),
                     labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  theme(legend.title = element_text(size=16), legend.text = element_text(size=14), legend.text.align = 0) +
  geom_hline(yintercept = 95, color = "black",linetype='dotted')

plot_cp_HIC_N5000_timevarying <- ggplot(data=data_plot_HIC_N5000_timevarying, aes(x = time)) + 
  geom_point(aes(y = IJI, color = "Sandwich", shape = "Sandwich"), size = 1) +
  geom_point(aes(y = I, color = "I_inv", shape = "I_inv"), size = 1) +
  geom_point(aes(y = III, color = "Gray", shape = "Gray"), size = 1) +
  geom_line(aes(y = IJI, color = "Sandwich", linetype = "Sandwich"), size = 0.5) +
  geom_line(aes(y = I, color = "I_inv", linetype = "I_inv"), size = 0.5) +
  geom_line(aes(y = III, color = "Gray", linetype = "Gray"), size = 0.5) +
  scale_x_continuous(name= "time") + 
  scale_y_continuous(name= "coverage probability", limits = c(50,100)) +
  theme_bw() + 
  ggtitle("Hastie") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  #theme(text= element_text(size=14)) + theme(axis.text= element_text(size=14)) +
  theme(axis.title.y = element_text(margin= margin(t=0, r=10, b=0, l=0)))+
  scale_color_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                     values = c('Sandwich'='red',
                                'I_inv' = 'dark green',
                                'Gray' = 'purple'),
                     labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  scale_linetype_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                        values = c('Sandwich'= 1,
                                   'I_inv' = 5,
                                   'Gray' = 3),
                        labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  scale_shape_manual(name='VarianceMatrix', breaks = c('Sandwich', 'I_inv', 'Gray'),
                     values = c('Sandwich'= 16,
                                'I_inv' = 3,
                                'Gray' = 17),
                     labels = expression(I[lambda]^"-1"*J[lambda]*I[lambda]^"-1", I[lambda]^"-1", I[lambda]^"-1"*I*I[lambda]^"-1")) +
  theme(legend.title = element_text(size=16), legend.text = element_text(size=14), legend.text.align = 0) +
  geom_hline(yintercept = 95, color = "black",linetype='dotted')

# multi.page <- ggarrange(plot_cp_AIC_N1000_constant + rremove("ylab") + rremove("xlab"), plot_cp_TIC_N1000_constant + rremove("ylab") + rremove("xlab"), plot_cp_GIC_N1000_constant + rremove("ylab") + rremove("xlab"), plot_cp_HIC_N1000_constant + rremove("ylab") + rremove("xlab"),
#                         plot_cp_AIC_N1000_timevarying + rremove("ylab") + rremove("xlab"), plot_cp_TIC_N1000_timevarying + rremove("ylab") + rremove("xlab"), plot_cp_GIC_N1000_timevarying + rremove("ylab") + rremove("xlab"),plot_cp_HIC_N1000_timevarying + rremove("ylab") + rremove("xlab"),
#                         plot_cp_AIC_N5000_constant + rremove("ylab") + rremove("xlab"), plot_cp_TIC_N5000_constant + rremove("ylab") + rremove("xlab"), plot_cp_GIC_N5000_constant + rremove("ylab") + rremove("xlab"), plot_cp_HIC_N5000_constant + rremove("ylab") + rremove("xlab"),
#                         plot_cp_AIC_N5000_timevarying + rremove("ylab") + rremove("xlab"), plot_cp_TIC_N5000_timevarying + rremove("ylab") + rremove("xlab"), plot_cp_GIC_N5000_timevarying + rremove("ylab") + rremove("xlab"), plot_cp_HIC_N5000_timevarying + rremove("ylab") + rremove("xlab"),
#                         nrow = 4, ncol = 4, 
#                         # labels = c("N1000 Constant", "","","N1000 Time Varying", "","","N5000 Constant", "","","N1000 Time Varying", "",""),
#                         common.legend = TRUE, legend = "right")
# # multi.page <- ggarrange(ggarrange(plot_cp_AIC_N1000_constant + rremove("ylab") + rremove("xlab"), plot_cp_TIC_N1000_constant + rremove("ylab") + rremove("xlab"), plot_cp_GIC_N1000_constant + rremove("ylab") + rremove("xlab"), ncol = 1),
# #                         ggarrange(plot_cp_AIC_N1000_timevarying + rremove("ylab") + rremove("xlab"), plot_cp_TIC_N1000_timevarying + rremove("ylab") + rremove("xlab"), plot_cp_GIC_N1000_timevarying + rremove("ylab") + rremove("xlab"), ncol = 1),
# #                         ggarrange(plot_cp_AIC_N5000_constant + rremove("ylab") + rremove("xlab"), plot_cp_TIC_N5000_constant + rremove("ylab") + rremove("xlab"), plot_cp_GIC_N5000_constant + rremove("ylab") + rremove("xlab"), ncol = 1),
# #                         ggarrange(plot_cp_AIC_N5000_timevarying + rremove("ylab") + rremove("xlab"), plot_cp_TIC_N5000_timevarying + rremove("ylab") + rremove("xlab"), plot_cp_GIC_N5000_timevarying + rremove("ylab") + rremove("xlab"), ncol = 1),
# #                         common.legend = TRUE, legend = "right")
# 
# multi.page <- annotate_figure(multi.page,
#                               bottom = textGrob("time", rot = 0, vjust = 0, hjust = 2, gp = gpar(cex = 1.1)),
#                               left = textGrob("Coverage Probability (%)", rot = 90, vjust = 1, gp = gpar(cex = 1.1)))
# multi.page
# 
# png(filename=("paper_cp.png"), height=9, width=9, res=600, units="in")
# multi.page
# dev.off()


###1
CPPlotN1000constant <- ggarrange(plot_cp_AIC_N1000_constant + rremove("ylab") + rremove("xlab"), plot_cp_TIC_N1000_constant + rremove("ylab") + rremove("xlab"), plot_cp_GIC_N1000_constant + rremove("ylab") + rremove("xlab"), plot_cp_HIC_N1000_constant + rremove("ylab") + rremove("xlab"),
                        nrow = 1, ncol = 4,
                        # labels = c("N1000 Constant", "","","N1000 Time Varying", "","","N5000 Constant", "","","N1000 Time Varying", "",""),
                        common.legend = TRUE, legend = "right")
CPPlotN1000constant <- annotate_figure(CPPlotN1000constant,
                              bottom = textGrob("time", rot = 0, vjust = 0, hjust = 2, gp = gpar(cex = 1.1)),
                              left = textGrob("Coverage Probability (%)", rot = 90, vjust = 1, gp = gpar(cex = 1.1)))
CPPlotN1000constant

png(filename=("CPPlotN1000constant.png"), height=2.5, width=16.5, res=600, units="in")
CPPlotN1000constant
dev.off()

###2
CPPlotN1000_timevarying <- ggarrange(plot_cp_AIC_N1000_timevarying + rremove("ylab") + rremove("xlab"), plot_cp_TIC_N1000_timevarying + rremove("ylab") + rremove("xlab"), plot_cp_GIC_N1000_timevarying + rremove("ylab") + rremove("xlab"),plot_cp_HIC_N1000_timevarying + rremove("ylab") + rremove("xlab"),
                                 nrow = 1, ncol = 4,
                                 # labels = c("N1000 Constant", "","","N1000 Time Varying", "","","N5000 Constant", "","","N1000 Time Varying", "",""),
                                 common.legend = TRUE, legend = "right")
CPPlotN1000_timevarying <- annotate_figure(CPPlotN1000_timevarying,
                                       bottom = textGrob("time", rot = 0, vjust = 0, hjust = 2, gp = gpar(cex = 1.1)),
                                       left = textGrob("Coverage Probability (%)", rot = 90, vjust = 1, gp = gpar(cex = 1.1)))
CPPlotN1000_timevarying

png(filename=("CPPlotN1000_timevarying.png"), height=2.5, width=16.5, res=600, units="in")
CPPlotN1000_timevarying
dev.off()


###3
CPPlotN5000constant <- ggarrange(plot_cp_AIC_N5000_constant + rremove("ylab") + rremove("xlab"), plot_cp_TIC_N5000_constant + rremove("ylab") + rremove("xlab"), plot_cp_GIC_N5000_constant + rremove("ylab") + rremove("xlab"), plot_cp_HIC_N5000_constant + rremove("ylab") + rremove("xlab"),
                                 nrow = 1, ncol = 4,
                                 # labels = c("N1000 Constant", "","","N1000 Time Varying", "","","N5000 Constant", "","","N1000 Time Varying", "",""),
                                 common.legend = TRUE, legend = "right")
CPPlotN5000constant <- annotate_figure(CPPlotN5000constant,
                                       bottom = textGrob("time", rot = 0, vjust = 0, hjust = 2, gp = gpar(cex = 1.1)),
                                       left = textGrob("Coverage Probability (%)", rot = 90, vjust = 1, gp = gpar(cex = 1.1)))
CPPlotN5000constant

png(filename=("CPPlotN5000constant.png"), height=2.5, width=16.5, res=600, units="in")
CPPlotN5000constant
dev.off()


###4
CPPlotN5000_timevarying <- ggarrange(plot_cp_AIC_N5000_timevarying + rremove("ylab") + rremove("xlab"), plot_cp_TIC_N5000_timevarying + rremove("ylab") + rremove("xlab"), plot_cp_GIC_N5000_timevarying + rremove("ylab") + rremove("xlab"), plot_cp_HIC_N5000_timevarying + rremove("ylab") + rremove("xlab"),
                                 nrow = 1, ncol = 4,
                                 # labels = c("N1000 Constant", "","","N1000 Time Varying", "","","N5000 Constant", "","","N1000 Time Varying", "",""),
                                 common.legend = TRUE, legend = "right")
CPPlotN5000_timevarying <- annotate_figure(CPPlotN5000_timevarying,
                                       bottom = textGrob("time", rot = 0, vjust = 0, hjust = 2, gp = gpar(cex = 1.1)),
                                       left = textGrob("Coverage Probability (%)", rot = 90, vjust = 1, gp = gpar(cex = 1.1)))
CPPlotN5000_timevarying

png(filename=("CPPlotN5000_timevarying.png"), height=2.5, width=16.5, res=600, units="in")
CPPlotN5000_timevarying
dev.off()

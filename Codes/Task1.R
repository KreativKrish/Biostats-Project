library(ggplot2 )
library(readxl )
library(ggpubr )
library(dplyr )
library(tidyr)
library(MASS)
library(scales)
library(ggeasy)
library(epitools)
library(BSDA)

# AS both groups have ratio scaled variables -> Parametric tests 
dataset <- read_excel("D:/sem-4/BE303/dataset.xlsx")
df <- data.frame(dataset)
# Removing characters / useless data
df$...1 <- NULL

# Mean of both groups 

mean_diet <- mean(df$Diet,na.rm = T)
mean_control <- mean(df$Control,na.rm = T)

#Standard deviations of both groups 
diet_std_dev <- sd(df$Diet,na.rm = T)
control_std_dev <- sd(df$Control,na.rm = T)

#difference b/w means and std dev
diff_mean <- abs(mean_diet - mean_control)
print(diff_mean)

diff_std <- abs(diet_std_dev - control_std_dev)
print(diff_std) 

#plotting histograms for distribution
p1 <- ggplot(df,aes(x = Diet))+
  geom_histogram(binwidth = 0.2)+
  geom_vline(xintercept = mean_diet,color="red",linetype="dashed")+
  theme_bw()+
  xlab("Iron Conc. in mcg/L")+
  ylab("Frequency")+
  labs(title="Distribution of Diet group",fill=" ")+
  easy_center_title()

p2 <- ggplot(df,aes(x = Control))+
  geom_histogram(binwidth = 0.2,na.rm = T)+
  geom_vline(xintercept = mean_control,color="red",linetype="dashed")+
  theme_bw()+
  xlab("Iron Conc. in mcg/L")+
  ylab("Frequency")+
  labs(title="Distribution of Control group",fill=" ")+
  easy_center_title()
p3 <- ggarrange(p1,p2,ncol=1,nrow=2)
plot(p3)
# distribution seems to be normally distributed 

#Assumption for Normal distribution is checked using  QQ-plot / Shapiro-Wilk-test

#QQ Plots
qqnorm(df$Diet)
qqline(df$Diet)
qqnorm(df$Control)
qqline(df$Control)
#=================Test for Normality =====================
#Shapiro-Wilk Test.
# Diet group
res1 <- shapiro.test(df$Diet)
print(res1)
#Control Group
res2 <- shapiro.test(df$Control)
print(res2)

    # results
    # Shapiro-Wilk normality test
    # 
    # data:  df$Diet
    # W = 0.97246, p-value = 0.5145
    # 
    # 
    # Shapiro-Wilk normality test
    # 
    # data:  df$Control
    # W = 0.97546, p-value = 0.6435

# As the p-value is greater than 0.05 data can be assumed to be normally distributed 

#Inference <- Data can be assumed to be normally distributed 
#as seen from qqplots(lies roughly along the line) and histogram(looks bell-shaped)

#============================Test for equal variances===========================
#1. Rule of thumb:- 
ratio_of_var <- control_std_dev**2/diet_std_dev**2
if (ratio_of_var<4){
  print("Variances are approx. similar")
}else {
  print("Variances are not similar")
}

#2. F-score test
F <- control_std_dev**2/diet_std_dev**2
#corresponding p-value =~ 0.000 . it is less than 0.05 and thus we reject the hypothesis that variances are equal 
#=============================Welch's test=====================
# as there is significant differences b/w variances and number
#of observables is different Welch's test is applied
results <- t.test(dataset$Diet,dataset$Control,alternate ="greater")
print(results$p.value)
#as the pvalue is less than alpha threshold of 0.05 <- null hypothesis is rejected 
#With alpha threshold of 5% It can be said that there is a statistically significant difference between the two groups 

#Calculating effect size 
n_diet<- 35
n_control <- 33

sp <- sqrt((n_diet-1)*diet_std_dev**2+(n_control-1)*control_std_dev**2)/sqrt(n_diet+n_control-2)
cohen_d <- diff_mean/sp
print(cohen_d)#0.6339032
#Effect size is medium 

#if we have to apply two sampled unpaired t-test we must assume that variances of two groups are equal



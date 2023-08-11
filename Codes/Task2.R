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


# Two way anova as independent or explanatory variables are categorical 
#and dependent variable is continuous
#check for dependence between the variables 

# Reading/loading the data
data2 <- read_excel("D:/sem-4/BE303/dataset.xlsx", 
                    sheet = "Task 2")
df2 <- data.frame(data2)
df2$...1 <- NULL #Removing redundant info

corr_matrix <- cor(df2) #Correlation matrix
print(corr_matrix) 
#Not much correlation between the groups so anova can be applied

Drug <- c(rep("A",15),rep("B",15),rep("C",15),
          rep("A",15),rep("B",15),rep("C",15),
          rep("A",15),rep("B",15),rep("C",15))
Doses <- rep(c("1","2","3"),each= 45)
values <- c(df2$S01,df2$S02,df2$S03,df2$S04,df2$S05,df2$S06,df2$S07,df2$S08,df2$S09)
my_data <- data.frame(Drug,Doses,values)



#plots for distributions 
p1 <- ggplot(my_data,aes(x=Doses,y= values,color= Drug)) +
  geom_boxplot()+
  labs(x="Number of doses")+
  theme_bw()+
  ggtitle("Viral load Distribution of different groups")+
  theme ( axis.text = element_text(size =12),
          axis.title =element_text(size=12),
          plot.title=element_text(hjust = 0.5))
plot(p1)

#Anova
model <- aov(values ~ Drug * Doses, data = my_data)
print(summary(model))

#F_Drug= 0.136< 3.07 Drugs are not having any significant differnce on viral load
#F_Doses = 2.594  < 3.07 Doses are not having any significant differnce on viral load
#F_DOses:Drugs = 1.121 < 2.44 => No relation between Drugs and doses
# There is no significant relation between any variables 



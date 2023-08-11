#In this case we want to analyze if there is an effect in the number of T cells 
#with Vit.C levels 
#so, we will do correlation analysis. Note- These are continuous variables

#H0: No dependence on amount of T_cells with Vitamin-C levels 
#both variables are ratio-scaled
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

dataset <- read_excel("D:/sem-4/BE303/dataset.xlsx", 
                      sheet = "Task 3")

df3 <- data.frame(dataset)
df3$...1 <- NULL
#Checking correlation between the two groups 
corr <- cor(df3$C_level,df3$T_cell) # -0.7140102

#Computing mean and variance
mean_T_cell <- mean(df3$T_cell)
mean_C_level <- mean(df3$C_level)

var_T <- var(df3$T_cell)
var_C <- var(df3$C_level)

ratio <- var_C/var_T

#Distribution
p1 <- ggplot(df3,aes(x =T_cell))+
  geom_histogram()+
  geom_vline(xintercept = mean_T_cell,color="red",linetype="dashed")+
  theme_bw()+
  xlab("T Cell Count in cells/mm3")+
  ylab("Frequency")+
  labs(title="Distribution of T_cell ",fill=" ")+
  easy_center_title()

p2 <- ggplot(df3,aes(x = C_level))+
  geom_histogram()+
  geom_vline(xintercept = mean_C_level,color="red",linetype="dashed")+
  theme_bw()+
  xlab("Plasma level of vitamin C measured in 2000 mcg/mm3")+
  ylab("Frequency")+
  labs(title="Distribution of C_Level ",fill=" ")+
  easy_center_title()
p3 <- ggarrange(p1,p2,ncol=1,nrow=2)
plot(p3)

#plots
model <- glm(T_cell~C_level, data=df3)
p <- ggplot(df3,aes(x = C_level , y = T_cell)) +
  geom_point() +
  geom_smooth(method='lm')

plot(p)

alpha <- 0.05;

x_mean <- mean_T_cell 
y_mean <- mean_C_level

y <- df3$T_cell
x <- df3$C_level

y_ymean <- y- y_mean;
x_xmean <- x - x_mean;

b1 <- sum(x_xmean*y_ymean)/sum(x_xmean^2);
b0 <- y_mean - b1*x_mean;

reg_out <- b1*x + b0; 
RSS <- sum((y - reg_out)^2);

SEb1 <- sqrt((RSS)/((98)*sum((x - x_mean)^2)));

SEb0 <- sqrt((RSS/98)*((1/100)+(x_mean^2/sum((x - x_mean)^2))));


t_quantile <- qt(1-alpha/2,98);
CIb0_0 <- b0-t_quantile*SEb0
CIb0_1 <-  b0+t_quantile*SEb0;
CIb1_0 <- b1-t_quantile*SEb1;
CIb1_1 <- b1+t_quantile*SEb1;


y1 <- CIb1_0*x + CIb0_0;
y2 <- CIb1_0*x + CIb0_1;
y3 <- CIb1_1*x + CIb0_0;
y4 <- CIb1_1*x + CIb0_1; 

g <- p
g <- g + geom_line(aes(y=y1), colour="red")
g <- g + geom_line(aes(y=y2), colour="green")
g <- g + geom_line(aes(y=y3), colour="black")
g <- g + geom_line(aes(y=y4), colour="yellow")
plot(g)






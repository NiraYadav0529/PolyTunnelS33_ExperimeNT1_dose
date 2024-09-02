setwd("C:\\Users\\90958427\\OneDrive - Western Sydney University\\Phd File\\Phosphate absorption\\Data")

#reading CSV file 
data1<- read.csv("Sampling code Lot 1 & 2.csv")

#boxplot of result by PH 
boxplot(Result~ph, data=data1)

install.packages("tidyverse")
install.packages("Rmisc")
install.packages("ggplot2")
install.packages("multcompview")
install.packages("data.table")
install.packages("agricolae")
install.packages("lubridate")
library(ggplot2)
library(dplyr)
library(multcompview)
library(agricolae)
library(data.table)
library(tidyverse)
library(Rmisc)


#SITARAM DAI PHOSPHATE ABSORPTION
#Set working directory

data1<- read.csv("ph3.csv")
head(data1)
tail(data1)
#Conc=1
C1<- data1[c(1:37), ]
C1
C10<- data1[c(38:72),]
C10
#normality
shapiro.test(C10$Result)
#NC1<- data.table(C1) 
C1$logResult<- log(C1$Result)
#NC1[,
     # .(W = shapiro.test(C1$Result)$statistic, P.value = shapiro.test(C1$Result)$p.value),
    #  by = .(Sample)]      # normally distributed
#shapiro.test(C10$Result)
#NC1<- data.table(C10) 

#NC10<- data.table(C10)
#NC10[,
    #.(W = shapiro.test(C1$Result)$statistic, P.value = shapiro.test(C1$Result)$p.value),
    #by = .(Sample)]      # normally distributed

# ANOVA Total
modelC1<-aov(Result~Sample, data=C1) 
summary.lm(modelC1)
tukey.testC1 <- TukeyHSD(modelC1) #post hoc test
tukey.testC1
letterC1<-HSD.test(modelC1, "Sample", group=TRUE) #generating significant letters
letterC1


#GGplot
head(C1)
a<-summarySE(C1, measurevar="Result", groupvars=c("Sample", "Treatment"))
a
Position<- c("N15", "HIE4", "BB12","N5", "N7", "BB14" )
Position1<- aes(x = reorder(f.name, -Result), y = Result)
ggplot(data=a, aes(x=Sample, y=Result, fill=Treatment)) +
  geom_bar(stat="identity", position = position_dodge())+
  geom_errorbar(aes(ymin=Result-sd, ymax=Result+sd), width=.2,
                position=position_dodge(.9))+ aes(x = reorder(Sample, +Result), y = Result)+
  xlab("Sample")+
  ylab("Phosphate absorbed")
 
head(C10)
a<-summarySE(C10, measurevar="Result", groupvars=c("Sample", "Treatment"))
a
Position<- c("N15", "HIE4", "BB12","N5", "N7", "BB14" )
Position1<- aes(x = reorder(f.name, -Result), y = Result)
ggplot(data=a, aes(x=Sample, y=Result, fill=Treatment)) +
  geom_bar(stat="identity", position = position_dodge())+
  geom_errorbar(aes(ymin=Result-sd, ymax=Result+sd), width=.2,
                position=position_dodge(.9))+ aes(x = reorder(Sample, +Result), y = Result)+
  xlab("Sample")+
  ylab("Phosphate absorbed")



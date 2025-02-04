#setworking directory to C:/documents
setwd("C:/Users/90958427/OneDrive - Western Sydney University/Phd File/Hanxia and Niraj Experiment/Data file/GC data/csv data")
getwd()
dir()
read.csv("df1-urine-A.csv")
dir("data")
install.packages("gplots")
library()

head("df1-urine-A.csv"))
head(df1-urine-A.csv)
data <- read.csv("df1-urine-A.csv")
data
head(data)
tail(data)
summary(data)
with(data, plot(group,bHeight_First))
library(ggplot2)
ggplot((data, aes(x= X, y=  lPhosphate_Second.Lechates )))

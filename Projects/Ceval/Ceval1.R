# import sasdata into R
library(haven)
library(nycflights13)
library(tidyverse)
library(ggpubr)
library(rstatix)  #in this package t_test can use pipe

data <- read_rds("/home/xinlong.yang/Projects/Ceval/Datasets/ceval.rds")

view(data)
##############################################################################################################
# Method one
# output the result seperated

# select the analysis columns
eff1 <- select(data,VISITN,FORM,ARD,AD)
# view(eff1)

# compute the mean of ARD or AD about different Visit

# probability density
# dnorm(0.95, mean =0, sd = 1)
# pnorm(0.95, mean =0, sd = 1)
ciceff <- qnorm(0.975,mean=0,sd=1)
# how to compute the confidence interval:correct to Count indicators not useful quantitative index
# lower=mean-qnorm(0.975,mean=0,sd=1)*std
# upper=mean+qnorm(0.975,mean=0,sd=1)*std

# ARD :compute the grouped mean for all
ard_all <- filter(group_by(eff1, FORM),!is.na(ARD))
summarise(ard_all, 
          cnt=n(),
          mean = mean(ARD, na.rm = TRUE),
          median=median(ARD, na.rm = TRUE),
          std=sd(ARD, na.rm = TRUE) )
# perform the t-test to get the 95%CI
ard_all_ysi <- filter(ard_all,FORM=="YSI-CGM")
t.test(ARD ~ 1, data =ard_all_ysi,conf.level=0.95)

group_by(ard_all,FORM) 

t.test(ARD ~ 1,data=group_by(ard_all,FORM),conf.level=0.95)
t.test(ARD ~ 1,data=filter(ard_all,FORM=="YSI-CGM"),conf.level=0.95)
t.test(ARD ~ 1,data=filter(ard_all,FORM=="BGM-CGM"),conf.level=0.95)
# how to perform t-tests for stratified data?


# ARD :compute the grouped mean for different Visit
# group by form and visit
by_visit <- group_by(eff1, FORM,VISITN)
summarise(filter(by_visit,!is.na(ARD)&!is.na(VISITN)), 
          cnt=n(),
          mean = mean(ARD, na.rm = TRUE),
          median=median(ARD, na.rm = TRUE),
          std=sd(ARD, na.rm = TRUE))
# perform the t-test to get the 95%CI
t.test(ARD ~ 1, data = by_visit)

# AD :compute the grouped mean for all
summarise(filter(group_by(eff1, FORM),!is.na(AD)), 
          cnt=n(),
          mean = mean(AD, na.rm = TRUE),
          median=median(AD, na.rm = TRUE),
          std=sd(AD, na.rm = TRUE))
##############################################################################################################
# how to preprocessing data frame
# add index variable to the data frame

(.ard_all <- filter(eff1,!is.na(ARD)))

# add column to data frame
cbind(.ard_all,index=c(5))

(.ad_all <- filter(eff1,!is.na(AD)))
.ard_all$index <- c(1)
.ard_all$stvar <- .ard_all$ARD
.ad_all$index <- c(2)
.ad_all$stvar <- .ad_all$AD*18
##############################################################################################################
# Method two
# modify the visitn value of BGM-CGM,make it same sa YSI-CGM, for add stratified variable
eff2 <- eff1

eff2['VISITN'][eff2['FORM']=='BGM-CGM'&eff1['VISITN']==4] <- 10
eff2['VISITN'][eff2['FORM']=='BGM-CGM'&eff1['VISITN']==3] <- 7
eff2['VISITN'][eff2['FORM']=='BGM-CGM'&eff1['VISITN']==2] <- 4

# ARD value is not missing
ard_nomiss <- filter(eff2,!is.na(ARD))
# AD value is not missing
ad_nomiss <- filter(eff2,!is.na(AD))

# add roaws to data frame
.com <- rbind(
    # ARD for all data point
  cbind(ard_nomiss,index=c(1),stvar=ard_nomiss$ARD),
    # AD for all data point
  cbind(ad_nomiss,index=c(2),stvar=ad_nomiss$AD*18),
    # ARD for different VISIT with excluding the NULL value of ARD 
  cbind(filter(ard_nomiss,VISITN==1),index=c(3),stvar=filter(ard_nomiss,VISITN==1)$ARD),
  cbind(filter(ard_nomiss,VISITN==4),index=c(4),stvar=filter(ard_nomiss,VISITN==4)$ARD),
  cbind(filter(ard_nomiss,VISITN==7),index=c(5),stvar=filter(ard_nomiss,VISITN==7)$ARD),
  cbind(filter(ard_nomiss,VISITN==10),index=c(6),stvar=filter(ard_nomiss,VISITN==10)$ARD),
  cbind(filter(ard_nomiss,VISITN==14),index=c(7),stvar=filter(ard_nomiss,VISITN==14)$ARD)
)
# Compute statistic with pipe (the pipe is not useful for ggplot2 package)
# without format ,after transpose the decimal digits while greater than 2
optsum_noformat <- .com %>% 
  group_by(FORM,index) %>% 
  summarise(
    cnt=n(),
    mean = mean(stvar, na.rm = TRUE),
    median=median(stvar, na.rm = TRUE),
    std=sd(stvar, na.rm = TRUE))

# format(t(ttest["conf.low"]),digits = 1, nsmall = 1)
optsum <- .com %>% 
  group_by(FORM,index) %>% 
  summarise(
    cnt=n(),
    mean = format(mean(stvar, na.rm = TRUE),digits = 1, nsmall = 1),
    median=format(median(stvar, na.rm = TRUE),digits = 1, nsmall = 1),
    std=format(sd(stvar, na.rm = TRUE),digits = 1, nsmall = 1))

optsum
##############################################################################################################
# https://www.datanovia.com/en/lessons/how-to-do-a-t-test-in-r-calculation-and-reporting/
# T-Test
.com2 <- .com
(optsum2 <- optsum)
# YSI-CGM t-test
ysi1 <- filter(.com2,FORM=="YSI-CGM"&index==1)
t.test(ARD ~ 1,data=ysi1,conf.level=0.95)
# use functions:t_test to get confidence interval(using pipe)
ttest <- .com2 %>% group_by(FORM,index) %>% 
  t_test(stvar ~ 1 ,mu=25,conf.level=0.95,detailed = TRUE)
ttest

# add the column:conf.low conf.high to optsum

cbind(optsum2,conf.low=ttest["conf.low"],conf.high=ttest["conf.high"])
ttest["conf.low"]
# justify the number format
round(ttest["conf.low"], 3)
format(t(ttest["conf.low"]),digits = 1, nsmall = 1)
format(2.999,digits = 1, nsmall = 1)

ci1 <- paste(t(format(t(ttest["conf.low"]),digits = 1, nsmall = 1)),t(format(t(ttest["conf.high"]),digits = 1, nsmall = 1)),sep=',')

optfinal <- cbind(optsum2,ci=ci1)
##############################################################################################################
# matrix transpose
# assign row names to dataframe
(optysi <- filter(optfinal,FORM=="YSI-CGM"))
(optbgm <- filter(optfinal,FORM=="BGM-CGM"))

# modify the names of dataframe rows or columns before tranpose
rownames(optysi, do.NULL = TRUE, prefix = "row")
(rownm <- paste('col',c(1:nrow(optysi)),sep = ""))
rownames(optysi) <- rownm
t(optysi)
t(optbgm)
# the dataframe must have the same row ands colunms
# this code is false
# rbind(
#   t(optysi),
#   t(optbgm))

# this runs with no error
view(rbind(
  t(optysi),
  cbind(t(optbgm),col7="NA")
))
##############################################################################################################
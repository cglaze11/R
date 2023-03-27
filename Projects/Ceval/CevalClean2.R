
# Header Specification ----------------------------------------------------

# the SAS output result
# 表5 CEVAL研究门诊监测日数据（每天） - 全部研究中心(N=53)
# MARD 	MAD 	MARD 
# (%) 	(mg/dL) 	第1天 	第4天 	第7天 	第10天 	第14天 
# 数据点(n) 	13680 	4196 	2830 	2868 	2809 	2737 	2436 
# 平均数 	11.6 	14.7 	11.9 	10.4 	11.0 	12.3 	12.4 
# 中位数 	9.4 	11.4 	9.6 	8.3 	8.7 	10.3 	10.3 
# SD 	9.6 	12.6 	10.0 	8.5 	9.4 	10.1 	9.7 
# 95%CI 	11.4,11.7 	14.3,15.1 	11.5,12.3 	10.1,10.7 	10.6,11.3 	11.9,12.7 	12.0,12.8 

# 表5 CEVAL研究门诊监测日数据（每天） - 全部研究中心(N=53)
# MARD 	MAD 	MARD 
# (%) 	(mg/dL) 	第1天 	第4天 	第7天 	第10天 	第14天 
# 数据点(n) 	13680 	4196 	2830 	2868 	2809 	2737 	2436 
# 平均数 	11.6 	14.7 	11.9 	10.4 	11.0 	12.3 	12.4 
# 中位数 	9.4 	11.4 	9.6 	8.3 	8.7 	10.3 	10.3 
# SD 	9.6 	12.6 	10.0 	8.5 	9.4 	10.1 	9.7 
# 95%CI 	11.4,11.7 	14.3,15.1 	11.5,12.3 	10.1,10.7 	10.6,11.3 	11.9,12.7 	12.0,12.8 

# outstanding problems
# 1.can't remove the grouped variable while selecting columns using select(,-())?
# 2.how to assign variable label of matrix cloumns ?
# 3.the output sas7bdat can't open in SAS.Why?

# import data -------------------------------------------------------------

# setup the packages
library(haven)
library(nycflights13)
library(tidyverse)
library(ggpubr)
library(rstatix)  #in this package t_test can use pipe
# import data
data <- read_rds("/home/xinlong.yang/Projects/Ceval/Datasets/ceval.rds")
# select the analysis columns
eff1 <- select(data,VISITN,FORM,ARD,AD)

# Statistic Compute -------------------------------------------------------

# quantitative description
eff2 <- eff1
# adjust the VISIT Number for Form=='BGM-CGM'
eff2['VISITN'][eff2['FORM']=='BGM-CGM'&eff1['VISITN']==4] <- 10
eff2['VISITN'][eff2['FORM']=='BGM-CGM'&eff1['VISITN']==3] <- 7
eff2['VISITN'][eff2['FORM']=='BGM-CGM'&eff1['VISITN']==2] <- 4

# ARD value is not missing
ard_nomiss <- filter(eff2,!is.na(ARD))
# AD value is not missing
ad_nomiss <- filter(eff2,!is.na(AD))

# add roaws to data frame.insert the var:index
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
# Compute statistic with pipe
optsum <- .com %>% 
  group_by(FORM,index) %>% 
  summarise(
    cnt=format(n(),digits = 0, nsmall = 0),
    mean = format(mean(stvar, na.rm = TRUE),digits = 1, nsmall = 1),
    median=format(median(stvar, na.rm = TRUE),digits = 1, nsmall = 1),
    std=format(sd(stvar, na.rm = TRUE),digits = 1, nsmall = 1))

optsum

# T-Test ------------------------------------------------------------------

.com2 <- .com
optsum2 <- optsum
# use functions:t_test to get confidence interval(using pipe)
ttest <- .com2 %>% group_by(FORM,index) %>% 
  t_test(stvar ~ 1 ,mu=25,conf.level=0.95,detailed = TRUE)
ttest

# add the column:conf.low conf.high to optsum
ci1 <- paste(t(format(t(ttest["conf.low"]),digits = 1, nsmall = 1)),t(format(t(ttest["conf.high"]),digits = 1, nsmall = 1)),sep=',')
# select columns
(optfinal <- cbind(optsum2,ci=ci1))

# (optfinal <- cbind(optsum2,ci=ci1) %>% 
#     select(cnt,mean,median,std,ci))

# Pivot Output ------------------------------------------------------------

# pivot
pivotfinal <- optfinal %>% group_by(FORM,index)
  pivot_longer(c("cnt","mean","median","std","ci"), names_to = "para", values_to = "value")

# the param cols of function pivot_longer must be the same attrobute
pivotfinal <- optfinal %>% 
  pivot_longer(c(cnt,mean,median,std,ci), names_to = "para", values_to = "value") %>% 
  pivot_wider(names_from = index, values_from = value)



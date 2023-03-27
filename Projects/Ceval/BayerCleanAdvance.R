
# Reference Information ---------------------------------------------------

# the rules to judge visit num
  # CGMTM le 1440  <- AVISITN=2 <- 1day
  # CGMTM le 7200  <- AVISITN=3 <- 5day
  # CGMTM le 14400  <- AVISITN=4 <- 10day
  # CGMTM le 21600  <- AVISITN=5 <- 15day

# Setup -------------------------------------------------------------------

# setup the packages
library(tidyverse)
library(rstatix)  #in this package t_test can use pipe
library(berryFunctions)

# Import Data -------------------------------------------------------------

data <- read_rds("/home/xinlong.yang/Projects/Ceval/Datasets/bayer.rds")

# Preprocess --------------------------------------------------------------

# derive the visit number
vstall <- data %>% filter(ANATYPE=="原始数据") %>% 
    cbind(visitn=1) 

# separate visitn
vst <- vstall
# Before
# vst$visitn[vst$CGMTM <= 1440] <- 2  # 1 day
# vst$visitn[vst$CGMTM <= 7200 & vst$CGMTM > 1440] <- 3  # 5 day
# vst$visitn[vst$CGMTM <= 14400 & vst$CGMTM > 7200] <- 4 # 10 day
# vst$visitn[vst$CGMTM <= 21600 & vst$CGMTM > 14400] <- 5  # 15 day
      # cnt <- vst %>% group_by(visitn) %>% summarise(n=n())
      # visitn     n
      # <dbl> <int>
      # 1      2   679
      # 2      3   921
      # 3      4   927
      # 4      5   833
# Advance
# as.integer(cut(1320, breaks=c(0,1440,7200,14400,21600),labels=c(2,3,4,5)))
# cut(1320, breaks=c(0,1440,7200,14400,21600),labels=c(2,3,4,5))
vst$visitn <- as.integer(cut(vst$CGMTM, breaks=c(0,1440,7200,14400,21600)))+1

# pre-visit
st <- rbind(
  vstall,
  vst
)

# index stra fro YSI or CGM
st2 <- rbind(
  mutate(st,stvar=st$YSIRES,stra=1),
  mutate(st,stvar=st$CGMRES,stra=2)
)

# Statistics --------------------------------------------------------------

mean <- st2 %>% 
  group_by(visitn,stra,CGMLOCN) %>% 
  summarise(
    cnt=format(n(),digits = 0, nsmall = 0),
    mean = format(mean(stvar, na.rm = TRUE),digits = 2, nsmall = 2),
    median=format(median(stvar, na.rm = TRUE),digits = 2, nsmall = 2),
    std=format(sd(stvar, na.rm = TRUE),digits = 3, nsmall = 3),
    min=format(min(stvar),digits = 1, nsmall = 1),
    max=format(max(stvar),digits = 1, nsmall = 1),
    q1=format(quantile(stvar,probs=c(0.25),na.rm=TRUE,type=2),digits = 2, nsmall = 2),
    q3=format(quantile(stvar,probs=c(0.75),na.rm=TRUE,type=2),digits = 2, nsmall = 2),
    
    mnstd=paste(c(mean,'(',std,")"),sep = "",collapse = ""),
    q13=paste(c(q1,',',q3),sep = "",collapse = ""),
    ext=paste(c(min,',',max),sep = "",collapse = "")
  ) 

# transpose mean
rst <- mean %>% 
  group_by(visitn,stra) %>% 
  pivot_longer(c("cnt","mnstd","median","q13","ext"), names_to = "para", values_to = "value") %>% 
  select(-(mean:q3)) 

# MARD-MAD ----------------------------------------------------------------

# compute MARD , MAD and MARD Difference
mst <- st %>% group_by(visitn,CGMLOCN) %>% 
  summarise(
    mard=mean(ARD,na.rm=TRUE),
    mad=mean(AD,na.rm=TRUE) 
  ) 

# left join abm MADR to compute the MARD difference
# select the abm MARD
abm <-  filter(mst,CGMLOCN==1) %>% 
  select(visitn,mard) %>% rename("abmard"="mard")

mst2 <- left_join(mst,abm,by=c("visitn")) %>% 
  mutate(
    difmard=mard-abmard,# MARD difference
    mad2=paste(format(mad*18,digits=1,nsmall=1)) ,# mad:mg/dL
    mard=paste(format(mard,digits=1,nsmall=1),"%",sep=""),
    mad=paste(format(mad,digits=1,nsmall=2),sep=""),
    difmard=paste(format(difmard,digits=1,nsmall=1),"%",sep="")
  )

mst2$difmard[mst2$CGMLOCN==1] <- NA

# transpose
mst3 <- mst2 %>% select(visitn,CGMLOCN,mard,mad,mad2,difmard) %>% 
  group_by(visitn,CGMLOCN) %>% 
  pivot_longer(c("mard","mad","mad2","difmard"),names_to = "para",values_to = "value")

# add stra variable to mst
mst4 <- cbind(mst3,stra=NA)

mst4$stra[mst4$para=="mard"|mst4$para=="difmard"] <- 3
mst4$stra[is.na(mst4$stra)] <- 4

# CI(T-test) --------------------------------------------------------------

# index fro ARD or AD
ci <- rbind(
        mutate(st,stvar=st$ARD,stra=3),
        mutate(st,stvar=st$AD,stra=4)
      ) %>% group_by(visitn,stra,CGMLOCN) %>% 
          t_test(stvar ~ 1 ,mu=25,conf.level=0.95,detailed = TRUE)

ci2 <- filter(ci,stra==3) %>% 
  mutate(
    ci=paste(format(conf.low,digits=1,nsmall=1),'%,',format(conf.high,digits=1,nsmall=1),'%',sep = "")
  )
ci3 <- filter(ci,stra==4) %>% 
  mutate(
    ci=paste(format(conf.low,digits=1,nsmall=2),',',format(conf.high,digits=1,nsmall=2),sep = "")
  )

ci4 <- rbind(ci2,ci3) %>% select(visitn,stra,CGMLOCN,ci) %>% 
    group_by(visitn,stra,CGMLOCN) %>% 
      pivot_longer(c("ci"),names_to = "para",values_to = "value")

# Adjust Formats ----------------------------------------------------------

# adjust value

vst <- c("合计","第1天访视","第5天访视","第10天访视","第15天访视")
cat <- c("YSI(mg/dL)","BGM(mg/dL)")
para <- c("N","Mean(SD)","Median","Q1,Q3","Min,Max","MARD^1","MARD差值^2","95% CI","MAD(mM)^3","MAD(mg/dL) ")
paran <- c(1:5,1,3,2,1,2)
nm <- c("cnt","mnstd","median","q13","ext","mard","difmard","ci","mad","mad2")
names(paran) <- nm
names(para) <- nm

# set rst mst4 ci4,and pivot_wider by cgmlocn
final <- rbind(rst,mst4,ci4) %>% group_by(visitn,stra) %>% 
  pivot_wider(names_from = CGMLOCN, values_from = value) %>% arrange(visitn,stra) 

# add variable:order to adjust the sequence of para
final <- final %>% 
  cbind(
    order=paran[final$para]
  ) %>% 
  arrange(visitn,stra,order) %>% select(-order) %>% 
  mutate(
    n=row_number(),
    n2=n()
  )

# add null rows
last <- which(final$n == final$n2) %>% as.vector()
len <- length(last)-1
inst <- last[1:len]+c(1:len)

final <- insertRows(final,inst[1:len],new=NA,rcurrent =FALSE) %>% select(-(n:n2)) %>% 
  # fill visitn and stra with previous value
    as.data.frame() %>% fill(visitn,stra) %>% as.tibble()

# not the first value of the variable,then set NA
final$stra[duplicated(select(final,visitn,stra))] <- NA
final$visitn[duplicated(final$visitn)] <- NA

final$visitn <- vst[c(final$visitn)]
final$stra <- cat[c(final$stra)]
final$para <- para[c(final$para)]



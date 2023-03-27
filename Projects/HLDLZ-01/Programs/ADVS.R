library(haven)
library(nycflights13)
library(tidyverse)
library(ggpubr)
library(rstatix)  #in this package t_test can use pipe
library(berryFunctions)
library(readxl)

DomainName <- c("ADVS")

ADVSSPEC <- read_excel("Documents/F-SA-08-02-CN ADS Programming Specifications Template_V1.0.xlsx",sheet = DomainName)
# 导入数据
vs <- read_csv("Rawdata/vs.csv",locale = locale(encoding = "GB2312"))
vst <- read_csv("Rawdata/visit.csv",locale = locale(encoding = "GB2312"))
adsl <- read_csv("Anadata/ADSL_lr.csv",locale = locale(encoding = "GB2312"))
adsl2 <- select(adsl,all_of(toupper(c(t(ADVSSPEC[1:18,1])))))

# 首次治疗日期用来判断基线
fstvst <- filter(vst,VISIT=='V1：治疗期（首次治疗，第1天）') %>% 
  select(USUBJID,visdat) %>% 
  # rename_at('visdat',~'FIRSTDTC')
  rename(FIRSTDTC=visdat)

# Codelists
# avisit <- tribble(
#   ~VISIT,             ~AVISITN,
#   #-----------------|--------|------
#   "V0：筛选期（第-7天～0天）",  1, 
#   "V1：治疗期（首次治疗，第1天）",   2, 
#   "V2：治疗期（治疗中点，第6天或第7天）",   3, 
#   "V3：治疗期（末次治疗，第14±1天）", 4,
#   "V4：随访期（末次治疗后14±2天，第28±3天）", 5
# )

vstest <- tribble(
  ~VSTEST,        ~VSTESTN,
  #-----------------|--------|------
  "体温（℃）",  1, 
  "呼吸（次/分）",   2, 
  "心率（次/分）",   3, 
  "收缩压（mmHg）", 4,
  "舒张压（mmHg）", 5
)

vs2 <- vs %>% 
  # left_join(avisit, by = "VISIT") %>% 
  left_join(vstest, by = "VSTEST") %>% 
  left_join(fstvst, by = "USUBJID") %>% 
  mutate(
    AVISITN=as.numeric(str_sub(vs$VISIT, 2, 2))+1
    )

# str_view(vs$VISIT,"V(\\d)")
# as.numeric(str_extract(vs$VISIT,"V(\\d)",group=1))+1

# 基线值
vsbase <- vs2 %>% 
  filter(vsdat < FIRSTDTC) %>% 
  rename(BSVSOR = VSOR) %>% 
  select(USUBJID,VSTEST,BSVSOR)
  # 验证是否存在多个基线值
  # group_by(USUBJID,VSTEST) %>% 
  # filter(n() > 1)

# 计算差值
vs3 <- left_join(vs2,vsbase, by = c("USUBJID","VSTEST"),multiple = "all")
# filter(vs3,vsdat < FIRSTDTC) %>% 
vs4 <- mutate(vs3,DEV=VSOR-BSVSOR) %>% 
  select(USUBJID,VISIT:DEV)

# 基线前及治疗当天不计算差值
vs4$DEV[vs4$vsdat <= vs4$FIRSTDTC] <- NA
vs4 <- select(vs4,-FIRSTDTC)

# 调整变量名与顺序
rst <- left_join(vs4,adsl2,by="USUBJID",multiple = "all")

# (Renmc <- c(AVISIT = "VISIT",VSCLSIN="vsclsi_u",VSDTC='vsdat'))
# colnames(rst) %in% c("VISIT","vsdat","vsclsi_u")
# colnames(rst)[colnames(rst) %in% c("VISIT","vsdat","vsclsi_u")]
colnames(rst)[colnames(rst) %in% c("VISIT","vsdat","vsclsi_u")] <- c("AVISIT","VSDTC","VSCLSIN")

# colnames(rst)[(colnames(rst) %in% c("VISIT","vsclsi_u","vsdat"))==TRUE] <- c("AVISIT","VSDTC","VSCLSIN")
# which((colnames(rst) %in% c("VISIT","vsclsi_u","vsdat"))==TRUE)

isTRUE(colnames(rst) %in% c("VISIT","vsdat","vsclsi_u"))
# colnm <- colnames(rst)
# which(colnm=="USUBJID")
# colnm %in% c("USUBJID","VISIT")
# 
# all_of(c("USUBJID","AVISIT"))
# which(colnm==all_of(c("USUBJID","AVISIT")))
# 
# colnames(rst)[colnames(rst)=="YY"] <- "AVISIT"
# colnames(rst)[all_of(c("XX","YY"))] <- all_of(c("USUBJID","AVISIT"))
# colnames(rst)[[c("XX","YY")]]

ADVS <- rst %>% 
  # rename(all_of(Renmc)) %>% 
  select(all_of(toupper(c(t(ADVSSPEC[,1]))))) %>% 
  arrange(USUBJID,AVISITN,VSTESTN)
  # write.table("Anadata/ADVS.csv")

# 添加标签
for (i in 1:length(c(t(ADVSSPEC[,1])))) {
  attr(ADVS[[i]],"label") <- as.character(ADVSSPEC[i,2])
}

write.table(ADVS,str_c("Anadata/",DomainName ,".csv"))

# 与advs数据集进行比对
# library(arsenal)
# sasadvs <- read_sas("Anadata/advs.sas7bdat",encoding = "GB2312") %>% 
#   select(all_of(toupper(c(t(ADVSSPEC[,1]))))) %>% 
#   arrange(USUBJID,AVISITN,VSTESTN)
# 
# summary(comparedf(ADVS,sasadvs))

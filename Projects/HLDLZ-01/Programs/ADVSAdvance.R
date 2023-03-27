# Env Clear ---------------------------------------------------------------

rm(list = ls())
DomainName <- c("ADVS")

# 导入数据 --------------------------------------------------------------------

ADVSSPEC <- read_excel("Documents/F-SA-08-02-CN ADS Programming Specifications Template_V1.0.xlsx",sheet = DomainName)
vs <- read_csv("Rawdata/vs.csv",locale = locale(encoding = "GB2312"))
vst <- read_csv("Rawdata/visit.csv",locale = locale(encoding = "GB2312"))

adsl <- read_csv("Anadata/ADSL_lr.csv",locale = locale(encoding = "GB2312"))
adsl2 <- select(adsl,all_of(toupper(c(t(ADVSSPEC[1:18,1])))))

# 首次治疗日期用来判断基线
fstvst <- filter(vst,VISIT=='V1：治疗期（首次治疗，第1天）') %>% 
  select(USUBJID,visdat) %>% 
  rename(FIRSTDTC=visdat)

vstest <- c("体温（℃）","呼吸（次/分）","心率（次/分）","收缩压（mmHg）","舒张压（mmHg）")

vs2 <- vs %>% 
  left_join(fstvst, by = "USUBJID") %>% 
  mutate(
    AVISITN=as.numeric(str_sub(.$VISIT, 2, 2))+1,
    VSTESTN=match(.$VSTEST,vstest)
  )

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

vs4 <- mutate(vs3,DEV=VSOR-BSVSOR) %>% 
  select(USUBJID,VISIT:DEV)

# 基线前及治疗当天不计算差值
vs4$DEV[vs4$vsdat <= vs4$FIRSTDTC] <- NA
vs4 <- select(vs4,-FIRSTDTC)

# 调整格式 --------------------------------------------------------------------

rst <- left_join(vs4,adsl2,by="USUBJID",multiple = "all")

colnames(rst)[colnames(rst) %in% c("VISIT","vsdat","vsclsi_u")] <- c("AVISIT","VSDTC","VSCLSIN")

ADVS <- rst %>% 
  select(all_of(toupper(c(t(ADVSSPEC[,1]))))) %>% 
  arrange(USUBJID,AVISITN,VSTESTN)

# 添加标签
for (i in 1:length(c(t(ADVSSPEC[,1])))) {
  attr(ADVS[[i]],"label") <- as.character(ADVSSPEC[i,2])
}

# 导出数据 --------------------------------------------------------------------

write.table(ADVS,str_c("Anadata/",DomainName ,".csv"))


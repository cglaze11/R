rm(list = ls())

adsl <- read_sas("Anadata/adsl.sas7bdat",encoding="GB2312") %>% 
  filter(ENRLFL=="Y") %>% 
  select(USUBJID,TRTPN,DSYN,DSREAS,DSREASN,DSREASP,RANDYN,DSYN2,DSORRES,DSORRESN,DSORRESP)

# 原始数据预处理
adsl2 <- adsl
adsl2["TRTPN"] <- 9
adsl3 <- rbind(adsl,adsl2)

st1 <- filter(adsl3,TRTPN==9) %>% 
  cbind(index=1,stvar="筛选受试者")

st2 <- filter(adsl3,TRTPN==9 , DSYN != "是") %>% 
  cbind(index=2,stvar="筛选失败受试者数")

st3 <- filter(adsl3,RANDYN == "是") %>% 
  cbind(index=3,stvar="随机化受试者数")

st4 <- filter(adsl3,DSYN2 == "是",RANDYN == "是") %>% 
  cbind(index=4,stvar="完成试验的受试者数")

st5 <- filter(adsl3,DSYN2 != "是",RANDYN == "是") %>% 
  cbind(index=5,stvar="未完成试验的受试者数")

st6 <- filter(adsl3,DSYN2 != "是",RANDYN == "是") %>% 
  cbind(index=6,stvar=paste(str_pad(.$DSORRESN,2,"left","0")," ",str_split_fixed(.$DSORRES,"：|、|，|；",n=5)[,2]))

st7 <- filter(adsl3,DSYN2 != "是",RANDYN == "是",str_detect(adsl3$DSORRES, "其他")==TRUE) %>% 
  cbind(index=7,stvar=paste("    ",str_split_fixed(.$DSORRES," ",n=2)[,2]))

st <- rbind(
  st1,st2,st3,st4,st5,st6,st7
  ) %>% 
  arrange(index,stvar,TRTPN) %>% 
  group_by(index,stvar,TRTPN) %>% 
  summarise(
    cnt=n()
  ) 

# 提取分母
den <- st %>% 
  filter(index==1|index==3|index==5|(index==6 & stvar=="10   其他")) 
den2 <- rbind(
    cbind(filter(den,index==1),index2=2),
    cbind(filter(den,index==1),index2=3),
    cbind(filter(den,index==3),index2=4),
    cbind(filter(den,index==3),index2=5),
    cbind(filter(den,index==5),index2=6),
    cbind(filter(den,index==6),index2=7)
  ) %>% 
  group_by(index2,TRTPN) %>%
  select(index2,TRTPN,cnt) %>% 
  rename(index=index2,total=cnt)

# 计算百分比并转置
rst <- left_join(st,den2,by=c("index","TRTPN")) %>% 
  mutate(
    pct=ifelse(is.na(total),paste(cnt),paste(cnt,'(',format(cnt/total*100,digits = 1, nsmall = 1),'%)'))
  ) %>% 
  pivot_wider(id_cols=c("index","stvar"),names_from = TRTPN,values_from = pct,names_prefix="col") %>% 
  select(index,stvar,col1,col2,col9) %>% 
  arrange(index,stvar)

rst$stvar[which(rst$index==6)] <- as.character(str_sub(filter(rst,index==6)$stvar,3)) 
rst$col1[which(rst$index >= 3 & is.na(rst$col1))] <- "0"
rst$col2[which(rst$index >= 3 & is.na(rst$col2))] <- "0"
rst$index[which(rst$index %in% c(5,7))] <- 6


# 调整格式 --------------------------------------------------------------------

insert <- tribble(
  ~index,        ~stvar,~col1,~col2,~col3,
  #-----------------|--------|------
  6          ,"提前退出/中止/终止试验的原因",NA,NA,NA
)
rst2 <- insertRows(rst,5,new=insert,rcurrent=TRUE) %>% 
  group_by(index)

# 增加空行
inst <- which(!duplicated(rst2$index))
len <- length(inst)-1
inst2 <- inst[2:(len+1)]+c(1:len)-1

final2 <- insertRows(rst2,inst[2:(length(inst))],new=NA,rcurrent=T)

final <- insertRows(rst2,inst2,new=NA,rcurrent=FALSE) %>% 
  ungroup() %>% 
  select(-index)


# 输出 ----------------------------------------------------------------------

# library(rtf)
# rtffile <- RTF("rtf.doc")  # this can be an .rtf or a .doc
# addParagraph(rtffile, "This is the output of a regression coefficients:\n")
# addTable(rtffile, as.data.frame(round(coef(s2), 2)))
# addParagraph(rtffile, "\n\nThis is the nicer looking table we made above:\n")
# addTable(rtffile, cbind(rownames(outtab), outtab))
# done(rtffile)

final2 <- final %>% 
  mutate_if(is.character,~`Encoding<-`(.,"GB2312"))

final3 <- final2 %>% 
  mutate_if(is.character,stri_enc_toutf8)

rtf <- RTF("Programs/Outputs/T_9_1_1.rtf")
addTable(rtf, final)
done(rtf)



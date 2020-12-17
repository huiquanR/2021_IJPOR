#### Load all the packages ####

Sys.info()
R.version

# Sys.info()
#   sysname        release        version       nodename        machine 
# "Windows"       "10 x64"  "build 19042"         "USER"       "x86-64" 

# > R.version                           
# platform       x86_64-w64-mingw32          
# arch           x86_64                      
# os             mingw32                     
# system         x86_64, mingw32
# version.string R version 4.0.2 (2020-06-22)
# nickname       Taking Off Again        

#
memory.size(max=T) # set mem size
options(scipen=0)  # scientific notation
library(easypackages) # load packages #
easypackages::libraries("foreign", "readstata13", "tidyverse", 
                        "knitr", "qdap", "kableExtra", "tableone",
                        "lme4", "robustlmm", "lmerTest", "reshape2", 
                        "LMERConvenienceFunctions", "ecodist",  
                        "effects", "ggthemes", "ggeffects", "ggnewscale", 
                        "ggmap", "ggalt", "ggradar", "ggpubr", "captioner", 
                        "MatchIt", "acid", "laeken", "dplyr", "xlsx", 
                        "readxl", "data.table", "texreg", "scatterpie",
                        "descr", "pacman", "spatstat", "wakefield", 
                        "Amelia", "cplm", "car", "mgcv", "Rmisc", "MASS", 
                        "plyr",  "timeDate", "lubridate", "scales", # this includes::pretty-breaks 
                        "sjPlot", "sjmisc", "maptools","rJava", "bbplot", 
                        "cowplot", "data.table", "tidyr", "tibble",
                        "knitr", "visreg", "dotwhisker", "broom")
# set fonts #
windowsFonts(Arial=windowsFont("TT Arial"),
             Century=windowsFont("TT Century"),
             Times=windowsFont("TT Times New Roman"),
             Centaur=windowsFont("TT Centaur"),
             Palatino=windowsFont("TT Palatino"),
             Garamond=windowsFont("TT Garamond"))

#### [Read data into envir] Asian Barometer Survey ####

ABS1 = read.dta13("user_path/Data/ABS/abs1.dta")
ABS2 = read.dta13("user_path/Data/ABS/abs2.dta")
ABS3 = read.dta13("user_path/Data/ABS/abs3.dta", encoding = "UTF-8")
ABS4 = read.dta13("user_path/Data/ABS/abs4.dta")

#### [Cleaning] ABS wave 1 ####

# Basics: WAVE #
ABS1$wave = "ABS1"
ABS2$wave = "ABS2"
ABS3$wave = "ABS3"
ABS4$wave = "ABS4"

# YEAR OF SURVEY # 
ABS1$year = as.numeric(as.character(ABS1$yrsurvey))

# Respondent ID #
ABS1$RID = seq(1, length(ABS1$year))
ABS1$RID = formatC(ABS1$RID, width = 6, flag = 0)
head(ABS1$RID)
ABS1$ID = paste(ABS1$wave, ABS1$RID, sep = "_")

# MALE #
ABS1$male = NA
ABS1$male = ifelse(as.character(ABS1$se002)=="1", 1, ABS1$male)
ABS1$male = ifelse(as.character(ABS1$se002)=="2", 0, ABS1$male)

# AGE & BIRTH YEAR # 
ABS1$age = as.numeric(as.character(ABS1$se003a))
ABS1$age = ifelse(ABS1$age < 98, ABS1$age, NA)
ABS1$birthyear = ABS1$year - as.numeric(ABS1$age)

# Marital #
ABS1$marital = NA
table(ABS1$marital)
ABS1 = within(ABS1, {
 marital [se004=="6"] = "0.Unmarried"
 marital [se004=="1"|se004=="2"] = "1.Married"
 marital [se004=="3"|se004=="4"|se004=="5"] = "2.Other"
})

# Education #
ABS1$edulevel = NA
ABS1 = within(ABS1, {
  edulevel [trimws(as.character(ABS1$se005))=="1"|
              trimws(as.character(se005))=="2"] = "0.NoOrLittle"
  edulevel [trimws(as.character(se005))=="3"|
              trimws(as.character(se005))=="4"] = "1.Elementary"
  edulevel [trimws(as.character(se005))=="5"|
              trimws(as.character(se005))=="6"] = "2.Middle"
  edulevel [trimws(as.character(se005))=="7"|
              trimws(as.character(se005))=="8"] = "3.High"
  edulevel [trimws(as.character(se005))=="9"|
              trimws(as.character(se005))=="10"]= "4.CollegeAbove"
})
ABS1$eduyear = round(ABS1$se005a)
ABS1$eduyear = ifelse(ABS1$eduyear>90, NA, ABS1$eduyear)

# Religion #
ABS1$religion = "4.Other"
ABS1 = within(ABS1, {
  religion [se006=="11"|se006=="97"] = "0.None"
  religion [se006=="1" |se006=="5" ] = "1.Buddhist"
  religion [se006=="4" |se006=="8" |se006=="9" ] = "2.Christian"
  religion [se006=="7" ] = "3.Islamic"
})

# frequency of religious activity #
ABS1$relig_freq = NA
ABS1 = within(ABS1, {
  relig_freq [se007 >= 1 & se007 <= 3] = "1.Daily"
  relig_freq [se007 == 4             ] = "2.Weekly"
  relig_freq [se007 == 5             ] = "3.Monthly"
  relig_freq [se007 >= 6 & se007 <=7 ] = "4.Yearly"
  relig_freq [se007 >= 8 & se007 <=97] = "5.Never"
})

# Number of People in the same household #
ABS1$HHSIZE = ifelse(ABS1$se008a>=0 & 
                       ABS1$se008a<=26, 
                     ABS1$se008a, NA)

# income level #
ABS1$inclevel = NA
ABS1 = within(ABS1, {
  inclevel [se009 == 1] = "1st Lowest Quin"
  inclevel [se009 == 2] = "2nd Quintile"
  inclevel [se009 == 3] = "3rd Quintile"
  inclevel [se009 == 4] = "4th Quintile"
  inclevel [se009 == 5] = "5th Top Quin"
})

# weight #
ABS1$pweight = ABS1$w_all

# urban rural #
ABS1$urban = ifelse(ABS1$level3 == 1, 1, 0)

# [DV] corruption: local vs central #
ABS1$corrupt_loc = NA
ABS1$corrupt_nat = NA

ABS1 = within(ABS1, {
  corrupt_loc [q114 == 1] = "4.AlmostEveryone"
  corrupt_nat [q115 == 1] = "4.AlmostEveryone"
  corrupt_loc [q114 == 2] = "3.Majority"
  corrupt_nat [q115 == 2] = "3.Majority"
  corrupt_loc [q114 == 3] = "2.Minority"
  corrupt_nat [q115 == 3] = "2.Minority"
  corrupt_loc [q114 == 4] = "1.NoCorruption"
  corrupt_nat [q115 == 4] = "1.NoCorruption"
})

# who remains silent on corruption? #
ABS1$corrupt_loc_silent = ifelse(is.na(ABS1$corrupt_loc), 1, 0)
ABS1$corrupt_nat_silent = ifelse(is.na(ABS1$corrupt_nat), 1, 0)

# country #
ABS1 = within(ABS1, {
  country [country == 1] = "01.Japan"
  country [country == 2] = "02.HK"
  country [country == 3] = "03.Korea"
  country [country == 4] = "04.China"
  country [country == 5] = "05.Mongolia"
  country [country == 6] = "06.Philippines"
  country [country == 7] = "07.Taiwan"
  country [country == 8] = "08.Thailand"
})

#### [Cleaning] ABS wave 2 ####

# SURVEY YEAR and RID #
ABS2$year = as.numeric(as.character(ABS2$ir007_3))
ABS2$year = ifelse(ABS2$year<=2005, 2006, ABS2$year)
ABS2$year = ifelse(ABS2$country == 3, 2006, ABS2$year)
ABS2$year = ifelse(ABS2$country == 4, 2008, ABS2$year) # note - Dec 2007 china.
ABS2$year = ifelse(ABS2$country == 6, 2005, ABS2$year)
ABS2$year = ifelse(ABS2$country == 8, 2006, ABS2$year)
ABS2$year = ifelse(ABS2$country ==10, 2006, ABS2$year)
ABS2$year = ifelse(ABS2$country ==13, 2007, ABS2$year)

# Respondent ID #
ABS2$RID = seq(1, length(ABS2$year))
ABS2$RID = formatC(ABS2$RID, width = 6, flag = 0)
ABS2$ID = paste(ABS2$wave, ABS2$RID, sep = "_")

# MALE # 
ABS2$male = NA
ABS2$male = ifelse(as.character(ABS2$se002)=="1", 1, ABS2$male)
ABS2$male = ifelse(as.character(ABS2$se002)=="2", 0, ABS2$male)

# AGE & BIRTH YEAR # 
ABS2$age = as.numeric(as.character(ABS2$se003a))
ABS2$age = ifelse(ABS2$age < 96, ABS2$age, NA)
ABS2$birthyear = ABS2$year - as.numeric(ABS2$age)

# Marital # 
ABS2$marital = NA
ABS2 = within(ABS2, {
  marital [se004=="1"] = "0.Unmarried"
  marital [se004=="2"|se004=="3"] = "1.Married"
  marital [se004=="6"|se004=="4"|se004=="5"] = "2.Other"
})

# Education #
ABS2$edulevel = NA
ABS2 = within(ABS2, {
  edulevel [trimws(as.character(ABS2$se005))=="1"|
              trimws(as.character(se005))=="2"] = "0.NoOrLittle"
  edulevel [trimws(as.character(se005))=="3"|
              trimws(as.character(se005))=="4"] = "1.Elementary"
  edulevel [trimws(as.character(se005))=="5"|
              trimws(as.character(se005))=="6"] = "2.Middle"
  edulevel [trimws(as.character(se005))=="7"|
              trimws(as.character(se005))=="8"] = "3.High"
  edulevel [trimws(as.character(se005))=="9"|
              trimws(as.character(se005))=="10"]= "4.CollegeAbove"
})
ABS2$eduyear = round(ABS2$se005a)
ABS2$eduyear = ifelse(ABS2$eduyear>42, NA, ABS2$eduyear)

# Religion #
ABS2$religion = "4.Other"
ABS2 = within(ABS2, {
  religion [se006== 90 ] = "0.None"
  religion [se006== 60 ] = "1.Buddhist"
  religion [se006 >=10 & se006 <= 27|
              se006 >=72 & se006 <= 74|
              se006 >=81 & se006 <= 84 ] = "2.Christian"
  religion [se006 >= 40 & se006 <= 42 ] = "3.Islamic"
})

# frequency of religious activity #
ABS2$relig_freq = NA
ABS2 = within(ABS2, {
  relig_freq [se007>= 1 & se007 <=3] = "1.Daily"
  relig_freq [se007==4] = "2.Weekly"
  relig_freq [se007==5] = "3.Monthly"
  relig_freq [se007>= 6 & se007 <=7] = "4.Yearly"
  relig_freq [se007>= 8 & se007 <=97] = "5.Never"
})

# Number of People in the same household #
ABS2$HHSIZE = ifelse(ABS2$se008a>=0 & 
                       ABS2$se008a<=90, 
                     ABS2$se008a, NA)

# Anual Income (in quintiles - 20%) #
ABS2$inclevel = NA
ABS2 = within(ABS2, {
  inclevel [se009 == 1] = "1st Lowest Quin"
  inclevel [se009 == 2] = "2nd Quintile"
  inclevel [se009 == 3] = "3rd Quintile"
  inclevel [se009 == 4] = "4th Quintile"
  inclevel [se009 == 5] = "5th Top Quin"
})

# pweight - is by country. #
ABS2$pweight = NA
ABS2$pweight = ifelse(!is.na(ABS2$w_JP), ABS2$w_JP, ABS2$pweight)
ABS2$pweight = ifelse(!is.na(ABS2$w_KR), ABS2$w_KR, ABS2$pweight)
ABS2$pweight = ifelse(!is.na(ABS2$w_CN), ABS2$w_CN, ABS2$pweight)
ABS2$pweight = ifelse(!is.na(ABS2$w_MN), ABS2$w_MN, ABS2$pweight)
ABS2$pweight = ifelse(!is.na(ABS2$w_PH), ABS2$w_PH, ABS2$pweight)
ABS2$pweight = ifelse(!is.na(ABS2$w_TW4),ABS2$w_TW4,ABS2$pweight)
ABS2$pweight = ifelse(!is.na(ABS2$w_TH), ABS2$w_TH, ABS2$pweight)
ABS2$pweight = ifelse(!is.na(ABS2$w_SG), ABS2$w_SG, ABS2$pweight)
ABS2$pweight = ifelse(!is.na(ABS2$w_KH), ABS2$w_KH, ABS2$pweight)
ABS2$pweight = ifelse(!is.na(ABS2$w_MY), ABS2$w_MY, ABS2$pweight)
ABS2$pweight = ifelse(is.na(ABS2$pweight), 1, ABS2$pweight)
tail(dist_tab(ABS2$pweight))

# urban rural #
tail(dist_tab(ABS2$level3))
ABS2$urban = ifelse(ABS2$level3 == 1, 1, 0)
tail(dist_tab(ABS2$urban))

# country #
dist_tab(ABS2$country)
ABS2 = within(ABS2, {
  country [country == 1] = "01.Japan"
  country [country == 2] = "02.HK"
  country [country == 3] = "03.Korea"
  country [country == 4] = "04.China"
  country [country == 5] = "05.Mongolia"
  country [country == 6] = "06.Philippines"
  country [country == 7] = "07.Taiwan"
  country [country == 8] = "08.Thailand"
  country [country == 9] = "09.Indonesia"
  country [country ==10] = "10.Singapore"
  country [country ==11] = "11.Vietnam"
  country [country ==12] = "12.Cambodia"
  country [country ==13] = "13.Malaysia"
})

# [DV] corruption: local vs central #
ABS2$corrupt_loc = NA
ABS2$corrupt_nat = NA
ABS2 = within(ABS2, {
  corrupt_loc [q114 == 1] = "4.AlmostEveryone"
  corrupt_nat [q115 == 1] = "4.AlmostEveryone"
  corrupt_loc [q114 == 2] = "3.Majority"
  corrupt_nat [q115 == 2] = "3.Majority"
  corrupt_loc [q114 == 3] = "2.Minority"
  corrupt_nat [q115 == 3] = "2.Minority"
  corrupt_loc [q114 == 4] = "1.NoCorruption"
  corrupt_nat [q115 == 4] = "1.NoCorruption"
})

# who remains silent on corruption? #
ABS2$corrupt_loc_silent = ifelse(is.na(ABS2$corrupt_loc), 1, 0)
ABS2$corrupt_nat_silent = ifelse(is.na(ABS2$corrupt_nat), 1, 0)

#### [Cleaning] ABS wave 3 ####
 
ABS3 = as.data.frame(ABS3)
ABS3 = as.data.table(ABS3)
ABS3$year = ifelse(is.na(ABS3$ir9), 2011,
                   as.numeric(as.character(year(as.Date(as.character(ABS3$ir9))))))

ABS3$year = ifelse(ABS3$year < 2010, 2010, ABS3$year)
ABS3$year = ifelse(ABS3$year > 2012, 2012, ABS3$year)
ABS3$year = ifelse(ABS3$country == 4, 2011, ABS3$year)
ABS3$year = ifelse(ABS3$country ==10, 2010, ABS3$year)
dist_tab(ABS3$year)

# RID #
ABS3$RID = seq(1, length(ABS3$year))
ABS3$RID = formatC(ABS3$RID, width = 6, flag = 0)
# Respondent ID #
ABS3$ID = paste(ABS3$wave, ABS3$RID, sep = "_")

# MALE # 
dist_tab(ABS3$se2) 
ABS3$male = NA
ABS3$male = ifelse(as.character(ABS3$se2)=="1", 1, ABS3$male)
ABS3$male = ifelse(as.character(ABS3$se2)=="2", 0, ABS3$male)
dist_tab(ABS3$male) 
dist_tab(ABS3$country [is.na(ABS3$male)])

# AGE & BIRTH YEAR # 
dist_tab(ABS3$se3a) 
ABS3$age = as.numeric(as.character(ABS3$se3a))
ABS3$age = ifelse(ABS3$age <= 96 & ABS3$age >= 16, ABS3$age, NA)
ABS3$birthyear = ABS3$year - as.numeric(ABS3$age)

# Marital # 
ABS3$marital = NA
table(ABS3$marital)
ABS3 = within(ABS3, {
  marital [se4=="1"] = "0.Unmarried"
  marital [se4=="2"|se4=="3"] = "1.Married"
  marital [se4=="6"|se4=="4"|se4=="5"] = "2.Other"
})

# Education #
ABS3$edulevel = NA
ABS3 = within(ABS3, {
  edulevel [trimws(as.character(ABS3$se5))=="1"|
              trimws(as.character(se5))=="2"] = "0.NoOrLittle"
  edulevel [trimws(as.character(se5))=="3"|
              trimws(as.character(se5))=="4"] = "1.Elementary"
  edulevel [trimws(as.character(se5))=="5"|
              trimws(as.character(se5))=="6"] = "2.Middle"
  edulevel [trimws(as.character(se5))=="7"|
              trimws(as.character(se5))=="8"] = "3.High"
  edulevel [trimws(as.character(se5))=="9"|
              trimws(as.character(se5))=="10"]= "4.CollegeAbove"
})

# in years #
ABS3$eduyear = round(ABS3$se5a)
ABS3$eduyear = ifelse(ABS3$eduyear>60, NA, ABS3$eduyear)
dist_tab(ABS3$eduyear)
ABS3 = within(ABS3,{
  eduyear [is.na(eduyear) & trimws(as.character(ABS3$se5))=="1"] = 0
  eduyear [is.na(eduyear) & trimws(as.character(ABS3$se5))=="2"] = 3
  eduyear [is.na(eduyear) & trimws(as.character(ABS3$se5))=="3"] = 6
  eduyear [is.na(eduyear) & trimws(as.character(ABS3$se5))=="4"] = 8
  eduyear [is.na(eduyear) & trimws(as.character(ABS3$se5))=="5"] = 9
  eduyear [is.na(eduyear) & trimws(as.character(ABS3$se5))=="6"] = 11
  eduyear [is.na(eduyear) & trimws(as.character(ABS3$se5))=="7"] = 12
  eduyear [is.na(eduyear) & trimws(as.character(ABS3$se5))=="8"] = 14
  eduyear [is.na(eduyear) & trimws(as.character(ABS3$se5))=="9"] = 16
  eduyear [is.na(eduyear) & trimws(as.character(ABS3$se5))=="10"] =19
})

# Religion #
dist_tab(ABS3$se6) #  
ABS3$religion = "4.Other"
ABS3 = within(ABS3, {
  religion [se6== 90 ] = "0.None"
  religion [se6>= 60 & se6 <=63 ] = "1.Buddhist"
  religion [se6 >=10 & se6 <= 29|
              se6 >=72 & se6 <= 74|
              se6 >=82 & se6 <= 88 ] = "2.Christian"
  religion [se6 >= 40 & se6 <= 42 ] = "3.Islamic"
})

# frequency of religious activity #
ABS3$relig_freq = NA
ABS3 = within(ABS3, {
  relig_freq [se7>= 1 & se7 <=3] = "1.Daily"
  relig_freq [se7>= 4 & se7 <=5] = "2.Weekly"
  relig_freq [se7>= 6 & se7 <=7] = "3.Monthly"
  relig_freq [se7 == 8] = "4.Yearly"
  relig_freq [se7>= 9 & se7 <=89] = "5.Never"
  relig_freq [religion == "0.None" & se7>=90] = "5.Never"
})

#   #
# Number of People in the same household #
dist_tab(ABS3$se8a)
ABS3$HHSIZE = ifelse(ABS3$se8a>=0 & ABS3$se8a<=90, ABS3$se8a, NA)
fivenum(ABS3$HHSIZE)
hist(ABS3$HHSIZE)

# Anual Income (in quintiles - 20%) #
dist_tab(ABS3$se13)
ABS3$inclevel = NA
ABS3 = within(ABS3, {
  inclevel [se13 == 1] = "1st Lowest Quin"
  inclevel [se13 == 2] = "2nd Quintile"
  inclevel [se13 == 3] = "3rd Quintile"
  inclevel [se13 == 4] = "4th Quintile"
  inclevel [se13 == 5] = "5th Top Quin"
})
 
# weight #
tail(dist_tab(ABS3$allweight))
ABS3$pweight = ifelse(is.na(ABS3$allweight), 1, ABS3$allweight)

# urban rural #
tail(dist_tab(ABS3$level3))
ABS3$urban = ifelse(ABS3$level3 == 1, 1, 0)
ABS3 = within(ABS3, {
  country [country == 1] = "01.Japan"
  country [country == 2] = "02.HK"
  country [country == 3] = "03.Korea"
  country [country == 4] = "04.China"
  country [country == 5] = "05.Mongolia"
  country [country == 6] = "06.Philippines"
  country [country == 7] = "07.Taiwan"
  country [country == 8] = "08.Thailand"
  country [country == 9] = "09.Indonesia"
  country [country ==10] = "10.Singapore"
  country [country ==11] = "11.Vietnam"
  country [country ==12] = "12.Cambodia"
  country [country ==13] = "13.Malaysia"
})
dist_tab(ABS3$country)

# [DV] corruption: local vs central #
tail(dist_tab(ABS3$q116))
tail(dist_tab(ABS3$q117))

ABS3$corrupt_loc = NA
ABS3$corrupt_nat = NA 
# please note starting from Wave 3, it is reversely coded.
ABS3 = within(ABS3, {
  corrupt_loc [q116 == 4] = "4.AlmostEveryone"
  corrupt_nat [q117 == 4] = "4.AlmostEveryone"
  corrupt_loc [q116 == 3] = "3.Majority"
  corrupt_nat [q117 == 3] = "3.Majority"
  corrupt_loc [q116 == 2] = "2.Minority"
  corrupt_nat [q117 == 2] = "2.Minority"
  corrupt_loc [q116 == 1] = "1.NoCorruption"
  corrupt_nat [q117 == 1] = "1.NoCorruption"
})

dist_tab(ABS1$corrupt_loc)
dist_tab(ABS2$corrupt_loc)
dist_tab(ABS3$corrupt_loc)
dist_tab(ABS4$corrupt_loc)

# who remains silent on corruption? #
ABS3$corrupt_loc_silent = ifelse(is.na(ABS3$corrupt_loc), 1, 0)
ABS3$corrupt_nat_silent = ifelse(is.na(ABS3$corrupt_nat), 1, 0)
tail(dist_tab(ABS3$corrupt_loc_silent))
tail(dist_tab(ABS3$corrupt_nat_silent))


#### [Cleaning] ABS wave 4 ####

# RID #
ABS4$RID = seq(1, length(ABS4$year))
ABS4$RID = formatC(ABS4$RID, width = 6, flag = 0)
head(ABS4$RID)
# Respondent ID #
ABS4$ID = paste(ABS4$wave, ABS4$RID, sep = "_")
tail(dist_tab(ABS4$ID))

# MALE # 
dist_tab(ABS4$se2) # 17 missing!
ABS4$male = NA
ABS4$male = ifelse(as.character(ABS4$se2)=="Male",   1, ABS4$male)
ABS4$male = ifelse(as.character(ABS4$se2)=="Female", 0, ABS4$male)
dist_tab(ABS4$male) #
dist_tab(ABS4$country [is.na(ABS4$male)]) 

# AGE & BIRTH YEAR # 
dist_tab(ABS4$se3_2) 
ABS4$age = as.numeric(as.character(ABS4$se3_2))
ABS4$age = ifelse(ABS4$age <= 110 & ABS4$age >= 16, ABS4$age, NA)
ABS4$birthyear = ABS4$year - as.numeric(ABS4$age)

# Marital in stata and R. # 
ABS4$marital = NA
table(ABS4$marital)
ABS4 = within(ABS4, {
  marital [as.numeric(se4)==2] = "0.Unmarried"
  marital [as.numeric(se4)==3|
             as.numeric(se4)==4] = "1.Married"
  marital [as.numeric(se4)==5|
             as.numeric(se4)==6] = "2.Other"
})

# Education #
ABS4$edulevel = NA
ABS4 = within(ABS4, {
  edulevel [as.numeric(ABS4$se5)==2|
              as.numeric(se5)==3] = "0.NoOrLittle"
  edulevel [as.numeric(se5)==4|
              as.numeric(se5)==5] = "1.Elementary"
  edulevel [as.numeric(se5)==6|
              as.numeric(se5)==7] = "2.Middle"
  edulevel [as.numeric(se5)==8|
              as.numeric(se5)==9] = "3.High"
  edulevel [as.numeric(se5)==10|
              as.numeric(se5)==11]= "4.CollegeAbove"
})
dist_tab(ABS4$edulevel)

ABS4$eduyear = round(ABS4$se5a)
ABS4$eduyear = ifelse(ABS4$eduyear > 60 | ABS4$eduyear < 0, NA, ABS4$eduyear)
dist_tab(ABS4$eduyear)
ABS4 = within(ABS4,{
  eduyear [is.na(eduyear) & edulevel =="0.NoOrLittle"] = 0
  eduyear [is.na(eduyear) & edulevel =="1.Elementary"] = 6
  eduyear [is.na(eduyear) & edulevel =="2.Middle"] = 9
  eduyear [is.na(eduyear) & edulevel =="3.High"] = 12
  eduyear [is.na(eduyear) & edulevel =="4.CollegeAbove"] = 16
})
dist_tab(ABS4$eduyear)

# Religion #
dist_tab(ABS4$se6) #  
dist_tab(as.numeric(ABS4$se6)) #  
ABS4$religion = "4.Other"
ABS4 = within(ABS4, {
  religion [as.numeric(se6) == 32 ] = "0.None"
  religion [as.numeric(se6)>= 14 &
              as.numeric(se6) <=15 ] = "1.Buddhist"
  religion [as.numeric(se6)>= 3 &
              as.numeric(se6) <=5 |
              as.numeric(se6)>= 22 &
              as.numeric(se6) <=24  ] = "2.Christian"
  religion [as.numeric(se6)>= 10 &
              as.numeric(se6) <=12 ] = "3.Islamic"
})
dist_tab(ABS4$religion)

# frequency of religious activity #
dist_tab(ABS4$se7) #  
dist_tab(as.numeric(ABS4$se7)) #
ABS4$relig_freq = NA
ABS4 = within(ABS4, {
  relig_freq [as.numeric(ABS4$se7) >= 3 & 
                as.numeric(ABS4$se7) <=5] = "1.Daily"
  relig_freq [as.numeric(ABS4$se7) >= 6 & 
                as.numeric(ABS4$se7) <=7] = "2.Weekly"
  relig_freq [as.numeric(ABS4$se7) >= 8 & 
                as.numeric(ABS4$se7) <=8] = "3.Monthly"
  relig_freq [as.numeric(ABS4$se7) >= 9 & 
                as.numeric(ABS4$se7) <=10] = "4.Yearly"
  relig_freq [as.numeric(ABS4$se7) >= 11 & 
                as.numeric(ABS4$se7) <=12] = "5.Never"
  relig_freq [religion == "0.None" ] = "5.Never"
})
dist_tab(ABS4$relig_freq)

# Number of People in the same household #
dist_tab(ABS4$se8a)
ABS4$HHSIZE = ifelse(ABS4$se8a>=0 & ABS4$se8a<=90, ABS4$se8a, NA)
fivenum(ABS4$HHSIZE)
hist(ABS4$HHSIZE)

# Anual Income (in quintiles - 20%) #
ABS4$inclevel = NA
ABS4 = within(ABS4, {
  inclevel [as.numeric(ABS4$se14) == 2] = "1st Lowest Quin"
  inclevel [as.numeric(ABS4$se14) == 3] = "2nd Quintile"
  inclevel [as.numeric(ABS4$se14) == 4] = "3rd Quintile"
  inclevel [as.numeric(ABS4$se14) == 5] = "4th Quintile"
  inclevel [as.numeric(ABS4$se14) == 6] = "5th Top Quin"
})
dist_tab(ABS4$inclevel)

# weight #
tail(dist_tab(ABS4$w_cross))
ABS4$pweight = ifelse(is.na(ABS4$w_cross), 1, ABS4$w_cross)

# urban rural #
tail(dist_tab(ABS4$level))
ABS4$urban = ifelse(ABS4$level == "Urban", 1, 0)

# country = country #
dist_tab(ABS4$country)
ABS4$country1 = NA
ABS4 = within(ABS4, {
  country1 [country =="Japan"]     = "01.Japan"
  country1 [country =="Hong Kong"] = "02.HK"
  country1 [country =="Korea"]     = "03.Korea"
  country1 [country =="China"]     = "04.China"
  country1 [country =="Mongolia"]  = "05.Mongolia"
  country1 [country=="Philippines"]= "06.Philippines"
  country1 [country =="Taiwan"]    = "07.Taiwan"
  country1 [country =="Thailand"]  = "08.Thailand"
  country1 [country =="Indonesia"] = "09.Indonesia"
  country1 [country =="Singapore"] = "10.Singapore"
  country1 [country =="Vietnam"]   = "11.Vietnam"
  country1 [country =="Cambodia"]  = "12.Cambodia"
  country1 [country =="Malaysia"]  = "13.Malaysia"
  country1 [country =="Myanmar"]   = "14.Myanmar"
})
warnings()
dist_tab(ABS4$country1)
ABS4 = subset(ABS4, select=-c(country))
data.table::setnames(ABS4, 
                     old=c("country1"), 
                     new=c("country"))
dist_tab(ABS4$country)

# [DV] corruption: local vs central #
ABS4$corrupt_loc = NA
ABS4$corrupt_nat = NA # please note starting from Wave 3, it is reversely coded.
ABS4 = within(ABS4, {
  corrupt_loc [as.numeric(q117) == 5] = "4.AlmostEveryone"
  corrupt_nat [as.numeric(q118) == 5] = "4.AlmostEveryone"
  corrupt_loc [as.numeric(q117) == 4] = "3.Majority"
  corrupt_nat [as.numeric(q118) == 4] = "3.Majority"
  corrupt_loc [as.numeric(q117) == 3] = "2.Minority"
  corrupt_nat [as.numeric(q118) == 3] = "2.Minority"
  corrupt_loc [as.numeric(q117) == 2] = "1.NoCorruption"
  corrupt_nat [as.numeric(q118) == 2] = "1.NoCorruption"
})

# who remains silent on corruption? #
ABS4$corrupt_loc_silent = ifelse(is.na(ABS4$corrupt_loc), 1, 0)
ABS4$corrupt_nat_silent = ifelse(is.na(ABS4$corrupt_nat), 1, 0)
tail(dist_tab(ABS4$corrupt_loc_silent))
tail(dist_tab(ABS4$corrupt_nat_silent))

#### [Recode] Subjective Social Class ####
dist_tab(ABS1$se017)
dist_tab(ABS2$se017)
dist_tab(ABS3$se12)
dist_tab(ABS4$se12)
class(ABS1$se017)
class(ABS2$se017)
class(ABS3$se12)
class(ABS4$se12)

ABS1$subclass = NA
ABS2$subclass = NA
ABS3$subclass = NA
ABS4$subclass = NA

ABS1 = within(ABS1, {
  subclass [se017 == "5"] = 1 
  subclass [se017 == "5" & eduyear >=11] = 2
  subclass [se017 == "4"] = 3 
  subclass [se017 == "4" & eduyear >=11] = 4 
  subclass [se017 == "3"] = 5 
  subclass [se017 == "3" & eduyear >=11] = 6
  subclass [se017 == "2"] = 7 
  subclass [se017 == "2" & eduyear >=11] = 8 
  subclass [se017 == "1"] = 9 
  subclass [se017 == "1" & eduyear >=11] = 10 
  })

ABS2 = within(ABS2, {
  subclass [se017 == "1"] = 1 
  subclass [se017 == "2"] = 2
  subclass [se017 == "3"] = 3 
  subclass [se017 == "4"] = 4 
  subclass [se017 == "5"] = 5 
  subclass [se017 == "6"] = 6
  subclass [se017 == "7"] = 7 
  subclass [se017 == "8"] = 8 
  subclass [se017 == "9"] = 9 
  subclass [se017 == "10"] = 10 
})

ABS3 = within(ABS3, {
  subclass [se12 == "1"] = 1 
  subclass [se12 == "2"] = 2
  subclass [se12 == "3"] = 3 
  subclass [se12 == "4"] = 4 
  subclass [se12 == "5"] = 5 
  subclass [se12 == "6"] = 6
  subclass [se12 == "7"] = 7 
  subclass [se12 == "8"] = 8 
  subclass [se12 == "9"] = 9 
  subclass [se12 == "10"] = 10 
})

ABS4 = within(ABS4, {
  subclass [se12 == "Lowest status"] = 1 
  subclass [se12 == "2"] = 2
  subclass [se12 == "3"] = 3 
  subclass [se12 == "4"] = 4 
  subclass [se12 == "5"] = 5 
  subclass [se12 == "6"] = 6
  subclass [se12 == "7"] = 7 
  subclass [se12 == "8"] = 8 
  subclass [se12 == "9"] = 9 
  subclass [se12 == "Highest status"] = 10 
})

#### [Recode] General Trust ####

ABS1$trust_gen = NA
ABS1$trust_gen = ifelse(ABS1$q024=="2", 1, 0)
ABS2$trust_gen = NA
ABS2$trust_gen = ifelse(ABS2$q024=="2", 1, 0)

ABS3$trust_gen = NA
ABS3$trust_gen = ifelse(ABS3$q23=="1", 1, 0)
ABS4$trust_gen = NA
ABS4$trust_gen = ifelse(ABS4$q23=="Most people can be trusted", 1, 0)

#### [Recode] Trust Variables ####

# % q8 trust in courts ##

ABS1$trust_court = NA
ABS1 = within(ABS1,{
  trust_court [q007 == "1"] = "1.Not at all"
  trust_court [q007 == "2"] = "2.Not very much"
  trust_court [q007 == "3"] = "3.Some trust"
  trust_court [q007 == "4"] = "4.Very much trust"
})
dist_tab(ABS1$trust_court)

#
ABS2$trust_court = NA
ABS2 = within(ABS2,{
  trust_court [q007 == "1"] = "1.Not at all"
  trust_court [q007 == "2"] = "2.Not very much"
  trust_court [q007 == "3"] = "3.Some trust"
  trust_court [q007 == "4"] = "4.Very much trust"
})
dist_tab(ABS2$trust_court)

# wave 3: reverse code - from no trust to more trust
ABS3$trust_court = NA
ABS3 = within(ABS3,{
  trust_court [q8 == "4"] = "1.Not at all"
  trust_court [q8 == "3"] = "2.Not very much"
  trust_court [q8 == "2"] = "3.Some trust"
  trust_court [q8 == "1"] = "4.Very much trust"
})
dist_tab(ABS3$trust_court)

# wave 4: reverse code - from no trust to more trust
ABS4$trust_court = NA
ABS4 = within(ABS4,{
  trust_court [as.numeric(as.factor(ABS4$q8)) == "5"] = "1.Not at all"
  trust_court [as.numeric(as.factor(ABS4$q8)) == "4"] = "2.Not very much"
  trust_court [as.numeric(as.factor(ABS4$q8)) == "3"] = "3.Some trust"
  trust_court [as.numeric(as.factor(ABS4$q8)) == "2"] = "4.Very much trust"
})
dist_tab(ABS4$trust_court)

# % q9 trust in national gov ##

ABS1$trust_natgov = NA
ABS1 = within(ABS1,{
  trust_natgov [q008 == "1"] = "1.Not at all"
  trust_natgov [q008 == "2"] = "2.Not very much"
  trust_natgov [q008 == "3"] = "3.Some trust"
  trust_natgov [q008 == "4"] = "4.Very much trust"
})
dist_tab(ABS1$trust_natgov)

#
ABS2$trust_natgov = NA
ABS2 = within(ABS2,{
  trust_natgov [q008 == "1"] = "1.Not at all"
  trust_natgov [q008 == "2"] = "2.Not very much"
  trust_natgov [q008 == "3"] = "3.Some trust"
  trust_natgov [q008 == "4"] = "4.Very much trust"
})
dist_tab(ABS2$trust_natgov)

# wave 3: reverse code - from no trust to more trust
ABS3$trust_natgov = NA
ABS3 = within(ABS3,{
  trust_natgov [q9 == "4"] = "1.Not at all"
  trust_natgov [q9 == "3"] = "2.Not very much"
  trust_natgov [q9 == "2"] = "3.Some trust"
  trust_natgov [q9 == "1"] = "4.Very much trust"
})
dist_tab(ABS3$trust_natgov)

# wave 4: reverse code - from no trust to more trust
ABS4$trust_natgov = NA
ABS4 = within(ABS4,{
  trust_natgov [as.numeric(as.factor(ABS4$q9)) == "5"] = "1.Not at all"
  trust_natgov [as.numeric(as.factor(ABS4$q9)) == "4"] = "2.Not very much"
  trust_natgov [as.numeric(as.factor(ABS4$q9)) == "3"] = "3.Some trust"
  trust_natgov [as.numeric(as.factor(ABS4$q9)) == "2"] = "4.Very much trust"
})
dist_tab(ABS4$trust_natgov)

# %q10 trust in political parties ##

ABS1$trust_parties = NA
ABS1 = within(ABS1,{
  trust_parties [q009 == "1"] = "1.Not at all"
  trust_parties [q009 == "2"] = "2.Not very much"
  trust_parties [q009 == "3"] = "3.Some trust"
  trust_parties [q009 == "4"] = "4.Very much trust"
})
dist_tab(ABS1$trust_parties)

#
ABS2$trust_parties = NA
ABS2 = within(ABS2,{
  trust_parties [q009 == "1"] = "1.Not at all"
  trust_parties [q009 == "2"] = "2.Not very much"
  trust_parties [q009 == "3"] = "3.Some trust"
  trust_parties [q009 == "4"] = "4.Very much trust"
})
dist_tab(ABS2$trust_parties)

# wave 3: reverse code - from no trust to more trust
ABS3$trust_parties = NA
ABS3 = within(ABS3,{
  trust_parties [q10 == "4"] = "1.Not at all"
  trust_parties [q10 == "3"] = "2.Not very much"
  trust_parties [q10 == "2"] = "3.Some trust"
  trust_parties [q10 == "1"] = "4.Very much trust"
})
dist_tab(ABS3$trust_parties)

# wave 4: reverse code - from no trust to more trust
dist_tab(as.numeric(as.factor(ABS4$q10)))
ABS4$trust_parties = NA
ABS4 = within(ABS4,{
  trust_parties [as.numeric(as.factor(ABS4$q10)) == "5"] = "1.Not at all"
  trust_parties [as.numeric(as.factor(ABS4$q10)) == "4"] = "2.Not very much"
  trust_parties [as.numeric(as.factor(ABS4$q10)) == "3"] = "3.Some trust"
  trust_parties [as.numeric(as.factor(ABS4$q10)) == "2"] = "4.Very much trust"
})
dist_tab(ABS4$trust_parties)

# %q11 trust in parliaments ##
dist_tab(ABS1$q010)
dist_tab(ABS2$q010)
dist_tab(ABS3$q11)
dist_tab(ABS4$q11)

ABS1$trust_congress = NA
ABS1 = within(ABS1,{
  trust_congress [q010 == "1"] = "1.Not at all"
  trust_congress [q010 == "2"] = "2.Not very much"
  trust_congress [q010 == "3"] = "3.Some trust"
  trust_congress [q010 == "4"] = "4.Very much trust"
})
dist_tab(ABS1$trust_congress)

#
ABS2$trust_congress = NA
ABS2 = within(ABS2,{
  trust_congress [q010 == "1"] = "1.Not at all"
  trust_congress [q010 == "2"] = "2.Not very much"
  trust_congress [q010 == "3"] = "3.Some trust"
  trust_congress [q010 == "4"] = "4.Very much trust"
})
dist_tab(ABS2$trust_congress)

# wave 3: reverse code - from no trust to more trust
ABS3$trust_congress = NA
ABS3 = within(ABS3,{
  trust_congress [q11 == "4"] = "1.Not at all"
  trust_congress [q11 == "3"] = "2.Not very much"
  trust_congress [q11 == "2"] = "3.Some trust"
  trust_congress [q11 == "1"] = "4.Very much trust"
})
dist_tab(ABS3$trust_congress)

# wave 4: reverse code - from no trust to more trust
ABS4$trust_congress = NA
ABS4 = within(ABS4,{
  trust_congress [as.numeric(as.factor(ABS4$q11)) == "5"] = "1.Not at all"
  trust_congress [as.numeric(as.factor(ABS4$q11)) == "4"] = "2.Not very much"
  trust_congress [as.numeric(as.factor(ABS4$q11)) == "3"] = "3.Some trust"
  trust_congress [as.numeric(as.factor(ABS4$q11)) == "2"] = "4.Very much trust"
})
dist_tab(ABS4$trust_congress)

# %q12 trust in civil service ##

ABS1$trust_civilserv = NA
ABS1 = within(ABS1,{
  trust_civilserv [q011 == "1"] = "1.Not at all"
  trust_civilserv [q011 == "2"] = "2.Not very much"
  trust_civilserv [q011 == "3"] = "3.Some trust"
  trust_civilserv [q011 == "4"] = "4.Very much trust"
})
dist_tab(ABS1$trust_civilserv)

#
ABS2$trust_civilserv = NA
ABS2 = within(ABS2,{
  trust_civilserv [q011 == "1"] = "1.Not at all"
  trust_civilserv [q011 == "2"] = "2.Not very much"
  trust_civilserv [q011 == "3"] = "3.Some trust"
  trust_civilserv [q011 == "4"] = "4.Very much trust"
})
dist_tab(ABS2$trust_civilserv)

# wave 3: reverse code - from no trust to more trust
ABS3$trust_civilserv = NA
ABS3 = within(ABS3,{
  trust_civilserv [q12 == "4"] = "1.Not at all"
  trust_civilserv [q12 == "3"] = "2.Not very much"
  trust_civilserv [q12 == "2"] = "3.Some trust"
  trust_civilserv [q12 == "1"] = "4.Very much trust"
})
dist_tab(ABS3$trust_civilserv)

# wave 4: reverse code - from no trust to more trust
ABS4$trust_civilserv = NA
ABS4 = within(ABS4,{
  trust_civilserv [as.numeric(as.factor(ABS4$q12)) == "5"] = "1.Not at all"
  trust_civilserv [as.numeric(as.factor(ABS4$q12)) == "4"] = "2.Not very much"
  trust_civilserv [as.numeric(as.factor(ABS4$q12)) == "3"] = "3.Some trust"
  trust_civilserv [as.numeric(as.factor(ABS4$q12)) == "2"] = "4.Very much trust"
})
dist_tab(ABS4$trust_civilserv)

# %q13 trust in military ##

dist_tab(ABS1$q012)
dist_tab(ABS2$q012)
dist_tab(ABS3$q13)
dist_tab(ABS4$q13)

ABS1$trust_army = NA
ABS1 = within(ABS1,{
  trust_army [q012 == "1"] = "1.Not at all"
  trust_army [q012 == "2"] = "2.Not very much"
  trust_army [q012 == "3"] = "3.Some trust"
  trust_army [q012 == "4"] = "4.Very much trust"
})
dist_tab(ABS1$trust_army)

#
ABS2$trust_army = NA
ABS2 = within(ABS2,{
  trust_army [q012 == "1"] = "1.Not at all"
  trust_army [q012 == "2"] = "2.Not very much"
  trust_army [q012 == "3"] = "3.Some trust"
  trust_army [q012 == "4"] = "4.Very much trust"
})
dist_tab(ABS2$trust_army)

# wave 3: reverse code - from no trust to more trust
ABS3$trust_army = NA
ABS3 = within(ABS3,{
  trust_army [q13 == "4"] = "1.Not at all"
  trust_army [q13 == "3"] = "2.Not very much"
  trust_army [q13 == "2"] = "3.Some trust"
  trust_army [q13 == "1"] = "4.Very much trust"
})
dist_tab(ABS3$trust_army)

# wave 4: reverse code - from no trust to more trust
ABS4$trust_army = NA
ABS4 = within(ABS4,{
  trust_army [as.numeric(as.factor(ABS4$q13)) == "5"] = "1.Not at all"
  trust_army [as.numeric(as.factor(ABS4$q13)) == "4"] = "2.Not very much"
  trust_army [as.numeric(as.factor(ABS4$q13)) == "3"] = "3.Some trust"
  trust_army [as.numeric(as.factor(ABS4$q13)) == "2"] = "4.Very much trust"
})
dist_tab(ABS4$trust_army)

# %q14 trust in police ##
dist_tab(ABS1$q013)
dist_tab(ABS2$q013)
dist_tab(ABS3$q14)
dist_tab(ABS4$q14)

ABS1$trust_police = NA
ABS1 = within(ABS1,{
  trust_police [q013 == "1"] = "1.Not at all"
  trust_police [q013 == "2"] = "2.Not very much"
  trust_police [q013 == "3"] = "3.Some trust"
  trust_police [q013 == "4"] = "4.Very much trust"
})

# 2019.5.2 TRUST in police #
dist_tab(ABS2$q013)
ABS2$trust_police = NA
ABS2 = within(ABS2,{
  trust_police [q013 == "1"] = "1.Not at all"
  trust_police [q013 == "2"] = "2.Not very much"
  trust_police [q013 == "3"] = "3.Some trust"
  trust_police [q013 == "4"] = "4.Very much trust"
})


# 2019.5.2 TRUST in police #
dist_tab(ABS3$q14)
ABS3$trust_police = NA
ABS3 = within(ABS3,{
  trust_police [q14 == "4"] = "1.Not at all"
  trust_police [q14 == "3"] = "2.Not very much"
  trust_police [q14 == "2"] = "3.Some trust"
  trust_police [q14 == "1"] = "4.Very much trust"
})
dist_tab(ABS3$trust_police)


# TRUST in police #
dist_tab(ABS4$q14)
ABS4$trust_police = NA
ABS4 = within(ABS4,{
  trust_police [as.numeric(as.factor(ABS4$q14)) == "5"] = "1.Not at all"
  trust_police [as.numeric(as.factor(ABS4$q14)) == "4"] = "2.Not very much"
  trust_police [as.numeric(as.factor(ABS4$q14)) == "3"] = "3.Some trust"
  trust_police [as.numeric(as.factor(ABS4$q14)) == "2"] = "4.Very much trust"
})
dist_tab(ABS4$trust_police)

# %q15 trust in local gov ##
dist_tab(ABS1$q014)
dist_tab(ABS2$q014)
dist_tab(ABS3$q15)
dist_tab(ABS4$q15)

ABS1$trust_locgov = NA
ABS1 = within(ABS1,{
  trust_locgov [q014 == "1"] = "1.Not at all"
  trust_locgov [q014 == "2"] = "2.Not very much"
  trust_locgov [q014 == "3"] = "3.Some trust"
  trust_locgov [q014 == "4"] = "4.Very much trust"
})
dist_tab(ABS1$trust_locgov)

#
ABS2$trust_locgov = NA
ABS2 = within(ABS2,{
  trust_locgov [q014 == "1"] = "1.Not at all"
  trust_locgov [q014 == "2"] = "2.Not very much"
  trust_locgov [q014 == "3"] = "3.Some trust"
  trust_locgov [q014 == "4"] = "4.Very much trust"
})
dist_tab(ABS2$trust_locgov)

# wave 3: reverse code - from no trust to more trust
ABS3$trust_locgov = NA
ABS3 = within(ABS3,{
  trust_locgov [q15 == "4"] = "1.Not at all"
  trust_locgov [q15 == "3"] = "2.Not very much"
  trust_locgov [q15 == "2"] = "3.Some trust"
  trust_locgov [q15 == "1"] = "4.Very much trust"
})
dist_tab(ABS3$trust_locgov)

# wave 4: reverse code - from no trust to more trust
ABS4$trust_locgov = NA
ABS4 = within(ABS4,{
  trust_locgov [as.numeric(as.factor(ABS4$q15)) == "5"] = "1.Not at all"
  trust_locgov [as.numeric(as.factor(ABS4$q15)) == "4"] = "2.Not very much"
  trust_locgov [as.numeric(as.factor(ABS4$q15)) == "3"] = "3.Some trust"
  trust_locgov [as.numeric(as.factor(ABS4$q15)) == "2"] = "4.Very much trust"
})
dist_tab(ABS4$trust_locgov)

# trust in newspaper #
dist_tab(ABS1$q015)
dist_tab(ABS2$q015)
dist_tab(ABS3$q16)
dist_tab(ABS4$q16)

ABS1$trust_newspaper = NA
ABS1 = within(ABS1,{
  trust_newspaper [q015 == "1"] = "1.Not at all"
  trust_newspaper [q015 == "2"] = "2.Not very much"
  trust_newspaper [q015 == "3"] = "3.Some trust"
  trust_newspaper [q015 == "4"] = "4.Very much trust"
})
dist_tab(ABS1$trust_newspaper)

#
ABS2$trust_newspaper = NA
ABS2 = within(ABS2,{
  trust_newspaper [q015 == "1"] = "1.Not at all"
  trust_newspaper [q015 == "2"] = "2.Not very much"
  trust_newspaper [q015 == "3"] = "3.Some trust"
  trust_newspaper [q015 == "4"] = "4.Very much trust"
})
dist_tab(ABS2$trust_newspaper)

# wave 3: reverse code - from no trust to more trust
ABS3$trust_newspaper = NA
ABS3 = within(ABS3,{
  trust_newspaper [q16 == "4"] = "1.Not at all"
  trust_newspaper [q16 == "3"] = "2.Not very much"
  trust_newspaper [q16 == "2"] = "3.Some trust"
  trust_newspaper [q16 == "1"] = "4.Very much trust"
})
dist_tab(ABS3$trust_newspaper)

# wave 4: reverse code - from no trust to more trust
ABS4$trust_newspaper = NA
ABS4 = within(ABS4,{
  trust_newspaper [as.numeric(as.factor(ABS4$q16)) == "5"] = "1.Not at all"
  trust_newspaper [as.numeric(as.factor(ABS4$q16)) == "4"] = "2.Not very much"
  trust_newspaper [as.numeric(as.factor(ABS4$q16)) == "3"] = "3.Some trust"
  trust_newspaper [as.numeric(as.factor(ABS4$q16)) == "2"] = "4.Very much trust"
})
dist_tab(ABS4$trust_newspaper)

# trust in TV #
dist_tab(ABS1$q016)
dist_tab(ABS2$q016)
dist_tab(ABS3$q17)
dist_tab(ABS4$q17)


ABS1$trust_TV = NA
ABS1 = within(ABS1,{
  trust_TV [q016 == "1"] = "1.Not at all"
  trust_TV [q016 == "2"] = "2.Not very much"
  trust_TV [q016 == "3"] = "3.Some trust"
  trust_TV [q016 == "4"] = "4.Very much trust"
})
dist_tab(ABS1$trust_TV)

#
ABS2$trust_TV = NA
ABS2 = within(ABS2,{
  trust_TV [q016 == "1"] = "1.Not at all"
  trust_TV [q016 == "2"] = "2.Not very much"
  trust_TV [q016 == "3"] = "3.Some trust"
  trust_TV [q016 == "4"] = "4.Very much trust"
})
dist_tab(ABS2$trust_TV)

# wave 3: reverse code - from no trust to more trust
ABS3$trust_TV = NA
ABS3 = within(ABS3,{
  trust_TV [q17 == "4"] = "1.Not at all"
  trust_TV [q17 == "3"] = "2.Not very much"
  trust_TV [q17 == "2"] = "3.Some trust"
  trust_TV [q17 == "1"] = "4.Very much trust"
})
dist_tab(ABS3$trust_TV)

# wave 4: reverse code - from no trust to more trust
ABS4$trust_TV = NA
ABS4 = within(ABS4,{
  trust_TV [as.numeric(as.factor(ABS4$q17)) == "5"] = "1.Not at all"
  trust_TV [as.numeric(as.factor(ABS4$q17)) == "4"] = "2.Not very much"
  trust_TV [as.numeric(as.factor(ABS4$q17)) == "3"] = "3.Some trust"
  trust_TV [as.numeric(as.factor(ABS4$q17)) == "2"] = "4.Very much trust"
})
dist_tab(ABS4$trust_TV)

# trust in election #
dist_tab(ABS1$q017)
dist_tab(ABS2$q017)
dist_tab(ABS3$q18)
dist_tab(ABS4$q18)


ABS1$trust_elect = NA
ABS1 = within(ABS1,{
  trust_elect [q017 == "1"] = "1.Not at all"
  trust_elect [q017 == "2"] = "2.Not very much"
  trust_elect [q017 == "3"] = "3.Some trust"
  trust_elect [q017 == "4"] = "4.Very much trust"
})
dist_tab(ABS1$trust_elect)

#
ABS2$trust_elect = NA
ABS2 = within(ABS2,{
  trust_elect [q017 == "1"] = "1.Not at all"
  trust_elect [q017 == "2"] = "2.Not very much"
  trust_elect [q017 == "3"] = "3.Some trust"
  trust_elect [q017 == "4"] = "4.Very much trust"
})
dist_tab(ABS2$trust_elect)

# wave 3: reverse code - from no trust to more trust
ABS3$trust_elect = NA
ABS3 = within(ABS3,{
  trust_elect [q18 == "4"] = "1.Not at all"
  trust_elect [q18 == "3"] = "2.Not very much"
  trust_elect [q18 == "2"] = "3.Some trust"
  trust_elect [q18 == "1"] = "4.Very much trust"
})
dist_tab(ABS3$trust_elect)

# wave 4: reverse code - from no trust to more trust
ABS4$trust_elect = NA
ABS4 = within(ABS4,{
  trust_elect [as.numeric(as.factor(ABS4$q18)) == "5"] = "1.Not at all"
  trust_elect [as.numeric(as.factor(ABS4$q18)) == "4"] = "2.Not very much"
  trust_elect [as.numeric(as.factor(ABS4$q18)) == "3"] = "3.Some trust"
  trust_elect [as.numeric(as.factor(ABS4$q18)) == "2"] = "4.Very much trust"
})
dist_tab(ABS4$trust_elect)

# trust in NGOs #
dist_tab(ABS1$q018)
dist_tab(ABS2$q018)
dist_tab(ABS3$q19)
dist_tab(ABS4$q19)


ABS1$trust_NGO = NA
ABS1 = within(ABS1,{
  trust_NGO [q018 == "1"] = "1.Not at all"
  trust_NGO [q018 == "2"] = "2.Not very much"
  trust_NGO [q018 == "3"] = "3.Some trust"
  trust_NGO [q018 == "4"] = "4.Very much trust"
})
dist_tab(ABS1$trust_NGO)

#
ABS2$trust_NGO = NA
ABS2 = within(ABS2,{
  trust_NGO [q018 == "1"] = "1.Not at all"
  trust_NGO [q018 == "2"] = "2.Not very much"
  trust_NGO [q018 == "3"] = "3.Some trust"
  trust_NGO [q018 == "4"] = "4.Very much trust"
})
dist_tab(ABS2$trust_NGO)

# wave 3: reverse code - from no trust to more trust
ABS3$trust_NGO = NA
ABS3 = within(ABS3,{
  trust_NGO [q19 == "4"] = "1.Not at all"
  trust_NGO [q19 == "3"] = "2.Not very much"
  trust_NGO [q19 == "2"] = "3.Some trust"
  trust_NGO [q19 == "1"] = "4.Very much trust"
})
dist_tab(ABS3$trust_NGO)

# wave 4: reverse code - from no trust to more trust
ABS4$trust_NGO = NA
ABS4 = within(ABS4,{
  trust_NGO [as.numeric(as.factor(ABS4$q19)) == "5"] = "1.Not at all"
  trust_NGO [as.numeric(as.factor(ABS4$q19)) == "4"] = "2.Not very much"
  trust_NGO [as.numeric(as.factor(ABS4$q19)) == "3"] = "3.Some trust"
  trust_NGO [as.numeric(as.factor(ABS4$q19)) == "2"] = "4.Very much trust"
})
dist_tab(ABS4$trust_NGO)



#### 2019.05.07 Political Involvement ####

# How much are you interested in politics? #
dist_tab(ABS1$q056)
dist_tab(ABS2$q056)
dist_tab(ABS3$q43)
dist_tab(ABS4$q44)

# How often do you follow news? #
dist_tab(ABS1$q057)
dist_tab(ABS2$q057)
dist_tab(ABS3$q44)
dist_tab(ABS4$q45)

ABS1$polit_interest = NA
ABS1$polit_infofreq = NA
ABS1 = within(ABS1, {
  polit_interest [q056 == "1"] = "1.Not at all"
  polit_interest [q056 == "2"] = "2.Not very"
  polit_interest [q056 == "3"] = "3.Somewhat"
  polit_interest [q056 == "4"] = "4.Very Interested"
  polit_infofreq [q057 == "2"] = "1.Never"
  polit_infofreq [q057 == "3"] = "2.Monthly"
  polit_infofreq [q057 == "4"] = "3.Once-Twice Weekly"
  polit_infofreq [q057 == "5"] = "4.Several Times Weekly"
  polit_infofreq [q057 == "6"] = "5.Daily"
})
ABS1$polit_interest = as.factor(ABS1$polit_interest)
ABS1$polit_infofreq = as.factor(ABS1$polit_infofreq)
dist_tab(ABS1$polit_interest)
dist_tab(ABS1$polit_infofreq)

ABS2$polit_interest = NA
ABS2$polit_infofreq = NA
ABS2 = within(ABS2, {
  polit_interest [q056 == "1"] = "1.Not at all"
  polit_interest [q056 == "2"] = "2.Not very"
  polit_interest [q056 == "3"] = "3.Somewhat"
  polit_interest [q056 == "4"] = "4.Very Interested"
  polit_infofreq [q057 == "2"] = "1.Never"
  polit_infofreq [q057 == "3"] = "2.Monthly"
  polit_infofreq [q057 == "4"] = "3.Once-Twice Weekly"
  polit_infofreq [q057 == "5"] = "4.Several Times Weekly"
  polit_infofreq [q057 == "6"] = "5.Daily"
})
ABS2$polit_interest = as.factor(ABS2$polit_interest)
ABS2$polit_infofreq = as.factor(ABS2$polit_infofreq)
dist_tab(ABS2$polit_interest)
dist_tab(ABS2$polit_infofreq)

ABS3$polit_interest = NA
ABS3$polit_infofreq = NA
ABS3 = within(ABS3, {
  polit_interest [q43 == "1"] = "1.Not at all"
  polit_interest [q43 == "2"] = "2.Not very"
  polit_interest [q43 == "3"] = "3.Somewhat"
  polit_interest [q43 == "4"] = "4.Very Interested"
  polit_infofreq [q44 == "5"] = "1.Never"
  polit_infofreq [q44 == "4"] = "2.Monthly"
  polit_infofreq [q44 == "3"] = "3.Once-Twice Weekly"
  polit_infofreq [q44 == "2"] = "4.Several Times Weekly"
  polit_infofreq [q44 == "1"] = "5.Daily" 
  # please note here it is reversed, compared with ABS 1&2
})

ABS3$polit_interest = as.factor(ABS3$polit_interest)
ABS3$polit_infofreq = as.factor(ABS3$polit_infofreq)
dist_tab(ABS3$polit_interest)
dist_tab(ABS3$polit_infofreq)

ABS4$polit_interest = NA
ABS4$polit_infofreq = NA
ABS4 = within(ABS4, {
  polit_interest [trimws(as.character(q44)) == "Not at all interested"] = "1.Not at all"
  polit_interest [trimws(as.character(q44)) == "Not very interested"] = "2.Not very"
  polit_interest [trimws(as.character(q44)) == "Somewhat interested"] = "3.Somewhat"
  polit_interest [trimws(as.character(q44)) == "Very interested"] = "4.Very Interested"
  polit_infofreq [trimws(as.character(q45)) == "Practically never"] = "1.Never"
  polit_infofreq [trimws(as.character(q45)) == "Not even once a week"] = "2.Monthly"
  polit_infofreq [trimws(as.character(q45)) == "Once or twice a week"] = "3.Once-Twice Weekly"
  polit_infofreq [trimws(as.character(q45)) == "Several times a week"] = "4.Several Times Weekly"
  polit_infofreq [trimws(as.character(q45)) == "Everyday"] = "5.Daily" 
  # please note here it is reversed from ABS1/2
})

ABS4$polit_interest = as.factor(ABS4$polit_interest)
ABS4$polit_infofreq = as.factor(ABS4$polit_infofreq)
dist_tab(ABS4$polit_interest)
dist_tab(ABS4$polit_infofreq)

#### [Combine] Merge Four Waves into One Data File ####

ABS1 = as.data.frame(ABS1) # only data.frame supports [i,j] locating
ABS2 = as.data.frame(ABS2) # data.table does not allow data[,t]
ABS3 = as.data.frame(ABS3)
ABS4 = as.data.frame(ABS4)

varlist = c("wave", "year", "ID", "male", 
            "age", "birthyear", "country",
            "marital", "edulevel", "eduyear", 
            "religion", "relig_freq", "subclass",
            "HHSIZE", "inclevel", "pweight", 
            "urban", "corrupt_loc", "corrupt_nat", 
            "trust_natgov", "trust_locgov", 
            "trust_police", "trust_newspaper", "trust_TV",
            "trust_elect", "trust_NGO", "freq_web", "trust_gen",
            "freq_pol_news", "polit_interest", "polit_infofreq")

# ABS select the relevant vars only
ABS1_clean = data.frame(ABS1)[, varlist]
ABS2_clean = data.frame(ABS2)[, varlist]
ABS3_clean = data.frame(ABS3)[, varlist]
ABS4_clean = data.frame(ABS4)[, varlist]

# display class of vars; convert all string vars to factor#
sapply(ABS1_clean, class)
t = which(sapply(ABS1_clean, is.character))
ABS1_clean[,t] = lapply(ABS1_clean[,t], as.factor)
t = which(sapply(ABS1_clean, is.factor))
ABS1_clean[,t] = lapply(ABS1_clean[,t], as.character)
sapply(ABS1_clean, class)

# convert all char to factor #
sapply(ABS2_clean, class)
t = which(sapply(ABS2_clean, is.character))
ABS2_clean[,t] = lapply(ABS2_clean[,t], as.factor)
t = which(sapply(ABS2_clean, is.factor))
ABS2_clean[,t] = lapply(ABS2_clean[,t], as.character)
sapply(ABS2_clean, class)

# convert all char to factor #
sapply(ABS3_clean, class)
t = which(sapply(ABS3_clean, is.character))
ABS3_clean[,t] = lapply(ABS3_clean[,t], as.factor)
t = which(sapply(ABS3_clean, is.factor))
ABS3_clean[,t] = lapply(ABS3_clean[,t], as.character)
sapply(ABS3_clean, class)

# convert all char to factor #
sapply(ABS4_clean, class)
t = which(sapply(ABS4_clean, is.character))
ABS4_clean[,t] = lapply(ABS4_clean[,t], as.factor)
t = which(sapply(ABS4_clean, is.factor))
ABS4_clean[,t] = lapply(ABS4_clean[,t], as.character)
sapply(ABS4_clean, class)

#### Merge & Check ####

# combine #
ABS_All = rbind(ABS1_clean, ABS2_clean, ABS3_clean, ABS4_clean)

# checking if there are typos in factor vars (inconsistency) #
t = which(sapply(ABS_All, is.character))
ABS_All[,t] = lapply(ABS_All[,t], as.factor)
tail_table = function(d) {
  tail(dist_tab(d))
}
sapply(ABS_All, tail_table)

# before MI, make sure the variable types are correct #
ABS_All$year   = as.factor(as.character(ABS_All$year))
ABS_All$male   = as.factor(as.character(ABS_All$male))
ABS_All$age    = as.factor(as.character(ABS_All$age))
ABS_All$eduyear= as.factor(as.character(ABS_All$eduyear))
ABS_All$edulevel=as.factor(as.character(ABS_All$edulevel))
ABS_All$urban  = as.factor(as.character(ABS_All$urban))
ABS_All$HHSIZE = as.factor(as.character(ABS_All$HHSIZE))

#### Amelia: Multiple Imputation ####

# Before Amelia #
varlist = c("wave", "year", "ID", "male", "age", 
            "marital", "edulevel", "eduyear", "urban",
            "religion", "country", "HHSIZE", "subclass",
            "inclevel", "pweight",  "freq_web", "freq_pol_news", 
            "trust_locgov", "trust_natgov", "trust_police", 
            "trust_gen", "corrupt_loc", "corrupt_nat", 
            "polit_interest", "polit_infofreq")
ABS_All2 = ABS_All[, varlist]

####  2020.8.22 Before Amelia: Show descriptive ####

toNum = function(i) {as.numeric(as.character(i))}
ABS_All2$birth = toNum(ABS_All2$year) - toNum(ABS_All2$age) 
ABS_All2$cohort = NA
ABS_All2 = within(ABS_All2, {
  cohort [birth <= 2020] = "1990-now"
  cohort [birth <= 1989] = "1980-89"
  cohort [birth <= 1979] = "1970-79"
  cohort [birth <= 1969] = "1960-69"
  cohort [birth <= 1959] = "1950-59"
  cohort [birth <= 1949] = "1940-49"
  cohort [birth <= 1939] = "1900-39"
})

#### [check] cronbach's alpha across DV items ####

# alpha #
ABS_All2$var01 = as.numeric(ABS_All2$corrupt_nat)
ABS_All2$var02 = as.numeric(ABS_All2$corrupt_loc)
VAR1 = c("var01", "var02")
a = psy::cronbach(ABS_All2[VAR1])

#### [Check] Missings ####

length(ABS_All2$wave [is.na(ABS_All2$wave)])
CheckMissing = function(i) {length(i[is.na(i)])}
sapply(ABS_All2, CheckMissing)/72118

#### [MI step] Amelia ####

set.seed(1234)
a.out = amelia(ABS_All2, 
               m = 5, max.resample = 50,
               idvars=c("ID", "country", "wave", "year", "pweight"),
               noms=c("male", "age", "marital", "edulevel", "eduyear", 
                      "religion", "HHSIZE", "inclevel", "urban", 
                      "subclass", "trust_locgov", "trust_natgov", 
                      "trust_police", "trust_gen", "corrupt_loc", 
                      "corrupt_nat", "freq_web", "freq_pol_news", 
                      "polit_interest", "polit_infofreq"),
               tolerance = 0.01, incheck=F)

# ABSclean = na.exclude(a.out$imputations$imp1)

#### [Write/Read Data] & Change Variable Type ####

# write_rds(ABSclean, "D:/2020_IJPOR_ABSclean.rds", compress = "gz")
ABSclean = read_rds("user_path/2020_IJPOR_ABSclean.rds")

# convert back to numbers #
library(tidyverse)
ABSclean$eduyear = as.numeric(as.character(ABSclean$eduyear))
ABSclean$HHSIZE  = as.numeric(as.character(ABSclean$HHSIZE))
ABSclean$year    = as.numeric(as.character(ABSclean$year))
ABSclean$age     = as.numeric(as.character(ABSclean$age))
ABSclean$eduyear = ifelse(ABSclean$eduyear>20, 20, ABSclean$eduyear)

# convert trust vars to numeric #
varlist  = grep("trust_", names(ABSclean), value=TRUE)
varlist2 = setdiff(names(ABSclean), varlist)
ABS1 = ABSclean[,varlist] %>% lapply(as.numeric)
ABS1 = do.call(cbind.data.frame, ABS1)
ABS2 = ABSclean[,varlist2]
ABSclean = cbind(ABS2, ABS1)

# convert corrupt vars to numeric: hi num = severe corruption #
ABSclean$corrupt_sum = as.numeric(ABSclean$corrupt_loc) + 
  as.numeric(ABSclean$corrupt_nat) - 2

# convert pol_trust vars to numeric: hi num = hi trust #
dist_tab(ABSclean$trust_locgov)
dist_tab(ABSclean$trust_natgov)
ABSclean$trustgov_sum = ABSclean$trust_locgov + 
  ABSclean$trust_natgov - 2 
dist_tab(ABSclean$trustgov_sum)

# age clean #
ABSclean$age = ifelse(ABSclean$age<18, 18, ABSclean$age)
ABSclean$age = ifelse(ABSclean$age>99, 99, ABSclean$age)

# 2020.03.29 Birth Year #
ABSclean$birth = ABSclean$year - ABSclean$age

# change singapore 2015 to 2014; change China 2016 to 2015
ABSclean = within(ABSclean, {
  year [year == 2016 & country == "04.China"]     = 2015
  year [year == 2015 & country == "10.Singapore"] = 2014
})
ABSclean$CW = paste0(ABSclean$country, sep = "_", ABSclean$wave)
ABSclean$CY = paste0(ABSclean$country, sep = "_", ABSclean$year)
ABSclean$CW = factor(ABSclean$CW)
ABSclean$CY = factor(ABSclean$CY)

#### [Aggregate Data] CYdata cleaning and analysis ####

# clean #
CYdata = readxl::read_xlsx("user_path/CY.xlsx")
names(CYdata) = c("CY", "GDP", "GDP_RATE", 
                  "GDPPC", "Free_PR", "Free_CL", 
                  "Free_STATUS", "GINI_DIST", "GINI_MKT",
                  "SIZE", "POP", "DENSITY", "CPI")

# check inconsistency in macro/micro data #
L1 = unique(CYdata$CY)
L2 = unique(ABSclean$CY)
setdiff(L1, L2)
setdiff(L2, L1) # no consistency

# merge national level and individual data # 
CYdata$CY = factor(CYdata$CY)
ABSmerge = merge(ABSclean, CYdata, by = "CY")
rm(list = setdiff(ls(), c("ABSclean", "CYdata", "ABSmerge")))

# change type of variables #
sapply(ABSmerge, class)
ABSmerge$CW = factor(ABSmerge$CW)
ABSmerge$CY = factor(ABSmerge$CY)

# create cohort #
ABSmerge$cohort = NA
ABSmerge = within(ABSmerge, {
  cohort [birth>1905 & birth <=1945] = "1906-1945"
  cohort [birth>1945 & birth <=1955] = "1946-1955"
  cohort [birth>1955 & birth <=1965] = "1956-1965"
  cohort [birth>1965 & birth <=1975] = "1966-1975"
  cohort [birth>1975 & birth <=1985] = "1976-1985"
  cohort [birth>1985] = "1986-"
})
ABSmerge$cohort = factor(ABSmerge$cohort)

#### [Table 1] Descriptive Analysis ####

# 01 Comparison
library(pacman)
pacman::p_load(tableone, knitr, kableExtra)

# variable list #
t1 = c("wave", "country", "age", "male", 
       "religion", "HHSIZE", "inclevel", 
       "eduyear", "urban", "corrupt_sum",
       "trust_natgov")
t2 = c("wave", "country", "male", 
       "religion", "inclevel", "urban")

# generate Table 1 #
table1 = CreateTableOne(vars = t1, 
                        factorVars = t2, 
                        data = ABSmerge)
# outputting table 1 #
table1 = print(table1, printToggle = FALSE, noSpaces = TRUE)

#### [Model] with CPI index conrolled ####

rm(list = setdiff(ls(), c("ABSclean", 
                          "CYdata", 
                          "ABSmerge")))
t1 = lmerControl(calc.derivs = FALSE, 
                 boundary.tol = 1e-3, 
                 optCtrl=list(maxfun=20))

# models #
n1 = lmer(trust_natgov ~ age + male + religion + marital +
            HHSIZE + inclevel + 
            log(GDPPC+1) + GINI_DIST + Free_STATUS + CPI +
            eduyear + corrupt_sum + urban +
            (1|CY), data = ABSmerge, 
          weights = pweight, control = t1, REML = F)

n2 = lmer(trust_natgov ~ age + male + religion + marital +
            HHSIZE + inclevel + 
            log(GDPPC+1) + GINI_DIST + Free_STATUS + CPI +
            eduyear * corrupt_sum + urban +
            (1|CY), data = ABSmerge, 
          weights = pweight, control = t1, REML = F)

n3 = lmer(trust_natgov ~ age + male + religion + marital +
            HHSIZE + inclevel + 
            log(GDPPC+1) + GINI_DIST + Free_STATUS + CPI +
            eduyear * corrupt_sum * urban +
            (1|CY), data = ABSmerge, 
          weights = pweight, control = t1, REML = F)

texreg::htmlreg(list(n1, n2, n3), digits = 3, file="d:/Table2.html")

#### [Fig 2.] Figures / Effect Plots and GGsave ####

URBAN.labs = c("Rural", "Urban")
names(URBAN.labs) = c("0", "1")

# theme # 
theme_user = theme_bw()+
  theme(plot.title = element_text(lineheight=2, hjust=.5, vjust=1.5),
        title = element_text(size=rel(1.5), family="Century"), 
        axis.text.x = element_text(size=rel(1.6), family="Century"),
        axis.text.y = element_text(size=rel(1.6), family="Century"),
        legend.text = element_text(size=rel(1.3), family="Century"),
        strip.text.x = element_text(size = rel(1.4), colour = "grey82"),
        legend.direction = "vertical",
        legend.key = element_rect(size = 1.3, color = 'white'),
        legend.background = element_rect(fill="grey95", size=.5),
        legend.position = c(0.19,0.88),
        strip.background = element_rect(fill="grey76"),
        legend.key.size = unit(0.65, "cm"))

# nat #
eff3 = effect('eduyear * corrupt_sum * urban', 
              mod = n3,  
              xlevels=list(eduyear=seq(0, 20, 2),
                           corrupt_sum=seq(0,6,6)), 
              confidence.level = 0.95)
x = as.data.frame(eff3)
x

#
pp1 = ggplot(x, aes(x = eduyear, y = fit, 
                    color=factor(corrupt_sum), 
                    shape=factor(corrupt_sum), 
                    group=factor(corrupt_sum))) + 
  geom_line(aes(size=0.02, 
                linetype=factor(corrupt_sum))) + 
  geom_point(aes(size=0.05)) + 
  geom_errorbar(aes(ymin=fit-se, ymax=fit+se), width=0.12) +
  facet_wrap(. ~ urban, ncol = 2, 
             labeller = labeller(urban = URBAN.labs)) + 
  scale_color_manual(values = c("coral2", "black"),
                     name="Individual Perception of Corruption",
                     labels=c("Not Severe = 0", "Severe = 6"))  +
  scale_shape_manual(values = c(18, 3),
                     name="Individual Perception of Corruption",
                     labels=c("Not Severe = 0", "Severe = 6"))  +
  scale_linetype_manual(values = c("dotdash", "solid"),
                        name="Individual Perception of Corruption",
                        labels=c("Not Severe = 0", "Severe = 6"))  +
  scale_y_continuous(breaks = pretty_breaks(n = 12)) + 
  guides(size = "none", 
         linetype = guide_legend(override.aes = list(size=1.04)),
         shape = guide_legend(override.aes = list(size=1.15))) +
  ylab("Trust in Central Government") + 
  xlab("Years of Education") + theme_user + 
  theme(legend.position=c(0.87, 0.51),
        strip.text.x = element_text(size = rel(1.6), 
                                    family="Century", 
                                    colour = "Black"))

png("d:/Figure1.png", 
    height = 24, width = 46, units = 'cm', 
    type = c("cairo-png"), res = 900)
pp1
dev.off()

# Ending #

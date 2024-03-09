# Sun 7/30/23
# Running Tree Dataset Prep for O'Harrell Canyon
# Stand Metrics, prepping OH1, 2, and 3 for DChurchill_ICO and GPT_maps scripts
library(dplR)
library(tidyverse)

# Replacing IS instances with OH

#1 Started by making the final .rwl collection for Indiana Summit, IS_2022 (in Images and POS Files > Indiana Summit).
# add all .pos files to a rwl, excluding original core if a duplicate was made 
# Read in the files. Need rwl collection from OH, and core height data.

OH_rwl = read.rwl("OH_2022.rwl")
#OH_rwl <- subset(OH_rwl, select = -c(OH115, OH11a, OH271, OH3128, OH264)) # N = 81
OH_rwl <- subset(OH_rwl, select = -c(OH115, OH11a, OH271, OH3128, OH264, OH2117, OH2_502)) # N = 79
# 2/17/24 removed an additional 2 cores not found in core_height sheet

#setwd("S:/FacultyData/LATIMER/LATIMERShared/Paige/PaigeCores/Spreadsheets")
core_height = read.csv("coredata_PK.csv")

# 3. dplR can extract establishment year and age for each core
#rwl.stats(OH_rwl)

# BUT these numbers don't include pith correction; created a .csv file to read in

OH_pith = read.csv("OH_pith.csv") # do not know why this was so hard; trying to do them as a set caused vector length error
# e.g. Warning message:
#   In OH_pith$Core != c("OH115", "OH11a", "OH271", "OH3128", "OH264") :
#   longer object length is not a multiple of shorter object length
 OH_pith <- OH_pith[OH_pith$Core != "OH115",]
 OH_pith <- OH_pith[OH_pith$Core != "OH11a",]
 OH_pith <- OH_pith[OH_pith$Core != "OH271",]
 OH_pith <- OH_pith[OH_pith$Core != "OH3128",]
 OH_pith <- OH_pith[OH_pith$Core != "OH264",]
 # now includes just good cores and just PIJE
 OH_pith <- OH_pith[OH_pith$Core != "OH2_502",]
 OH_pith <- OH_pith[OH_pith$Core != "OH2117",] # remove the two cores not in core_height

# Need a species-specific height correction function
# Check spp distributions
tree_data <- read.csv("Treedata_9-3_Em_DB.csv")
# fix data entry error in row 192
tree_data[192,7] = 36.7
tree_data <- tree_data %>% 
  filter(sqrt(((X)^2)+((Y)^2))<sqrt(10000/pi))

#filter(X>((-1)*sqrt(10000/pi))&X<sqrt(10000/pi)) %>% 
#filter(Y>((-1)*sqrt(10000/pi))&X<sqrt(10000/pi))

# How many of each species are there? ____________________
OH_trees <- tree_data[tree_data$Site=="OH",]
names(OH_trees)[5] <- "dbh"
unique(OH_trees$Spec)
# [1] "PIJE"  "JUN"   "ABCO"  "CELE"  "JUGR"  "PICO"  "PIFL"  "CELE " "PIJE " "BEOC" 
OH_trees <- OH_trees %>% 
  filter(Spec != "CELE") %>% 
  filter(Spec != "CELE ") %>%  
  filter(Spec != "BEOC")
OH_trees$Spec <- gsub("PIJE ", "PIJE", OH_trees$Spec)
OH_trees$Spec <- gsub("PIFL", "PIJE", OH_trees$Spec)
OH_trees$Spec <- gsub("JUN", "JUGR", OH_trees$Spec)
unique(OH_trees$Spec)
#[1] "PIJE" "JUGR" "ABCO" "PICO"

# OH_count <- OH_trees %>% 
#   group_by(Spec) %>% 
#   summarize(abundance = sum(!is.na(dbh)))
# ggplot(data=OH_count)+
#   geom_bar(stat="identity", mapping = aes(x=Spec, y=abundance))
# ggplot(OH_trees, aes(x=dbh, fill=Spec))+
#   geom_histogram() + ggtitle("Species at OHarrell Canyon")
# PIJE 59.6%, JUGR 19.7%, ABCO 15.3%, PICO 5.4%

## Species-specific core height correction
# ABCO: see Taylor and Halpern 1991; reported ages at coring height due to variable growth rates
# JUGR: see below for Landis and Bailey growth model; no pith correction
# PICO: in NZ alpine (1350m = ~4500ft), PICO stems est. 4y to 20cm. --> 5y to 25cm (avg at OH)
# data from Tomiolo et al. 2016
# added this correction in spreadsheet coredata_OH2023.csv

# PIJE: same methods as IS, using OH data______________
# GR5fun = function(x){
#   non_na_widths = x[which(!is.na(x))]
#   first_5 = non_na_widths[1:5]
#   return(sum(first_5))
# }
GR15fun = function(x){
  non_na_widths = x[which(!is.na(x))]
  first_15 = non_na_widths[1:15]
  return(mean(first_15))
}

#OH_correction <- cbind(cbind(rwl.stats(OH_rwl), apply(OH_rwl,2,GR15fun)), OH_pith)
OH_correction <- cbind(cbind(rwl.stats(OH_rwl), apply(OH_rwl,2,GR15fun)), OH_pith)


## 4.1 Incorporate core height. First, fix core_height Code names to match .rwl file
core_height$Code[core_height$Code=="OH1501"] <- "OH1_501"
# core_height$Code[core_height$Code=="OH2502"] <- "OH2_502"
core_height$Code[core_height$Code=="OH1503"] <- "OH1_503"
core_height$Code[core_height$Code=="OH2504"] <- "OH2_504"
core_height$Code[core_height$Code=="OH2505"] <- "OH2_505"
core_height$Code[core_height$Code=="OH3506"] <- "OH3_506"

OH_correction$dbh <- core_height$DBH[match(OH_correction$series, core_height$Code)]
OH_correction$Spec <- core_height$Spec[match(OH_correction$series, core_height$Code)]
OH_correction$HT <- core_height$Ht[match(OH_correction$series, core_height$Code)]
#names(OH_correction)[11] <- "GR5"
names(OH_correction)[11] <- "GR15"

## 5. Age correction function: PIJE from Fraver 
# JUGR from Landis and Bailey 2006;
# ABCO from our O'Harrell 2023 data, no pith ht correction;
# PICO from O'Harrell 2023 data, pith ht correction from Tomiolo 2016

# Other caveat; the function provided in Wong and Lertzman is for *years to bh* (not years to core height)
# trying Fraver years to coring ht, from P. resinosa


# OH_correction <- OH_correction %>% 
#   filter(!is.na(dbh)) %>% 
#   mutate(wonglert =
#            10.094*(GR5)^(-0.993)) %>% 
#   mutate(corrected_age =
#            year + wonglert + YearsToPith)

OH_correction <- OH_correction %>%
 # filter(!is.na(dbh)) %>% 
  mutate(fraver =
  0.591 * (HT+1)^0.665 + GR15^-0.497) %>% # Fraver equation
  mutate(corrected_age =
              year + fraver + YearsToPith)


# Age-size regressions for PIJE at OH; for ABCO and PICO
library(ggplot2)

#plot the data
# ggplot(data=OH_correction,aes(x=dbh,y=corrected_age))+
#   geom_point()+
#   stat_smooth(method="lm",formula = y~x,se=F,color="blue")+
#   labs(x="Diameter at Breast Height (cm)", y="Age") +
#   ggtitle("Age-Size Regression for Jeffrey Pine")
#   theme_bw(base_size=22)

## 5.2 New model and model comparison based on polynomials
OH_lm <- lm(corrected_age ~ dbh, data = OH_correction)
# OH_lm2 <- lm(corrected_age ~ dbh + I(dbh^2), data = OH_correction)
# OH_lm3 <- lm(corrected_age ~ dbh + I(dbh^2) + I(dbh^3), data = OH_correction)
# OH_lm4 <- lm(corrected_age ~ dbh + I(dbh^2) + I(dbh^3) + I(dbh^4), data = OH_correction)
# OH_lm5 <- lm(corrected_age ~ dbh + I(dbh^2) + I(dbh^3) + I(dbh^4) + I(dbh^5), data = OH_correction)
# OH_exp <- nls(corrected_age ~ a*dbh^b, data = OH_correction, start = list(a=1, b=2))
# AIC(OH_lm, OH_lm2, OH_lm3, OH_lm4, OH_lm5, OH_exp)
# # OH_lm is third-lowest but only by one point at 917. Pick OH_lm
# 
# summary(OH_lm)
# I will proceed using OH_lm, which has the form age = 3.4328x + 26.2246
# Multiple R-squared:  0.646,	Adjusted R-squared:  0.6414 
# F-statistic: 140.5 on 1 and 77 DF,  p-value: < 2.2e-16

#______________________________________________________________________________#

# have no data on the JUGR age-size relationship; Landis and Bailey 2006 provide an age 
# estimate for Utah juniper (juniperus osteosperma) based on diameter at root collar. 
# Best available method!
# age = (39.9 * ln DRC) + 24.2

#______________________________________________________________________________#

# ABCO regression from 2023 data
# need dataframe with corrected age and dbh
OH_2023 <- read.csv("coredata_OH2023.csv")
OH_ABCO <- OH_2023[OH_2023$spec == "ABCO",]
# ggplot(data=OH_ABCO,aes(x=dbh,y=age_est))+
#   geom_point()+
#   stat_smooth(method="lm",formula = y~x,se=F,color="blue")+
#   labs(x="Diameter at Breast Height (cm)", y="Age") +
#   ggtitle("Age-Size Regression for White Fir") +
#   theme_bw(base_size=22)
ABCO_lm <- lm(age_est ~ dbh, data = OH_ABCO)
#summary(ABCO_lm)
# age = 0.7771x + 65.8335
# Multiple R-squared:  0.5136,	Adjusted R-squared:  0.4528 
# F-statistic: 8.447 on 1 and 8 DF,  p-value: 0.0197

#______________________________________________________________________________#

# PICO regression from 2023 data
OH_PICO <- OH_2023[OH_2023$spec == "PICO",]
# ggplot(data=OH_PICO,aes(x=dbh,y=age_est))+
#   geom_point()+
#   stat_smooth(method="lm",formula = y~x,se=F,color="blue")+
#   labs(x="Diameter at Breast Height (cm)", y="Age") +
#   ggtitle("Age-Size Regression for Lodgepole Pine") +
#   theme_bw(base_size=22)
PICO_lm <- lm(age_est ~ dbh, data = OH_PICO)
#summary(PICO_lm)
# age = 1.1501x + 64.0018
# Multiple R-squared:  0.5618,	Adjusted R-squared:  0.4992 
# F-statistic: 8.973 on 1 and 7 DF,  p-value: 0.02007

#______________________________________________________________________________#

## 6. Age-size regression model: with OH_lm, ABCO_lm, PICO_lm and age = (39.9 * ln DRC) + 24.2 for JUGR
# Idea: use mutate to apply each relevant equation in turn, creating 4 age columns named by spp
# Then, select which of the 4 based on spec

# PIJE:  age = 3.4328x + 26.2246
# dbh = (age - 26.2246)/3.4328

# JUGR: age = (39.9 * ln DRC) + 24.2, Landis and Bailey 2006
# dbh = e^((age - 24.2)/39.9)

# ABCO: age = 0.7771x + 65.8335
# dbh = (age-65.8335)/0.7771

# PICO: age = 1.1501x + 64.0018
# dbh = (age-64.0018)/1.1501

# Steps:
# Find model-estimated age for each tree in the dataset, BY SPECIES
# Use that to find model-estimated age in 1941 (simple subtraction)
# Use 1941 age to calculate model-estimated *size* in 1941
# Calculate stand metrics for 1941 vs. 2019

# OH_trees is defined above

# Prepare tree dataset for snag/log establishment date correction

# Pull out standing snags
OH_snags <- OH_trees[!is.na(OH_trees$Dec),]
OH_livetrees <- OH_trees[is.na(OH_trees$Dec),] # later I will do stand metrics on just these trees!

# Don't have spp-specific decay rates yet
# Most recent fire at O'Harrell is OHAREL fire from 2007
# 95% of PIJE have fallen by year 10; using 10 may cause underestimate of tree age, therefore bias size-age ratio up
#NOTE! Age estimate inferred from DBH should not change for standing dead trees; the establishment date is the number that will change. 
#Example: Tree of diameter 23.7 is still 77 years old at IS, but if decay class is 4, then establishment date = 2018-(77+10)
# Need to add column for age estimate:
OH_snags <- OH_snags %>%
  mutate(PIJE_est = predict(OH_lm, newdata = OH_snags)) %>% 
  mutate(JUGR_est = 39.9*log(dbh)+24.2) %>% 
  mutate(ABCO_est = predict(ABCO_lm, newdata = OH_snags)) %>% 
  mutate(PICO_est = predict(PICO_lm, newdata = OH_snags)) %>% 
  mutate(age_est = case_when(Spec=="PIJE*" ~ PIJE_est, # fixing these 2/16/24
                             Spec=="UNK" ~ PIJE_est, # fixing these 2/16/24
                             Spec=="PIJE" ~ PIJE_est,
                             Spec=="JUGR" ~ JUGR_est,
                             Spec=="ABCO" ~ ABCO_est,
                             Spec=="PICO" ~ PICO_est)) %>%
  mutate(dec_correction =
           case_when(Dec == 1 ~ 2,
                     TRUE ~ 9)) %>%
  mutate(estab_est = round(2018 - (age_est+dec_correction),0))

# SNAGS ARE READY AT O'HARRELL CANYON

OH_livetrees <- OH_livetrees %>% 
  mutate(Code = paste0(Plot, Core.))

OH_correction$Code <- gsub("_","", OH_correction$series)
OH_correction$Code <- gsub("b","", OH_correction$Code)
sum(OH_livetrees$Code %in% OH_correction$Code) # 71

allcodes <- OH_livetrees$Code
goodcodes <- allcodes[c(which(OH_livetrees$Code %in% OH_correction$Code))]

# now 71 livetrees have a Code I can look up in the OH_correction df to pull corrected_age (and reassign to age_est)

OH_livetrees <- OH_livetrees %>%
  mutate(PIJE_est = predict(OH_lm, newdata = OH_livetrees)) %>% 
  mutate(JUGR_est = 39.9*log(dbh)+24.2) %>% 
  mutate(ABCO_est = predict(ABCO_lm, newdata = OH_livetrees)) %>% 
  mutate(PICO_est = predict(PICO_lm, newdata = OH_livetrees)) %>% 
  mutate(age_est = case_when(Spec=="PIJE" ~ PIJE_est,
                             Spec=="JUGR" ~ JUGR_est,
                             Spec=="ABCO" ~ ABCO_est,
                             Spec=="PICO" ~ PICO_est)) 
# !! need to re-assign age_est with the core-based ages of the trees that we cored
for(i in 1:length(goodcodes)){
  OH_livetrees[OH_livetrees$Code == goodcodes[i],]$age_est <- OH_correction[OH_correction$Code == goodcodes[i], 26]
}

# still need to force ages from OH_2023 (all in OH2) -- PICO and ABCO
#View(OH_livetrees[OH_livetrees$Plot =="OH2" & OH_livetrees$Spec=="PICO",])

OH_livetrees[which(OH_livetrees$Plot =="OH2" & 
                     OH_livetrees$Spec == "PICO" & 
                     OH_livetrees$dbh == 8.6),]$age_est <- OH_2023[20,]$age_est
OH_livetrees[which(OH_livetrees$Plot =="OH2" & 
                     OH_livetrees$Spec == "PICO" & 
                     OH_livetrees$dbh == 8.7),]$age_est <- OH_2023[19,]$age_est
OH_livetrees[which(OH_livetrees$Plot =="OH2" & 
                     OH_livetrees$Spec == "PICO" & 
                     OH_livetrees$dbh == 9.2),]$age_est <- OH_2023[21,]$age_est
OH_livetrees[which(OH_livetrees$Plot =="OH2" & 
                     OH_livetrees$Spec == "PICO" & 
                     OH_livetrees$dbh == 10.7),]$age_est <- OH_2023[17,]$age_est
OH_livetrees[which(OH_livetrees$Plot =="OH2" & 
                     OH_livetrees$Spec == "PICO" & 
                     OH_livetrees$dbh == 10.8),]$age_est <- OH_2023[16,]$age_est
OH_livetrees[which(OH_livetrees$Plot =="OH2" & 
                     OH_livetrees$Spec == "PICO" & 
                     OH_livetrees$dbh == 12.0),]$age_est <- OH_2023[18,]$age_est
OH_livetrees[which(OH_livetrees$Plot =="OH2" & 
                     OH_livetrees$Spec == "PICO" & 
                     OH_livetrees$dbh == 27.8),]$age_est <- OH_2023[14,]$age_est
OH_livetrees[which(OH_livetrees$Plot =="OH2" & 
                     OH_livetrees$Spec == "PICO" & 
                     OH_livetrees$dbh == 29.0),]$age_est <- OH_2023[15,]$age_est

# yuck! Now ABCO
#View(OH_livetrees[OH_livetrees$Plot =="OH2" & OH_livetrees$Spec=="ABCO",])

OH_livetrees[which(OH_livetrees$Plot =="OH2" & 
                     OH_livetrees$Spec == "ABCO" & 
                     OH_livetrees$X == -51.8 & 
                     OH_livetrees$Y == 2.3),]$age_est <- OH_2023[OH_2023$number=="2001",]$age_est
OH_livetrees[which(OH_livetrees$Plot =="OH2" & 
                     OH_livetrees$Spec == "ABCO" & 
                     OH_livetrees$X == -48.5 & 
                     OH_livetrees$Y == 7.3),]$age_est <- OH_2023[OH_2023$number=="2002",]$age_est
OH_livetrees[which(OH_livetrees$Plot =="OH2" & 
                     OH_livetrees$Spec == "ABCO" & 
                     OH_livetrees$X == -43.2 & 
                     OH_livetrees$Y == 5.5),]$age_est <- OH_2023[OH_2023$number=="2003",]$age_est
OH_livetrees[which(OH_livetrees$Plot =="OH2" & 
                     OH_livetrees$Spec == "ABCO" & 
                     OH_livetrees$X == -8.7 & 
                     OH_livetrees$Y == 51.1),]$age_est <- OH_2023[OH_2023$number=="2015",]$age_est
OH_livetrees[which(OH_livetrees$Plot =="OH2" & 
                     OH_livetrees$Spec == "ABCO" & 
                     OH_livetrees$X == 7.1 & 
                     OH_livetrees$Y == -14.3),]$age_est <- OH_2023[OH_2023$number=="2016",]$age_est
OH_livetrees[which(OH_livetrees$Plot =="OH2" & 
                     OH_livetrees$Spec == "ABCO" & 
                     OH_livetrees$X == -3.1 & 
                     OH_livetrees$Y == -53.4),]$age_est <- OH_2023[OH_2023$number=="2017",]$age_est
OH_livetrees[which(OH_livetrees$Plot =="OH2" & 
                     OH_livetrees$Spec == "ABCO" & 
                     OH_livetrees$X == 0.4 & 
                     OH_livetrees$Y == 54.9),]$age_est <- OH_2023[OH_2023$number=="2018",]$age_est
OH_livetrees[which(OH_livetrees$Plot =="OH2" & 
                     OH_livetrees$Spec == "ABCO" & 
                     OH_livetrees$X == 1.1 & 
                     OH_livetrees$Y == 37.3),]$age_est <- OH_2023[OH_2023$number=="2019",]$age_est
OH_livetrees[which(OH_livetrees$Plot =="OH2" & 
                     OH_livetrees$Spec == "ABCO" & 
                     OH_livetrees$X == 35.1 & 
                     OH_livetrees$Y == 42.1),]$age_est <- OH_2023[OH_2023$number=="2020",]$age_est
OH_livetrees[which(OH_livetrees$Plot =="OH2" & 
                     OH_livetrees$Spec == "ABCO" & 
                     OH_livetrees$X == 1.3 & 
                     OH_livetrees$Y == 52.0),]$age_est <- OH_2023[OH_2023$number=="2021",]$age_est

OH_livetrees <- OH_livetrees %>%
  mutate(estab_est = round(2018 - age_est,0))

OH_livetrees$dec_correction <- 0

# SO ARE LIVETREES

#______________________________________________________________________________#
# Add logs, based on above age-size regression and published decay rates from Raphael and Morrison
log_data <- read.csv("logdata_9-3_DB_PK.csv")
log_data <- log_data %>% 
  filter(sqrt((X^2)+(Y^2))<sqrt(10000/pi)) # boundary check

# Use LgDIA coordinate as base of tree, and LgDIA as original DBH.
log_data <- log_data[,c(1:9)]
log_data <- rename(log_data, "dbh" = "LgDia")
colnames(log_data)[1] = "Site"
# subset to OH
OH_logs <- log_data[log_data$Site=="OH",]
#unique(OH_logs$Spec)
#[1] "PIJE*" "PIJE"  "JUGR"  "PICO"  "ABCO"  "UNK"  
OH_logs$Spec[OH_logs$Spec=="PIJE*"] <- "PIJE"
OH_logs$Spec[OH_logs$Spec=="UNK"] <- "PIJE"

# 2018 - [Years since death (based on decay class) + age from DBH] = establishment year
# I will do a *placeholder*/best guess age correction for the logs. 1 = 10y, 2-3 = 15y, 4-5 = 20y
OH_logs$dec_correction[OH_logs$Dec == 1]=10
OH_logs$dec_correction[OH_logs$Dec > 1 & OH_logs$Dec < 4]=15
OH_logs$dec_correction[OH_logs$Dec > 3 & OH_logs$Dec <=5]=20
OH_logs <- OH_logs %>%
  mutate(PIJE_est = predict(OH_lm, newdata = OH_logs)) %>% 
  mutate(JUGR_est = 39.9*log(dbh)+24.2) %>% 
  mutate(ABCO_est = predict(ABCO_lm, newdata = OH_logs)) %>% 
  mutate(PICO_est = predict(PICO_lm, newdata = OH_logs)) %>% 
  mutate(age_est = case_when(Spec=="PIJE*" ~ PIJE_est, # fixing these 2/17/24
                             Spec=="UNK" ~ PIJE_est, # fixing these 2/17/24
                             Spec=="PIJE" ~ PIJE_est,
                             Spec=="JUGR" ~ JUGR_est,
                             Spec=="ABCO" ~ ABCO_est,
                             Spec=="PICO" ~ PICO_est)) %>%
  mutate(estab_est = round(2018 - (age_est+dec_correction),0))

#______________________________________________________________________________#
# rbind livetrees, logs, and snags to get complete tree dataset for all times and peoples:
# names(OH_livetrees)
# names(OH_snags)
# names(OH_logs) 
OH_trees <- rbind(OH_livetrees[,c(1:9,19,20,21)],OH_snags[,c(1:9,18,20,21)],OH_logs[,c(1:9,15,16,10)])

#______________________________________________________________________________#
# find dbh from 1941 age
OH_trees1941 <- OH_trees %>% 
  mutate(age1941 = 1941 - estab_est) %>% 
  mutate(PIJE_1941 = (age1941 - 26.2246)/3.4328) %>% 
  mutate(JUGR_1941 = exp((age1941 - 24.2)/39.9)) %>% 
  mutate(ABCO_1941 = (age1941-65.8335)/0.7771) %>% 
  mutate(PICO_1941 = (age1941-64.0018)/1.1501) %>% 
  mutate(dbh1941 = case_when(Spec=="PIJE" ~ PIJE_1941,
                             Spec=="JUGR" ~ JUGR_1941,
                             Spec=="ABCO" ~ ABCO_1941,
                             Spec=="PICO" ~ PICO_1941)) %>% 
  # need to filter out rows with trees that have NaN or <5 DBH
  filter(!is.na(dbh1941)) %>% # this takes it from 480 to 459 trees
  filter(dbh1941>=5) # and this goes from 459 to 183 ! OH_livetrees has 344 observations ...!
#hist(OH_trees1941$dbh1941, breaks = 10)
#hist(OH_livetrees$dbh, breaks = 10)
# then we are ready to compare stand metrics in 1941 to 2018

#______________________________________________________________________________#
# Size in 2006, before O'Harrell Fire
# need to account for trees that were dead *before* O'Harrell Fire, i.e. all logs Dec > 1 were snags in 2006 (all snags, and logs Dec = 1, were live trees in 2006)

OH_trees2006 <- OH_trees %>% 
  mutate(age2006 = 2006 - estab_est) %>% 
  mutate(PIJE_2006 = (age2006 - 26.2246)/3.4328) %>% 
  mutate(JUGR_2006 = exp((age2006 - 24.2)/39.9)) %>% 
  mutate(ABCO_2006 = (age2006-65.8335)/0.7771) %>% 
  mutate(PICO_2006 = (age2006-64.0018)/1.1501) %>% 
  mutate(dbh2006 = case_when(Spec=="PIJE" ~ PIJE_2006,
                             Spec=="JUGR" ~ JUGR_2006,
                             Spec=="ABCO" ~ ABCO_2006,
                             Spec=="PICO" ~ PICO_2006))  %>% 
  # need to filter out rows with trees that have NaN or <5 DBH
  filter(!is.na(dbh2006)) %>% # 
  filter(dbh2006>=5)

OH_snags2006 <- OH_trees2006 %>% 
  filter(dec_correction > 14) # any logs with Dec >1 were 2006 snags (so dec_correction ≥15)

OH_trees2006 <- OH_trees2006 %>% 
  filter(dec_correction < 14) # remove trees dead before O'Harrell Fire, but keep all live trees as of 2015

#______________________________________________________________________________#
# Prepping all OH sites in 2018
OH1_2018 <- OH_livetrees[OH_livetrees$Plot == "OH1",]
OH2_2018 <- OH_livetrees[OH_livetrees$Plot == "OH2",]
OH3_2018 <- OH_livetrees[OH_livetrees$Plot == "OH3",]

# Prepping all OH sites in 2006
OH1_2006 <- OH_trees2006[OH_trees2006$Plot == "OH1",]
OH2_2006 <- OH_trees2006[OH_trees2006$Plot == "OH2",]
OH3_2006 <- OH_trees2006[OH_trees2006$Plot == "OH3",]
# Need to reassign dbh to 2006 value
OH1_2006 <- rename(OH1_2006, "dbh2018" = "dbh", "dbh" = "dbh2006")
OH2_2006 <- rename(OH2_2006, "dbh2018" = "dbh", "dbh" = "dbh2006")
OH3_2006 <- rename(OH3_2006, "dbh2018" = "dbh", "dbh" = "dbh2006")

# Prepping all OH sites in 1941
OH1_1941 <- OH_trees1941[OH_trees1941$Plot == "OH1",]
OH2_1941 <- OH_trees1941[OH_trees1941$Plot == "OH2",]
OH3_1941 <- OH_trees1941[OH_trees1941$Plot == "OH3",]
# Need to reassign dbh to 1941 value
OH1_1941 <- rename(OH1_1941, "dbh2018" = "dbh", "dbh" = "dbh1941")
OH2_1941 <- rename(OH2_1941, "dbh2018" = "dbh", "dbh" = "dbh1941")
OH3_1941 <- rename(OH3_1941, "dbh2018" = "dbh", "dbh" = "dbh1941")

#______________________________________________________________________________#
# establishment histogram for cored trees
OH_estab_cores <- hist(rwl.stats(OH_rwl)$first, breaks = (1980-1538)/10)
# for estimated establishment (all trees)
OH_estab_trees <- hist(OH_trees$estab_est, breaks=(1974-1539)/10)

# size class distn
OH_2018_hist <- hist(OH_trees$dbh)
OH_1941_hist <- hist(OH_trees1941$dbh1941)

# plot comparing age estimate model with observed, corrected ages for cored trees (purple dots)
# need to correct for PIJE* and UNK
model_v_cores_OH <- ggplot() +
  geom_point(data = OH_trees, aes(x=dbh, y=age_est, group=Spec, color = Spec)) +
  geom_point(data = OH_correction, aes(x=dbh, y=corrected_age), col="purple")

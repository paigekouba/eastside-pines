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
OH_rwl <- subset(OH_rwl, select = -c(OH115, OH11a, OH271, OH3128, OH264)) # N = 81

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
core_height$Code[core_height$Code=="OH2502"] <- "OH2_502"
core_height$Code[core_height$Code=="OH1503"] <- "OH1_503"
core_height$Code[core_height$Code=="OH2504"] <- "OH2_504"
core_height$Code[core_height$Code=="OH2505"] <- "OH2_505"
core_height$Code[core_height$Code=="OH3506"] <- "OH3_506"

OH_correction$dbh <- core_height$DBH[match(OH_correction$series, core_height$Code)]
OH_correction$Spec <- core_height$Spec[match(OH_correction$series, core_height$Code)]
OH_correction$HT <- core_height$Ht[match(OH_correction$series, core_height$Code)]
#names(OH_correction)[11] <- "GR5"
names(OH_correction)[11] <- "GR15"

## 5. Age correction function: PIJE from Wong and Lertzman 2001; 
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
  filter(!is.na(dbh)) %>% 
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
#   ggtitle("Age-Size Regression for Jeffrey Pine") +
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

# JUGR: age = (39.9 * ln DRC) + 24.2
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
  mutate(age_est = case_when(Spec=="PIJE" ~ PIJE_est,
                             Spec=="JUGR" ~ JUGR_est,
                             Spec=="ABCO" ~ ABCO_est,
                             Spec=="PICO" ~ PICO_est)) %>%
  mutate(snag_correction =
           case_when(Dec == 1 ~ 2,
                     TRUE ~ 10)) %>%
  mutate(estab_est = round(2018 - (age_est+snag_correction),0))

# SNAGS ARE READY AT O'HARRELL CANYON

OH_livetrees <- OH_livetrees %>%
  mutate(PIJE_est = predict(OH_lm, newdata = OH_livetrees)) %>% 
  mutate(JUGR_est = 39.9*log(dbh)+24.2) %>% 
  mutate(ABCO_est = predict(ABCO_lm, newdata = OH_livetrees)) %>% 
  mutate(PICO_est = predict(PICO_lm, newdata = OH_livetrees)) %>% 
  mutate(age_est = case_when(Spec=="PIJE" ~ PIJE_est,
                             Spec=="JUGR" ~ JUGR_est,
                             Spec=="ABCO" ~ ABCO_est,
                             Spec=="PICO" ~ PICO_est)) %>% 
  mutate(estab_est = round(2018 - age_est,0))
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
# 2018 - [Years since death (based on decay class) + age from DBH] = establishment year
# I will do a *placeholder*/best guess age correction for the logs. 1 = 10y, 2-3 = 15y, 4-5 = 20y
OH_logs$log_correction[OH_logs$Dec == 1]=10
OH_logs$log_correction[OH_logs$Dec > 1 & OH_logs$Dec < 4]=15
OH_logs$log_correction[OH_logs$Dec > 3 & OH_logs$Dec <=5]=20
OH_logs <- OH_logs %>%
  mutate(PIJE_est = predict(OH_lm, newdata = OH_logs)) %>% 
  mutate(JUGR_est = 39.9*log(dbh)+24.2) %>% 
  mutate(ABCO_est = predict(ABCO_lm, newdata = OH_logs)) %>% 
  mutate(PICO_est = predict(PICO_lm, newdata = OH_logs)) %>% 
  mutate(age_est = case_when(Spec=="PIJE" ~ PIJE_est,
                             Spec=="JUGR" ~ JUGR_est,
                             Spec=="ABCO" ~ ABCO_est,
                             Spec=="PICO" ~ PICO_est)) %>%
  mutate(estab_est = round(2018 - (age_est+log_correction),0))

#______________________________________________________________________________#
# rbind livetrees, logs, and snags to get complete tree dataset for all times and peoples:
# names(OH_livetrees)
# names(OH_snags)
# names(OH_logs) 
OH_trees <- rbind(OH_livetrees[,c(1:9,18,19)],OH_snags[,c(1:9,18,20)],OH_logs[,c(1:9,15,16)])

#______________________________________________________________________________#
# find dbh from 1941 age using dbh = (age^(1/0.9783))/3.2088
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
# Prepping all OH sites in 2018
OH1_2018 <- OH_livetrees[OH_livetrees$Plot == "OH1",]
OH2_2018 <- OH_livetrees[OH_livetrees$Plot == "OH2",]
OH3_2018 <- OH_livetrees[OH_livetrees$Plot == "OH3",]

# Prepping all OH sites in 1941
OH1_1941 <- OH_trees1941[OH_trees1941$Plot == "OH1",]
OH2_1941 <- OH_trees1941[OH_trees1941$Plot == "OH2",]
OH3_1941 <- OH_trees1941[OH_trees1941$Plot == "OH3",]
# Need to reassign dbh to 1941 value
OH1_1941 <- rename(OH1_1941, "dbh2018" = "dbh", "dbh" = "dbh1941")
OH2_1941 <- rename(OH2_1941, "dbh2018" = "dbh", "dbh" = "dbh1941")
OH3_1941 <- rename(OH3_1941, "dbh2018" = "dbh", "dbh" = "dbh1941")

#______________________________________________________________________________#

# NON-SPATIAL METRICS

# Get avg DBH, max DBH, stems/ha, BA, and QMD for IS live trees in 2018:
meanOH <- mean(OH_livetrees$dbh)
sd(OH_livetrees$dbh) #28.5
maxOH <- max(OH_livetrees$dbh)
stemsOH <- nrow(OH_livetrees)/3
BA_OH <- sum(pi*(OH_livetrees$dbh/200)^2)/3
QMD_OH <- sqrt(sum(OH_livetrees$dbh^2)/nrow(OH_livetrees))

# Get avg DBH, max DBH, stems/ha, BA, and QMD for IS live trees in 1941:
meanOH1941 <- mean(OH_trees1941$dbh1941)
maxOH1941 <- max(OH_trees1941$dbh1941)
stemsOH1941 <- nrow(OH_trees1941)/3
BA_OH1941 <- sum(pi*(OH_trees1941$dbh1941/200)^2)/3
QMD_OH1941 <- sqrt(sum(OH_trees1941$dbh1941^2)/nrow(OH_trees1941))

OH_metrics <- data.frame(Metric = c("MeanDBH", "MaxDBH", "Stems","BasalArea","QMD"), 
                         OH2018 = c(meanOH, maxOH, stemsOH, BA_OH, QMD_OH),
                         OH1941 = c(meanOH1941, maxOH1941, stemsOH1941, BA_OH1941, QMD_OH1941))

# SPATIAL STUFF

#install.packages("spatstat")
library(spatstat)
# subset just IS1 for mapping practice
IS1_2018 <- IS_livetrees[IS_livetrees$Plot == "IS1",]
IS1_1941 <- IS_trees1941[IS_trees1941$Plot == "IS1",]

# convert x, y data to point patter
IS1_2018ppp <- ppp(IS1_2018$X, IS1_2018$Y, c(-60,60), c(-60,60))
IS1_1941ppp <- ppp(IS1_1941$X, IS1_1941$Y, c(-60,60), c(-60,60))
# plot with dbh
marks(IS1_1941ppp) <- IS1_1941[,13]
marks(IS1_2018ppp) <- IS1_2018[,5]
par(mfrow=c(1,2))
plot(IS1_2018ppp)
plot(IS1_1941ppp)

par(mfrow=c(1,2)) # plot as heatmap of biggest-dbh trees
plot(Smooth(IS1_1941ppp))
plot(Smooth(IS1_2018ppp))

par(mfrow=c(1,2)) # plot as heatmap of most stems per area
plot(density(IS1_1941ppp))
plot(density(IS1_2018ppp))

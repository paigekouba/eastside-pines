# Fri 3/17/23
# Updating Core Height Correction, Checking Age-Height Model
# For Ian's Presentation with the Luck o the Irish
library(dplR)
library(tidyverse)

# I am taking code from Eastiside_Analyses.R, cutting out OH portions, adding an ANOVA

#1 Started by making the final .rwl collection for Indiana Summit, IS_2022 (in Images and POS Files > Indiana Summit).
# add all .pos files to a rwl, excluding original core if a duplicate was made (e.g. for IS140 and IS140B, keep only IS140B). N = 91
# Read in the files. Need rwl collection from IS, and core height data.
setwd("S:/FacultyData/LATIMER/LATIMERShared/Paige/PaigeCores/Images and POS files/Indiana_Summit")
IS_data = read.rwl("IS_2022.rwl")

# IS21003 has no pith estimate due to aberrations; remove from df. 
IS_data <- subset(IS_data, select = -c(IS21003))

setwd("S:/FacultyData/LATIMER/LATIMERShared/Paige/PaigeCores/Spreadsheets")
core_height = read.csv("coredata_PK.csv")

# 3. dplR can extract establishment year and age for each core
rwl.stats(IS_data)

# STOP: checking against Eastside Cores Aging spreadsheet, these numbers don't include pith correction
# Try again with .rwl including comments (OH_2022comments)
# OHc_data = read.rwl("OH_2022commments.rwl") not working "cannot open the connection; cannot open file ... no such file or directory
# Try again with .txt file of distance to pith
# This involved a lot of data cleaning of the .txt files to turn them into these .csv files

# 4 import distance-to-pith data. Ultimately we just need to add these values on to the final age est.
IS_pith = read.csv("IS_pith.csv")

# remove pithless ones
IS_pith <- subset(IS_pith, Core != "IS21003")

## Method from Fraver et al. (P. resinosa):
#4. Calculate GR15 (average annual growth rate of first 15 rings)
#GR15 = function(x){
#  non_na_widths = x[which(!is.na(x))]
#  first_15 = non_na_widths[1:15]
#  return(mean(first_15))
#}

## Method from Wong & Lertzman (P. ponderosa):
#4. Calculate GR5 (*cumulative* growth of the first 5 rings) UNFORTUNATE DETAIL their paper uses first 5y at BH...
GR5 = function(x){
  non_na_widths = x[which(!is.na(x))]
  first_5 = non_na_widths[1:5]
  return(sum(first_5))
}
  
IS_correction <- cbind(cbind(rwl.stats(IS_data), apply(IS_data,2,GR5)), IS_pith)

## 4.1 Incorporate core height. 
IS_correction$dbh <- core_height$DBH[match(IS_correction$series, core_height$Code)]
IS_correction$HT <- core_height$Ht[match(IS_correction$series, core_height$Code)]
names(IS_correction)[11] <- "GR5"

## 5. Age correction function. Other caveat; the function provided in Wong and Lertzman is for *years to bh* (not years to core height)
IS_correction <- IS_correction %>% 
  filter(!is.na(dbh)) %>% 
  mutate(correction =
           10.094*(GR5)^(-0.993)) %>% 
  mutate(corrected_age =
           year + correction + YearsToPith)
  #  0.591 * (HT+1)^0.665 + GR15^-0.497) %>% # Fraver equation

## 5.1 Plot the data
plot(IS_correction$dbh, IS_correction$corrected_age)
abline(lm(IS_correction$corrected_age ~ IS_correction$dbh))
summary(lm(IS_correction$corrected_age ~ IS_correction$dbh))$coefficients
#                   Estimate Std. Error    t value     Pr(>|t|)
# (Intercept)       5.700546 16.5464806  0.3445171 7.312985e-01
# IS_correction$dbh 2.842544  0.2267683 12.5350135 4.151434e-21

# suggestions to make this quadratic
# m2 <- lm(corrected_age ~ dbh + I(dbh^2), data = IS_correction)
# m2 <- lm(corrected_age ~ poly(dbh, 2), data = IS_correction)
#summary(m2)


# TO COMPARE WHICH OF THESE MODELS IS BETTER, NEED TO HOLD OUT SOME DATA AND PREDICT TO IT AND EVALUATE ACCURACY (CROSS-VALIDATION)
# step 1 - split data into training and testing sets (maybe once or maybe multiple times)
# step 2 - fit model to training data 
# step 3 - using fitted model, predict to testing data - "predict()"? 
# step 4 - calculate error or predictive skill metric 




# mean(as.matrix(IS_data), na.rm = TRUE)
# [1] 1.4624; indicates the average ring is 1.46 mm wide. 
# Coefficient above (2.84) indicates average ring is 10mm/cm x 1 cm/2.84 rings = 3.52 mm/ring


# STOP. Everything below is based on an incorrect model! The coefficient is wrong :( Will fix it
## 6. Age-size regression
# DBH = a + corrected_age*b
lm_IS <- lm(dbh ~ 0 + corrected_age, IS_correction)
summary(lm_IS)
# a = 0  b = 0.307204
# DBH = corrected_age*0.307204
# find DBH of tree that established in 1941 (whose age is 2018 - 1941 = 77)
77*0.307204
# = 23.7 cm !!!

#______________________________________________________________________________#
tree_data <- read.csv("Treedata_9-3_Em_DB.csv")
# fix data entry error in row 192
tree_data[192,7] = 36.7

IS_trees <- tree_data[tree_data$Site=="IS",]

# Prepare tree dataset for snag/log establishment date correction

# Pull out standing snags
IS_snags <- IS_trees[!is.na(IS_trees$Dec),]
IS_livetrees <- IS_trees[is.na(IS_trees$Dec),] # later I will do stand metrics on just these trees!

# If Dec = 1, assume tree died in 2016 Clark Fire; adjusted estab is estab_est + 2
# If Dec > 1, assume tree died 10 years ago; adjusted age is age_est - 10, and adjusted estab is estab_est + 10
# 95% of PIJE have fallen by year 10; using 10 may cause underestimate of tree age, therefore bias size-age ratio up
#NOTE! Age estimate inferred from DBH should not change for standing dead trees; the establishment date is the number that will change. 
#Example: Tree of diameter 23.7 is still 77 years old at IS, but if decay class is 4, then establishment date = 2018-(77+10)
# Need to add column for age estimate:
IS_snags <- IS_snags %>%
  mutate(age_est = DBH/0.307204)%>%
  mutate(snag_correction =
           case_when(Dec == 1 ~ 2,
                     TRUE ~ 10)) %>%
  mutate(estab_est = round(2018 - (age_est+snag_correction),0))

# SNAGS ARE READY AT INDIANA SUMMIT

IS_livetrees <- IS_livetrees %>%
  mutate(age_est = DBH/0.307204) %>%
  mutate(estab_est = round(2018 - age_est,0))
# SO ARE LIVETREES

#______________________________________________________________________________#
# Add logs, based on above age-size regression and published decay rates from Raphael and Morrison
log_data <- read.csv("logdata_9-3_DB_PK.csv")
# Use LgDIA coordinate as base of tree, and LgDIA as original DBH.
log_data <- log_data[,c(1:9)]
log_data <- rename(log_data, "DBH" = "LgDia")
colnames(log_data)[1] = "Site"
# subset to IS
IS_logs <- log_data[log_data$Site=="IS",]
# 2018 - [Years since death (based on decay class) + age from DBH] = establishment year
# I will do a *placeholder*/best guess age correction for the logs. 1 = 10y, 2-3 = 15y, 4-5 = 20y
IS_logs$log_correction[IS_logs$Dec == 1]=10
IS_logs$log_correction[IS_logs$Dec > 1 & IS_logs$Dec < 4]=15
IS_logs$log_correction[IS_logs$Dec > 3 & IS_logs$Dec <=5]=20
IS_logs <- IS_logs %>%
  mutate(age_est = DBH/0.307204) %>%
  mutate(estab_est = round(2018 - (age_est+log_correction),0))

#______________________________________________________________________________#
# rbind livetrees, logs, and snags to get complete tree dataset for all times and peoples:
names(IS_livetrees)
names(IS_snags)
names(IS_logs)
IS_trees <- rbind(IS_livetrees[,c(1:9,14,15)],IS_snags[,c(1:9,14,16)],IS_logs[,c(1:9,11,12)])

#______________________________________________________________________________#
# Size in 1941 = (1941 - estab. year) * size coefficient
# simplest, works for trees snags and logs!
# 1941 tree dataframe
#IS_trees1941 <- IS_trees %>%
#  mutate(dbh1941_est = (1941 - estab_est)*0.307204) %>%
#  filter(dbh1941_est >= 5)
# ^ I don't like this anymore; each tree should get its true dbh minus 23.7cm (estimate of average growth increment in 77y)
# but snags and logs shouldn't be penalized for the time they spent dead; they get back x years of deadness * growth coefficient

# For 1941 reconstruction: DBH1941 = DBH - 23.7 + (coeff * snag_correction)
#  DBH1941 = DBH - 23.7 + (coeff * log_correction)
IS_livetrees <- IS_livetrees %>% 
  mutate(DBH1941 = DBH - (2018-1941)*0.307204) %>%
  filter(DBH1941 >= 5)

IS_snags <- IS_snags %>% 
  mutate(DBH1941 = DBH - (2018-1941-snag_correction)*0.307204) %>%
  filter(DBH1941 >= 5)

IS_logs <- IS_logs %>% 
  mutate(DBH1941 = DBH - (2018-1941-log_correction)*0.307204) %>%
  filter(DBH1941 >= 5)

IS_trees1941 <- rbind(IS_livetrees[,c(1:9,14,15,16)],IS_snags[,c(1:9,14,16,17)],IS_logs[,c(1:9,11,12,13)])
# then we are ready to compare stand metrics in 1941 to 2018 using MANOVA
#______________________________________________________________________________#

# Get avg DBH, max DBH, stems/ha, BA, and QMD for IS live trees in 2018:
meanIS <- mean(IS_livetrees$DBH)
sd(IS_livetrees$DBH) #28.5
maxIS <- max(IS_livetrees$DBH)
stemsIS <- nrow(IS_livetrees)/3
BA_IS <- sum(pi*(IS_livetrees$DBH/200)^2)/3
QMD_IS <- sqrt(sum(IS_livetrees$DBH^2)/nrow(IS_livetrees))

meanIS1941 <- mean(IS_trees1941$DBH1941)
maxIS1941 <- max(IS_trees1941$DBH1941)
stemsIS1941 <- nrow(IS_trees1941)/3
BA_IS1941 <- sum(pi*(IS_trees1941$DBH1941/200)^2)/3
QMD_IS1941 <- sqrt(sum(IS_trees1941$DBH1941^2)/nrow(IS_trees1941))

IS_metrics <- data.frame(Metric = c("MeanDBH", "MaxDBH", "Stems","BasalArea","QMD"), 
                         IS2018 = c(meanIS, maxIS, stemsIS, BA_IS, QMD_IS),
                         IS1941 = c(meanIS1941, maxIS1941, stemsIS1941, BA_IS1941, QMD_IS1941))

#______________________________________________________________________________#
install.packages("spatstat")
library(spatstat)
IS1_2018 <- IS_livetrees[IS_livetrees$Plot == "IS1",]
IS1_1941 <- IS_trees1941[IS_trees1941$Plot == "IS1",]

IS1_2018ppp <- ppp(IS1_2018$X, IS1_2018$Y, c(-60,60), c(-60,60))
IS1_1941ppp <- ppp(IS1_1941$X, IS1_1941$Y, c(-60,60), c(-60,60))
par(mfrow=c(1,2))
plot(IS1_2018ppp)
plot(IS1_1941ppp)

par(mfrow=c(1,2))
plot(Kest(IS1_2018ppp))
plot(Kest(IS1_1941ppp))

IS2_2018 <- IS_livetrees[IS_livetrees$Plot == "IS2",]
IS2_1941 <- IS_trees1941[IS_trees1941$Plot == "IS2",]

IS2_2018ppp <- ppp(IS2_2018$X, IS2_2018$Y, c(-60,60), c(-60,60))
IS2_1941ppp <- ppp(IS2_1941$X, IS2_1941$Y, c(-60,60), c(-60,60))


par(mfrow=c(1,2))
plot(Kest(IS2_2018ppp))
plot(Kest(IS2_1941ppp))

IS3_2018 <- IS_livetrees[IS_livetrees$Plot == "IS3",]
IS3_1941 <- IS_trees1941[IS_trees1941$Plot == "IS3",]

IS3_2018ppp <- ppp(IS3_2018$X, IS3_2018$Y, c(-60,60), c(-60,60))
IS3_1941ppp <- ppp(IS3_1941$X, IS3_1941$Y, c(-60,60), c(-60,60))


par(mfrow=c(1,2))
plot(Kest(IS3_2018ppp))
plot(Kest(IS3_1941ppp))
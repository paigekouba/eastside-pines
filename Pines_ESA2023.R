# Mon 5/8/23
# Updating Age-Size Regression Model
# Redoing Stand Metrics for ESA Abstract

# Sun 2/25/24
# Final Analyses

# Fri 3/8/24
# Adding Pre-Fire Timestep
library(dplR)
library(tidyverse)

# I am taking code from Eastside_Analyses.R, cutting out OH portions

#1 Started by making the final .rwl collection for Indiana Summit, IS_2022 (in Images and POS Files > Indiana Summit).
# add all .pos files to a rwl, excluding original core if a duplicate was made (e.g. for IS140 and IS140B, keep only IS140B). N = 91
# Read in the files. Need rwl collection from IS, and core height data.
#setwd("S:/FacultyData/LATIMER/LATIMERShared/Paige/PaigeCores/Images and POS files/Indiana_Summit")
IS_data = read.rwl("IS_2022.rwl")

# IS21003 has no pith estimate due to aberrations; remove from df. 
IS_data <- subset(IS_data, select = -c(IS21003, IS115, IS118)) # N = 88
# 2/17/24 removed an additional 2 cores marked "out" in core_height sheet

#setwd("S:/FacultyData/LATIMER/LATIMERShared/Paige/PaigeCores/Spreadsheets")
core_height = read.csv("coredata_PK.csv")

# 3. dplR can extract establishment year and age for each core
#rwl.stats(IS_data)

# STOP: checking against Eastside Cores Aging spreadsheet, these numbers don't include pith correction
# Try again with .rwl including comments (OH_2022comments)
# OHc_data = read.rwl("OH_2022commments.rwl") not working "cannot open the connection; cannot open file ... no such file or directory
# Try again with .txt file of distance to pith
# This involved a lot of data cleaning of the .txt files to turn them into these .csv files

# 4 import distance-to-pith data. Ultimately we just need to add these values on to the final age est.
IS_pith = read.csv("IS_pith.csv")

# remove pithless ones
IS_pith <- subset(IS_pith, Core != "IS21003")
IS_pith <- IS_pith[IS_pith$Core != "IS115",] # 2/17/24 remove the two cores not in core_height
IS_pith <- IS_pith[IS_pith$Core != "IS118",]

## Method from Fraver et al. (P. resinosa):
#4. Calculate GR15 (average annual growth rate of first 15 rings)
GR15fun = function(x){
  non_na_widths = x[which(!is.na(x))]
  first_15 = non_na_widths[1:15]
  return(mean(first_15))
}

## Method from Wong & Lertzman (P. ponderosa):
#4. Calculate GR5 (*cumulative* growth of the first 5 rings) UNFORTUNATE DETAIL their paper uses first 5y at BH...
# GR5fun = function(x){
#   non_na_widths = x[which(!is.na(x))]
#   first_5 = non_na_widths[1:5]
#   return(sum(first_5))
# }


#IS_correction <- cbind(cbind(rwl.stats(IS_data), apply(IS_data,2,GR5fun)), IS_pith)
IS_correction <- cbind(cbind(rwl.stats(IS_data), apply(IS_data,2,GR15fun)), IS_pith)

## 4.1 Incorporate core height. 
IS_correction$dbh <- core_height$DBH[match(IS_correction$series, core_height$Code)]
IS_correction$HT <- core_height$Ht[match(IS_correction$series, core_height$Code)]
#names(IS_correction)[11] <- "GR5"
names(IS_correction)[11] <- "GR15"

## 5. Age correction function. Other caveat; the function provided in Wong and Lertzman is for *years to bh* (not years to core height)
# IS_correction <- IS_correction %>% 
#   filter(!is.na(dbh)) %>% 
#   mutate(correction =
#            10.094*(GR5)^(-0.993)) %>% 
#   mutate(corrected_age =
#            year + correction + YearsToPith)

IS_correction <- IS_correction %>%
  #filter(!is.na(dbh)) %>% 
  mutate(fraver =
           0.591 * (HT+1)^0.665 + GR15^-0.497) %>% # Fraver equation
  mutate(corrected_age =
           year + fraver + YearsToPith)

#MESSING AROUND WITH STATISTICAL MAGIC
# library(ggplot2)
# 
# #plot the data
#ggplot(data=IS_correction,aes(x=dbh,y=corrected_age))+

  #add Points with different shapes depending on factor z
#  geom_point()+
#   stat_smooth(method="lm",formula = y~x,se=F,color="blue")#+
#   #stat_smooth(method="loess")
#   
#   #Add line using non-linear regression
#   #  stat_smooth(method="nls",formula =  y~poly(x,2),method.args=list(start=c(a=2,b=2)),se=F,color="red")+
#  # stat_smooth(method="gam",formula =  y~s(x),color="red")
# 
# #add line using linear regression; messing around
# #stat_smooth(method="lm",formula =  y~exp(-x),se=F,color="blue")
# 
## 5.2 New model and model comparison based on polynomials
IS_lm <- lm(corrected_age ~ dbh, data = IS_correction)
# IS_lm2 <- lm(corrected_age ~ dbh + I(dbh^2), data = IS_correction)
# IS_lm3 <- lm(corrected_age ~ dbh + I(dbh^2) + I(dbh^3), data = IS_correction)
# IS_lm4 <- lm(corrected_age ~ dbh + I(dbh^2) + I(dbh^3) + I(dbh^4), data = IS_correction)
# IS_lm5 <- lm(corrected_age ~ dbh + I(dbh^2) + I(dbh^3) + I(dbh^4) + I(dbh^5), data = IS_correction)
# summary(IS_lm)
# summary(IS_lm3)
# AIC(IS_lm, IS_lm2, IS_lm3, IS_lm4, IS_lm5)#, IS_exp_inv, IS_exp)
# # 
# # # IS_lm3 performs the best with an AIC of 980.7556, but as shown below its behavior at low and high dbh makes no sense :(
# # # visualize the model!
# ggplot(data=IS_correction,aes(x=dbh,y=corrected_age))+
#   geom_point() +
#   stat_smooth(method = lm, formula = y ~ poly(x, 3, raw= TRUE))
# 
# poly 2 is less over-fitted; bascially a straight line
# ggplot(data=IS_correction,aes(x=dbh,y=corrected_age))+
#   geom_point() +
#   stat_smooth(method = lm, formula = y ~ poly(x, 2, raw= TRUE))

# # just lm
# ggplot(data=IS_correction,aes(x=dbh,y=corrected_age))+
#   geom_point() +
#   stat_smooth(method = lm, formula = y ~ poly(x, 1, raw= TRUE))

# # try fitting a line for a power law fit
# ggplot(data=IS_correction,aes(x=dbh,y=corrected_age))+
#   geom_point() +
#     geom_smooth(data = IS_correction,
#       method = "nls",
#       method.args=list(formula = y ~ a*(x^b) + c, start = list(a=1, b=2, c=5)),
#       se=FALSE) +
#   labs(x="Diameter at Breast Height (cm)", y="Age") +
#   ggtitle("Age-Size Regression for Jeffrey Pine") 
#  theme_bw(base_size=22)

# it looks almost indistinguishable from a straight line model
IS_exp <- nls(corrected_age ~ a*dbh^b, data = IS_correction, start = list(a=1, b=2))
#summary(IS_exp) # a = 3.61166, b = 0.95850
# plotting IS_exp and the 2-degree polynomial
# ggplot(data=IS_correction,aes(x=dbh,y=corrected_age))+
#   geom_point() +
#   #geom_smooth(data = IS_correction,
#             #  method = "nls",
#            #   method.args=list(formula = y ~ a*(x^b)+ c, start = list(a=1, b=2, c=5)),
#             #  se=FALSE, color="red")#+
#   stat_smooth(method = lm, formula = y ~ poly(x, 2, raw= TRUE))
# 
# # add an intercept to IS_exp
# IS_expc <- nls(corrected_age ~ a*(dbh^b) + c, data = IS_correction, start = list(a=1, b=2, c=5))
# summary(IS_expc)
# AIC(IS_exp, IS_expc)
# 
# # more messing around
# IS_exp_inv <- nls(dbh ~ a*corrected_age^b, data = IS_correction, start = list(a=1, b=.1))
# #summary(IS_exp_inv)
# #IS_exponential <- nls(corrected_age ~ a*exp(dbh*b), data = IS_correction, start = list(a=1, b=0.1))
# #summary(IS_exponential)
# 
# # Test AICs
# AIC(IS_lm, IS_lm2, IS_lm3, IS_lm4, IS_lm5, IS_exp_inv, IS_exp)
# # the inverse model has almost 200 points lower AIC does this matter?? No
# # plotting it 
# ggplot(data=IS_correction,aes(y=dbh,x=corrected_age))+
#   geom_point() +
#   geom_smooth(data = IS_correction,
#               method = "nls",
#               method.args=list(formula = y ~ a*(x^b), start = list(a=1, b=2)),
#               se=FALSE) 
# 
# # Cross-validation
# # 1. split data into training and testing sets
# # 2. fit model to training data
# # 3. using fitted model, predict testing data predict()
# # 4. calculate error or predictive skill metric
# 
# # use 90% of the data set as training set and 10% as test set
# sample <- sample(c(TRUE, FALSE), nrow(IS_correction), replace = TRUE, prob=c(0.9, 0.1))
# train <- IS_correction[sample ,]
# test <- IS_correction[!sample ,]
# 
# IS_lm2.1 <- lm(corrected_age ~ dbh + I(dbh^2), data = train)
# summary(IS_lm2)
# summary(IS_lm2.1)
# 
# IS_lm3.1 <- lm(corrected_age ~ dbh + I(dbh^2), data = train)
# predict(IS_lm3.1, newdata = test)
# test[,22]
# 
# # predict testing data using IS_lm2.1
# predict(IS_lm2.1, newdata = test)
# #    IS120     IS129      IS19    IS237B     IS251   IS31016     IS375     IS378 
# #280.39474 392.98486 109.07211  46.27213 213.61213 276.89902  69.54169  78.94808 
# 
# # How do the predicted ages above compare to the observed ages for this set of trees? NB these will change each time
# # you run the code bc the random draw of train/test changes.
# test[,1]
# #"IS120"   "IS129"   "IS19"    "IS237B"  "IS251"   "IS31016" "IS375"   "IS378"  
# test[,22]
# #266.71765 269.23951  86.24924  83.93438 244.09579 260.47968  70.43932  96.90547
# 
# # Questions: How do I compare test dataset with predictions for a given model? 
# # How do I compare that metric between different models?
# 
# # For now I will proceed using IS_exp, which has the form age = 3.61166 (x^0.95850)

#______________________________________________________________________________#
# 

## 6. Age-size regression model: 
#IS_exp <- nls(corrected_age ~ a*dbh^b, data = IS_correction, start = list(a=1, b=2))
# age = 3.61166 (x^0.95850)
# dbh = (age^(1/0.95850))/3.61166

# Steps:
# Find model-estimated age for each tree in the dataset
# Use that to find model-estimated age in 1941 (simple subtraction)
# Use 1941 age to calculate model-estimated *size* in 1941
# Calculate stand metrics for 1941 vs. 2019

tree_data <- read.csv("Treedata_9-3_Em_DB.csv")
# fix data entry error in row 192
tree_data[192,7] = 36.7
tree_data <- tree_data %>% 
  filter(sqrt((X^2)+(Y^2))<sqrt(10000/pi))

IS_trees <- tree_data[tree_data$Site=="IS",]
names(IS_trees)[5] <- "dbh"

# IS_count <- IS_trees %>% 
#   group_by(Spec) %>% 
#   summarize(abundance = sum(!is.na(dbh)))
# ggplot(data=IS_count)+
#   geom_bar(stat="identity", mapping = aes(x=Spec, y=abundance))
# ggplot(IS_trees, aes(x=dbh, fill=Spec))+
#   geom_histogram() + ggtitle("Species at Indiana Summit")
# # 546 PIJE, 4 PICO

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
  mutate(age_est = predict(IS_lm, newdata = IS_snags)) %>% 
# mutate(age_est = DBH/0.307204)%>%  # Old method based on straight-line regression; now need to use predict
  mutate(dec_correction =
           case_when(Dec == 1 ~ 2,
                     TRUE ~ 9)) %>%
  mutate(estab_est = round(2018 - (age_est+dec_correction),0))

# SNAGS ARE READY AT INDIANA SUMMIT

IS_livetrees <- IS_livetrees %>% 
  mutate(Code = paste0(Plot, Core.))
IS_correction$Code <- gsub("B","", IS_correction$series) # remove all the Bs
sum(IS_livetrees$Code %in% IS_correction$Code) # 68

allcodes <- IS_livetrees$Code
goodcodes <- allcodes[c(which(IS_livetrees$Code %in% IS_correction$Code))]

# now 68 livetrees have a Code I can look up in the IS_correction df to pull corrected_age (and reassign to age_est)

IS_livetrees <- IS_livetrees %>%
     mutate(age_est = predict(IS_lm, newdata = IS_livetrees)) 
# !! need to re-assign age_est with the core-based ages of the trees that we cored
for(i in 1:length(goodcodes)){
  IS_livetrees[IS_livetrees$Code == goodcodes[i],]$age_est <- IS_correction[IS_correction$Code == goodcodes[i], 22]
}

IS_livetrees <- IS_livetrees %>%
  mutate(estab_est = round(2018 - age_est,0))

IS_livetrees$dec_correction <- 0

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
# subset to IS
IS_logs <- log_data[log_data$Site=="IS",]
unique(IS_logs$Spec) 
# [1] "PIJE"  "PICO"  "PIJE*"
# 2018 - [Years since death (based on decay class) + age from DBH] = establishment year
# I will do a *placeholder*/best guess age correction for the logs. 1 = 10y, 2-3 = 15y, 4-5 = 20y
IS_logs$dec_correction[IS_logs$Dec == 1]=10
IS_logs$dec_correction[IS_logs$Dec > 1 & IS_logs$Dec < 4]=15
IS_logs$dec_correction[IS_logs$Dec > 3 & IS_logs$Dec <=5]=20
IS_logs <- IS_logs %>%
  mutate(age_est = predict(IS_lm, newdata = IS_logs)) %>% 
  mutate(estab_est = round(2018 - (age_est+dec_correction),0))

#______________________________________________________________________________#
# rbind livetrees, logs, and snags to get complete tree dataset for all times and peoples:
# names(IS_livetrees)
# names(IS_snags)
# names(IS_logs) 

IS_trees <- rbind(IS_livetrees[,c(1:9,15,16,17)],IS_snags[,c(1:9,14,16,15)],IS_logs[,c(1:9,11,12,10)])
unique(IS_trees$Spec)
# [1] "PIJE"  "PICO" "PIJE*"
IS_trees$Spec[IS_trees$Spec=="PIJE*"] <- "PIJE"
unique(IS_trees$Spec)
# [1] "PIJE"  "PICO"

#______________________________________________________________________________#
# Size in 1941 = (1941 - estab. year) * size coefficient
# simplest, works for trees snags and logs!
# 1941 tree dataframe
#IS_trees1941 <- IS_trees %>%
#  mutate(dbh1941_est = (1941 - estab_est)*0.307204) %>%
#  filter(dbh1941_est >= 5)
# ^ I don't like this anymore; each tree should get its true dbh minus 23.7cm (estimate of average growth increment in 77y)
# but snags and logs shouldn't be penalized for the time they spent dead; they get back x years of deadness * growth coefficient
# NEW MODEL NEW METHOD
# Subtract 77y from each age in 2018 = 1941 age
# find dbh from 1941 age using dbh = (age^(1/0.95850))/3.61166
IS_trees1941 <- IS_trees %>% 
  mutate(age1941 = 1941 - estab_est) %>% 
  #mutate(dbh1941 =round(age1941^(1/0.95850)/3.61166)) %>% 
  mutate(dbh1941 = round((age1941 - 9.178)/2.889)) %>% 
   # I think this worked; need to filter out rows with trees that have NaN or <5 DBH
  filter(!is.na(dbh1941)) %>% # this takes it from 650 to 412 trees
  filter(dbh1941>=5) # and this goes from 412 to 326 ! #IS_livetrees has 361 observations :)
# hist(IS_trees1941$dbh1941, breaks = 10)
# hist(IS_livetrees$dbh, breaks = 10)
# then we are ready to compare stand metrics in 1941 to 2018 using MANOVA
#______________________________________________________________________________#
# Size in 2015, before Clark Fire
# need to account for trees that were dead *before* Clark Fire, i.e. Dec = ≥2 for snags and all logs
# simpler way is to use dec_correction (in years) and filter for >2, <10y (snags)

IS_trees2015 <- IS_trees %>% 
  mutate(age2015 = 2015 - estab_est) %>% 
  mutate(dbh2015 = dbh - ((2018-2015)*2.889)) %>% # for this very recent forest, just subtract, not estimating size from age
  # need to filter out rows with trees that have NaN or <5 DBH
  filter(!is.na(dbh2015)) %>% # 
  filter(dbh2015>=5)

IS_snags2015 <- IS_trees2015 %>% 
  filter(dec_correction > 2 & dec_correction < 10) # 2015 snags will be those with >2 but <10y correction

IS_trees2015 <- IS_trees2015 %>% 
  filter(dec_correction < 3) # remove trees dead before Clark Fire, but keep all live trees as of 2015


#______________________________________________________________________________#
# Prepping all IS sites in 2018
IS1_2018 <- IS_livetrees[IS_livetrees$Plot == "IS1",]
IS2_2018 <- IS_livetrees[IS_livetrees$Plot == "IS2",]
IS3_2018 <- IS_livetrees[IS_livetrees$Plot == "IS3",]

# Prepping all IS sites in 2015
IS1_2015 <- IS_trees2015[IS_trees2015$Plot == "IS1",]
IS2_2015 <- IS_trees2015[IS_trees2015$Plot == "IS2",]
IS3_2015 <- IS_trees2015[IS_trees2015$Plot == "IS3",]
# Need to reassign dbh to 2015 value
IS1_2015 <- rename(IS1_2015, "dbh2018" = "dbh", "dbh" = "dbh2015")
IS2_2015 <- rename(IS2_2015, "dbh2018" = "dbh", "dbh" = "dbh2015")
IS3_2015 <- rename(IS3_2015, "dbh2018" = "dbh", "dbh" = "dbh2015")

# Prepping all IS sites in 1941
IS1_1941 <- IS_trees1941[IS_trees1941$Plot == "IS1",]
IS2_1941 <- IS_trees1941[IS_trees1941$Plot == "IS2",]
IS3_1941 <- IS_trees1941[IS_trees1941$Plot == "IS3",]
# Need to reassign dbh to 1941 value
IS1_1941 <- rename(IS1_1941, "dbh2018" = "dbh", "dbh" = "dbh1941")
IS2_1941 <- rename(IS2_1941, "dbh2018" = "dbh", "dbh" = "dbh1941")
IS3_1941 <- rename(IS3_1941, "dbh2018" = "dbh", "dbh" = "dbh1941")

#______________________________________________________________________________#
# establishment histogram for cored trees
IS_estab_cores <- hist(rwl.stats(IS_data)$first, breaks = (1982-1595)/10)
# for estimated establishment (all trees)
IS_estab_trees <- hist(IS_trees$estab_est, breaks=(1999-1603)/10)

# size class distn
IS_2018_hist <- hist(IS_trees$dbh)
IS_1941_hist <- hist(IS_trees1941$dbh1941)

# plot comparing age estimate model with observed, corrected ages for cored trees (purple dots)
# need to correct for PIJE* and UNK
model_v_cores_IS <- ggplot() +
  geom_point(data = IS_trees, aes(x=dbh, y=age_est, group=Spec, color = Spec)) +
  geom_point(data = IS_correction, aes(x=dbh, y=corrected_age), col="purple")

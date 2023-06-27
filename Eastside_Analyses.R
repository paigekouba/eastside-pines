# Wed 3/16/22
# Final Analyses for Eastside Pines Paper
library(dplR)
library(tidyverse)

#1 Started by making the final .rwl collection for Indiana Summit, IS_2022 (in Images and POS Files > Indiana Summit).
# add all .pos files to a rwl, excluding original core if a duplicate was made (e.g. for IS140 and IS140B, keep only IS140B). N = 91
# Read in the files. Need rwl collection from IS, and core height data.
#setwd("S:/FacultyData/LATIMER/LATIMERShared/Paige/PaigeCores/Images and POS files/Indiana_Summit")
IS_data = read.rwl("IS_2022.rwl")

#2 Same for OH; excluded OH129, OH13, OH250, OH252, and OH3110 due to no date. Excluded OH3119 because of formatting issues. N = 86
#setwd("S:/FacultyData/LATIMER/LATIMERShared/Paige/PaigeCores/Images and POS files/OHarrell_Canyon")
OH_data = read.rwl("OH_2022.rwl")
# IS21003, OH115, and OH11a have no pith estimate due to aberrations; remove from df. 
IS_data <- subset(IS_data, select = -c(IS21003))
OH_data <- subset(OH_data, select = -c(OH115, OH11a))

#setwd("S:/FacultyData/LATIMER/LATIMERShared/Paige/PaigeCores/Spreadsheets")
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
OH_pith = read.csv("OH_pith.csv")
# remove pithless ones
IS_pith <- subset(IS_pith, Core != "IS21003")
OH_pith <- subset(OH_pith, Core != c("OH115", "OH11a"))

## 4. Calculate GR15 (average annual growth rate of first 15 rings)
GR15 = function(x){
  non_na_widths = x[which(!is.na(x))]
  first_15 = non_na_widths[1:15]
  return(mean(first_15))
}
# apply(IS_data, 2, GR15)
# apply(OH_data, 2, GR15)

IS_correction <- cbind(cbind(rwl.stats(IS_data), apply(IS_data,2,GR15)), IS_pith)
OH_correction <- cbind(cbind(rwl.stats(OH_data), apply(OH_data,2,GR15)), OH_pith)

## 4.1 Incorporate core height. 
IS_correction$dbh <- core_height$DBH[match(IS_correction$series, core_height$Code)]
IS_correction$HT <- core_height$Ht[match(IS_correction$series, core_height$Code)]
names(IS_correction)[11] <- "GR15"
OH_correction$dbh <- core_height$DBH[match(OH_correction$series, core_height$Code)]
OH_correction$HT <- core_height$Ht[match(OH_correction$series, core_height$Code)]
names(OH_correction)[11] <- "GR15"


## 5. Age correction function
IS_correction <- IS_correction %>% 
  filter(!is.na(dbh)) %>% 
  mutate(correction =
           0.591 * (HT+1)^0.665 + GR15^-0.497) %>% # from Fraver 2011 (Pinus resinosa)
  mutate(corrected_age =
           year + correction + YearsToPith)

OH_correction <- OH_correction %>% 
  filter(!is.na(dbh)) %>% 
  mutate(correction =
           0.591 * (HT+1)^0.665 + GR15^-0.497) %>% 
  mutate(corrected_age =
           year + correction + YearsToPith)

## 6. Age-size regression for ONLY PIJE
# DBH = a + corrected_age*b
lm_IS <- lm(dbh ~ 0 + corrected_age, IS_correction)
summary(lm_IS)
# a = 0  b = 0.307204
# DBH = corrected_age*0.307204
# find DBH of tree that established in 1941 (whose age is 2018 - 1941 = 77)
77*0.307204
# = 23.7 cm !!!

lm_OH <- lm(dbh ~ 0 + corrected_age, OH_correction)
summary(lm_OH)
# a = 0  b = 0.241820
# DBH = corrected_age*0.241820
# find DBH of PIJE tree that established in 1941 (whose age is 2018 - 1941 = 77)
77*0.241820
# = 18.6 cm !!!

#______________________________________________________________________________#
# What is the species breakdown of each site? *Agnostic to Decay Class
tree_data <- read.csv("Treedata_9-3_Em_DB.csv")
IS_trees <- tree_data[tree_data$Site=="IS",]
# fix one tree with X = 367.0, presumed data entry error. Reassign 36.7
IS_trees$X[IS_trees$X == 367.0] <- 36.7
OH_trees <- tree_data[tree_data$Site=="OH",]
# For OH, drop CELE and BEOC, and reassign PIFL to PIJE, and get rid of "PIJE "
unique(OH_trees$Spec)
# [1] "PIJE"  "JUN"   "ABCO"  "CELE"  "JUGR"  "PICO"  "PIFL"  "CELE " "PIJE " "BEOC" 
OH_trees <- OH_trees %>% 
  filter(Spec != "CELE") %>% 
  filter(Spec != "CELE ") %>%  
  filter(Spec != "BEOC")
OH_trees$Spec <- gsub("PIJE ", "PIJE", OH_trees$Spec)
OH_trees$Spec <- gsub("PIFL", "PIJE", OH_trees$Spec)
OH_trees$Spec <- gsub("JUN", "JUGR", OH_trees$Spec)
# unique(OH_trees$Spec)
#[1] "PIJE" "JUGR" "ABCO" "PICO"

IS_count <- IS_trees %>% 
  group_by(Spec) %>% 
  summarize(abundance = sum(!is.na(DBH)))
ggplot(data=IS_count)+
  geom_bar(stat="identity", mapping = aes(x=Spec, y=abundance))

OH_count <- OH_trees %>% 
  group_by(Spec) %>% 
  summarize(abundance = sum(!is.na(DBH)))
ggplot(data=OH_count)+
  geom_bar(stat="identity", mapping = aes(x=Spec, y=abundance))

ggplot(IS_trees, aes(x=DBH, fill=Spec))+
  geom_histogram() + ggtitle("Species at Indiana Summit")
ggplot(OH_trees, aes(x=DBH, fill=Spec))+
  geom_histogram() + ggtitle("Species at OHarrell Canyon")
# should this exclude, or somehow note, the dead trees?

#______________________________________________________________________________#
# Establishment dates at IS, NOT ACCOUNTING FOR SNAGS AND NOT INCLUDING LOGS
#IS_trees <- IS_trees %>% 
#  mutate(age_est =
#           DBH/0.307204) %>% 
#  mutate(estab_est =
#           round(2018 - age_est,0))

#______________________________________________________________________________#
# Pull out standing snags
IS_snags <- IS_trees[!is.na(IS_trees$Dec),]
IS_livetrees <- IS_trees[is.na(IS_trees$Dec),]
# If Dec = 1, assume tree died in 2016 Clark Fire; adjusted age is age_est - 2, and adjusted estab is estab_est + 2
# If Dec > 1, assume tree died 10 years ago; adjusted age is age_est - 10, and adjusted estab is estab_est + 10
# 95% of PIJE have fallen by year 10; using 10 may cause underestimate of tree age, therefore bias size-age ratio up
#STOP! Age estimate inferred from DBH should not change for standing dead trees; the establishment date is the number that will change. Example: Tree of diameter 23.7 is still 77 years old at IS, but if decay class is 4, then establishment date = 2018-(77+10)
# Need to add column for age estimate:
IS_snags <- IS_snags %>% 
  mutate(age_est = DBH/0.307204)

IS_snags <- IS_snags %>% 
  mutate(snag_correction =
            case_when(Dec == 1 ~ 2, 
                    TRUE ~ 10))
# old mistake code: (the actual age of the tree depends on DBH and is not adjusted for decay;
# all that's affected by decay of 1-5 is the estab_est)
#IS_snags <- IS_snags %>% 
#  mutate(age_est = age_est - snag_correction) %>% 
#  mutate(estab_est = round(2018 - age_est,0))
IS_snags <- IS_snags %>% 
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
# have to rbind livetrees, logs, and snags when all is said and done. See next line:
IS_trees <- rbind(IS_livetrees[,c(1:9,14,15)],IS_snags[,c(1:9,15,16)],IS_logs[,c(1:9,11,12)])
# export as .csv file for Ian
write.csv(IS_trees, "/Users/paigekouba/Documents/UC_Davis/North/EastSierras_manuscript/R_scripts/IS_trees2019_2.14.23.csv", row.names=FALSE)
# Stopping here for the day. Next time, double check why no logs became <5cm ?

#______________________________________________________________________________#
# Size in 1941 = (1941 - estab. year) * size coefficient
# simplest, works for trees snags and logs!
# 1941 tree dataframe
IS_trees1941 <- IS_trees %>% 
  mutate(dbh1941_est = (1941 - estab_est)*0.307204) %>% 
  filter(dbh1941_est >= 5) 


# For 1941 reconstruction: DBH1941 = DBH - 23.7 + (coeff * snag_correction)
                          #  DBH1941 = DBH - 23.7 + (coeff * log_correction)

#______________________________________________________________________________#
# OLD code
#IS_trees needs to be updated to include age-corrected snags:
#IS_trees <- rbind(IS_livetrees,IS_snags[,-16])

#jpeg("IS_Ages",1920,1200, quality = 100, pointsize = 46)
hist(IS_trees$estab_est, breaks = 20, main=paste ("Establishment Dates at Indiana Summit"), col="lightblue",
     xlim=c(1625,2010), ylim=c(0,120), xlab="Estimated First Year of Growth")
#    text(paste("N = ", length(inplot$age_est)), x=350, y=49)
abline(v=1941, lwd = 5.5, lty = 2, col="red")



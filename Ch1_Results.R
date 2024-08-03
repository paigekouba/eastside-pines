# Chapter 1 Results
# Thu 4/18/24

# run after deadwood, Pines_ESA2023, OHPines_ESA2023, EastPinesICO_edgecorr, PatchMorph_ICO

# t.tests on nonspatial metrics

# Welch two-sample t-test, DBH
t.test(IS_trees1941$dbh1941, IS_trees1995$dbh1995, var.equal=FALSE)
# t = -0.99664, df = 612.05, p-value = 0.3193  difference in means is not significant; 1995 trees are ~ same dbh as 1941
t.test(IS_livetrees$dbh, IS_trees1995$dbh1995, var.equal=FALSE) # t = 6.9658, df = 706.3, p-value = 7.486e-12,  47.37528 - 33.56814 = 13.80714
t.test(OH_trees1941$dbh1941, OH_trees2006$dbh2006, var.equal=FALSE)
# p-value = 0.172 difference in means not significant; *no change* in size at OH
t.test(OH_livetrees$dbh, OH_trees2006$dbh2006, var.equal=FALSE) # p-value = 0.3934

# Welch two-sample t-test, QMD
# QMD_1941 <- unlist(lapply(c(2,4,6,8,10,12), function(x) plots_out[[x]][[5]][4]))
# QMD_FE <- unlist(lapply(c(13:18), function(x) plots_out[[x]][[5]][4]))
# QMD_2018 <- unlist(lapply(c(1,3,5,7,9,11), function(x) plots_out[[x]][[5]][4]))

# 1941 vs fire-excluded, all
t.test(QMD_1941, QMD_FE) # p-value = 0.8886
# fire-excluded vs 2018, all
t.test(QMD_FE, QMD_2018) # p-value = 0.168
IS_QMD_1941 <- unlist(lapply(c(2,4,6), function(x) plots_out[[x]][[5]][4]))
IS_QMD_FE <- unlist(lapply(c(13:15), function(x) plots_out[[x]][[5]][4]))
IS_QMD_2018 <- unlist(lapply(c(1,3,5), function(x) plots_out[[x]][[5]][4]))
#sqrt(sum(((IS_livetrees$dbh)^2))/nrow(IS_livetrees))
t.test(IS_QMD_1941, IS_QMD_FE) # p-value = 0.8854
t.test(IS_QMD_FE, IS_QMD_2018) # p-value = 0.05282

OH_QMD_1941 <- unlist(lapply(c(8,10,12), function(x) plots_out[[x]][[5]][4]))
OH_QMD_FE <- unlist(lapply(c(16:18), function(x) plots_out[[x]][[5]][4]))
OH_QMD_2018 <- unlist(lapply(c(7,9,11), function(x) plots_out[[x]][[5]][4]))
t.test(OH_QMD_1941, OH_QMD_FE) # p-value 0.6877
t.test(OH_QMD_FE, OH_QMD_2018) # 0.7822

# Welch two-sample t-test, SDI (English)
SDI_1941 <- unlist(lapply(c(2,4,6,8,10,12), function(x) plots_out[[x]][[5]][11]))
SDI_FE <- unlist(lapply(c(13:18), function(x) plots_out[[x]][[5]][11]))
SDI_2018 <- unlist(lapply(c(1,3,5,7,9,11), function(x) plots_out[[x]][[5]][11]))
# 1941 vs fire-excluded, all
t.test(SDI_1941, SDI_FE) # 74.56667 145.38333 p-value = 0.0006699
# fire-excluded vs 2018, all
t.test(SDI_FE, SDI_2018) # p-value =  0.3341

# TPH
t.test(c(dotplots_ISn$TPH.2, dotplots_OHn$TPH.2), c(dotplots_ISn$TPH, dotplots_OHn$TPH)) # prefire v 1941
# t = 2.6436, df = 8.6219, p-value = 0.02772  132.16667 - 70.33333 = 61.83334
t.test(c(dotplots_ISn$TPH.1, dotplots_OHn$TPH.1), c(dotplots_ISn$TPH.2, dotplots_OHn$TPH.2)) # prefire v 2018; p-value = p-value = 0.1284

# BAH
t.test(c(dotplots_ISn$BAH.2, dotplots_OHn$BAH.2), c(dotplots_ISn$BAH, dotplots_OHn$BAH)) # prefire v 1941
# t = 6.7624, df = 6.6658, p-value = 0.0003249   22.63333 - 11.91667  = 10.71666
t.test(c(dotplots_ISn$BAH.1, dotplots_OHn$BAH.1), c(dotplots_ISn$BAH.2, dotplots_OHn$BAH.2))# prefire v 2018; p-value = 0.5833


####################
# Cluster size change

# what is average cluster size at IS in 1941, 2018, and 1995?
# get average cluster size from plots c(2,4,6) [1941] and c(1,3,5) [2018]
mean(c(plots_out[[2]]$clusters$size, plots_out[[4]]$clusters$size, plots_out[[6]]$clusters$size)) # 1.453552 in 1941
mean(c(plots_out[[1]]$clusters$size, plots_out[[3]]$clusters$size, plots_out[[5]]$clusters$size)) # 2.340741 in 2018
mean(c(plots_out[[13]]$clusters$size, plots_out[[14]]$clusters$size, plots_out[[15]]$clusters$size)) # 2.526829 in 1995

t.test(c(plots_out[[13]]$clusters$size, plots_out[[14]]$clusters$size, plots_out[[15]]$clusters$size), c(plots_out[[2]]$clusters$size, plots_out[[4]]$clusters$size, plots_out[[6]]$clusters$size))
# 1941 v 1995: t = 5.684, df = 293.76, p-value = 3.172e-08,  2.526829  1.453552 
t.test(c(plots_out[[13]]$clusters$size, plots_out[[14]]$clusters$size, plots_out[[15]]$clusters$size), c(plots_out[[1]]$clusters$size, plots_out[[3]]$clusters$size, plots_out[[5]]$clusters$size)) # 1995 v 2018: p=0.4738


# what is average cluster size at OH in 1941 and 2018?
# get average cluster size from plots c(8,10,12) [1941] and c(7,9,11) [2018]
mean(c(plots_out[[8]]$clusters$size, plots_out[[10]]$clusters$size, plots_out[[12]]$clusters$size)) # 1.295455 in 1941
mean(c(plots_out[[7]]$clusters$size, plots_out[[9]]$clusters$size, plots_out[[11]]$clusters$size)) # 1.906832 in 2018
mean(c(plots_out[[16]]$clusters$size, plots_out[[17]]$clusters$size, plots_out[[18]]$clusters$size))# 2.016484 in 2006

t.test(c(plots_out[[16]]$clusters$size, plots_out[[17]]$clusters$size, plots_out[[18]]$clusters$size),c(plots_out[[8]]$clusters$size, plots_out[[10]]$clusters$size, plots_out[[12]]$clusters$size))
# 2006 v 1941; t = 4.3477, df = 230.59, p-value = 2.065e-05,  1.967033  1.280303 
t.test(c(plots_out[[16]]$clusters$size, plots_out[[17]]$clusters$size, plots_out[[18]]$clusters$size), c(plots_out[[7]]$clusters$size, plots_out[[9]]$clusters$size, plots_out[[11]]$clusters$size))
# 2006 v 2018: p-value = 0.6082
# what is average cluster size across both sites in 1941 and 2018?
mean(c(plots_out[[8]]$clusters$size, plots_out[[10]]$clusters$size, plots_out[[12]]$clusters$size, plots_out[[2]]$clusters$size, plots_out[[4]]$clusters$size, plots_out[[6]]$clusters$size)) # 1.387302 in 1941
mean(c(plots_out[[13]]$clusters$size, plots_out[[14]]$clusters$size, plots_out[[15]]$clusters$size, plots_out[[16]]$clusters$size, plots_out[[17]]$clusters$size, plots_out[[18]]$clusters$size)) # 2.286822 in prefire
mean(c(plots_out[[7]]$clusters$size, plots_out[[9]]$clusters$size, plots_out[[11]]$clusters$size, plots_out[[1]]$clusters$size, plots_out[[3]]$clusters$size, plots_out[[5]]$clusters$size)) # 2.10473 in 2018

t.test(c(plots_out[[8]]$clusters$size, plots_out[[10]]$clusters$size, plots_out[[12]]$clusters$size, plots_out[[2]]$clusters$size, plots_out[[4]]$clusters$size, plots_out[[6]]$clusters$size), c(plots_out[[13]]$clusters$size, plots_out[[14]]$clusters$size, plots_out[[15]]$clusters$size, plots_out[[16]]$clusters$size, plots_out[[17]]$clusters$size, plots_out[[18]]$clusters$size))
# 1941 to prefire: t = -6.9986, df = 538.68, p-value = 7.715e-12, 1.389937  2.278481 
t.test(c(plots_out[[13]]$clusters$size, plots_out[[14]]$clusters$size, plots_out[[15]]$clusters$size, plots_out[[16]]$clusters$size, plots_out[[17]]$clusters$size, plots_out[[18]]$clusters$size), c(plots_out[[7]]$clusters$size, plots_out[[9]]$clusters$size, plots_out[[11]]$clusters$size, plots_out[[1]]$clusters$size, plots_out[[3]]$clusters$size, plots_out[[5]]$clusters$size))
# prefire to 2018: p-value = 0.2795

#######
# ICO changes
IS18_clusters_ub <- c(plots_out[[1]]$clusters$size, plots_out[[3]]$clusters$size, plots_out[[5]]$clusters$size)
IS41_clusters_ub <- c(plots_out[[2]]$clusters$size, plots_out[[4]]$clusters$size, plots_out[[6]]$clusters$size)
IS95_clusters_ub <- c(plots_out[[13]]$clusters$size, plots_out[[14]]$clusters$size, plots_out[[15]]$clusters$size)
# hist(IS41_clusters_ub, col=rgb(1,0,0,1/4), breaks = 6, xlim=c(0,15), ylim=c(0,170))
# hist(IS18_clusters_ub, col=rgb(0,0,1,1/4), breaks = 15, xlim=c(0,15), ylim=c(0,170), add=T)

# numbers 1-15, since max clust size at IS is 15; plus how many times that # shows up in each list (IS18, IS95 or IS41)
IS_clust_ub_table <- data.frame(c(1:15))
for (i in 1:15){
  IS_clust_ub_table[i,2] <- sum(i == IS18_clusters_ub)
  IS_clust_ub_table[i,3] <- sum(i == IS95_clusters_ub)
  IS_clust_ub_table[i,4] <- sum(i == IS41_clusters_ub)
}
names(IS_clust_ub_table) <- c("clust.sz","IS2018","IS1995","IS1941")
fisher.test(t(IS_clust_ub_table)[3:4,], simulate.p.value=TRUE) # 1995 v 1941, p value =  0.0004998
fisher.test(t(IS_clust_ub_table)[2:3,]) # 2018 v 1995, p-value = 0.9445

OH18_clusters_ub <- c(plots_out[[7]]$clusters$size, plots_out[[9]]$clusters$size, plots_out[[11]]$clusters$size)
OH41_clusters_ub <- c(plots_out[[8]]$clusters$size, plots_out[[10]]$clusters$size, plots_out[[12]]$clusters$size)
OH06_clusters_ub <- c(plots_out[[16]]$clusters$size, plots_out[[17]]$clusters$size, plots_out[[18]]$clusters$size)
# hist(OH41_clusters_ub, col=rgb(1,0,0,1/4), breaks = 4, xlim=c(0,20), ylim=c(0,100))
# hist(OH18_clusters_ub, col=rgb(0,0,1,1/4), breaks = 20, xlim=c(0,20), ylim=c(0,100), add=T)

OH_clust_ub_table <- data.frame(c(1:14))
for (i in 1:14){
  OH_clust_ub_table[i,2] <- sum(i == OH18_clusters_ub)
  OH_clust_ub_table[i,3] <- sum(i == OH06_clusters_ub)
  OH_clust_ub_table[i,4] <- sum(i == OH41_clusters_ub)
}

names(OH_clust_ub_table) <- c("clust.sz","OH2018","OH2006","OH1941")
fisher.test(t(OH_clust_ub_table)[3:4,]) # 2006 v 1941, p-value = 0.02704
fisher.test(t(OH_clust_ub_table)[2:3,]) # 2018 v 2006, p-value = 0.9215

##############
# binned forest openings
plotnames <- c("IS1", "IS1", "IS2", "IS2", "IS3", "IS3", "OH1", "OH1", "OH2", "OH2", "OH3", "OH3", "IS1", "IS2", "IS3", "OH1", "OH2", "OH3")
plotyears <- c(rep(c(2018,1941),6),rep("Fire Excluded",6))
gap_bins <- c("82-500", "500-1500","1500-2500",">2500")
bin_brks <- c(82,500,1500,2500,7000)
opes_bins <- data.frame(matrix(NA, nrow=72, ncol=4))
j=1
plotcounts <- list()
for (j in 1:length(plots_out)){
  plotcounts[[j]] <- vector()
  countperha <- vector()
  for (i in 1:length(gap_bins)){
    thingy <- sum(opes_sr[[j]]$area < bin_brks[i+1] & opes_sr[[j]]$area > bin_brks[i])
    countperha <- c(countperha, thingy)
  }
  plotcounts[[j]] <- countperha}

opes_bins[,1] <- rep(plotyears, each = 4)
opes_bins[,2] <- rep(plotnames, each = 4)
opes_bins[,3] <- rep(gap_bins,18)
opes_bins[,4] <- unlist(plotcounts)
names(opes_bins) <- c("Year", "Plot", "gap_bin", "countperha")

opes_bins$gap_bin <- factor(opes_bins$gap_bin, levels = c("82-500", "500-1500","1500-2500",">2500"))
opes_bins$Year <- factor(opes_bins$Year, levels = c("1941","Fire Excluded","2018"))
opes_bins$Plot <- as.character((opes_bins)[,2])

opes_bins_wide <- opes_bins %>% 
  pivot_wider(names_from = "gap_bin", values_from = "countperha")
# df for comparing 1941 to fire-excluded
obw_1 <- filter(opes_bins_wide, Year %in% c(1941, "Fire Excluded"))
# df for comparing fire-excluded to 2018
obw_2 <- filter(opes_bins_wide, Year %in% c("Fire Excluded", 2018))

response_matrix <- obw_1[, gap_bins]
adonis2(response_matrix ~ Year, obw_1, strata = obw_1$Plot) # Pr(>F) 0.03125 

response_matrix <- obw_2[, gap_bins]
adonis2(response_matrix ~ Year, obw_2, strata = obw_2$Plot) # Pr(>F) 0.3125

##############
# forest gaps + cluster sizes PERMANOVA
plotnames <- c("IS1", "IS1", "IS2", "IS2", "IS3", "IS3", "OH1", "OH1", "OH2", "OH2", "OH3", "OH3", "IS1", "IS2", "IS3", "OH1", "OH2", "OH3")
plotyears <- c(rep(c(2018,1941),6),rep("Fire Excluded",6))
bin_names # "1"   "2-4" "5-9" "10+"
clust_bins <- data.frame(matrix(NA, nrow=72, ncol=4))
j=1
plotClustCounts <- list()
for (j in 1:length(plots_out)){
  plotClustCounts[[j]] <- vector()
  clustperha <- vector()
  for (i in 1:length(bin_names)){
    thingy <- sum(plots_out[[j]][[12]][9] == bin_names[i])
    clustperha <- c(clustperha, thingy)
  }
  plotClustCounts[[j]] <- clustperha}

clust_bins[,1] <- rep(plotyears, each = 4)
clust_bins[,2] <- rep(plotnames, each = 4)
clust_bins[,3] <- rep(bin_names,18)
# clust_bins[,4] <- unlist(plotClustCounts2)
clust_bins[,4] <- unlist(plotClustCounts)
names(clust_bins) <- c("Year", "Plot", "clust_bin", "clustperha")

clust_bins$clust_bin <- factor(clust_bins$clust_bin, levels = bin_names)
clust_bins$Year <- factor(clust_bins$Year, levels = c("1941","Fire Excluded","2018"))
clust_bins$Plot <- as.character((clust_bins)[,2])

# PERMANOVA on gaps *and* clusters
# first get clusters into wide format
# (Year, Plot, Bin1_ct, Bin2_ct, Bin3_ct, Bin4_ct)
clust_bins_wide <- clust_bins %>% 
  pivot_wider(names_from = "clust_bin", values_from = "clustperha") 
# combine opes_bins_wide and clust_bins_wide ?

all_stx <- cbind(opes_bins_wide, clust_bins_wide[,3:6])
# df for comparing 1941 to fire-excluded
all_stx1 <- filter(all_stx, Year %in% c(1941, "Fire Excluded"))
# df for comparing fire-excluded to 2018
all_stx2 <- filter(all_stx, Year %in% c("Fire Excluded", 2018))

response_matrix <- all_stx1[, c(gap_bins, bin_names)]
adonis2(response_matrix ~ Year, all_stx1, strata = all_stx1$Plot) #  p = 0.03125

response_matrix <- all_stx2[, c(gap_bins, bin_names)]
adonis2(response_matrix ~ Year, all_stx2, strata = all_stx2$Plot) # p = 0.0625



##############
# proportion of trees that were singletons
# (from summary table)

# proportion of clusters that were [bin]
# proportions of clusters in singletons, both sites
sum(c(plots_out[[2]]$clusters$size==1), plots_out[[4]]$clusters$size==1, plots_out[[6]]$clusters$size==1, plots_out[[8]]$clusters$size==1, plots_out[[10]]$clusters$size==1, plots_out[[12]]$clusters$size==1) # 238  in 1941
length(c(plots_out[[2]]$clusters$size, plots_out[[4]]$clusters$size, plots_out[[6]]$clusters$size, plots_out[[8]]$clusters$size, plots_out[[10]]$clusters$size, plots_out[[12]]$clusters$size)) # 311 clusters in 1941
238/311 # 0.7652733, 76.5% of clusters are singletons in 1941

sum(c(plots_out[[13]]$clusters$size==1), plots_out[[14]]$clusters$size==1, plots_out[[15]]$clusters$size==1, plots_out[[16]]$clusters$size==1, plots_out[[17]]$clusters$size==1, plots_out[[18]]$clusters$size==1) # 224 prefire
length(c(plots_out[[13]]$clusters$size, plots_out[[14]]$clusters$size, plots_out[[15]]$clusters$size, plots_out[[16]]$clusters$size, plots_out[[17]]$clusters$size, plots_out[[18]]$clusters$size)) # 384 clusters prefire
224/384 # 0.5833333, 58.3% of clusters are singletons in fire-excluded year

# proportions of clusters in small clusters (2-4)
sum(c(plots_out[[2]]$clusters$bin=="2-4"), plots_out[[4]]$clusters$bin=="2-4", plots_out[[6]]$clusters$bin=="2-4", plots_out[[8]]$clusters$bin=="2-4", plots_out[[10]]$clusters$bin=="2-4", plots_out[[12]]$clusters$bin=="2-4") # 71 in 1941
# 311 clusters in 1941
71/311 # 0.2282958, 22.8% of clusters are 2-4s in 1941
sum(c(plots_out[[13]]$clusters$bin=="2-4"), plots_out[[14]]$clusters$bin=="2-4", plots_out[[15]]$clusters$bin=="2-4", plots_out[[16]]$clusters$bin=="2-4", plots_out[[17]]$clusters$bin=="2-4", plots_out[[18]]$clusters$bin=="2-4") # 116 prefire
130/384 # 0.3385417, 33.9%

# proportions of clusters in medium clusters (5-9)
sum(c(plots_out[[2]]$clusters$bin=="5-9"), plots_out[[4]]$clusters$bin=="5-9", plots_out[[6]]$clusters$bin=="5-9", plots_out[[8]]$clusters$bin=="5-9", plots_out[[10]]$clusters$bin=="5-9", plots_out[[12]]$clusters$bin=="5-9") # 1 in 1941
# 311 clusters in 1941
1/311 # 0.003215434, 0.3% of clusters are 5-9s in 1941
sum(c(plots_out[[13]]$clusters$bin=="5-9"), plots_out[[14]]$clusters$bin=="5-9", plots_out[[15]]$clusters$bin=="5-9", plots_out[[16]]$clusters$bin=="5-9", plots_out[[17]]$clusters$bin=="5-9", plots_out[[18]]$clusters$bin=="5-9") # 35 in prefire
# 384 clusters in prefire
35/384 # 0.09114583, 9.1% of clusters are 5-9s in prefire

# proportions of clusters in large clusters (10+)
sum(c(plots_out[[2]]$clusters$bin=="10+"), plots_out[[4]]$clusters$bin=="10+", plots_out[[6]]$clusters$bin=="10+", plots_out[[8]]$clusters$bin=="10+", plots_out[[10]]$clusters$bin=="10+", plots_out[[12]]$clusters$bin=="10+") # 1 in 1941
# 311 clusters in 1941
1/311 # 0.003215434, 0.3% of clusters are 10+s in 1941
sum(c(plots_out[[13]]$clusters$bin=="10+"), plots_out[[14]]$clusters$bin=="10+", plots_out[[15]]$clusters$bin=="10+", plots_out[[16]]$clusters$bin=="10+", plots_out[[17]]$clusters$bin=="10+", plots_out[[18]]$clusters$bin=="10+") # 9 prefire
# 384 clusters in prefire
9/384 # 0.0234375, 2.3% of clusters are 10+ prefire

##############
# binned clump size contingency analysis
# what I want is a table where each row is a year (1941 OR 2018) 
# and each column is a count of [clusters in the focal bin OR clusters NOT in the focal bin]
## FOR 1941 V PRE-FIRE
# singletons
fisher.test(matrix(c(sum(bin_tests_41[1,3]), sum(bin_tests_41[-1,3]), 
                     sum(bin_tests_prefire[c(1,5),3]), sum(bin_tests_prefire[-c(1,5),3])), byrow=TRUE, 2, 2))
# p-value < 2.2e-16

# 2-4
fisher.test(matrix(c(sum(bin_tests_41[2,3]), sum(bin_tests_41[-2,3]), 
                     sum(bin_tests_prefire[c(2,6),3]), sum(bin_tests_prefire[-c(2,6),3])), byrow=TRUE, 2, 2))
# p-value = 0.4991

# 5-9
fisher.test(matrix(c(sum(bin_tests_41[3,3]), sum(bin_tests_41[-3,3]), 
                     sum(bin_tests_prefire[c(3,7),3]), sum(bin_tests_prefire[-c(3,7),3])), byrow=TRUE, 2, 2))
# p-value < 2.2e-16

# 10+
fisher.test(matrix(c(sum(bin_tests_41[4,3], na.rm = TRUE), sum(bin_tests_41[,3]), sum(bin_tests_prefire[c(4,8),3]), sum(bin_tests_prefire[-c(4,8),3])), byrow=TRUE, 2, 2))
# p-value = 2.239e-11

## FOR PRE-FIRE V 2018
# singletons
fisher.test(matrix(c(sum(bin_tests_18[1,3]), sum(bin_tests_18[-1,3]), 
                     sum(bin_tests_prefire[c(1,5),3]), sum(bin_tests_prefire[-c(1,5),3])), byrow=TRUE, 2, 2))
# p-value 0.6287

# 2-4
fisher.test(matrix(c(sum(bin_tests_18[2,3]), sum(bin_tests_18[-2,3]), 
                     sum(bin_tests_prefire[c(2,6),3]), sum(bin_tests_prefire[-c(2,6),3])), byrow=TRUE, 2, 2))
# p-value = 0.04325

# 5-9
fisher.test(matrix(c(sum(bin_tests_18[3,3]), sum(bin_tests_18[-3,3]), 
                     sum(bin_tests_prefire[c(3,7),3]), sum(bin_tests_prefire[-c(3,7),3])), byrow=TRUE, 2, 2))
# p-value <  0.1146

# 10+
fisher.test(matrix(c(sum(bin_tests_18[4,3], na.rm = TRUE), sum(bin_tests_18[,3]), sum(bin_tests_prefire[c(4,8),3]), sum(bin_tests_prefire[-c(4,8),3])), byrow=TRUE, 2, 2))
# p-value = 0.02506



##############
# openings per ha, average gap size
# mean openings in 1941
mean(unlist(lapply(c(2,4,6,8,10,12), function(x) opes_sr[[x]]$area))) # 1086.887
min(unlist(lapply(c(2,4,6,8,10,12), function(x) opes_sr[[x]]$area))) # 136.32
max(unlist(lapply(c(2,4,6,8,10,12), function(x) opes_sr[[x]]$area))) # 5713.91

# mean openings in Fire Excluded
mean(unlist(lapply(c(13:18), function(x) opes_sr[[x]]$area))) # 493.691
min(unlist(lapply(c(13:18), function(x) opes_sr[[x]]$area))) # 116.29
max(unlist(lapply(c(13:18), function(x) opes_sr[[x]]$area))) # 1932.31
t.test(unlist(lapply(c(2,4,6,8,10,12), function(x) opes_sr[[x]]$area)), unlist(lapply(c(13:18), function(x) opes_sr[[x]]$area)))
# p-value = 0.06755

# mean openings in 2018
mean(unlist(lapply(c(1,3,5,7,9,11), function(x) opes_sr[[x]]$area))) # 1014.161
min(unlist(lapply(c(1,3,5,7,9,11), function(x) opes_sr[[x]]$area))) # 121.35
max(unlist(lapply(c(1,3,5,7,9,11), function(x) opes_sr[[x]]$area))) # 5910.23
t.test(unlist(lapply(c(13:18), function(x) opes_sr[[x]]$area)), unlist(lapply(c(1,3,5,7,9,11), function(x) opes_sr[[x]]$area))) 
# p-value =  0.1297

# did 2018 gaps get restored to 1941 sizes?
t.test(unlist(lapply(c(2,4,6,8,10,12), function(x) opes_sr[[x]]$area)), unlist(lapply(c(1,3,5,7,9,11), function(x) opes_sr[[x]]$area))) # p-value = 0.8664

##############
# PERMANOVA on all structural categories
# PatchMorph to find openings

# what gap radius?

# "set gap threshold to the *minimum* diameter for crowns across all plots"
# "We set ours to 3 m, as all but one tree in our plots had a crown radius > 1.5 m" -- Ng 2020

allcrwndf <- lapply(plots_out, function(x) x[[11]])
allcrwn <- unlist(lapply(allcrwndf, function(x) x[[6]]))

ggplot(as.data.frame(allcrwn), aes(x = allcrwn*2)) + # distribution of tree crown *diameters*
  geom_histogram() +
  geom_vline(aes(xintercept = quantile(allcrwn*2, 0.1)), color = "grey", size = 0.5) + # 2.574679 
  geom_vline(aes(xintercept = quantile(allcrwn*2, 0.9)),  color="grey", size=0.5) + # 7.97808 
  geom_vline(aes(xintercept = mean(allcrwn*2, na.rm = TRUE)),  color="red", size=0.5) +# 4.596546
  ggtitle("Crown Diameters, All Sites, All Years") +
  xlab("Crown Diamter (m)") + ylab("Frequency")

sum(allcrwn < 1)*100/length(allcrwn) # 0.5085529
sum(allcrwn > 5)*100/length(allcrwn) # 1.849283
# sum(allcrwn > 4)*100/length(allcrwn) # 9.98613

min(allcrwn)*2 # 1.486 is the smallest tree crown across all the plots and both times
max(allcrwn)*2 # 11.95104 is the biggest tree crown across all the plots and both times

library(sf)
library(terra)

# patchMorph code:
# https://rdrr.io/github/bi0m3trics/patchwoRk/man/patchMorph.html

#devtools::install_github("bi0m3trics/patchwoRk")
#library(patchwoRk)

# explanation of gap and spur, conceptually, from Girvetz and Greco 2007:
# (1) land cover density threshold (suitThresh), (2) habitat gap maximum thickness (gapThresh), and (3) habitat patch minimum thickness (spurThresh)

# fn to convert plot x,y coords to spatRaster
# set up
ctr = data.frame(X = 0, Y = 0) # plot center to draw boundary of 1ha circle
bound = st_as_sf(ctr, coords = c("X", "Y")) |> st_buffer(sqrt(10000/pi)) # boundary of 1ha circle 

# fn to convert plot x,y coords to not-crowns spatRaster with local crs [and vals for crown area = 0]
xy_sr <- function(plot){ 
  df <- data.frame(X=plots_out[[plot]]$trees.noedge$x, Y=plots_out[[plot]]$trees.noedge$y, crown=plots_out[[plot]]$trees.noedge$crown)
  stems <- st_as_sf(df, coords = c("X", "Y")) #, crs = custom_crs) # points for each tree w/dbh attribute
  crowns = st_buffer(stems, dist = stems$crown) 
  notcrowns <- st_difference(bound, st_union(crowns)) # all area in non-crown space
  yescrowns <- st_difference(bound, st_union(notcrowns)) # all area in crown space (non-overlapping, values = 1)
  r_notcrowns <- rasterize(notcrowns, rast(ext(bound), res = 0.1)) 
  r_yescrowns <- rasterize(yescrowns, rast(ext(bound), res=0.1))
  values(r_yescrowns)[values(r_yescrowns) == 1] <- 0
  rastLayer <- merge(r_notcrowns, r_yescrowns)
  #spatRast <- rast(merge(r_notcrowns, r_yescrowns))
  crs(rastLayer) <- "local"
  return(rastLayer)
}
# test
#plot(xy_sr(4)) # works!

# Need to define patchMorph functions because this version of R ("Innocent and Trusting") doesn't work with the new update
getCircleKernel <- function(radius)
{
  kernel_side <- 2 * as.integer(radius) + 1
  kernel_y <- matrix(rep(radius:-radius, kernel_side), ncol=kernel_side)
  kernel_x <- -t(kernel_y)
  kernel   <- matrix(as.matrix(dist(cbind(as.vector(kernel_x), as.vector(kernel_y))))[as.integer((kernel_side^2) / 2) + 1,], ncol=kernel_side)
  kernel[kernel <= radius] <- 0
  kernel[kernel > 0]  <- 1
  kernel <- 1 - kernel
  return(kernel)
}

patchMorph.SpatRaster <- function(data_in, buffer = 2, suitThresh = 1, gapThresh = 2, spurThresh = 2, verbose = TRUE)
{
  if(!is.numeric(c(suitThresh, gapThresh, spurThresh)))
    stop("suitThresh, gapThresh, and spurThresh must be numeric!!")
  if(gapThresh < max(terra::res(data_in)) | spurThresh < max(terra::res(data_in)))
    stop("Gap/Spur threshold is too small!! Must be at least twice the maximum resolution of the provided raster.")
  if (is.na(crs(data_in)) || crs(data_in) == "")
    stop("CRS is NULL or blank!!")
  
  ## Set up the crs, the extent, and a NA mask for the original raster
  r.crs <- terra::crs(data_in)
  r.e <- terra::ext(data_in)
  e.mask <- terra::mask(data_in, subst(data_in, 0:1, 1))
  
  ## Extend the raster by the buffer (cropped before return)
  data_in <- terra::extend(data_in, buffer, fill=NA)
  
  ## Get the associated kernels
  gapKernel  <- getCircleKernel(ceiling((gapThresh / 2)))
  spurKernel <- getCircleKernel(ceiling((spurThresh / 2)))
  
  ## Get the euclidean distances to suitable habitat, and ensure the extent is the same as original
  data_in <- terra::distance(data_in, target = data_in[data_in <= suitThresh])
  
  ## Apply a focal maximum
  data_in <- terra::focal(data_in, gapKernel, fun="max", na.policy="omit", na.rm=TRUE)
  # data_in <- terra::mask(data_in, e.mask)
  
  if(verbose == TRUE)
    cat("Processing gap threshold diameter:", ncol(gapKernel)-1, terra::units(data_in), "\n")
  ## Reclassify based on the gap threshold
  data_in[data_in <= (ncol(gapKernel)+1)/2] <- 1
  data_in[data_in > (ncol(gapKernel)+1)/2] <- 0
  
  ## Check to make sure there's still non-suitable pixels in the raster, othwewise return data_in
  if( (sum(data_in[terra::values(data_in)==1]) + sum(is.na(terra::values(data_in))) ) == ( nrow(data_in)*ncol(data_in)) ) return(data_in)
  
  ## Get the euclidean distances to non-suitable habitat, and ensure the extent is the same as original
  data_in <- terra::distance(data_in, target = data_in[data_in <= suitThresh])
  
  ## Apply a focal maximum
  data_in <- terra::focal(data_in, spurKernel, fun="max", na.policy="omit", na.rm=TRUE)
  # data_in <- terra::mask(data_in, e.mask)
  
  
  if(verbose == TRUE)
    cat("Processing spur threshold diameter:",ncol(spurKernel)-1, terra::units(data_in), "\n")
  ## Reclassify based on the spur threshold
  data_in[data_in <= (ncol(spurKernel)+1)/2] <- 0
  data_in[data_in > (ncol(spurKernel)+1)/2] <- 1
  
  data_in <- terra::crop(data_in, e.mask, mask=TRUE)
  
  return(data_in)
}

# for loop for openings spatrasters from plots, converted to sf polygons and filtered for "1" attribute

opes_sr <- list()
for (i in 1:length(plots_out)){
  thingy <- patchMorph.SpatRaster(xy_sr(i), buffer=5, suitThresh=1, gapThresh=12, spurThresh=10, verbose=FALSE) %>%
  as.polygons(values = TRUE) %>% 
  st_as_sf() %>% 
# filter(focal_max == 1) %>% # this part could be a place I can fix the 1/0 issue
  filter(focal_max == 0) %>% # indeed, for now the 1s and 0s are flipped in PM; filter out 1s, keep 0s
  st_cast("POLYGON")
  
  st_crs(thingy) <- "local" # attempt to fix ggplot error "cannot transform sfc object with missing crs"
  
  opes_sr[[i]] <- thingy
} 

# I will calculate their associated areas
for (i in 1:length(plots_out)){
  opes_sr[[i]]$area <- st_area(opes_sr[[i]])
}

# first try at gap distn histogram # missing opes_all? :(
# gap_distn <- ggplot(opes_all, aes(x=Opes, fill=as.factor(Year))) +
#   geom_histogram(bins = 15, position="dodge") +
#   scale_fill_manual(values = c("#cf4411", "black", "darkgray")) +
#   stat_bin(geom="text", bins=15, aes(label=after_stat(count), group=as.factor(Year)), vjust = -0.5, position = position_dodge()) +
#   scale_x_continuous(breaks = round(seq(50, 5750, length.out = 15))) +
#   scale_y_continuous(expand=expansion(mult=c(0,0.05))) +
#   labs(title = "Gap Size Distribution", hjust = 5, x = "Forest Canopy Gaps (m^2)", y = "Count", fill="Year") +
#   theme_classic()


# first try of all gap distns combined
# sensible bins: 82-500, 500-1500, 1500-2500, >2500
# I want a df with one row for each count/ha, with Year, Plot, and Bin along with it
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
library(ggpubr)

LydersenFig3 <- 
ggbarplot(opes_bins, x="gap_bin", y = "countperha",  add = "mean_se", fill = "Year", position = position_dodge(0.8)) +
  scale_fill_manual(values=c("#d8b365", "#5ab4ac", "black")) +
  #stat_friedman_test(aes(wid=Plot, group=Year), within = "group", label = "p = {p.format}") +
  labs(title = "Gaps at Both Sites",
       x = "Gap Size (m2)",
       y = "Frequency / ha")


# trying lydersenfig3 for one site at a time, IS first
IS_bins <- data.frame(matrix(NA, nrow=36, ncol=4))
opes_IS <- opes_sr[c(1:6,13:15)]
j=1
plotcounts <- list()
for (j in c(1:6,13:15)){
  plotcounts[[j]] <- vector()
  countperha <- vector()
  for (i in 1:length(gap_bins)){
    thingy <- sum(opes_sr[[j]]$area < bin_brks[i+1] & opes_sr[[j]]$area > bin_brks[i])
    countperha <- c(countperha, thingy)
  }
  plotcounts[[j]] <- countperha}

IS_bins[,1] <- rep(plotyears[c(1:6,13:15)], each = 4)
IS_bins[,2] <- rep(plotnames[c(1:6,13:15)], each = 4)
IS_bins[,3] <- rep(gap_bins,length(opes_IS))
IS_bins[,4] <- unlist(plotcounts)
names(IS_bins) <- c("Year", "Plot", "gap_bin", "countperha")

IS_bins$gap_bin <- factor(IS_bins$gap_bin, levels = c("82-500", "500-1500","1500-2500",">2500"))
IS_bins$Year <- factor(IS_bins$Year, levels = c("1941","Fire Excluded","2018"))

#library(ggpubr)
LydersenFig3_IS <- ggbarplot(IS_bins, x="gap_bin", y = "countperha",  add = "mean_se", fill = "Year", position = position_dodge(0.8)) +
  scale_fill_manual(values=c("#d8b365", "#5ab4ac", "black")) +
  #stat_friedman_test(aes(wid=Plot, group=Year), within = "group", label = "p = {p.format}") +
  #stat_compare_means(paired=TRUE) +
  labs(title = "Gaps at Indiana Summit",
       x = "Gap Size (m2)",
       y = "Frequency / ha")

# now OH
OH_bins <- data.frame(matrix(NA, nrow=36, ncol=4))
opes_OH <- opes_sr[c(7:12,16:18)]
j=1
plotcounts <- list()
for (j in c(7:12,16:18)){
  plotcounts[[j]] <- vector()
  countperha <- vector()
  for (i in 1:length(gap_bins)){
    thingy <- sum(opes_sr[[j]]$area < bin_brks[i+1] & opes_sr[[j]]$area > bin_brks[i])
    countperha <- c(countperha, thingy)
  }
  plotcounts[[j]] <- countperha}

OH_bins[,1] <- rep(plotyears[c(7:12,16:18)], each = 4)
OH_bins[,2] <- rep(plotnames[c(7:12,16:18)], each = 4)
OH_bins[,3] <- rep(gap_bins,length(opes_OH))
OH_bins[,4] <- unlist(plotcounts)
names(OH_bins) <- c("Year", "Plot", "gap_bin", "countperha")

OH_bins$gap_bin <- factor(OH_bins$gap_bin, levels = c("82-500", "500-1500","1500-2500",">2500"))
OH_bins$Year <- factor(OH_bins$Year, levels = c("1941","Fire Excluded","2018"))

LydersenFig3_OH <- ggbarplot(OH_bins, x="gap_bin", y = "countperha",  add = "mean_se", fill = "Year", position = position_dodge(0.8)) +
  scale_fill_manual(values=c("#d8b365", "#5ab4ac", "black")) +
  #stat_friedman_test(aes(wid=Plot, group=Year), within = "group", label = "p = {p.format}") +
  #stat_compare_means(paired=TRUE) +
  labs(title = "Gaps at O'Harrell Canyon",
       x = "Gap Size (m2)",
       y = "Frequency / ha")

#grid.arrange(LydersenFig3, LydersenFig3_IS, LydersenFig3_OH, ncol = 1)

# same figure again, but for binned clusters
# I want a df with one row for each count/ha, with Year, Plot, and Bin along with it
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
clust_bins[,4] <- unlist(plotClustCounts) # clust_bins[,4] <- unlist(plotClustCounts2)
names(clust_bins) <- c("Year", "Plot", "clust_bin", "clustperha")

clust_bins$clust_bin <- factor(clust_bins$clust_bin, levels = bin_names)
clust_bins$Year <- factor(clust_bins$Year, levels = c("1941","Fire Excluded","2018"))
clust_bins$Plot <- as.character((clust_bins)[,2])
library(ggpubr)

LydersenFig3_clusters <- 
  ggbarplot(clust_bins, x="clust_bin", y = "clustperha",  add = "mean_se", fill = "Year", position = position_dodge(0.8)) +
  scale_fill_manual(values=c("#d8b365", "#5ab4ac", "black")) +
  #stat_friedman_test(aes(wid=Plot, group=Year), within = "group", label = "p = {p.format}") +
  labs(title = "Clumps at Both Sites",
       x = "Clump Bin",
       y = "Frequency / ha")


# PERMANOVA
library(PERMANOVA)
# first get opes_bins (which now has Year, Plot, gap_bin, countperha) into wide format 
# (Year, Plot, Bin1_ct, Bin2_ct, Bin3_ct, Bin4_ct)
opes_bins_wide <- opes_bins %>% 
  pivot_wider(names_from = "gap_bin", values_from = "countperha") #%>% 
  #filter(Plot %in% c("IS1","IS2","IS3"))
  #filter(Plot %in% c("OH1","OH2","OH3"))

# one df for comparing 1941 to fire-excluded, one for comparing fire-excluded to 2018

# df for comparing 1941 to fire-excluded
obw_1 <- filter(opes_bins_wide, Year %in% c(1941, "Fire Excluded"))
# df for comparing fire-excluded to 2018
obw_2 <- filter(opes_bins_wide, Year %in% c("Fire Excluded", 2018))

# create a multivariate response variable w/ only the columns of the data from that include the counts in bins
mv_counts_response1 = obw_1[, gap_bins]
mv_counts_response2 = obw_2[, gap_bins]
# create an explanatory variable that is the Year column of the data frame 
mv_counts_explan1 = droplevels(obw_1$Year)
mv_counts_explan2 = droplevels(obw_2$Year)
# run PERMANOVA on that response variable, using Year as the explanatory variable
PERMANOVA(DistContinuous(mv_counts_response1),mv_counts_explan1) # difference from 1941 to fire-excluded?
# MANOVA
# Explained Residual df Num df Denom     F-exp       p-value      p-value adj.
# Total  7.416667 39.16667      2        9 0.8521277 0.1728272    0.1728272

PERMANOVA(DistContinuous(mv_counts_response2),mv_counts_explan2) # difference from fire-excluded to 2018?
# MANOVA
# Explained Residual df Num df Denom     F-exp       p-value      p-value adj.
# Total         4 43.33333      2        9 0.4153846 0.3696304    0.3696304

library(vegan)
response_matrix <- obw_1[, gap_bins]
adonis2(response_matrix ~ Year, obw_1, strata = obw_1$Plot) # Pr(>F) 0.03125 

response_matrix <- obw_2[, gap_bins]
adonis2(response_matrix ~ Year, obw_2, strata = obw_2$Plot) # Pr(>F) 0.25


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

sum(opes_bins[opes_bins$Year==1941,]$countperha)
#[1] 26
sum(opes_bins[opes_bins$Year=="Fire Excluded",]$countperha)
#[1] 32
sum(opes_bins[opes_bins$Year==2018,]$countperha)
#[1] 21

# mean openings in 1941
mean(unlist(lapply(c(2,4,6,8,10,12), function(x) opes_sr[[x]]$area))) # 1109.351
min(unlist(lapply(c(2,4,6,8,10,12), function(x) opes_sr[[x]]$area))) # 121.46
max(unlist(lapply(c(2,4,6,8,10,12), function(x) opes_sr[[x]]$area))) # 5665.3

# mean openings in Fire Excluded
mean(unlist(lapply(c(13:18), function(x) opes_sr[[x]]$area))) # 496.544
min(unlist(lapply(c(13:18), function(x) opes_sr[[x]]$area))) # 117.52
max(unlist(lapply(c(13:18), function(x) opes_sr[[x]]$area))) # 3380.15
t.test(unlist(lapply(c(2,4,6,8,10,12), function(x) opes_sr[[x]]$area)), unlist(lapply(c(13:18), function(x) opes_sr[[x]]$area)))
# p-value = 0.06904

# mean openings in 2018
mean(unlist(lapply(c(1,3,5,7,9,11), function(x) opes_sr[[x]]$area))) # 1014.161
min(unlist(lapply(c(1,3,5,7,9,11), function(x) opes_sr[[x]]$area))) # 121.35
max(unlist(lapply(c(1,3,5,7,9,11), function(x) opes_sr[[x]]$area))) # 5910.23
t.test(unlist(lapply(c(13:18), function(x) opes_sr[[x]]$area)), unlist(lapply(c(1,3,5,7,9,11), function(x) opes_sr[[x]]$area))) 
# p-value =  0.1297

# min(opes_all[opes_all$Year==1941,]) # 32.01
# max(opes_all[opes_all$Year==1941,]) # 5727.85
# mean(sapply(opes_all[opes_all$Year==1941,], as.numeric)) # 1338.109
# 
# min(opes_all[opes_all$Year==2018,]) # 33.32
# max(opes_all[opes_all$Year==2018,]) # 5718.15
# mean(sapply(opes_all[opes_all$Year==2018,], as.numeric)) # 1302.589


# Pie Charts -- IS (1941, 2018) and OH (1941, 2018)

# pull area in clusters of each bin size
# I now think this should use trees.noedge also
# make it a nested for lop: the outer loop applies, across all 12 plots, the inner loop to calculate proportional cluster area (across all bins)
bin_names <- levels(plots_out[[9]]$trees$bin)

clust_areas_all <- list()
for (j in 1:length(plots_out)){
  clust_area_df <- data.frame(X=plots_out[[j]]$trees.noedge$x, Y=plots_out[[j]]$trees.noedge$y, 
                              crown=plots_out[[j]]$trees.noedge$crown, bin=plots_out[[j]]$trees.noedge$bin)
  clust_areas <- list()
  for (i in 1:length(bin_names)){ 
    thingy <- clust_area_df %>% 
      filter(bin==bin_names[i],) %>% 
      st_as_sf(coords = c("X","Y")) %>% 
      st_buffer(dist=clust_area_df$crown) %>% 
      st_union() %>% 
   #  st_cast("POLYGON") %>% 
      st_intersection(bound) %>% 
      st_area()
    clust_areas[[i]] <- thingy
  }
  clust_areas_all[[j]] <- clust_areas 
}
library(rlist)
piebins <- as.data.frame(list.rbind(clust_areas_all)) # make this a dataframe with one row per plot (12) and one column per clump size (6)
piebins[piebins=="numeric(0)"] <- 0
names(piebins) <- bin_names # change column names to reflect bin names
piebins <- sapply(piebins, as.numeric) # need piebins not to be a list ! this fixes it
piebins <- cbind(gap = 0, piebins) # add a column at left for gaps
piebins <- cbind(interst = 0, piebins)
piebins <- as.data.frame(piebins)
# now need to draw total gap area from patchmorph section

opes_perplot <- vector(mode = "numeric", length = length(plots_out))
for (i in 1:length(plots_out)){
  opes_perplot[i] <- sum(opes_sr[[i]]$area)
}
piebins$gap <- as.numeric(opes_perplot)
piebins[4,6] # 0
# lastly add interstitial space
interst_perplot <- vector(mode = "numeric", length = 12)
for (i in 1:length(plots_out)){
  interst_perplot[i] <- 10000 - sum(unlist(piebins[i,]), na.rm=TRUE)
}
piebins$interst <- interst_perplot

tpiebins <- as.data.frame(t(piebins))
colnames(tpiebins) <- c(1:18)

# ok we will try to make pie charts using piebins. Start with plot 9   
library(RColorBrewer)
pie.cols <- brewer.pal(5, "YlGn")
pie.cols <- c("white", "#FFFAB2", pie.cols[2:5])

library(scales)
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=10)
  )

piefn <- function(plot) {
  ggplot(tpiebins, aes(x="", y=tpiebins[,plot], fill=factor(rownames(tpiebins), levels=c(rownames(tpiebins))))) + #"interstitial", "gap",bin_names[1:5])))) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start=0) +
    # scale_fill_brewer(palette = "YlGn", name = "Cluster Size") 
    scale_fill_manual(values = pie.cols, name = "Cluster Size") + blank_theme +
    theme(axis.text.x=element_blank()) +
    labs(title = paste("Spatial Composition, ", names[plot])) +
    guides(shape = guide_legend(override.aes = list(size = 1))) +
    guides(color = guide_legend(override.aes = list(size = 1))) +
    theme(legend.title = element_text(size = 5), legend.text = element_text(size = 5))
}
# piefn(8) # works

# ICO_pies <- list()
# for (i in 1:length(plots_out)){
#   #jpeg(paste("ICO_Pie_",names[i]),700,630)
#   p <- piefn(i)
#   ICO_pies[[i]] <- p
#   #dev.off()
# }
#library(gridExtra)
#grid.arrange(ICO_pies[[1]], ICO_pies[[3]], ICO_pies[[5]], ICO_pies[[2]], ICO_pies[[4]], ICO_pies[[6]], ncol=3) #IS
#grid.arrange(ICO_pies[[7]], ICO_pies[[9]], ICO_pies[[11]], ICO_pies[[8]], ICO_pies[[10]], ICO_pies[[12]], ncol=3) #OH

# one pie each for each site/year combo
# first, get columns of tpiebins broken out: IS 2018 = 1, 3, 5; IS 1941 = 2, 4, 6     OH 2018 = 7, 9, 11; OH 1941 = 8, 10, 12
bins_IS18 <- as.data.frame(sapply(rowSums(tpiebins[,c(1,3,5)]), as.numeric))
bins_IS41 <- as.data.frame(sapply(rowSums(tpiebins[,c(2,4,6)]), as.numeric))
bins_OH18 <- as.data.frame(sapply(rowSums(tpiebins[,c(7,9,11)]), as.numeric))
bins_OH41 <- as.data.frame(sapply(rowSums(tpiebins[,c(8,10,12)]), as.numeric))
bins_IS95 <- as.data.frame(sapply(rowSums(tpiebins[,c(13,14,15)]), as.numeric))
bins_OH06 <- as.data.frame(sapply(rowSums(tpiebins[,c(16,17,18)]), as.numeric))

# IS 2018
pie_IS18 <- ggplot(bins_IS18, aes(x="", y=bins_IS18[,], fill=factor(rownames(bins_IS18), levels=c(rownames(bins_IS18))))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_manual(values = pie.cols, name = "Cluster Size") + blank_theme +
  labs(title = "IS in 2018", size = 10) +
  theme(legend.position = "none")
  # guides(shape = guide_legend(override.aes = list(size = 1))) +
  # guides(color = guide_legend(override.aes = list(size = 1))) +
  # theme(legend.title = element_text(size = 5), legend.text = element_text(size = 5))

# IS 1995
pie_IS95 <- ggplot(bins_IS95, aes(x="", y=bins_IS95[,], fill=factor(rownames(bins_IS95), levels=c(rownames(bins_IS95))))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_manual(values = pie.cols, name = "Cluster Size") + blank_theme +
  labs(title = "IS in 1995", size = 10) +
  theme(legend.position = "none")
# guides(shape = guide_legend(override.aes = list(size = 1))) +
# guides(color = guide_legend(override.aes = list(size = 1))) +
# theme(legend.title = element_text(size = 5), legend.text = element_text(size = 5))

# IS 1941
pie_IS41 <- ggplot(bins_IS41, aes(x="", y=bins_IS41[,], fill=factor(rownames(bins_IS41), levels=c(rownames(bins_IS41))))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_manual(values = pie.cols, name = "Cluster Size") + blank_theme +
  labs(title = "IS in 1941", size = 10) +
  theme(legend.position = "none")
# guides(shape = guide_legend(override.aes = list(size = 1))) +
# guides(color = guide_legend(override.aes = list(size = 1))) +
# theme(legend.title = element_text(size = 5), legend.text = element_text(size = 5))

# OH 2018
pie_OH18 <- ggplot(bins_OH18, aes(x="", y=bins_OH18[,], fill=factor(rownames(bins_OH18), levels=c(rownames(bins_OH18))))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_manual(values = pie.cols, name = "Cluster Size") + blank_theme +
  labs(title = "OH in 2018", size = 10) +
  theme(legend.position = "none")
# guides(shape = guide_legend(override.aes = list(size = 1))) +
# guides(color = guide_legend(override.aes = list(size = 1))) +
# theme(legend.title = element_text(size = 5), legend.text = element_text(size = 5))

# OH 2006
pie_OH06 <- ggplot(bins_OH06, aes(x="", y=bins_OH06[,], fill=factor(rownames(bins_OH06), levels=c(rownames(bins_OH06))))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_manual(values = pie.cols, name = "Cluster Size") + blank_theme +
  labs(title = "OH in 2006", size = 10) +
  theme(legend.position = "none")
# guides(shape = guide_legend(override.aes = list(size = 1))) +
# guides(color = guide_legend(override.aes = list(size = 1))) +
# theme(legend.title = element_text(size = 5), legend.text = element_text(size = 5))

# OH 1941
pie_OH41 <- ggplot(bins_OH41, aes(x="", y=bins_OH41[,], fill=factor(rownames(bins_OH41), levels=c(rownames(bins_OH41))))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_manual(values = pie.cols, name = "Cluster Size") + blank_theme +
  labs(title = "OH in 1941", size = 10) +
  theme(legend.position = "none")
# guides(shape = guide_legend(override.aes = list(size = 1))) +
# guides(color = guide_legend(override.aes = list(size = 1))) +
# theme(legend.title = element_text(size = 5), legend.text = element_text(size = 5))

# grid.arrange(pie_IS41, pie_IS95, pie_IS18, ncol=3)
# grid.arrange(pie_OH41, pie_OH06, pie_OH18, ncol=3)
# 
# grid.arrange(pie_IS41, pie_IS95, pie_IS18, pie_OH41, pie_OH06, pie_OH18, ncol=3)

# math on area in interst, gap, size bins
IS_change <- cbind(bins_IS41, bins_IS95, bins_IS18)
names(IS_change) <- c("IS_1941", "IS_1995","IS_2018")
IS_change <- IS_change %>% mutate(change1 = (IS_1995/3) - (IS_1941/3)) %>% 
  mutate(pct_change1 = change1*100/10000) %>% 
  mutate(change2 = (IS_2018/3) - (IS_1995/3)) %>% 
  mutate(pct_change2 = change2*100/10000)

OH_change <- cbind(bins_OH41, bins_OH06, bins_OH18)
names(OH_change) <- c("OH_1941","OH_2006", "OH_2018")
OH_change <- OH_change %>% mutate(change1 = (OH_2006/3) - (OH_1941/3)) %>% 
  mutate(pct_change1 = change1*100/10000) %>% 
  mutate(change2 = (OH_2018/3) - (OH_2006/3)) %>% 
  mutate(pct_change2 = change2*100/10000)

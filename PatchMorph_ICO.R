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
library(patchwoRk)

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

# for loop for openings spatrasters from plots, converted to sf polygons and filtered for "1" attribute

opes_sr <- list()
for (i in 1:length(plots_out)){
  thingy <- patchMorph(xy_sr(i), buffer=5, suitThresh=1, gapThresh=12, spurThresh=10, verbose=FALSE) %>%
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

# first try at gap distn histogram
gap_distn <- ggplot(opes_all, aes(x=Opes, fill=as.factor(Year))) +
  geom_histogram(bins = 15, position="dodge") +
  scale_fill_manual(values = c("#cf4411", "black", "darkgray")) +
  stat_bin(geom="text", bins=15, aes(label=after_stat(count), group=as.factor(Year)), vjust = -0.5, position = position_dodge()) +
  scale_x_continuous(breaks = round(seq(50, 5750, length.out = 15))) +
  scale_y_continuous(expand=expansion(mult=c(0,0.05))) +
  labs(title = "Gap Size Distribution", hjust = 5, x = "Forest Canopy Gaps (m^2)", y = "Count", fill="Year") +
  theme_classic()


# first try of all gap distns combined
# sensible bins: 82-500, 500-1500, 1500-2500, >2500
# I want a df with one row for each count/ha, with Year, Plot, and Bin along with it
plotyears <- c(rep(c(2018,1941),6),rep("Prefire",6))
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
opes_bins[,2] <- rep(1:18, each = 4)
opes_bins[,3] <- rep(gap_bins,18)
opes_bins[,4] <- unlist(plotcounts)
names(opes_bins) <- c("Year", "Plot", "gap_bin", "countperha")

opes_bins$gap_bin <- factor(opes_bins$gap_bin, levels = c("82-500", "500-1500","1500-2500",">2500"))
opes_bins$Year <- factor(opes_bins$Year, levels = c("1941","Prefire","2018"))
opes_bins$Plot <- as.character((opes_bins)[,2])
library(ggpubr)

LydersenFig3 <- 
ggbarplot(opes_bins, x="gap_bin", y = "countperha",  add = "mean_se", fill = "Year", position = position_dodge(0.8)) +
  stat_friedman_test(aes(wid=Plot, group=gap_bin), within = "x", label = "p = {p.format}") +
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
IS_bins[,2] <- rep(c(1:6,13:15), each = 4)
IS_bins[,3] <- rep(gap_bins,length(opes_IS))
IS_bins[,4] <- unlist(plotcounts)
names(IS_bins) <- c("Year", "Plot", "gap_bin", "countperha")

IS_bins$gap_bin <- factor(IS_bins$gap_bin, levels = c("82-500", "500-1500","1500-2500",">2500"))
IS_bins$Year <- factor(IS_bins$Year, levels = c("1941","Prefire","2018"))

#library(ggpubr)
LydersenFig3_IS <- ggbarplot(IS_bins, x="gap_bin", y = "countperha",  add = "mean_se", fill = "Year", position = position_dodge(0.8)) +
  stat_compare_means(paired=TRUE) +
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
OH_bins[,2] <- rep(c(7:12,16:18), each = 4)
OH_bins[,3] <- rep(gap_bins,length(opes_OH))
OH_bins[,4] <- unlist(plotcounts)
names(OH_bins) <- c("Year", "Plot", "gap_bin", "countperha")

OH_bins$gap_bin <- factor(OH_bins$gap_bin, levels = c("82-500", "500-1500","1500-2500",">2500"))
OH_bins$Year <- factor(OH_bins$Year, levels = c("1941","Prefire","2018"))

LydersenFig3_OH <- ggbarplot(OH_bins, x="gap_bin", y = "countperha",  add = "mean_se", fill = "Year", position = position_dodge(0.8)) +
  stat_compare_means(paired=TRUE) +
  labs(title = "Gaps at O'Harrell Canyon",
       x = "Gap Size (m2)",
       y = "Frequency / ha")

grid.arrange(LydersenFig3, LydersenFig3_IS, LydersenFig3_OH, ncol = 1)


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
pie.cols <- c("white", "#FEC44F", pie.cols[2:5])

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

grid.arrange(pie_IS41, pie_IS95, pie_IS18, ncol=3)
grid.arrange(pie_OH41, pie_OH06, pie_OH18, ncol=3)

grid.arrange(pie_IS41, pie_IS95, pie_IS18, pie_OH41, pie_OH06, pie_OH18, ncol=3)

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

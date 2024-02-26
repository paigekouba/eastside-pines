# PatchMorph to find openings

# what gap radius?

# "set gap threshold to the *minimum* diameter for crowns across all plots"
# "We set ours to 3 m, as all but one tree in our plots had a crown radius > 1.5 m" -- Ng 2020

allcrwndf <- lapply(plots_out, function(x) x[[11]])
allcrwn <- unlist(lapply(allcrwndf, function(x) x[[6]]))

ggplot(as.data.frame(allcrwn), aes(x = allcrwn*2)) + # distribution of tree crown *diameters*
  geom_histogram() +
  geom_vline(aes(xintercept = quantile(allcrwn*2, 0.1)), color = "grey", size = 0.5) + # 2.6096  
  geom_vline(aes(xintercept = quantile(allcrwn*2, 0.9)),  color="grey", size=0.5) + # 7.955744 
  geom_vline(aes(xintercept = mean(allcrwn*2, na.rm = TRUE)),  color="red", size=0.5) +# 4.733178
  ggtitle("Crown Diameters, All Sites, All Years") +
  xlab("Crown Diamter (m)") + ylab("Frequency")

sum(allcrwn < 1)*100/length(allcrwn) # 0.49
sum(allcrwn > 5)*100/length(allcrwn) # 1.710098
# sum(allcrwn > 4)*100/length(allcrwn) # 9.771987

min(allcrwn)*2 # 1.662 is the smallest tree crown across all the plots and both times
max(allcrwn)*2 # 11.95104 is the biggest tree crown across all the plots and both times

library(raster)
library(sf)
library(sp)
library(terra)
# prepare data: one plot (plot 4, IS3 in 1941) for testing
## checking plot 9 (OH2 in 2018) because of the error at PM result stage
pm_df <- data.frame(X=plots_out[[4]]$trees.noedge$x, Y=plots_out[[4]]$trees.noedge$y, crown=plots_out[[4]]$trees.noedge$crown)
pm_df <- data.frame(X=plots_out[[9]]$trees.noedge$x, Y=plots_out[[9]]$trees.noedge$y, crown=plots_out[[9]]$trees.noedge$crown)
# reproducible example:
#Xs <- plots_out[[4]]$trees.noedge$x
# [1] -54.3 -53.6 -52.4 -48.5 -47.0 -45.8 -45.6 -40.6 -39.5 -39.2 -36.1 -35.2 -34.6 -34.1 -30.3 -28.2 -27.7 -26.0 -25.9
# [20] -25.9 -23.1 -21.6 -21.2 -20.5 -17.9 -17.3 -15.9 -15.4 -14.6 -10.0  -5.5  -4.9  -2.3  -1.9   0.3   2.2   4.3   4.7
# [39]   6.0   6.9   8.6   9.1  10.4  13.5  13.9  16.8  22.3  28.1  31.4  36.0  37.7  50.0  50.7   5.2  -3.3  27.4  26.6
# [58]  -2.2  -5.4   0.6 -29.9
#Ys <- plots_out[[4]]$trees.noedge$y
# [1]   3.2  -5.1   7.2 -20.5 -25.3 -26.0 -21.3  -0.2  12.7 -34.4 -35.6  36.5  23.5  -9.3  -1.9  35.4  18.0 -37.6 -26.9
# [20] -11.9 -41.0  24.3  12.2  30.0   5.4  32.9  -4.3 -28.8  24.1 -23.0 -55.7  14.4  12.2 -53.1  -7.1 -13.8 -13.2  48.1
# [39]   6.5  35.5  10.4  41.7  16.7  53.4  52.0 -12.8 -30.8 -25.0 -40.9 -22.7  41.5 -21.3  12.3  20.3  23.6  40.1 -10.8
# [58] -32.3 -34.9 -36.0  24.0
#Cs <- plots_out[[4]]$trees.noedge$crown
# [1] 1.4116 1.1624 1.2336 3.5476 2.4084 1.7676 2.7644 1.1268 3.7612 1.5540 1.2336 3.3340 1.1980 1.1624 2.2304 2.6220
# [17] 3.6188 1.5540 3.4764 3.8324 1.5896 2.4440 1.1268 3.2272 3.7968 3.4764 3.5476 3.7256 4.1528 2.7644 1.5896 2.8000
# [33] 4.2240 1.9812 4.4732 3.4408 2.8356 1.2336 3.6188 1.4828 3.9392 1.8744 5.1140 3.2984 3.5120 3.9748 3.4408 3.0136
# [49] 4.1528 5.2208 4.0460 2.6932 4.3308 2.9424 1.5896 1.5540 4.0104 2.1236 2.2304 2.6576 1.5540
# pm_df <- data.frame(X=Xs, Y=Ys, crown=Cs)

ctr = data.frame(X = 0, Y = 0) # plot center to draw boundary of 1ha circle
bound = st_as_sf(ctr, coords = c("X", "Y")) |> st_buffer(sqrt(10000/pi)) # boundary of 1ha circle
stems <- st_as_sf(pm_df, coords = c("X", "Y")) # points for each tree w/dbh attribute
crowns = st_buffer(stems, dist = stems$crown) # each crown defined as a circle with r=crown radius
notcrowns <- st_difference(bound, st_union(crowns)) # all area in non-crown space
yescrowns <- st_difference(bound, st_union(notcrowns)) # all area in crown space

r_notcrowns <- rasterize(notcrowns, raster(extent(bound), res = 0.1)) # turns sf into a rasterlayer -- this one is for all non-crown areas
# want now to make a raster with 1s for not-crown and 0s for crown
r_yescrowns <- rasterize(yescrowns, raster(extent(bound), res=0.1)) # sf --> rasterlayer, this time for all crown areas

values(r_yescrowns)[values(r_yescrowns) == 1] <- 0 # reassign all values in crown areas to 0 ("unsuitable")

r_test <- merge(r_notcrowns, r_yescrowns) # creates one rasterlayer with 0s in crown areas, 1s everywhere else
sr_test <- rast(r_test) # makes into a spatRaster
crs(sr_test) <- "local" # set crs to Cartesian plane in meters

# patchMorph code:
# https://rdrr.io/github/bi0m3trics/patchwoRk/man/patchMorph.html

devtools::install_github("bi0m3trics/patchwoRk")
library(patchwoRk)

# explanation of gap and spur, conceptually, from Girvetz and Greco 2007:
# (1) land cover density threshold (suitThresh), (2) habitat gap maximum thickness (gapThresh), and (3) habitat patch minimum thickness (spurThresh)

plot(sr_test, col=c("white", "#FEC44F"), main="Starting SpatRaster") # 0s in crowns (white), 1s everywhere else (yellow)
pm.rast <- patchMorph(sr_test, buffer = 5, suitThresh = 1, gapThresh = 10, spurThresh = 6, verbose = TRUE) # 
plot(pm.rast,  main="PatchMorph Results (Gap-10 & Spur-6)", col=c("#FEC44F", "forestgreen")) # now the areas near crowns are 1 and openings are 0! ## I did not get this on the most recent run but I don't think I've changed anything....except I was using plot 9! also focus for kernel looks like crowns now?
plot(crowns, col="black", add=TRUE)

# convert to polygons for area calculations and mapping
pm.vect <- as.polygons(pm.rast, values = TRUE) # turns SpatRaster into SpatVector
pm.sf <- st_as_sf(pm.vect) # turns SpatVector into sf with a multipolygon for 0 and for 1
# # take the multipolygon for cells with "1" attribute and turn it into lots of polygons
# pm.sf1 <- pm.sf %>% filter(focal_max == 1)
# take the multipolygon for cells with "0" attribute and turn it into lots of polygons !! this because 0 and 1 are flipped !!
pm.sf0 <- pm.sf %>% filter(focal_max == 0)
# pm.sf1 <- pm.sf %>% filter(focal_max == 1)
# pm.sfs <- st_cast(pm.sf1, "POLYGON")
pm.sfs <- st_cast(pm.sf0, "POLYGON")
pm.sfs$area <- st_area(pm.sfs)
sum(pm.sfs$area) # 4887.83 

# loop this over all plots!
# fn to convert plot x,y coords to spatRaster
# set up
ctr = data.frame(X = 0, Y = 0) # plot center to draw boundary of 1ha circle
bound = st_as_sf(ctr, coords = c("X", "Y")) |> st_buffer(sqrt(10000/pi)) # boundary of 1ha circle 

# fn to convert plot x,y coords to not-crowns spatRaster with local crs [and vals for crown, notcrown, commmented out rn]
xy_sr <- function(plot){ 
  df <- data.frame(X=plots_out[[plot]]$trees.noedge$x, Y=plots_out[[plot]]$trees.noedge$y, crown=plots_out[[plot]]$trees.noedge$crown)
  stems <- st_as_sf(df, coords = c("X", "Y")) #, crs = custom_crs) # points for each tree w/dbh attribute
  crowns = st_buffer(stems, dist = stems$crown) 
  notcrowns <- st_difference(bound, st_union(crowns)) # all area in non-crown space
  yescrowns <- st_difference(bound, st_union(notcrowns)) # all area in crown space (non-overlapping, values = 1)
  r_notcrowns <- rasterize(notcrowns, raster(extent(bound), res = 0.1)) 
  r_yescrowns <- rasterize(yescrowns, raster(extent(bound), res=0.1))
  values(r_yescrowns)[values(r_yescrowns) == 1] <- 0
  spatRast <- rast(merge(r_notcrowns, r_yescrowns))
  crs(spatRast) <- "local"
  return(spatRast)
}
# test
# plot(xy_sr(4)) # works!

# for loop for openings spatrasters from plots, converted to sf polygons and filtered for "1" attribute

opes_sr <- list()
for (i in 1:length(plots_out)){
  thingy <- patchMorph(xy_sr(i), buffer=5, suitThresh=1, gapThresh=8, spurThresh=6, verbose=FALSE) %>%
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

# step A: add opes_sr polygons to ICO maps # check!

# step B: add opes_sr summed areas to pie charts (one per site per year = 4)

# step C: make gap size distribution by year. Start with this one
# opes_sr has 12 lists containing 1-10 polygons and their associated areas
# break these out by (site and) year
# names
# [1] "IS1 in 2018" "IS1 in 1941" "IS2 in 2018" "IS2 in 1941" "IS3 in 2018" "IS3 in 1941" "OH1 in 2018" "OH1 in 1941"
# [9] "OH2 in 2018" "OH2 in 1941" "OH3 in 2018" "OH3 in 1941"
# odds are 2018, evens are 1941

# ugly but functional?
opes2018 <- c(as.vector(opes_sr[[1]]$area, mode = "numeric"), as.vector(opes_sr[[3]]$area, mode = "numeric"), as.vector(opes_sr[[5]]$area, mode = "numeric"), as.vector(opes_sr[[7]]$area, mode = "numeric"), as.vector(opes_sr[[9]]$area, mode = "numeric"), as.vector(opes_sr[[11]]$area, mode = "numeric"))

opes1941 <- c(as.vector(opes_sr[[2]]$area, mode = "numeric"), as.vector(opes_sr[[4]]$area, mode = "numeric"), as.vector(opes_sr[[6]]$area, mode = "numeric"), as.vector(opes_sr[[8]]$area, mode = "numeric"), as.vector(opes_sr[[10]]$area, mode = "numeric"), as.vector(opes_sr[[12]]$area, mode = "numeric"))

opes_all <- data.frame(Opes = c(opes1941, opes2018), Year = c(rep(1941, length(opes1941)), rep(2018, length(opes2018))))

gap_distn <- ggplot(opes_all, aes(x=Opes, fill=as.factor(Year))) +
  geom_histogram(bins = 11, position="dodge") +
  scale_fill_manual(values = c("#cf4411", "black")) +
  stat_bin(geom="text", bins=11, aes(label=after_stat(count), group=as.factor(Year)), vjust = -0.5, position = position_dodge()) + 
           scale_x_continuous(breaks = round(seq(50, 7550, length.out = 11))) +
           scale_y_continuous(expand=expansion(mult=c(0,0.05))) +
  labs(title = "Gap Size Distribution", hjust = 5, x = "Forest Canopy Gaps (m^2)", y = "Count", fill="Year") +
  theme_classic()

min(opes_all[opes_all$Year==1941,]) # 69.8
max(opes_all[opes_all$Year==1941,]) # 6691.46
mean(sapply(opes_all[opes_all$Year==1941,], as.numeric)) # 1577.804

min(opes_all[opes_all$Year==2018,]) # 54.2
max(opes_all[opes_all$Year==2018,]) # 6419.23
mean(sapply(opes_all[opes_all$Year==2018,], as.numeric)) # 1381.669



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

opes_perplot <- vector(mode = "numeric", length = 12)
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
colnames(tpiebins) <- c(1:12)

# ok we will try to make pie charts using piebins. Start with plot 9   
library(RColorBrewer)
my.cols <- brewer.pal(5, "YlGn")
my.cols <- c("white", "#FEC44F", my.cols[2:5])

library(scales)
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

piefn <- function(plot) {
  ggplot(tpiebins, aes(x="", y=tpiebins[,plot], fill=factor(rownames(tpiebins), levels=c(rownames(tpiebins))))) + #"interstitial", "gap",bin_names[1:5])))) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start=0) +
    # scale_fill_brewer(palette = "YlGn", name = "Cluster Size") 
    scale_fill_manual(values = my.cols, name = "Cluster Size") + blank_theme +
    theme(axis.text.x=element_blank()) +
    labs(title = paste("Spatial Composition, ", names[plot]))
}
piefn(8) # works

ICO_pies <- list()
for (i in 1:length(plots_out)){
  #jpeg(paste("ICO_Pie_",names[i]),700,630)
  p <- piefn(i)
  ICO_pies[[i]] <- p
  #dev.off()
}
library(gridExtra)
grid.arrange(ICO_pies[[1]], ICO_pies[[3]], ICO_pies[[5]], ICO_pies[[2]], ICO_pies[[4]], ICO_pies[[6]], ncol=3) #IS
grid.arrange(ICO_pies[[7]], ICO_pies[[9]], ICO_pies[[11]], ICO_pies[[8]], ICO_pies[[10]], ICO_pies[[12]], ncol=3) #OH

# one pie each for each site/year combo
# first, get columns of tpiebins broken out: IS 2018 = 1, 3, 5; IS 1941 = 2, 4, 6     OH 2018 = 7, 9, 11; OH 1941 = 8, 10, 12
bins_IS18 <- as.data.frame(sapply(rowSums(tpiebins[,c(1,3,5)]), as.numeric))
bins_IS41 <- as.data.frame(sapply(rowSums(tpiebins[,c(2,4,6)]), as.numeric))
bins_OH18 <- as.data.frame(sapply(rowSums(tpiebins[,c(7,9,11)]), as.numeric))
bins_OH41 <- as.data.frame(sapply(rowSums(tpiebins[,c(8,10,12)]), as.numeric))

# IS 2018
pie_IS18 <- ggplot(bins_IS18, aes(x="", y=bins_IS18[,], fill=factor(rownames(bins_IS18), levels=c(rownames(bins_IS18))))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_manual(values = my.cols, name = "Cluster Size") + blank_theme +
  labs(title = "Spatial Composition at IS, 2018")

# IS 1941
pie_IS41 <- ggplot(bins_IS41, aes(x="", y=bins_IS41[,], fill=factor(rownames(bins_IS41), levels=c(rownames(bins_IS41))))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_manual(values = my.cols, name = "Cluster Size") + blank_theme +
  labs(title = "Spatial Composition at IS, 1941")

# OH 2018
pie_OH18 <- ggplot(bins_OH18, aes(x="", y=bins_OH18[,], fill=factor(rownames(bins_OH18), levels=c(rownames(bins_OH18))))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_manual(values = my.cols, name = "Cluster Size") + blank_theme +
  labs(title = "Spatial Composition at OH, 2018")

# OH 1941
pie_OH41 <- ggplot(bins_OH41, aes(x="", y=bins_OH41[,], fill=factor(rownames(bins_OH41), levels=c(rownames(bins_OH41))))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  scale_fill_manual(values = my.cols, name = "Cluster Size") + blank_theme +
  labs(title = "Spatial Composition at OH, 1941")

grid.arrange(pie_IS41, pie_IS18, ncol=2)
grid.arrange(pie_OH41, pie_OH18, ncol=2)

grid.arrange(pie_IS41, pie_IS18, pie_OH41, pie_OH18, ncol=2)

# math on area in interst, gap, size bins
IS_change <- cbind(bins_IS41, bins_IS18)
names(IS_change) <- c("IS_1941", "IS_2018")
IS_change <- IS_change %>% mutate(change = (IS_2018/3) - (IS_1941/3)) %>% 
  mutate(pct_change = change*100/10000)

OH_change <- cbind(bins_OH41, bins_OH18)
names(OH_change) <- c("OH_1941", "OH_2018")
OH_change <- OH_change %>% mutate(change = (OH_2018/3) - (OH_1941/3)) %>% 
  mutate(pct_change = change*100/10000)

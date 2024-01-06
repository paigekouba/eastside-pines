
###  Learning sf Approach with Derek  ###
#install.packages("sf")
library(sf)
# test plot: OH2 in 2018
# new df to convert to sf format
#sf_df <- data.frame(X = OH2_2018$X, Y=OH2_2018$Y, dbh=OH2_2018$dbh)
sf_df <- data.frame(X=plots_out[[9]]$trees$x, Y=plots_out[[9]]$trees$y, crown=plots_out[[9]]$trees$crown)
# for iterable version, change to plots_out[[9]]$trees$... (x, y, and crown)
head(sf_df)

ctr = data.frame(X = 0, Y = 0) # plot center to draw boundary of 1ha circle
bound = st_as_sf(ctr, coords = c("X", "Y")) |> st_buffer(sqrt(10000/pi)) # boundary of 1ha circle
stems <- st_as_sf(sf_df, coords = c("X", "Y")) # points for each tree w/dbh attribute
#crowns = st_buffer(stems, dist = stems$dbh/20) |> st_union() # PLACEHOLDER change to dist = cr.rad 
crowns = st_buffer(stems, dist = stems$crown) |> st_union() 
crowns_buffer = st_buffer(crowns, 5) # 5m buffer around each crown boundary
gaps2 = st_difference(bound, crowns_buffer) # total area in gaps w/ gap threshold from crowns_buffer
plot(gaps2, col="blue", add=TRUE)
gaps3 = st_buffer(gaps2, 5) # a 5m buffer around the area at least 5m from a crown edge—gap boundary
# this works bc it leaves out the areas that weren't big enough to be >5m away from a crown
# gaps3 started out as 1 observation containing all points
gaps3 = st_cast(gaps3, "POLYGON") # this makes it 12 observations

plot(crowns, col="black", add=TRUE)
plot(gaps3, col="red", add=TRUE) # overestimates gaps; likely are unobserved trees over plot boundary

bound_noedge = st_buffer(bound, -5) # since the edge effect overestimates the gaps at the boundary, take off 5m from the edge of the plot to find gaps

gaps3_noedge = st_intersection(gaps3, bound_noedge) # limits gaps3 to just areas ≥5m from plot edge

plot(crowns, col="green")
plot(bound, add=TRUE)
plot(gaps3, col="red", add=TRUE) 
plot(gaps3_noedge, col="yellow", add=TRUE)
plot(crowns, col="green", add=TRUE)

ggplot(gaps3_noedge) +
  geom_sf()

# weird thing: some trees appear outside boundary line drawn at sqrt(10000/pi)(=r) and center 0,0
# went back to ICO code and removed line that recentered plots
sf_df_check <- sf_df %>% 
  mutate(inside = sqrt((X^2)+(Y^2))<sqrt(10000/pi)) # now only one tree outside plot boundary

library(ggforce)
# Prototype map of true-to-scale crown radius, dbh, and gaps
ggplot() +
  geom_sf(data=bound, fill="black") +
  geom_circle(data = df, n=20, aes(x0 = trees.x, y0 = trees.y, r=trees.crown, x=trees.x, y=trees.y, 
                                   fill=factor(trees.bin), color=factor(trees.bin), alpha=0.75)) +
  geom_circle(data = df, n=20, aes(x0=trees.x, y0=trees.y, r=trees.dbh/200), color="burlywood4", 
              fill="burlywood4") +
  scale_fill_brewer(palette = "YlGn", name = "Cluster Size") +
  scale_colour_brewer(palette = "YlGn", name = "Cluster Size") +
  geom_sf(data=gaps3_noedge, col="white", fill= "purple", alpha=0.3) +
  guides(size = guide_legend(override.aes = list(color ="burlywood4"))) +
  theme_light()


### Set up Gapfinder function ###
ctr <- data.frame(X = 0, Y = 0)
bound <- st_as_sf(ctr, coords = c("X", "Y")) |> st_buffer(sqrt(10000/pi))
gap_r <- 5
bound_buff = st_buffer(bound, -gap_r)

# function to find gaps in a PLOT with a gap radius of GAP_R; using trees.noedge to draw most accurate gaps
gapfinder <- function(plot, gap_r){
    df <- data.frame(X=plots_out[[plot]]$trees.noedge$x, Y=plots_out[[plot]]$trees.noedge$y, 
                     crown=plots_out[[plot]]$trees.noedge$crown) 
    stems <- st_as_sf(df, coords = c("X","Y"))
    crowns = st_buffer(stems, dist = stems$crown) |> st_union() #crowns mapped
    crowns_buffer = st_buffer(crowns, gap_r) # 5m buffer around each crown boundary
    gaps2 = st_difference(bound, crowns_buffer) # total area in gaps w/ gap threshold from crowns_buffer
    gaps3 = st_buffer(gaps2, gap_r) # a 5m buffer around the area at least 5m from a crown edge—gap boundary
#  gaps3 = st_cast(gaps3, "POLYGON") # this makes it n observations instead of one 
    bound_buff = st_buffer(bound, -gap_r) # since the edge effect overestimates the gaps at the boundary, take off buffer of width gap_r from the edge of the plot to find gaps
    gaps3_buff = st_intersection(gaps3, bound_buff)
    return(gaps3_buff)
}
#gapfinder(9,5)
#plot(gapfinder(9,5)) # Works!!

# for loop for gap area using gapfinder function
results <- rep(NA, length(plots_out))
for (i in 1:length(plots_out)){
  results[i] <- gapfinder(i, 5)
}

results
plot(results[[9]]) #  WORKS!!!

# need to add gaps3 = st_cast(gaps3, "POLYGON") or equivalent; this makes it n observations instead of one 
# try with gaps3_buff output:
# multi_obs <- st_cast(results[[9]], "POLYGON")
# plot(multi_obs, col="pink") # works

# Pie charts of area in each tree bin + area in gaps
  # first, quantify area in gaps, using the output of the looped gapfinder function over all 12 plots
gap_areas <- rep (NA, length(results))
for (i in 1:length(results)){
  gap_areas[i] <- 
    (sum(st_area(st_cast(results[[i]], "POLYGON")))/st_area(bound_buff))*st_area(bound_buff)/10000
  # ( total area in gap polygons / total area not counting buffer ) * [scale up to 1ha full plot]
}
gap_areas # this is a value of type "double"; to coerce it to a vector, as.vector(gap_areas, "double")

  # pull area in clusters of each bin size
    # not calculated in ICO code, but can back it out with sf my new best friend
        # make a df with X, Y, crown, and bin
clust_area_df <- data.frame(X=plots_out[[9]]$trees$x, Y=plots_out[[9]]$trees$y, crown=plots_out[[9]]$trees$crown, bin=plots_out[[9]]$trees$bin)
        # split into bin levels, then calculate per-bin proportional areas
#levels(plots_out[[9]]$trees$bin) # 6 levels
# try this approach on cluster bin "2-4": make a polygon of just those trees, find the area of union, find intersection of that w bound
two_four = st_buffer(st_as_sf(clust_area_df[clust_area_df$bin == "2-4",], coords = c("X","Y")), dist = clust_area_df[clust_area_df$bin == "2-4",]$crown) |> st_union() 
plot(two_four)
two_four_bound <- st_intersection(bound, st_cast(two_four))
st_area(two_four_bound)/10000 # this worked; need to do it over all bins, for all plots


# make it a nested for lop: the outer loop applies, across all 12 plots, the inner loop to calculate proportional cluster area (across all bins)
bin_names <- levels(plots_out[[9]]$trees$bin)

clust_areas_all <- rep(list(list()),length(plots_out)) #(NA, length(plots_out))
for (j in 1:length(plots_out)){
  clust_area_df <- data.frame(X=plots_out[[j]]$trees.noedge$x, Y=plots_out[[j]]$trees.noedge$y, 
                              crown=plots_out[[j]]$trees.noedge$crown, bin=plots_out[[j]]$trees.noedge$bin)
  clust_areas <- rep(NA, length(bin_names))
#  print(plots_out[[j]]$plot.name) # this is to see where it's stopping if you get an error
  for (i in 1:length(bin_names)){ 
 #   print(bin_names[i]) # this is to see where it's stopping if you get an error
    tryCatch({ # this part overrides the error; in the case of an error, it assigns a 0 to the output in question
      clust_areas[i] <- st_area(st_intersection(st_cast(st_union(st_buffer(st_as_sf(clust_area_df[clust_area_df$bin == bin_names[i],], coords = c("X","Y")), dist = clust_area_df[clust_area_df$bin == bin_names[i],]$crown))), bound))/10000
      },
      error=function(e) {
        clust_areas[i]=0
      })
    }
  clust_areas_all[[j]] <- clust_areas 
  }
  
# Next I want to vectorize the list outputs for the plots, make them a df, add column names, and tack on the gap areas
# I'd like each row to be a plot and each column to be a bin size cluster. So, 12 x 6 ... getting 6 x 12 so far. just t!
# I actually think 6 x 12 might be better for plotting
clust_gap_df <- data.frame(clust_areas_all)
names(clust_gap_df) = names # changes column headers to plot names. OK!
# now add a row for the gap areas
#clust_gap_df[nrow(clust_gap_df)+1,] = gap_areas # makes gap the last row
clust_gap_df <- rbind(gap_areas, clust_gap_df)
clust_gap_df$bins <- c("gap",bin_names)

# ok we will try to make pie charts using clust_gap_df. Start with plot 9   
# fix scale_fill_brewer so gaps are purple
library(RColorBrewer)
my.cols <- brewer.pal(6, "YlGn")
my.cols <- c("#330066", my.cols)

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
  ggplot(clust_gap_df, aes(x="", y=clust_gap_df[,plot], fill=factor(bins, levels=c("gap",bin_names)))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
 # scale_fill_brewer(palette = "YlGn", name = "Cluster Size") 
    scale_fill_manual(values = my.cols, name = "Cluster Size") + blank_theme +
  theme(axis.text.x=element_blank()) +
    labs(title = paste("Spatial Composition, ", names[plot]))
  }
# piefn(9) 

for (i in 1:length(plots_out)){
  #jpeg(paste("ICO_Pie_",names[i]),700,630)
   print(piefn(i))
#dev.off()
   }


# Use proportional area table to do Chi-square test on structural category distribution between 1941 and 2018

clust_gap_df[is.na(clust_gap_df)] <- 0 # replace NAs with 0, since NA represents 0 area in this size category
clust_gap_table <- as.data.frame(t(clust_gap_df)) # transpose
clust_gap_table <- clust_gap_table[-13,] # remove bin names row
clust_gap_table <- data.frame(apply(clust_gap_table, 2, function(x) as.numeric(as.character(x)))) # convert to numeric
colnames(clust_gap_table) <- c("gap",bin_names) # add column headers by bin names
rownames(clust_gap_table) <- names
write.csv(clust_gap_table,"clust_gap.csv") # yeet to Excel

# stinky little ANOVA, just IS so far
# for all 5 size groups, + gaps, do ANOVA on the proportional areas in each of the 3 plots, 1941 v 2018
dat <- data.frame(c(rep("2018",3),rep("1941",3)),c(clust_gap_table[c(1,3,5),1],clust_gap_table[c(2,4,6),1]))
colnames(dat) <- c("year","gap_prop")
ggplot(dat) + aes(x=year, y=gap_prop) + geom_point() + 
  labs( x = "Year", y = "Proportional Area") +
  ggtitle("Gap Sizes at IS")
summary(dat)
summary(aov(gap_prop ~ year, data = dat))
oneway.test(gap_prop ~ year, data = dat, var.equal = FALSE)

dat <- data.frame(c(rep("2018",3),rep("1941",3)),c(clust_gap_table[c(1,3,5),2],clust_gap_table[c(2,4,6),2]))
colnames(dat) <- c("year","one_prop")
#ggplot(dat) + aes(x=year, y=one_prop) + geom_point()
#summary(dat)
summary(aov(one_prop ~ year, data = dat))
oneway.test(one_prop ~ year, data = dat, var.equal = FALSE)

dat <- data.frame(c(rep("2018",3),rep("1941",3)),c(clust_gap_table[c(1,3,5),3],clust_gap_table[c(2,4,6),3]))
colnames(dat) <- c("year","twofour_prop")
# ggplot(dat) + aes(x=year, y=twofour_prop) + geom_point()
# summary(dat)
summary(aov(twofour_prop ~ year, data = dat))
oneway.test(twofour_prop ~ year, data = dat, var.equal = FALSE)

dat <- data.frame(c(rep("2018",3),rep("1941",3)),c(clust_gap_table[c(1,3,5),4],clust_gap_table[c(2,4,6),4]))
colnames(dat) <- c("year","fivenine_prop")
# ggplot(dat) + aes(x=year, y=fivenine_prop) + geom_point()
# summary(dat)
summary(aov(fivenine_prop ~ year, data = dat))
oneway.test(fivenine_prop ~ year, data = dat, var.equal = FALSE)

dat <- data.frame(c(rep("2018",3),rep("1941",3)),c(clust_gap_table[c(1,3,5),5],clust_gap_table[c(2,4,6),5]))
colnames(dat) <- c("year","tenfifteen_prop")
# ggplot(dat) + aes(x=year, y=tenfifteen_prop) + geom_point()
# summary(dat)
summary(aov(tenfifteen_prop ~ year, data = dat))
oneway.test(tenfifteen_prop ~ year, data = dat, var.equal = FALSE)



library(tidyverse)
library(palmerpenguins)
penguindat <- penguins %>% 
  select(species, flipper_length_mm)

# NEVER NEVER GIVE UP

# NEVER LET GO, NEVER SURRENDER

# Chi-square test on cluster counts
IS18_clusters <- c(plots_out[[1]]$clusters$bin, plots_out[[3]]$clusters$bin, plots_out[[5]]$clusters$bin)
IS41_clusters <- c(plots_out[[2]]$clusters$bin, plots_out[[4]]$clusters$bin, plots_out[[6]]$clusters$bin)
IS_clust_table <- data.frame("1" = c(sum(IS18_clusters=="1"),sum(IS41_clusters=="1")),
                          "2-4" = c(sum(IS18_clusters=="2-4"),sum(IS41_clusters=="2-4")),
                          "5-9" = c(sum(IS18_clusters=="5-9"),sum(IS41_clusters=="5-9")),
                          "10-15" = c(sum(IS18_clusters=="10-15"),sum(IS41_clusters=="10-15")),
                          "16-29" = c(sum(IS18_clusters=="16-29"),sum(IS41_clusters=="16-29"))) 
colnames(IS_clust_table) <- bin_names[1:5]
rownames(IS_clust_table) <- c("IS_2018", "IS_1941")
fisher.test(IS_clust_table) #p-value = 7.214e-08 
# not that helpful: want to know difference by structural category
write.csv(IS_clust_table,"IS_clust_counts.csv") # yeet to Excel



# add % labels
# +
#   geom_text(aes(y = clust_gap_df[,plot]), 
#             label = percent(clust_gap_df[,plot]*100), size=5)

  # pie chart 
# ggplot(PlantGrowth, aes(x=factor(1), fill=group))+
#   geom_bar(width = 1)+
#   coord_polar("y") +
# scale_fill_brewer("Blues") + blank_theme +
  # theme(axis.text.x=element_blank())+
  # geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
  #               label = percent(value/100)), size=5)



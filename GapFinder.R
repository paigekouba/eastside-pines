
# sf Approach with Derek
install.packages("sf")
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
sf_df_check <- sf_df %>% 
  mutate(inside = sqrt((X^2)+(Y^2))<sqrt(10000/pi)) # now only one tree outside plot boundary

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

ctr <- data.frame(X = 0, Y = 0)
bound <- st_as_sf(ctr, coords = c("X", "Y")) |> st_buffer(sqrt(10000/pi))
gap_r <- 5
bound_noedge = st_buffer(bound, -gap_r)


# function to find gaps in a PLOT with a gap radius of GAP_R
gapfinder <- function(plot, gap_r){
    df <- data.frame(X=plots_out[[plot]]$trees$x, Y=plots_out[[plot]]$trees$y, 
                     crown=plots_out[[plot]]$trees$crown) 
    stems <- st_as_sf(df, coords = c("X","Y"))
    crowns = st_buffer(stems, dist = stems$crown) |> st_union() #crowns mapped
    crowns_buffer = st_buffer(crowns, gap_r) # 5m buffer around each crown boundary
    gaps2 = st_difference(bound, crowns_buffer) # total area in gaps w/ gap threshold from crowns_buffer
    gaps3 = st_buffer(gaps2, gap_r) # a 5m buffer around the area at least 5m from a crown edge—gap boundary
#  gaps3 = st_cast(gaps3, "POLYGON") # this makes it n observations instead of one 
    bound_noedge = st_buffer(bound, -gap_r) # since the edge effect overestimates the gaps at the boundary, take off buffer of width gap_r from the edge of the plot to find gaps
    gaps3_noedge = st_intersection(gaps3, bound_noedge)
    return(gaps3_noedge)
}
#gapfinder(9,5)
#plot(gapfinder(9,5)) # Works!!

# for loop for gap area
results <- rep(NA, length(plots_out))
for (i in 1:length(plots_out)){
  results[i] <- gapfinder(i, 5)
}

results
#plot(results[[9]]) #  WORKS!!!

# need to add gaps3 = st_cast(gaps3, "POLYGON") or equivalent; this makes it n observations instead of one 
# try with gaps3_noedge output:
# multi_obs <- st_cast(results[[9]], "POLYGON")
# plot(multi_obs, col="pink") # works

# Pie charts of area in each tree bin + area in gaps
  # first, quantify area in gaps
gap_areas <- rep (NA, length(results))
for (i in 1:length(results)){
  gap_areas[i] <- 
    (sum(st_area(st_cast(results[[i]], "POLYGON")))/st_area(bound_noedge))*st_area(bound_noedge)/10000
  # ( total area in gap polygons / total area not counting buffer ) * [scale up to 1ha full plot]
}
gap_areas

  # pull area in clusters of each bin size
    # not calculated in ICO code, but can back it out with sf my new best friend
        # make a df with X, Y, crown, and bin
clust_area_df <- data.frame(X=plots_out[[9]]$trees$x, Y=plots_out[[9]]$trees$y, crown=plots_out[[9]]$trees$crown, bin=plots_out[[9]]$trees$bin)
        # split into bin levels
#levels(plots_out[[9]]$trees$bin) # 6 levels
# try this approach on cluster bin "2-4": make a polygon of just those trees, find the area of union, find intersection of that w bound
two_four = st_buffer(st_as_sf(clust_area_df[clust_area_df$bin == "2-4",], coords = c("X","Y")), dist = clust_area_df[clust_area_df$bin == "2-4",]$crown) |> st_union() 
plot(two_four)
two_four_bound <- st_intersection(bound, st_cast(two_four))
st_area(two_four_bound)/10000


clust_area_df <- data.frame(X=plots_out[[plot]]$trees$x, Y=plots_out[[plot]]$trees$y, crown=plots_out[[plot]]$trees$crown, bin=plots_out[[plot]]$trees$bin)

# make it a nested for lop
bin_names <- levels(plots_out[[9]]$trees$bin)

clust_areas_all <- rep(list(list()),length(plots_out)) #(NA, length(plots_out))
for (j in 1:length(plots_out)){
  clust_area_df <- data.frame(X=plots_out[[j]]$trees$x, Y=plots_out[[j]]$trees$y, 
                              crown=plots_out[[j]]$trees$crown, bin=plots_out[[j]]$trees$bin)
  clust_areas <- rep(NA, length(bin_names))
  print(plots_out[[j]]$plot.name)
  for (i in 1:length(bin_names)){ #unique(plots_out[[j]]$trees$bin))){ 
    print(bin_names[i])
    tryCatch({
      clust_areas[i] <- st_area(st_intersection(st_cast(st_union(st_buffer(st_as_sf(clust_area_df[clust_area_df$bin == bin_names[i],], coords = c("X","Y")), dist = clust_area_df[clust_area_df$bin == bin_names[i],]$crown))), bound))/10000
      },
      error=function(e) {
        clust_areas[i]=0
      })
    }
  clust_areas_all[[j]] <- clust_areas 
  }
  
  # st_union(st_buffer(st_as_sf(clust_area_df[clust_area_df$bin == "bin",], 
  #                             coords = c("X","Y")), dist = clust_area_df[clust_area_df$bin == "bin",]$crown))

  # try this approach on cluster bin "2-4": make a polygon of just those trees, find the area of union, find intersection of that w bound
  two_four = st_buffer(st_as_sf(clust_area_df[clust_area_df$bin == "2-4",], coords = c("X","Y")), dist = clust_area_df[clust_area_df$bin == "2-4",]$crown) |> st_union() 
  plot(two_four)
  two_four_bound <- st_intersection(bound, st_cast(two_four))
  st_area(two_four_bound)/10000

    # make it a loop

# st_area(st_intersection(st_cast(st_union(st_buffer(st_as_sf(clust_area_df[clust_area_df$bin == "i",], coords = c("X","Y")), dist = clust_area_df[clust_area_df$bin == "i",]$crown))), bound))/10000
# 
# st_area(st_intersection(st_cast(st_union(st_buffer(st_as_sf(clust_area_df[clust_area_df$bin == "2-4",], coords = c("X","Y")), dist = clust_area_df[clust_area_df$bin == "2-4",]$crown))), bound))/10000
      

  # pie chart 
# ggplot(PlantGrowth, aes(x=factor(1), fill=group))+
#   geom_bar(width = 1)+
#   coord_polar("y") +
# scale_fill_brewer("Blues") + blank_theme +
  # theme(axis.text.x=element_blank())+
  # geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
  #               label = percent(value/100)), size=5)



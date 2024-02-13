
###  Learning sf Approach with Derek  ###
#install.packages("sf")
library(sf)
# test plot: OH2 in 2018
# new df to convert to sf format
#sf_df <- data.frame(X = OH2_2018$X, Y=OH2_2018$Y, dbh=OH2_2018$dbh)
# sf_df <- data.frame(X=plots_out[[9]]$trees$x, Y=plots_out[[9]]$trees$y, crown=plots_out[[9]]$trees$crown)
# # for iterable version, change to plots_out[[9]]$trees$... (x, y, and crown)
# head(sf_df)
# 
# ctr = data.frame(X = 0, Y = 0) # plot center to draw boundary of 1ha circle
# bound = st_as_sf(ctr, coords = c("X", "Y")) |> st_buffer(sqrt(10000/pi)) # boundary of 1ha circle
# stems <- st_as_sf(sf_df, coords = c("X", "Y")) # points for each tree w/dbh attribute
# #crowns = st_buffer(stems, dist = stems$dbh/20) |> st_union() # PLACEHOLDER change to dist = cr.rad 
# crowns = st_buffer(stems, dist = stems$crown) |> st_union() 
# crowns_buffer = st_buffer(crowns, 5) # 5m buffer around each crown boundary
# gaps2 = st_difference(bound, crowns_buffer) # total area in gaps w/ gap threshold from crowns_buffer
# plot(gaps2, col="blue", add=TRUE)
# gaps3 = st_buffer(gaps2, 5) # a 5m buffer around the area at least 5m from a crown edge—gap boundary
# # this works bc it leaves out the areas that weren't big enough to be >5m away from a crown
# # gaps3 started out as 1 observation containing all points
# gaps3 = st_cast(gaps3, "POLYGON") # this makes it 12 observations
# 
# plot(crowns, col="black", add=TRUE)
# plot(gaps3, col="red", add=TRUE) # overestimates gaps; likely are unobserved trees over plot boundary
# 
# bound_noedge = st_buffer(bound, -5) # since the edge effect overestimates the gaps at the boundary, take off 5m from the edge of the plot to find gaps
# 
# gaps3_noedge = st_intersection(gaps3, bound_noedge) # limits gaps3 to just areas ≥5m from plot edge
# 
# plot(crowns, col="green")
# plot(bound, add=TRUE)
# plot(gaps3, col="red", add=TRUE) 
# plot(gaps3_noedge, col="yellow", add=TRUE)
# plot(crowns, col="green", add=TRUE)
# 
# ggplot(gaps3_noedge) +
#   geom_sf()
# 
# # weird thing: some trees appear outside boundary line drawn at sqrt(10000/pi)(=r) and center 0,0
# # went back to ICO code and removed line that recentered plots
# sf_df_check <- sf_df %>% 
#   mutate(inside = sqrt((X^2)+(Y^2))<sqrt(10000/pi)) # now only one tree outside plot boundary
# 
# library(ggforce)
# # Prototype map of true-to-scale crown radius, dbh, and gaps
# ggplot() +
#   geom_sf(data=bound, fill="black") +
#   geom_circle(data = df, n=20, aes(x0 = trees.x, y0 = trees.y, r=trees.crown, x=trees.x, y=trees.y, 
#                                    fill=factor(trees.bin), color=factor(trees.bin), alpha=0.75)) +
#   geom_circle(data = df, n=20, aes(x0=trees.x, y0=trees.y, r=trees.dbh/200), color="burlywood4", 
#               fill="burlywood4") +
#   scale_fill_brewer(palette = "YlGn", name = "Cluster Size") +
#   scale_colour_brewer(palette = "YlGn", name = "Cluster Size") +
#   geom_sf(data=gaps3_noedge, col="white", fill= "purple", alpha=0.3) +
#   guides(size = guide_legend(override.aes = list(color ="burlywood4"))) +
#   theme_light()


# what gap radius?
avgcrwn <- rep(NA, length(plots_out))
for(i in 1:length(plots_out)){
  avgcrwn[i] <- mean(plots_out[[i]]$trees.noedge$crown)}
# mean(avgcrwn) [1] 2.547888 the average crown radius across all plots was about 2.55m

# "set gap threshold to the *minimum* diameter for crowns across all plots"
# "We set ours to 3 m, as all but one tree in our plots had a crown radius > 1.5 m" -- Ng 2020

allcrwndf <- lapply(plots_out, function(x) x[[11]])
allcrwn <- unlist(lapply(allcrwndf, function(x) x[[6]]))

ggplot(as.data.frame(allcrwn), aes(x = allcrwn)) + 
  geom_histogram() +
  geom_vline(aes(xintercept = quantile(allcrwn, 0.1, na.rm = TRUE)), color = "grey", size = 0.5) + # 1.072585 
  geom_vline(aes(xintercept = quantile(allcrwn, 0.9, na.rm = TRUE)),  color="grey", size=0.5) + # 4.300508 
  geom_vline(aes(xintercept = mean(allcrwn, na.rm = TRUE)),  color="red", size=0.5) # 2.484347

mincrwn <- rep(NA, length(plots_out))
for(i in 1:length(plots_out)){
  mincrwn[i] <- min(plots_out[[i]]$trees.noedge$crown)}
# min(mincrwn)*2 [1] 1.282855, if following Ng 2020

# what buffer?
maxcrwn <- rep(NA, length(plots_out))
for(i in 1:length(plots_out)){
  maxcrwn[i] <- max(plots_out[[i]]$trees.noedge$crown)}
# max(maxcrwn) 5.91338; max crown diameter = 2*5.91338 = 11.82676
 
### Set up Gapfinder function ###
ctr <- data.frame(X = 0, Y = 0)
bound <- st_as_sf(ctr, coords = c("X", "Y")) |> st_buffer(sqrt(10000/pi))
#gap_r <- 5 # gap radius of 5
#gap_r <- 2.5 # gap radius of 2.5m
bound_buff = st_buffer(bound, -5) # boundary buffer of 5m 

# function to find gaps in a PLOT with a gap radius of GAP_R; using trees.noedge to draw most accurate gaps
gapfinder <- function(plot, gap_r){
    df <- data.frame(X=plots_out[[plot]]$trees.noedge$x, Y=plots_out[[plot]]$trees.noedge$y, 
                     crown=plots_out[[plot]]$trees.noedge$crown) 
    stems <- st_as_sf(df, coords = c("X","Y"))
    crowns = st_buffer(stems, dist = stems$crown) |> st_union() #crowns mapped
    crowns_buffer = st_buffer(crowns, gap_r) # [5m] buffer around each crown boundary
    gaps2 = st_difference(bound, crowns_buffer) # total area in gaps w/ gap threshold from crowns_buffer
    gaps3 = st_buffer(gaps2, gap_r) # a 5m buffer around the area at least 5m from a crown edge—gap boundary
#  gaps3 = st_cast(gaps3, "POLYGON") # this makes it n observations instead of one 
    bound_buff = st_buffer(bound, -5) # since the edge effect overestimates the gaps at the boundary, take off buffer of width 5m from the edge of the plot to find gaps
    gaps3_buff = st_intersection(gaps3, bound_buff)
    return(gaps3_buff)
}
#gapfinder(9,5)
plot(gapfinder(9,2.5)) # Works!!


# for loop for gap area using gapfinder function, gap radius of 5
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
    (sum(st_area(st_cast(results[[i]], "POLYGON")))/st_area(bound_buff))*10000
  # ( total area in gap polygons / total area not counting buffer ) * 10000 [scale up to 1ha full plot]
  # gets gap area per hectare, having corrected for buffer area
}
gap_areas # this is a value of type "double"; to coerce it to a vector, as.vector(gap_areas, "double")



  # pull area in clusters of each bin size
    # not calculated in ICO code, but can back it out with sf my new best friend
        # make a df with X, Y, crown, and bin, for plot # 9 as a test
clust_area_df <- data.frame(X=plots_out[[9]]$trees$x, Y=plots_out[[9]]$trees$y, crown=plots_out[[9]]$trees$crown, bin=plots_out[[9]]$trees$bin)
        # split into bin levels, then calculate per-bin proportional areas
#levels(plots_out[[9]]$trees$bin) # 6 levels
# try this approach on cluster bin "2-4": make a polygon of just those trees, find the area of union, find intersection of that w bound
two_four = st_buffer(st_as_sf(clust_area_df[clust_area_df$bin == "2-4",], coords = c("X","Y")), dist = clust_area_df[clust_area_df$bin == "2-4",]$crown) |> st_union() 
plot(two_four)
two_four_bound <- st_intersection(bound, st_cast(two_four))
st_area(two_four_bound)*10000/st_area(bound_buff) # this finds the per-ha area of the shape representing all 2-4 clusters
# this worked; need to do it over all bins, for all plots


# make it a nested for lop: the outer loop applies, across all 12 plots, the inner loop to calculate proportional cluster area (across all bins)
bin_names <- levels(plots_out[[9]]$trees$bin)

# think this should use trees, not trees.noedge. Output will be in sq meters
# TRY LATER: can I avoid tryCatch by assigning 0 for NAs going in? did this in area calc for chisq 
# clust_gap_df[is.na(clust_gap_df)] <- 0 # replace NAs with 0, since NA represents 0 area in this size category
clust_areas_all <- rep(list(list()),length(plots_out)) #(NA, length(plots_out))
for (j in 1:length(plots_out)){
  clust_area_df <- data.frame(X=plots_out[[j]]$trees$x, Y=plots_out[[j]]$trees$y, 
                              crown=plots_out[[j]]$trees$crown, bin=plots_out[[j]]$trees$bin)
  clust_areas <- rep(NA, length(bin_names))
#  print(plots_out[[j]]$plot.name) # this is to see where it's stopping if you get an error
  for (i in 1:length(bin_names)){ 
 #   print(bin_names[i]) # this is to see where it's stopping if you get an error
    tryCatch({ # this part overrides the error; in the case of an error, it assigns a 0 to the output in question
      clust_areas[i] <- st_area(st_intersection(st_cast(st_union(st_buffer(st_as_sf(clust_area_df[clust_area_df$bin == bin_names[i],], coords = c("X","Y")), dist = clust_area_df[clust_area_df$bin == bin_names[i],]$crown))), bound))*10000/st_area(bound_buff)
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

ICO_pies <- list()
for (i in 1:length(plots_out)){
  #jpeg(paste("ICO_Pie_",names[i]),700,630)
  p <- piefn(i)
  ICO_pies[[i]] <- p
#dev.off()
   }
grid.arrange(ICO_pies[[1]], ICO_pies[[3]], ICO_pies[[5]], ICO_pies[[2]], ICO_pies[[4]], ICO_pies[[6]], ncol=3) #IS
grid.arrange(ICO_pies[[7]], ICO_pies[[9]], ICO_pies[[11]], ICO_pies[[8]], ICO_pies[[10]], ICO_pies[[12]], ncol=3) #OH

# I think that I can redo the pie chart code above to get (1941 and 2018) for (IS and OH); display next to eyeballs



# gapfinder is ok for gaps but doesn't account for spurs, so we are getting supergaps that snake around through narrow openings between trees. If we set a minimum spur threshold, as we can with PatchMorph, those will get closed off and create 2(+) separate gaps.

# Try using patchMorph algorithm to define gaps
# need to turn tree map into a raster. suitable 1, unsuitable 0 == tree 1, nontree 0. 
# "Our aim was to describe areas that were not directly under any projected canopy area, and were greater in diameter than most tree crowns and could therefore represent an area comparable in size to the zone of dominance of a large tree in our data." (Lydersen) if I follow their methods, gapThresh = 1.28 and spurThresh = 11.8 (max; could be lower)

#install.packages("fasterize")
#library(fasterize)
library(raster)
library(sf)
library(sp)
library(terra)

# prepare data: one plot (plot 9, OH2 in 2018) for testing
pm_df <- data.frame(X=plots_out[[9]]$trees.noedge$x, Y=plots_out[[9]]$trees.noedge$y, crown=plots_out[[9]]$trees.noedge$crown)
# I want to use trees.noedge starting out because the buffer is built into PatchMorph

#custom_crs <- st_crs("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")
ctr = data.frame(X = 0, Y = 0) # plot center to draw boundary of 1ha circle
bound = st_as_sf(ctr, coords = c("X", "Y")) |> st_buffer(sqrt(10000/pi)) # boundary of 1ha circle #, crs = custom_crs) 
stems <- st_as_sf(pm_df, coords = c("X", "Y")) #, crs = custom_crs) # points for each tree w/dbh attribute
crowns = st_buffer(stems, dist = stems$crown) # |> st_union()
notcrowns <- st_difference(bound, st_union(crowns)) # all area in non-crown space
crowns1 <- st_difference(bound, st_union(notcrowns))
tester2 <- rasterize(notcrowns, raster(extent(notcrowns), res = 0.1)) # turns sf into a raster -- this one is for all non-crown areas and value = 1
plot(tester2)
plot(rast(tester2))
# crownID <- rasterize(crowns, raster(extent(crowns), res=0.5)) # if you go from crowns, rather than difference of st_union(crowns), you bring along the ID number and radius associated with each crown. If you rasterize (and then spatRaster - ize) the notcrowns object, it all gets assigned a value of 1
# plot(rast(crownID))


# I am a bit confused about whether the non-crown area should be assigned 1 or 0 to start out. The way it's working now, the resulting plots show "opening" shapes as 0, and interstitial space as 1.

# set crs to "local" so that it's a cartesian plane with units in m
crs(tester2)
crs(tester2) <- "local"
crs(tester) <- "local"
cellSize(rast(tester2), lyrs = FALSE, unit="m", transform = FALSE)

tester <- rasterize(crowns1, raster(extent(crowns1), res = 1)) # turns sf into a raster -- this one is for all crown areas and value = 1 ?
#plot(tester)

# pm_sr <- rast(tester) # turns raster into a SpatRaster
# plot(pm_sr)

# new idea: is PatchMorph taking threshold parameters in units of pixels, not meters??
library(terra)
cellStats(tester2, sum) + cellStats(tester, sum) # 9991, I think this means that crowns plus notcrowns = 10000 with rounding error
cellSize(rast(tester2), transform = FALSE)

# patchMorph code
# https://rdrr.io/github/bi0m3trics/patchwoRk/man/patchMorph.html

#patchMorph <- function(data_in, buffer = 2, res=-1, suitThresh=-1, gapThresh=-1, spurThresh=-1, suitVals=-1, gapVals=-1, spurVals=-1, proj4=-1,verbose = TRUE...)
patchMorph <- function(data_in, buffer = 2, suitThresh=-1, gapThresh=-1, spurThresh=-1, suitVals=-1, gapVals=-1, spurVals=-1, proj4=-1,verbose = TRUE...)
{  # check input parameters and determine appropriate method based on input data type
  if(length(suitThresh) == 1)
    class(data_in) <- "SpatRaster"
  if(length(suitVals) > 1)
    class(data_in) <- "pmMulti"
  UseMethod("patchMorph", data_in)
}

# getCircleKernel <- function(radius)
# { # generates a circular kernel matrix based on specified radius
#   # the kernel defines the spatial neighborhood around each pixel, identifying which neighboring pixels are in or outside the circle with the specified radius. In this code, the kernel applied for gaps uses a radius of as.integer(gapThresh/2), while the kernel applied for spurs uses a radius of as.integer(spurThresh/2)
#   kernel_side <- 2 * as.integer(radius) + 1 # kernel side based on radius
#   kernel_y <- matrix(rep(radius:-radius, kernel_side), ncol=kernel_side) # get kernel matrix coordinates
#   kernel_x <- -t(kernel_y)
#   kernel   <- matrix(as.matrix(dist(cbind(as.vector(kernel_x), as.vector(kernel_y))))[as.integer((kernel_side^2) / 2) + 1,], ncol=kernel_side) # calculates distances from kernel center
#   # set threshold distances to create circular kernel
#   kernel[kernel <= radius] <- 0
#   kernel[kernel > 0]  <- 1
#   kernel <- 1 - kernel # invert kernel to mark the area outside the circle
#   return(kernel) # by the end of this process, there's a square window (r/2)+1 pixels across, with a circle of 1s in the center that are <= r pixels from the central pixel
# } 

# trying getCircleKernel with conversion from m to pixels
getCircleKernel <- function(radius, resolution)
{
  # Calculate the number of pixels for the specified radius
  radius_pixels <- radius / resolution

  # Generate a circular kernel matrix based on the specified radius
  kernel_side <- 2 * as.integer(radius_pixels) + 1
  kernel_y <- matrix(rep(radius_pixels:-radius_pixels, kernel_side), ncol = kernel_side)
  kernel_x <- -t(kernel_y)
  kernel <- matrix(as.matrix(dist(cbind(as.vector(kernel_x), as.vector(kernel_y))))[as.integer((kernel_side^2) / 2) + 1,], ncol=kernel_side) # calculates distances from kernel center
  # Set threshold distances to create circular kernel
  kernel[kernel <= radius_pixels] <- 0
  kernel[kernel > 0] <- 1
  kernel <- 1 - kernel  # Invert kernel to mark the area outside the circle

  return(kernel)
}

#' @describeIn patchMorph.SpatRaster Input is a SpatRaster, and only a single suitability, gap, and spur
#' values is specified, for which the only that outcomes is returned
#' @method patchMorph SpatRaster
#' @export
patchMorph.SpatRaster <- function(data_in, buffer = 2, suitThresh = 1, gapThresh = 2, spurThresh = 2, verbose = TRUE)
  # try adding resolution as a parameter in the function definition, then change in "gapKernel" and "spurKernel" below
{ # check validity of threshold parameters specified in the function call
  if(!is.numeric(c(suitThresh, gapThresh, spurThresh)))
    stop("suitThresh, gapThresh, and spurThresh must be numeric.")
 # if(gapThresh < 2 | spurThresh < 2)
#    stop("Gap/Spur threshold is too small! Must be at least twice the raster resolution.")
  
  ## Set up the crs, the extent, and a NA mask for the original raster
  r.crs <- terra::crs(data_in)
  r.e <- terra::ext(data_in)
  e.mask <- terra::mask(data_in, subst(data_in, 0:1, 1))
  
  ## Extend the raster by the buffer (cropped before return)
  data_in <- terra::extend(data_in, buffer, fill=NA)
  
  ## Get circular kernels for gap and spur thresholds
  # gapKernel  <- getCircleKernel(as.integer(gapThresh / 2)) # original
  # spurKernel <- getCircleKernel(as.integer(spurThresh / 2))
  gapKernel  <- getCircleKernel(as.integer(gapThresh / 2), 1)
  spurKernel <- getCircleKernel(as.integer(spurThresh / 2), 1)
  
  ## Get the euclidean distances to suitable habitat, and ensure the extent is the same as original
  data_in <- terra::distance(data_in, target = data_in[data_in <= suitThresh])
  
  ## Apply a focal maximum
  data_in <- terra::focal(data_in, gapKernel, fun="max", na.policy="omit", na.rm=TRUE)
  # data_in <- terra::mask(data_in, e.mask)
  # a circular window defined  by the gapKernel is centered around each pixel; the values of all pixels within the focal window thus established are considered. The max pixel value is calculated and then assigned to the central pixel. This process is repeated for every pixel in the raster, resulting in a new raster where each pixel value represents the maximum value within its neighborhood.
  
  if(verbose == TRUE)
    cat("Processing gap threshold diameter:", ncol(gapKernel)-1,"pixels\n")
  ## Reclassify based on the gap threshold
  data_in[data_in <= (ncol(gapKernel)+1)/2] <- 1
  data_in[data_in > (ncol(gapKernel)+1)/2] <- 0
  
  ## Check to see if there's still non-suitable pixels in the raster, otherwise return data_in
  if( (sum(data_in[terra::values(data_in)==1]) + sum(is.na(terra::values(data_in))) ) == ( nrow(data_in)*ncol(data_in)) ) return(data_in)
  
  ## Get the euclidean distances to non-suitable habitat, and ensure the extent is the same as original
  data_in <- terra::distance(data_in, target = data_in[data_in <= suitThresh])
  
  ## Apply a focal maximum
  data_in <- terra::focal(data_in, spurKernel, fun="max", na.policy="omit", na.rm=TRUE)
  # data_in <- terra::mask(data_in, e.mask)
  
  if(verbose == TRUE)
    cat("Processing spur threshold diameter:",ncol(spurKernel)-1, "pixels\n")
  ## Reclassify based on the spur threshold
  data_in[data_in <= (ncol(spurKernel)+1)/2] <- 0
  data_in[data_in > (ncol(spurKernel)+1)/2] <- 1
  
  # Crop the raster to the original extent
  data_in <- terra::crop(data_in, e.mask, mask=TRUE)
  
  return(data_in)
}

# sort of works
# pm.result.single <- patchMorph(data_in = rast(tester2), buffer = 5, suitThresh = 1, gapThresh = 9, spurThresh = 6, verbose = FALSE)
# plot(pm.result.single, main="PatchMorph Results (Gap-9 & Spur-6)")

pm.result <- patchMorph.SpatRaster(rast(tester2))
pm.result <- patchMorph.SpatRaster(rast(tester2), buffer = 5, suitThresh = 1, gapThresh = 6, spurThresh = 6, verbose = TRUE)
plot(pm.result, main="PatchMorph Results (Gap-6 & Spur-6)", col = c("#332211", "#FEC44F"))


pm.result.single <- patchMorph(data_in = rast(tester2), buffer = 5, suitThresh = 1, gapThresh = 8, spurThresh = 6, verbose = TRUE)
pm.result.single2 <- patchMorph(data_in = rast(tester2), buffer = 5, suitThresh = 1, gapThresh = 2, spurThresh = 12, verbose = TRUE)
par(mfrow=c(1,2))
plot(pm.result.single, main="PatchMorph Results (Gap-8 & Spur-6)", col = c("#332211", "#FEC44F"))
plot(pm.result.single2, main="PatchMorph Results (Gap-2 & Spur-12)", col = c("#332211", "#FEC44F"))

# make a for loop to look at lots of values of this
pm.results <- list()
pm.labels <- rep(NA, rep(length(2:12)^2))
for (i in 2:12) {
  for (j in 2:12) {
    pm.results[[(i-2)*11+(j-1)]] <- patchMorph(data_in = rast(tester2), buffer = 5, suitThresh = 1, gapThresh = i, spurThresh = j, verbose = FALSE)
  #  pm.labels[(i-2)*11+(j-1)] <- paste0("PatchMorph Results (Gap-", i, " & Spur-", j,")")
  }
}

par(mfrow=c(3,3))
gap <- 5
spur <- 11
a <- (gap-2)*11+(spur-1)
for (i in a:(a+8)) {
  plot(pm.results[[i]], main = pm.labels[i], col = c("#FEC44F","#332211" ))
} # consecutive plots

#for (i in c(round(seq(23,90, length.out = 9)))) { # regularly spaced draws of 9
for (i in c(8,20,22,41,44,53,71,74,77)) {  
  plot(pm.results[[i]], main = pm.labels[i], col = c("#FEC44F","#332211" ))
} # selected sample


gap_areas <- rep (NA, length(results))
for (i in 1:length(results)){
  gap_areas[i] <- 
    (sum(st_area(st_cast(results[[i]], "POLYGON")))/st_area(bound_buff))*10000
  # ( total area in gap polygons / total area not counting buffer ) * 10000 [scale up to 1ha full plot]
  # gets gap area per hectare, having corrected for buffer area
}

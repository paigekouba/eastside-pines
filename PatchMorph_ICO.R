# PatchMorph to find openings
library(raster)
library(sf)
library(sp)
library(terra)
# prepare data: one plot (plot 9, OH2 in 2018) for testing
pm_df <- data.frame(X=plots_out[[9]]$trees.noedge$x, Y=plots_out[[9]]$trees.noedge$y, crown=plots_out[[9]]$trees.noedge$crown)
ctr = data.frame(X = 0, Y = 0) # plot center to draw boundary of 1ha circle
bound = st_as_sf(ctr, coords = c("X", "Y")) |> st_buffer(sqrt(10000/pi)) # boundary of 1ha circle #, crs = custom_crs) 
stems <- st_as_sf(pm_df, coords = c("X", "Y")) #, crs = custom_crs) # points for each tree w/dbh attribute
crowns = st_buffer(stems, dist = stems$crown) # |> st_union()
notcrowns <- st_difference(bound, st_union(crowns)) # all area in non-crown space
tester2 <- rasterize(notcrowns, raster(extent(notcrowns), res = 0.1)) # turns sf into a raster -- this one is for all non-crown areas and value = 1
crs(tester2) <- "local"

# patchMorph code original:
# https://rdrr.io/github/bi0m3trics/patchwoRk/man/patchMorph.html

patchMorph <- function(data_in, buffer = 2, suitThresh=-1, gapThresh=-1, spurThresh=-1, suitVals=-1, gapVals=-1, spurVals=-1, proj4=-1,verbose = TRUE...)
{  # check input parameters and determine appropriate method based on input data type
  if(length(suitThresh) == 1)
    class(data_in) <- "SpatRaster"
  if(length(suitVals) > 1)
    class(data_in) <- "pmMulti"
  UseMethod("patchMorph", data_in)
}

getCircleKernel <- function(radius)
{ # generates a circular kernel matrix based on specified radius
  # the kernel defines the spatial neighborhood around each pixel, identifying which neighboring pixels are in or outside the circle with the specified radius. In this code, the kernel applied for gaps uses a radius of as.integer(gapThresh/2), while the kernel applied for spurs uses a radius of as.integer(spurThresh/2)
  kernel_side <- 2 * as.integer(radius) + 1 # kernel side based on radius
  kernel_y <- matrix(rep(radius:-radius, kernel_side), ncol=kernel_side) # get kernel matrix coordinates
  kernel_x <- -t(kernel_y)
  kernel   <- matrix(as.matrix(dist(cbind(as.vector(kernel_x), as.vector(kernel_y))))[as.integer((kernel_side^2) / 2) + 1,], ncol=kernel_side) # calculates distances from kernel center
  # set threshold distances to create circular kernel
  kernel[kernel <= radius] <- 0
  kernel[kernel > 0]  <- 1
  kernel <- 1 - kernel # invert kernel to mark the area outside the circle
  return(kernel) # by the end of this process, there's a square window (r/2)+1 pixels across, with a circle of 1s in the center that are <= r pixels from the central pixel
}

patchMorph.SpatRaster <- function(data_in, buffer = 2, suitThresh = 1, gapThresh = 2, spurThresh = 2, verbose = TRUE)
  # try adding resolution as a parameter in the function definition, then change in "gapKernel" and "spurKernel" below
{ # check validity of threshold parameters specified in the function call
  if(!is.numeric(c(suitThresh, gapThresh, spurThresh)))
    stop("suitThresh, gapThresh, and spurThresh must be numeric.")
  if(gapThresh < 2 | spurThresh < 2)
    stop("Gap/Spur threshold is too small! Must be at least twice the raster resolution.")
  
  ## Set up the crs, the extent, and a NA mask for the original raster
  r.crs <- terra::crs(data_in)
  r.e <- terra::ext(data_in)
  e.mask <- terra::mask(data_in, subst(data_in, 0:1, 1))
  
  ## Extend the raster by the buffer (cropped before return)
  data_in <- terra::extend(data_in, buffer, fill=NA)
  
  ## Get circular kernels for gap and spur thresholds
  gapKernel  <- getCircleKernel(as.integer(gapThresh / 2)) # original
  spurKernel <- getCircleKernel(as.integer(spurThresh / 2))
  # gapKernel  <- getCircleKernel(as.integer(gapThresh / 2), 0.5) # with resolution 
  # spurKernel <- getCircleKernel(as.integer(spurThresh / 2), 0.5)
  
  ## Get the euclidean distances to suitable habitat, and ensure the extent is the same as original
  data_in <- terra::distance(data_in, target = data_in[data_in <= suitThresh])
  
  ## Apply a focal maximum
  data_in <- terra::focal(data_in, gapKernel, fun="max", na.policy="omit", na.rm=TRUE)
  # data_in <- terra::mask(data_in, e.mask)
  # a circular window defined  by the gapKernel is centered around each pixel; the values of all pixels within the focal window thus established are considered. The max pixel value is calculated and then assigned to the central pixel. This process is repeated for every pixel in the raster, resulting in a new raster where each pixel value represents the maximum value within its neighborhood.
  
  if(verbose == TRUE)
    cat("Processing gap threshold diameter:", ncol(gapKernel)-1,"pixels\n")
  ## Reclassify based on the gap threshold
  # data_in[data_in <= (ncol(gapKernel)+1)/2] <- 1 # original
  # data_in[data_in > (ncol(gapKernel)+1)/2] <- 0
  data_in[data_in <= (ncol(gapKernel)+1)/2] <- 0 
  data_in[data_in > (ncol(gapKernel)+1)/2] <- 1 # switched to make openings "suitable"
  
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
  # data_in[data_in <= (ncol(spurKernel)+1)/2] <- 0
  # data_in[data_in > (ncol(spurKernel)+1)/2] <- 1
  data_in[data_in <= (ncol(spurKernel)+1)/2] <- 1
  data_in[data_in > (ncol(spurKernel)+1)/2] <- 0 # switched to make spurs "unsuitable"
  
  # Crop the raster to the original extent
  data_in <- terra::crop(data_in, e.mask, mask=TRUE)
  
  return(data_in)
}

# explanation of gap and spur, conceptually, from Girvetz and Greco 2007:
# (1) land cover density threshold (suitThresh), (2) habitat gap maximum thickness (gapThresh), and (3) habitat patch minimum thickness (spurThresh)
pm.rast <- patchMorph.SpatRaster(rast(tester2), buffer = 5, suitThresh = 1, gapThresh = 6, spurThresh = 7, verbose = TRUE)
plot(pm.rast, main="PatchMorph Results (Gap-6 & Spur-7)", col = c("#332211", "#FEC44F"))
# this code with these parameters (gap 6, spur 7) produces a map that looks right; min opening size ~= max canopy, and openings can snake through the trees but not to a ridiculous extent (cf Lydersen)

# convert to polygons for area calculations and mapping
pm.vect <- as.polygons(pm.rast, values = TRUE) # turns SpatRaster into SpatVector
pm.sf <- st_as_sf(pm.vect) # turns SpatVector into sf with a multipolygon for 0 and for 1
# take the multipolygon for cells with "1" attribute and turn it into lots of polygons
pm.sf1 <- pm.sf %>% filter(focal_max == 1)
pm.sfs <- st_cast(pm.sf1, "POLYGON")
pm.sfs$area <- st_area(pm.sfs)
sum(pm.sfs$area) # 4672.42 

# loop this over all plots!
# fn to convert plot x,y coords to spatRaster
# set up
ctr = data.frame(X = 0, Y = 0) # plot center to draw boundary of 1ha circle
bound = st_as_sf(ctr, coords = c("X", "Y")) |> st_buffer(sqrt(10000/pi)) # boundary of 1ha circle 

xy_sr <- function(plot){ # fn to convert plot x,y coords to not-crowns spatRaster with local crs
  df <- data.frame(X=plots_out[[plot]]$trees.noedge$x, Y=plots_out[[plot]]$trees.noedge$y, crown=plots_out[[plot]]$trees.noedge$crown)
  stems <- st_as_sf(df, coords = c("X", "Y")) #, crs = custom_crs) # points for each tree w/dbh attribute
  crowns = st_buffer(stems, dist = stems$crown) 
  notcrowns <- st_difference(bound, st_union(crowns)) # all area in non-crown space
  spatRast <- rast(rasterize(notcrowns, raster(extent(notcrowns), res = 0.1)))  # turns sf into a raster -- this one is for all non-crown areas and value = 1
  crs(spatRast) <- "local"
  return(spatRast)
}
# test
#plot(xy_sr(9)) # works!

# for loop for openings spatrasters from plots, converted to sf polygons and filtered for "1" attribute
opes_sr <- list()
for (i in 1:length(plots_out)){
  opes_sr[i] <- st_cast(filter(st_as_sf(as.polygons(patchMorph.SpatRaster(xy_sr(i), buffer = 5, suitThresh = 1, gapThresh = 6, spurThresh = 7, verbose = FALSE)), values = TRUE), focal_max == 1), "POLYGON")
} # result should be a set of polygons for each of 12 plots, representing the separate openings

opes_sr <- list()
for (i in 1:length(plots_out)){
  thingy <- patchMorph.SpatRaster(xy_sr(i), buffer = 5, suitThresh = 1, gapThresh = 6, spurThresh = 7, verbose = FALSE) %>% 
  as.polygons(values = TRUE) %>% 
  st_as_sf() %>% 
  filter(focal_max ==1) %>% 
  st_cast("POLYGON")
  
  opes_sr[[i]] <- thingy
} 
# test -- not working bc number of items to replace is not a multiple of replacement length
# plot(opes_sr[[2]], col = "lavender")
# ok! I am not happy with the snakiness of the super gap in e.g. plot 2... but moving on for now

# I will calculate their associated areas
for (i in 1:length(plots_out)){
  opes_sr[[i]]$area <- st_area(opes_sr[[i]])
}

# step A: add opes_sr polygons to ICO maps

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
ggplot(opes_all, aes(x=Opes, fill=as.factor(Year))) +
  geom_histogram(bins = 10, position="dodge") +
  stat_bin(geom="text", bins=10, aes(label=after_stat(count), group=as.factor(Year)), position = position_dodge())+ 
           scale_x_continuous(breaks = round(seq(57, 7779.12, length.out = 10)))


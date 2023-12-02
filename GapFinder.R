ggplot(df, aes(x0 = trees.x, y0 = trees.y, r=trees.crown, x=trees.x, y=trees.y)) +
  # Crowns as green circles with width corresponding to crown diameter
  geom_circle(n=20, aes(fill=factor(trees.bin), color=factor(trees.bin), alpha=0.85)) +
  coord_fixed() +
  # stems as brown circles with width corresponding to dbh (in m)
  geom_circle(n=20, aes(x0=trees.x, y0=trees.y, r=trees.dbh/200), color="burlywood4", fill="burlywood4") +
  coord_fixed() +
  # Set the color palette for cluster sizes
  scale_fill_brewer(palette = "YlGn", name = "Cluster Size") +
  scale_colour_brewer(palette = "YlGn", name = "Cluster Size") +
  guides(size = guide_legend(override.aes = list(color ="burlywood4"))) 
plot(pts)

plot(IS1_2018ppp)
str(IS1_2018ppp)
# List of 6
# $ window    :List of 4
# ..$ type  : chr "rectangle"
# ..$ xrange: num [1:2] -60 60
# ..$ yrange: num [1:2] -60 60
# ..$ units :List of 3
# .. ..$ singular  : chr "unit"
# .. ..$ plural    : chr "units"
# .. ..$ multiplier: num 1
# .. ..- attr(*, "class")= chr "unitname"
# ..- attr(*, "class")= chr "owin"
# $ n         : int 111
# $ x         : num [1:111] -50.8 -49.8 -42 -40.7 -40.4 -39.5 -34.2 -33.7 -33.5 -32.5 ...
# $ y         : num [1:111] 3.9 -8.8 19.9 38.8 -15.9 8.5 -19.5 -16.4 -16.8 -10.2 ...
# $ markformat: chr "vector"
# $ marks     : num [1:111] 128.5 119 72 36.6 53 ...
# - attr(*, "class")= chr "ppp"
IS1_2018ppp[[3]] # = x values
IS1_2018ppp[[4]] # = y values
IS1_2018ppp[[6]] # = dbh values

# make this into a SpatVector?

install.packages("terra")
library(terra)
# IS1_2018ppp <- ppp(IS1_2018$X, IS1_2018$Y, c(-60,60), c(-60,60)) where the ppp came from
# new ppp for OH2 2018
OH2_2018ppp <- ppp(OH2_2018$X, OH2_2018$Y, c(-60, 60), c(-60,60))
marks(OH2_2018ppp) <- OH2_2018[,5]
terra_pts <- cbind(OH2_2018ppp[[3]], OH2_2018ppp[[4]])
pts <- vect(terra_pts)
# add attributes to SpatVector object; need data.frame with same nrows

# marks(IS1_2018ppp) <- IS1_2018[,5] this is how I added dbh to the ppp
# **will need to change this to cr.rad later**
ptv <- vect(terra_pts, atts=data.frame(ID=1:nrow(terra_pts),dbh=OH2_2018[,5]))
ptv

# now try with polygons
# terra_pts <- cbind(id=1, part=1, OH2_2018ppp[[3]], OH2_2018ppp[[4]])
# pols <- vect(terra_pts, type="polygons")
# lns <- vect(terra_pts, type="lines")
# plot(pts)

plot(buffer(ptv, width=ptv$dbh/100))
#dbh_vec <- buffer(ptv, width=ptv$dbh/100)
dbh_vec <- vect(buffer(ptv, width=ptv$dbh/100))
plot(gaps(dbh_vec))
points(dbh_vec)

# pausing here, trying a new tack









# sf Approach with Derek
install.packages("sf")
library(sf)
# test plot: OH2 in 2018
# new df to convert to sf format
sf_df <- data.frame(X = OH2_2018$X, Y=OH2_2018$Y, dbh=OH2_2018$dbh)
# for iterable version, change to plots_out[[9]]$trees$... (x, y, and crown)
head(sf_df)

ctr = data.frame(X = 0, Y = 0) # plot center to draw boundary of 1ha circle
bound = st_as_sf(ctr, coords = c("X", "Y")) |> st_buffer(sqrt(10000/pi)) # boundary of 1ha circle
stems <- st_as_sf(sf_df, coords = c("X", "Y")) # points for each tree w/dbh attribute
crowns = st_buffer(stems, dist = stems$dbh/20) |> st_union() # PLACEHOLDER change to dist = cr.rad 
crowns_buffer = st_buffer(crowns, 5) # 5m buffer around each crown boundary
gaps2 = st_difference(bound, crowns_buffer) # total area in gaps w/ gap threshold from crowns_buffer
plot(gaps2, col="blue")
gaps3 = st_buffer(gaps2, 5) # a 5m buffer around the area at least 5m from a crown edge—gap boundary
# this works bc it leaves out the areas that weren't big enough to be >5m away from a crown
# gaps3 started out as 1 observation containing all points
gaps3 = st_cast(gaps3, "POLYGON") # this makes it 12 observations

plot(crowns, col="green")
plot(gaps3, col="red", add=TRUE) # overestimates gaps; likely are unobserved trees over plot boundary

bound_noedge = st_buffer(bound, -5) # since the edge effect overestimates the gaps at the boundary, take off 5m from the edge of the plot to find gaps

gaps3_noedge = st_intersection(gaps3, bound_noedge) # limits gaps3 to just areas ≥5m from plot edge

plot(bound)
plot(gaps3, col="red", add=TRUE) 
plot(gaps3_noedge, col="yellow", add=TRUE)
plot(crowns, col="green", add=TRUE)

st_area(bound_noedge) # modified plot area: 1ha - 5m strip buffer around the boundary
sum(st_area(gaps3_noedge)) # gap area within edge buffer

(sum(st_area(gaps3_noedge))/st_area(bound_noedge))*st_area(bound_noedge)/10000
# fraction of gap/area corrected to apply to full 1ha plot: 0.40

# Use proportional area table to do Chi-square test on structural category distribution between 1941 and 2018

clust_gap_df[is.na(clust_gap_df)] <- 0 # replace NAs with 0, since NA represents 0 area in this size category
clust_gap_table <- as.data.frame(t(clust_gap_df)) # transpose
clust_gap_table <- clust_gap_table[-13,] # remove bin names row
clust_gap_table <- data.frame(apply(clust_gap_table, 2, function(x) as.numeric(as.character(x)))) # convert to numeric
colnames(clust_gap_table) <- c("gap",bin_names) # add column headers by bin names
rownames(clust_gap_table) <- names
write.csv(clust_gap_table,"clust_gap.csv") # yeet to Excel

# stinky little ANOVA, just IS so far
# for all 5 size groups, + gaps, do ANOVA on the areas in each of the 3 plots, 1941 v 2018
dat <- data.frame(c(rep("2018",3),rep("1941",3)),c(clust_gap_table[c(1,3,5),1],clust_gap_table[c(2,4,6),1]))
colnames(dat) <- c("year","gap_area")
ggplot(dat) + aes(x=year, y=gap_area) + geom_boxplot() + 
  labs( x = "Year", y = "Area (sq. m)") +
  ggtitle("Gap Sizes at IS")                    # change this to start at 0?

summary(dat)
summary(aov(gap_area ~ year, data = dat))
oneway.test(gap_area ~ year, data = dat, var.equal = FALSE)

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


# glmm stuff
model41 <- glmer(n ~ clust.sz + (1|Plot.unique) + (1|Site), data = MLR_41, family = poisson)
model18 <- glmer(n ~ clust.sz + (1|Plot.unique) + (1|Site), data = MLR_18, family = poisson)
summary(model41)

# glmer(n ~ clust.sz + (1|Plot) + (1|Site), data = MLR_41, family = negative binomial)

# glmm (n ~ clust.sz + Year + (1|Site:Plot), data = MLR_df, family = poisson OR negative binomial)
# anova to compare models
model_full_pois <- glmer(n ~ clust.sz*Year + (1|Site) + (1|Plot.unique), data = MLR_df, family = poisson)
# error: boundary (singular) fit: see help('isSingular'). 
# Perfect separation happens when a predictor variable perfectly predicts the outcome variable
# I think this is happening because between all the variables I have included the same # permutations as there are instances of the response variable n... 13 clust.sz * 2 Year * 2 Site * 6 Plot.unique = 312 ... ?

# check for correlation between the predictor variables
pred_vars <- subset(MLR_df, select = c("clust.sz", "Plot.unique", "Site", "Year"))
# pairs(pred_vars) still doesn't work bc non-numeric variables
library(car)
vif(model_full_pois)
vif(model_pois)
vif(model_full_nb)
# seems like clust.sz:Year might be having multicollinearity problem??
vif(model_andrew)
vif(mod_full_nb)
vif(mod_red_nb)

model_pois <- glmer(n ~ clust.sz + Year + (1|Site) + (1|Plot.unique), data = MLR_df, family = poisson)
model_full_nb <- glmer.nb(n ~ clust.sz*Year + (1|Site) + (1|Plot.unique), data = MLR_df) 
model_nb <- glmer.nb(n ~ clust.sz + Year + (1|Site) + (1|Plot.unique), data = MLR_df)
model_andrew <- glmer.nb(n ~ clust.sz*Year + (1|Site/Plot.unique), data = MLR_df)
model_andrew2 <- glmer.nb(n ~ clust.sz*Year + (1|Plot.unique), data = MLR_df, control = glmerControl(optimizer="bobyqa"))

model_andrew2@optinfo[c("optimizer","control")]

# all got: boundary (singular) fit: see help('isSingular')
# model_full_nb got "Warning message:
#In theta.ml(Y, mu, weights = object@resp$weights, limit = limit,  :
#              iteration limit reached"
summary(model_andrew2)

anova(model_full_pois, model_pois, model_full_nb, model_nb)
# npar     AIC     BIC  logLik deviance    Chisq Df Pr(>Chisq)    
# model_pois         5  463.68  474.99 -226.84   453.68                           
# model_full_pois    6  400.86  414.44 -194.43   388.86   64.814  1  8.232e-16 ***
# model_nb           6 1686.40 1699.97 -837.20  1674.40    0.000  0               
# model_full_nb      7  367.35  383.19 -176.68   353.35 1321.042  1  < 2.2e-16 ***

# model_full_nb is the winner?

# trying again with model that doesn't give isSingular error -- leave out Site 
mod_full <- glmer(n ~ clust.sz*Year + (1|Plot.unique), data = MLR_df, family = poisson)
mod_full_nb <- glmer.nb(n ~ clust.sz*Year + (1|Plot.unique), data = MLR_df)
mod_red <- glmer(n ~ clust.sz + Year + (1|Plot.unique), data = MLR_df, family = poisson)
mod_red_nb <- glmer.nb(n ~ clust.sz + Year + (1|Plot.unique), data = MLR_df)
# Warning message:
# In theta.ml(Y, mu, weights = object@resp$weights, limit = limit,  :
#               iteration limit reached
mod_simple_nb <- glmer.nb(n ~ clust.sz*Year + (1|Site), data = MLR_df)
anova(mod_simple, mod_simple_nb)


# create a new dataframe with the predictions assuming Year = 1941, one with Year = 2018
# make sure Year is a factor

ICO_predict <- cbind(MLR_df, predict(model_all, interval = 'confidence'))
ggplot(ICO_predict, aes(clust.sz, n)) +
  geom_point() 

MLR_df %>%
  mutate(my_model = predict(model_all)) %>%
  ggplot(aes(clust.sz, n, color = Year)) +
  geom_point(aes(fill=Year, color=Year)) +
  geom_line(aes(clust.sz, my_model)) +
  scale_color_manual(values=c("red","black"))

ggplot(MLR_df, aes(x=clust.sz, y=n, color = Year)) +
  geom_point(aes(fill= Year, color = Year)) +
  geom_smooth(aes(group = Year, color=Year), method = "glm", method.args = list(family = "poisson")) +
  scale_color_manual(values=c("red","black"))

library(jtools)
effect_plot(model41, pred = clust.sz, interval = TRUE, plot.points = TRUE)
effect_plot(model18, pred = clust.sz, interval = TRUE, plot.points = TRUE)
library(broom.mixed)
plot_summs(model41, model18)
library(patchwork)
effect_plot(model41, pred = clust.sz, interval = TRUE, plot.points = TRUE, colors = "red", xlim = 20) +
  effect_plot(model18, pred = clust.sz, interval = TRUE, plot.points = TRUE)

effect_plot(model18, pred = clust.sz, interval = TRUE, plot.points = TRUE)
effect_plot(model41, pred = clust.sz, interval = TRUE, plot.points = TRUE, colors = "red", xlim = 20) 
# can't figure out how to plot these together

#______________________________________________________________________________#



# NON-SPATIAL METRICS

# Get avg DBH, max DBH, stems/ha, BA, and QMD for IS live trees in 2018:
meanIS <- mean(IS_livetrees$dbh)
sd(IS_livetrees$dbh) #28.5
maxIS <- max(IS_livetrees$dbh)
stemsIS <- nrow(IS_livetrees)/3
BA_IS <- sum(pi*(IS_livetrees$dbh/200)^2)/3
QMD_IS <- sqrt(sum(IS_livetrees$dbh^2)/nrow(IS_livetrees))

# Get avg DBH, max DBH, stems/ha, BA, and QMD for IS live trees in 1941:
meanIS1941 <- mean(IS_trees1941$dbh1941)
maxIS1941 <- max(IS_trees1941$dbh1941)
stemsIS1941 <- nrow(IS_trees1941)/3
BA_IS1941 <- sum(pi*(IS_trees1941$dbh1941/200)^2)/3
QMD_IS1941 <- sqrt(sum(IS_trees1941$dbh1941^2)/nrow(IS_trees1941))

IS_metrics <- data.frame(Metric = c("MeanDBH", "MaxDBH", "Stems","BasalArea","QMD"), 
                         IS2018 = c(meanIS, maxIS, stemsIS, BA_IS, QMD_IS),
                         IS1941 = c(meanIS1941, maxIS1941, stemsIS1941, BA_IS1941, QMD_IS1941))

#______________________________________________________________________________#

# SPATIAL STUFF

#install.packages("spatstat")
library(spatstat)
# subset just IS1 for mapping practice
IS1_2018 <- IS_livetrees[IS_livetrees$Plot == "IS1",]
IS1_1941 <- IS_trees1941[IS_trees1941$Plot == "IS1",]

# convert x, y data to point patter
IS1_2018ppp <- ppp(IS1_2018$X, IS1_2018$Y, c(-60,60), c(-60,60))
IS1_1941ppp <- ppp(IS1_1941$X, IS1_1941$Y, c(-60,60), c(-60,60))
# plot with dbh
marks(IS1_1941ppp) <- IS1_1941[,13]
marks(IS1_2018ppp) <- IS1_2018[,5]
par(mfrow=c(1,2))
plot(IS1_2018ppp)
plot(IS1_1941ppp)

par(mfrow=c(1,2)) # plot as heatmap of biggest-dbh trees
plot(Smooth(IS1_1941ppp))
plot(Smooth(IS1_2018ppp))

par(mfrow=c(1,2)) # plot as heatmap of most stems per area
plot(density(IS1_1941ppp))
plot(density(IS1_2018ppp))

#______________________________________________________________________________#

# NON-SPATIAL METRICS

# Get avg DBH, max DBH, stems/ha, BA, and QMD for IS live trees in 2018:
meanOH <- mean(OH_livetrees$dbh)
sd(OH_livetrees$dbh) #28.5
maxOH <- max(OH_livetrees$dbh)
stemsOH <- nrow(OH_livetrees)/3
BA_OH <- sum(pi*(OH_livetrees$dbh/200)^2)/3
QMD_OH <- sqrt(sum(OH_livetrees$dbh^2)/nrow(OH_livetrees))

# Get avg DBH, max DBH, stems/ha, BA, and QMD for IS live trees in 1941:
meanOH1941 <- mean(OH_trees1941$dbh1941)
maxOH1941 <- max(OH_trees1941$dbh1941)
stemsOH1941 <- nrow(OH_trees1941)/3
BA_OH1941 <- sum(pi*(OH_trees1941$dbh1941/200)^2)/3
QMD_OH1941 <- sqrt(sum(OH_trees1941$dbh1941^2)/nrow(OH_trees1941))

OH_metrics <- data.frame(Metric = c("MeanDBH", "MaxDBH", "Stems","BasalArea","QMD"), 
                         OH2018 = c(meanOH, maxOH, stemsOH, BA_OH, QMD_OH),
                         OH1941 = c(meanOH1941, maxOH1941, stemsOH1941, BA_OH1941, QMD_OH1941))

# SPATIAL STUFF

#install.packages("spatstat")
library(spatstat)
# subset just IS1 for mapping practice
IS1_2018 <- IS_livetrees[IS_livetrees$Plot == "IS1",]
IS1_1941 <- IS_trees1941[IS_trees1941$Plot == "IS1",]

# convert x, y data to point patter
IS1_2018ppp <- ppp(IS1_2018$X, IS1_2018$Y, c(-60,60), c(-60,60))
IS1_1941ppp <- ppp(IS1_1941$X, IS1_1941$Y, c(-60,60), c(-60,60))
# plot with dbh
marks(IS1_1941ppp) <- IS1_1941[,13]
marks(IS1_2018ppp) <- IS1_2018[,5]
par(mfrow=c(1,2))
plot(IS1_2018ppp)
plot(IS1_1941ppp)

par(mfrow=c(1,2)) # plot as heatmap of biggest-dbh trees
plot(Smooth(IS1_1941ppp))
plot(Smooth(IS1_2018ppp))

par(mfrow=c(1,2)) # plot as heatmap of most stems per area
plot(density(IS1_1941ppp))
plot(density(IS1_2018ppp))

# Ripley's K
# A result of trees being significantly clumped at a certain distance means that on average, trees have more neighbors within that distance than would be expected with a random distribution (Illian et al., 2008). The square-root transformation, or L-function, was calculated for each plot in each dataset, using the default settings in Spatstat, so that spatial aggregation was assessed up to a maximum distance of 45 m (one quarter the length of the shortest plot dimension), over 512 equally spaced intervals. We used the isotropic correction to control for edge effects. Observed values of L(r) were compared to an envelope of complete spatial randomness (CSR) generated with 999 simulations.
# library(spatstat)
# test_ppp <- ppp(OH1_2018$X, OH1_2018$Y, window = disc(radius = sqrt(10000/pi), centre = c(0,0)))
# plot(Kest(test_ppp), correction="isotropic")
# plot(envelope(test_ppp, Lest, nsim=50))

#______________ Histogram stuff _______________# 
# age hist with pattern
ggplot(OH_livetrees, aes(x=estab_est))+
  geom_bar_pattern( fill="#addd8e",
                    binwidth = 20,
                    color="black",
                    pattern_fill="black",
                    pattern_angle=45,
                    pattern_density=0.1,
                    pattern_spacing=0.05,
                    pattern_key_scale_factor=0.6,
                    aes(pattern=Spec)) +
  scale_pattern_manual(values=c(ABCO="wave", JUGR="pch",PICO="stripe",PIJE="none"), name = "Species") +
  scale_y_continuous(limits=(c(0,80)), expand = expansion(mult = c(0, 0))) +
  geom_vline(xintercept=1941, size = 2, linetype="dashed",color="red4") +
  labs(x="Establishment Year") +
  ggtitle("Tree Age Distribution at O'Harrell Canyon") +
  theme_bw(base_size=22)
ggplot(IS_livetrees, aes(x=estab_est))+
  geom_bar_pattern( fill="#addd8e",
                    binwidth = 20,
                    color="black",
                    pattern_fill="black",
                    pattern_angle=45,
                    pattern_density=0.1,
                    pattern_spacing=0.05,
                    pattern_key_scale_factor=0.6,
                    aes(pattern=Spec)) +
  scale_pattern_manual(values=c(ABCO="wave", JUGR="pch",PICO="stripe",PIJE="none"), name = "Species") +
  scale_y_continuous(limits=(c(0,80)), expand = expansion(mult = c(0, 0))) +
  geom_vline(xintercept=1941, size = 2, linetype="dashed",color="red4") +
  labs(x="Establishment Year") +
  ggtitle("Tree Age Distribution at Indiana Summit") +
  theme_bw(base_size=22)

# age hists with species
ggplot(OH_livetrees, aes(x=estab_est, fill=factor(Spec)))+
  geom_histogram() +
  scale_fill_brewer(palette = "Set2", name = "Species") +
  scale_y_continuous(limits=(c(0,80)), expand = expansion(mult = c(0, 0))) +
  geom_vline(xintercept=1941, size = 2, linetype="dashed",color="red4") +
  labs(x="Establishment Year") +
  ggtitle("Tree Age Distribution at\n O'Harrell Canyon") +
  theme_bw(base_size=22)

ggplot(IS_livetrees, aes(x=estab_est))+
  geom_histogram(fill="#E78AC3") +
  # scale_colour_brewer(palette = "Set2", name = "Species") +
  scale_y_continuous(limits=(c(0,80)), expand = expansion(mult = c(0, 0))) +
  geom_vline(xintercept=1941, size = 2, linetype="dashed",color="red4") +
  labs(x="Establishment Year") +
  ggtitle("Tree Age Distribution at\n Indiana Summit") +
  theme_bw(base_size=22)
#install.packages("RColorBrewer")
#library(RColorBrewer)

ggplot(OH_trees1941, aes(x=estab_est))+
  geom_bar_pattern( fill="#addd8e",
                    binwidth = 20,
                    color="black",
                    pattern_fill="black",
                    pattern_angle=45,
                    pattern_density=0.1,
                    pattern_spacing=0.05,
                    pattern_key_scale_factor=0.6,
                    aes(pattern=Spec)) +
  scale_pattern_manual(values=c(ABCO="wave", JUGR="pch",PICO="stripe",PIJE="none"), name = "Species") +
  scale_y_continuous(limits=(c(0,80)), expand = expansion(mult = c(0, 0))) +
  geom_vline(xintercept=1941, size = 2, linetype="dashed",color="red4") +
  labs(x="Establishment Year") +
  ggtitle("Tree Age Distribution at O'Harrell Canyon, 1941") +
  theme_bw(base_size=22)

ggplot(IS_trees1941, aes(x=estab_est))+
  geom_bar_pattern( fill="#addd8e",
                    binwidth = 20,
                    color="black",
                    pattern_fill="black",
                    pattern_angle=45,
                    pattern_density=0.1,
                    pattern_spacing=0.05,
                    pattern_key_scale_factor=0.6,
                    aes(pattern=Spec)) +
  scale_pattern_manual(values=c(ABCO="wave", JUGR="pch",PICO="stripe",PIJE="none"), name = "Species") +
  scale_y_continuous(limits=(c(0,80)), expand = expansion(mult = c(0, 0))) +
  geom_vline(xintercept=1941, size = 2, linetype="dashed",color="red4") +
  labs(x="Establishment Year") +
  ggtitle("Tree Age Distribution at Indiana Summit, 1941") +
  theme_bw(base_size=22)

# change in Ripley's K: ANOVA of K-statistic for each of the 6 plots, 1941 group vs. 2018 group
IS1_2018ppp <- ppp(plots[[1]]$X, plots[[1]]$Y, window = disc(radius = sqrt(10000/pi), centre = c(0,0)), marks = as.numeric(rownames(plots[[1]])))
K_IS1_18 <- Kest(IS1_2018ppp, correction = "isotropic")

IS1_1941ppp <- ppp(plots[[2]]$X, plots[[2]]$Y, window = disc(radius = sqrt(10000/pi), centre = c(0,0)), marks = as.numeric(rownames(plots[[2]])))
K_IS1_41 <- Kest(IS1_1941ppp, correction = "isotropic")

plot(K_IS1_18, sqrt(iso/pi) ~ r)
plot(K_IS1_18, sqrt(./pi) ~ r, ylab="L(r)", main="L function for cells")

plot(K_IS1_41, sqrt(./pi) ~ r, ylab="L(r)", main="L function for cells")

# first attempt at PatchMorph
sf_df <- data.frame(X=plots_out[[9]]$trees$x, Y=plots_out[[9]]$trees$y, crown=plots_out[[9]]$trees$crown)
custom_crs <- st_crs("+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")
ctr = data.frame(X = 0, Y = 0) # plot center to draw boundary of 1ha circle
bound = st_as_sf(ctr, coords = c("X", "Y"), crs = custom_crs) |> st_buffer(sqrt(10000/pi)) # boundary of 1ha circle
stems <- st_as_sf(sf_df, coords = c("X", "Y"), crs = custom_crs) # points for each tree w/dbh attribute
#crowns = st_buffer(stems, dist = stems$crown) #|> st_union()
crowns = st_buffer(stems, dist = stems$crown) #|> st_union()
#notcrowns <- st_difference(bound, crowns) # need to try this step so I can get 0s within crowns and 1s within space
notcrowns <- st_difference(bound, st_union(crowns))
crowns1 <- st_difference(bound, notcrowns)
#|> st_union()
# crowns_buffer = st_buffer(crowns, 5) # 5m buffer around each crown boundary
# gaps2 = st_difference(bound, crowns_buffer) # total area in gaps w/ gap threshold from crowns_buffer
# plot(gaps2, col="blue")
# gaps3 = st_buffer(gaps2, 5) # a 5m buffer around the area at least 5m from a crown edge—gap boundary
# # this works bc it leaves out the areas that weren't big enough to be >5m away from a crown
# # gaps3 started out as 1 observation containing all points
# gaps3 = st_cast(gaps3, "POLYGON") # this makes it 12 observations

# Create a SpatialPoints object with your x, y coordinates
x <- plots_out[[9]]$trees.noedge$x
y <- plots_out[[9]]$trees.noedge$y
radii <- plots_out[[9]]$trees.noedge$crown
points <- SpatialPointsDataFrame(coords = cbind(x, y), data = data.frame(radii))

#st_set_crs(crowns, "local")
extent(crowns)
tester <- rasterize(crowns, raster(extent(crowns), res = 2))
plot(rasterize(crowns, raster(extent(crowns), res = 1)))
crs(tester) # Deprecated Proj.4 representation: NA 

# # Define the extent of the raster (xmin, xmax, ymin, ymax)
# extent <- extent(-sqrt(10000/pi), sqrt(10000/pi), -sqrt(10000/pi), sqrt(10000/pi))
# # Define the resolution of the raster (number of cells in x and y direction)
# # res <- c(1, 1)
# # # Create an empty raster
# # r <- raster(extent, res)
# # r[] <- 0

tester2 <- rasterize(notcrowns, raster(extent(notcrowns), res = 0.07))
plot(tester2)
tester3 <- rasterize(crowns1, raster(extent(crowns1), res = 0.05))
plot(tester3)

#' @param data_in A SpatRaster. Map of suitable/non-suitable habitat
#' @param suitThresh A interger. A threshold value over which some organism may perceive the area as
#' suitable habitat (resulting in a binary map of suitable and non-suitable pixels)
#' @param gapThresh A interger. The gap diameter of non-suitable land cover within a habitat patch
#' that should be considered part of the patch if small enough
#' @param spurThresh A interger. The width of a section of narrow, unsuitable edge habitat extending
#' out from a larger, wider patch that is too thin to be considered part of suitable habitat
#' @param suitVals Integer vector. A vector of size = 3 specifying the lower suitability threshold,
#' the upper suitability threshold, and the total number of values to be evaluated.
#' @param gapVals Integer vector. A vector of size = 3 specifying the lower gap threshold, the upper
#' gap threshold, and the total number of values to be evaluated.
#' @param spurVals Integer vector. A vector of size = 3 specifying the lower spur threshold, the upper
#' spur threshold, and the total number of values to be evaluated.
#' @return A RasterLayer or a list of RasterLayers of the same dimensions as data_in where 1's are
#' suitbale habitat and 0's are unsutiable habitat. In the case of PM_Hierarchy, patchMorph returns
#' a list of RasterLayers (one per suitability-gap-spur combination) outcomes, otherwise it returns
#' a single RasterLayer of the single resulting suitability-gap-spur outcome.
#' @references
#' Girvetz EH, and Greco SE. 2007. How to define a patch: a spatial model for hierarchically
#' delineating organism-specific habitat patches. Landscape Ecology 22: 1131-1142.
#' @examples
#' myFile <- system.file("extdata", "mixedconifer.tif", package="patchwoRk")
#' myRas <- rast(myFile)
#' 
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


#'
#pm.result.single <- patchMorph(data_in = tester, suitThresh = 1, gapThresh = 5, spurThresh = 10)
pm.result.single <- patchMorph(data_in = tester2, suitThresh = 1, gapThresh = 5, spurThresh = 12)
# 1: In .couldBeLonLat(x, warnings = warnings) :
#CRS is NA. Assuming it is longitude/latitude
plot(pm.result.single, main="PatchMorph Results (Gap-2 & Spur-2)")

#'
#' pm.layered.result <- patchMorph(data_in = myRas, suitVals = c(0, 1, 2),
#' gapVals = c(2, 6, 3), spurVals = c(2, 6, 3))
#' names(pm.layered.result)
#' plot(pm.layered.result[[1]], main=names(pm.layered.result)[1])
#'
#' @export
patchMorph <- function(data_in, res=-1, suitThresh=-1, gapThresh=-1, spurThresh=-1, suitVals=-1, gapVals=-1, spurVals=-1, proj4=-1,...)
{
  if(length(suitThresh) == 1)
    class(data_in) <- "SpatRaster"
  if(length(suitVals) > 1)
    class(data_in) <- "pmMulti"
  UseMethod("patchMorph", data_in)
}

#' @describeIn patchMorph.SpatRaster Input is a SpatRaster, and only a single suitability, gap, and spur
#' values is specified, for which the only that outcomes is returned
#' @method patchMorph SpatRaster
#' @export
patchMorph.SpatRaster <- function(data_in, suitThresh = 1, gapThresh = 2, spurThresh = 2)
{
  if(!is.numeric(c(suitThresh, gapThresh, spurThresh)))
    stop("suitThresh, gapThresh, and spurThresh must be numeric.")
  if(gapThresh < 2 | spurThresh < 2)
    stop("Gap/Spur threshold is too small! Must be at least twice the raster resolution.")
  
  ## Set up the crs, the extent, and a NA mask for the original raster
  r.crs <- "local" #terra::crs(data_in)
  r.e<-terra::ext(data_in)
  e.mask<-terra::mask(data_in, data_in, maskvalue=0, updatevalue=1)
  
  ## Get the associated kernels
  gapKernel  <- getCircleKernel(as.integer(gapThresh / 2))
  spurKernel <- getCircleKernel(as.integer(spurThresh / 2))
  
  ## Get the euclidean distances to suitable habitat, and ensure the extent is the same as original
  data_in <- terra::distance(data_in, target = data_in[data_in <= suitThresh])
  
  ## Apply a focal maximum
  data_in <- terra::focal(data_in, gapKernel, fun=max, na.rm=TRUE, na.policy="omit", fillvalue=NA, expand = TRUE)
  data_in<-terra::mask(data_in, e.mask)
  
  cat("Processing gap threshold diameter:", ncol(gapKernel)-1,"pixels\n")
  ## Reclassify based on the gap threshold
  data_in[data_in <= (ncol(gapKernel)+1)/2] <- 1
  data_in[data_in > (ncol(gapKernel)+1)/2] <- 0
  
  ## Check to make sure there's still non-suitable pixels in the raster, othwewise return data_in
  if( (sum(data_in[terra::values(data_in)==1]) + sum(is.na(terra::values(data_in))) ) == ( nrow(data_in)*ncol(data_in)) ) return(data_in)
  
  ## Get the euclidean distances to non-suitable habitat, and ensure the extent is the same as original
  data_in <- terra::distance(data_in, target = data_in[data_in <= suitThresh])
  
  ## Apply a focal maximum
  data_in <- terra::focal(data_in, spurKernel, fun=max, na.rm=TRUE, na.policy="omit", fillvalue=NA, expand = TRUE)
  data_in <- terra::mask(data_in, e.mask)
  
  cat("Processing spur threshold diameter:",ncol(spurKernel)-1, "pixels\n")
  ## Reclassify based on the spur threshold
  data_in[data_in <= (ncol(spurKernel)+1)/2] <- 0
  data_in[data_in > (ncol(spurKernel)+1)/2] <- 1
  
  return(data_in)
}

# ChatGPT raster from x, y data. did not work

# Function to create a circular kernel for focal function
create_circle_kernel <- function(radius, res) {
  diameter <- ceiling(radius * 2) + 1  # Ensure odd diameter
  if (diameter %% 2 == 0) diameter <- diameter + 1  # Ensure odd number of rows/columns
  circle <- matrix(0, nrow = diameter, ncol = diameter)
  center <- (diameter + 1) / 2  # Calculate center index
  for (i in 1:diameter) {
    for (j in 1:diameter) {
      if ((i - center)^2 + (j - center)^2 <= radius^2) {
        circle[i, j] <- 1
      }
    }
  }
  return(circle)
}

# Apply focal function to count points within the circle for each cell
count_points_within_circle <- function(radii, kernel) {
  point_count <- numeric(length(radii))
  for (i in seq_along(radii)) {
    radius <- radii[i]
    kernel_matrix <- create_circle_kernel(radius, res)
    # Use cellStats to calculate the sum within each focal window
    point_count[i] <- sum(cellStats(focal(r, w = kernel_matrix, pad = TRUE, padValue = NA), stat = "sum"))
  }
  return(point_count)
}

# Apply the function to each point
cell_values <- count_points_within_circle(points$radii, kernel = NULL)

# Plot the resulting raster
plot(cell_values, main = "Raster with points within circle")

# troubleshooting 
# Visualize the circular kernel matrix
radius <- 3  # Choose a sample radius for visualization
kernel_matrix <- create_circle_kernel(radius, res)
image(kernel_matrix, main = "Circular Kernel Matrix")

# PatchMorph practice

# try using PatchMorph on just boundary circle
bound_rast <- rasterize(bound, raster(extent(bound), res=0.07))
plot(patchMorph(data_in = rast(bound_rast), buffer = 5, suitThresh = 1, gapThresh = 8, spurThresh = 6, verbose = FALSE)) # good, says it's all suitable
# try using PatchMorph on just boundary circle with one crown of diameter = 10, right in the middle
OneCrown <- st_as_sf(ctr, coords = c("X", "Y"), crs = custom_crs) |> st_buffer(5)
boundCrown <- st_difference(bound, st_union(OneCrown))
OneCrown_rast <- rasterize(boundCrown, raster(extent(boundCrown), res=0.07))
plot(patchMorph(data_in = rast(OneCrown_rast), buffer = 5, suitThresh = 1, gapThresh = 10, spurThresh = 2, verbose = FALSE), col = c("#FEC44F","#332211" ))

TwoCrown <- st_as_sf(data.frame(X = 28.2, Y = 28.2), coords = c("X", "Y"), crs = custom_crs) |> st_buffer(5)
bound2Crown <- st_difference(st_difference(bound, st_union(OneCrown)), TwoCrown)
TwoCrown_rast <- rasterize(bound2Crown, raster(extent(bound2Crown), res=0.07))
plot(patchMorph(data_in = rast(TwoCrown_rast), buffer = 5, suitThresh = 1, gapThresh = 10, spurThresh = 2, verbose = FALSE), col = c("#FEC44F","#332211" ))

ThreeCrown <- st_as_sf(data.frame(X = 22.5, Y = 0), coords = c("X", "Y"), crs = custom_crs) |> st_buffer(2.5)
bound3Crown <- st_difference(st_difference(bound2Crown), ThreeCrown)
ThreeCrown_rast <- rasterize(bound3Crown, raster(extent(bound3Crown), res=0.07))
plot(patchMorph(data_in = rast(ThreeCrown_rast), buffer = 5, suitThresh = 1, gapThresh = 8, spurThresh = 3, verbose = FALSE), col = c("#FEC44F","#332211" ))


# getCircularKernel example with gapThresh = 4 (r = 2)
gap4kern <- matrix(as.matrix(dist(cbind(as.vector(matrix(rep(2:-2, 5), ncol=(2*2+1))), as.vector(-t(matrix(rep(2:-2, 5), ncol=(2*2+1)))))))[as.integer(((2*2+1)^2) / 2) + 1,], ncol=2*2+1)
gap4kern[gap4kern <= 2] <- 0
gap4kern[gap4kern > 0] <- 1
gap4kern <- 1 - gap4kern
gap4kern
# by the end of this process, there's a square window 5 pixels across, with a circle of 1s in the center that are <= r=2 pixels from the central pixel

# was within patchMorph.SpatRaster
# gapKernel  <- getCircleKernel(as.integer(10*gapThresh / 2)) # I want to try setting the radius to convert from m to pixels (factor of 10)
# spurKernel <- getCircleKernel(as.integer(10*spurThresh / 2))

# time when I switched the 1s and 0s
# data_in[data_in <= (ncol(gapKernel)+1)/2] <- 0
# data_in[data_in > (ncol(gapKernel)+1)/2] <- 1 # if the opening is above the minimum gapThresh, it is suitable
# # this may be silly but I recoded the 0 and 1 assignment so that "openings" are given 1, ie assigned "suitable"
# NEVER NEVER GIVE UP

# NEVER LET GO, NEVER SURRENDER
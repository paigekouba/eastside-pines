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

# original PatchMorph practice
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
# yes; but I am not sure how to convert betwixt them
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

# trying getCircleKernel with conversion from m to pixels
# getCircleKernel <- function(radius, resolution)
# {
#   # Calculate the number of pixels for the specified radius
#   radius_pixels <- radius / resolution
# 
#   # Generate a circular kernel matrix based on the specified radius
#   kernel_side <- 2 * as.integer(radius_pixels) + 1
#   kernel_y <- matrix(rep(radius_pixels:-radius_pixels, kernel_side), ncol = kernel_side)
#   kernel_x <- -t(kernel_y)
#   kernel <- matrix(as.matrix(dist(cbind(as.vector(kernel_x), as.vector(kernel_y))))[as.integer((kernel_side^2) / 2) + 1,], ncol=kernel_side) # calculates distances from kernel center
#   # Set threshold distances to create circular kernel
#   kernel[kernel <= radius_pixels] <- 0
#   kernel[kernel > 0] <- 1
#   kernel <- 1 - kernel  # Invert kernel to mark the area outside the circle
# # I will see if commenting out the above line changes the assignment of suitable/unsuitable
#   # no it did not seem to do anything
#   return(kernel)
# }

#' @describeIn patchMorph.SpatRaster Input is a SpatRaster, and only a single suitability, gap, and spur
#' values is specified, for which the only that outcomes is returned
#' @method patchMorph SpatRaster
#' @export
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
pm.result <- patchMorph.SpatRaster(rast(tester2), buffer = 5, suitThresh = 1, gapThresh = 6, spurThresh = 7, verbose = TRUE)
plot(pm.result, main="PatchMorph Results (Gap-6 & Spur-7)", col = c("#332211", "#FEC44F"))
# this code with these parameters (gap 6, spur 7) produces a map that looks right; min opening size ~= max canopy, and openings can snake through the trees but not to a ridiculous extent (cf Lydersen)

pm.result.single <- patchMorph(data_in = rast(tester2), buffer = 5, suitThresh = 1, gapThresh = 6, spurThresh = 7, verbose = TRUE)
pm.result.single2 <- patchMorph(data_in = rast(tester2), buffer = 5, suitThresh = 1, gapThresh = 2, spurThresh = 10, verbose = TRUE)
par(mfrow=c(1,2))
plot(pm.result.single, main="PatchMorph Results (Gap-6 & Spur-7)", col = c("#332211", "#FEC44F"))
plot(pm.result.single2, main="PatchMorph Results (Gap-2 & Spur-10)", col = c("#332211", "#FEC44F"))

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

# testing nested for loop for area in each bin
nine_df <- data.frame(X=plots_out[[9]]$trees.noedge$x, Y=plots_out[[9]]$trees.noedge$y, 
                      crown=plots_out[[9]]$trees.noedge$crown, bin=plots_out[[9]]$trees.noedge$bin)

nine_df %>% 
  filter(bin==bin_names[2],) %>% 
  st_as_sf(coords = c("X","Y")) %>% 
  st_buffer(dist=nine_df$crown) %>% 
  st_union() %>% 
  st_cast("POLYGON") %>% 
  st_intersection(bound) %>% 
  st_area()

st_area(st_intersection(st_cast(st_union(st_buffer(st_as_sf(clust_area_df[clust_area_df$bin == bin_names[i],], coords = c("X","Y")), dist = clust_area_df[clust_area_df$bin == bin_names[i],]$crown))), bound))

# had an extra one of these floating around: sapply(piebins, as.numeric) 

# leftover from GapFinder approach
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

# math on area in openings, etc

IS_change <- cbind(bins_IS41, bins_IS18)
names(IS_change) <- c("IS_1941", "IS_2018")
IS_change %>% mutate(change = (IS_2018/3) - (IS_1941/3)) %>% 
  mutate(pct_change = change*100/10000)

OH_change <- cbind(bins_OH41, bins_OH18)
names(OH_change) <- c("OH_1941", "OH_2018")
OH_change %>% mutate(change = (OH_2018/3) - (OH_1941/3)) %>% 
  mutate(pct_change = change*100/10000)

# saving a clean copy of the eyeball maps before messing with gaps and colors
ICO_maps <- list()
for (i in 1:length(plots_out)){
  p <- ggplot() +
    geom_sf(data=bound, fill="black") +
    geom_circle(data = as.data.frame(plots_out[[i]][11]), n=20, # #9 is trees; try 11 for trees.noedge
                aes(x0 = trees.noedge.x, y0 = trees.noedge.y, r=trees.noedge.crown, x=trees.noedge.x, y=trees.noedge.y, 
                    fill=factor(trees.noedge.bin), color=factor(trees.noedge.bin), alpha=0.75)) +
    geom_circle(data = as.data.frame(plots_out[[i]][11]), n=20, 
                aes(x0=trees.noedge.x, y0=trees.noedge.y, r=trees.noedge.dbh/200), color="burlywood4", fill="burlywood4") +
    scale_fill_brewer(palette = "YlGn", name = "Cluster Size") +
    scale_colour_brewer(palette = "YlGn", name = "Cluster Size") +
    geom_sf(data=results[[i]], col="white", linewidth = 0.7, fill= "purple", alpha=0.3) +
    guides(size = guide_legend(override.aes = list(color ="burlywood4"))) +
    #theme_classic(base_size=22) +
    theme(plot.title=element_text(hjust=0.5)) +
    labs(title = paste("Living Trees at", names[i], " (1-ha Plot)"),
         x = "Distance in m",
         y = "",
         size = "Crown (m)") +
    guides(size = guide_legend(override.aes = list(color ="#addd8e"))) +
    theme_light()#base_size = 22) )
  # dev.off()
  ICO_maps[[i]] <- p
}

# Old eyeball maps

# df <- as.data.frame(plots_out[[10]][11]) # try list #11 from plots_out, for trees.noedge
# ggplot(df, aes(x0 = trees.x, y0 = trees.y, r = trees.crown)) +
#   ggforce::geom_circle(n = 20) + #5-7x faster than default
#   coord_fixed()
# 
# ggplot(df, aes(x0 = trees.x, y0 = trees.y, r = trees.crown)) +
#   geom_circle(n = 20) + #5-7x faster than default
#   coord_fixed()
# # this seems to give true-to-size crowns; incorporate into color-coded plots
# 
# ggplot(df, aes(x0 = trees.x, y0 = trees.y, r=trees.crown, x=trees.x, y=trees.y)) +
#   # Crowns as green circles with width corresponding to crown diameter
#   geom_circle(n=20, aes(fill=factor(trees.bin), color=factor(trees.bin), alpha=0.85)) +
#   coord_fixed() +
#   # stems as brown circles with width corresponding to dbh (in m)
#   geom_circle(n=20, aes(x0=trees.x, y0=trees.y, r=trees.dbh/200), color="burlywood4", fill="burlywood4") +
#   coord_fixed() +
#   # Set the color palette for cluster sizes
#   scale_fill_brewer(palette = "YlGn", name = "Cluster Size") +
#   scale_colour_brewer(palette = "YlGn", name = "Cluster Size") +
#   # Customize the plot appearance
#   theme_classic(base_size=22) +
#   theme(plot.title=element_text(hjust=0.5)) +
#   labs(title = paste("Living Trees at", names[i], " (1-ha Plot)"),
#        x = "Distance in m",
#        y = "") +
#   guides(size = guide_legend(override.aes = list(color ="burlywood4"))) 
# 
# # add gap polygons
#     # function to create gap polygons -- GapFinder.R
#     # for loop to create gap polygons
# 
# # loop of ggplot with map of true-to-scale crown radius, dbh, and gaps
#   # good map w/ gaps
# ggplot() +
#   geom_sf(data=bound, fill="black") +
#   geom_circle(data = df, n=20, aes(x0 = trees.x, y0 = trees.y, r=trees.crown, x=trees.x, y=trees.y, 
#                                    fill=factor(trees.bin), color=factor(trees.bin), alpha=0.75)) +
#   geom_circle(data = df, n=20, aes(x0=trees.x, y0=trees.y, r=trees.dbh/200), color="burlywood4", 
#               fill="burlywood4") +
#   scale_fill_brewer(palette = "YlGn", name = "Cluster Size") +
#   scale_colour_brewer(palette = "YlGn", name = "Cluster Size") +
#   geom_sf(data=gaps3_noedge, col="white", linewidth = 0.7, fill= "purple", alpha=0.3) +
#   guides(size = guide_legend(override.aes = list(color ="burlywood4"))) +
#   theme_light()
# 
# ggplot() + # this one uses results vector from gapfinder script to call the purple blob geom
#   geom_sf(data=bound, fill="black") +
#   geom_circle(data = df, n=20, aes(x0 = trees.x, y0 = trees.y, r=trees.crown, x=trees.x, y=trees.y, 
#                                    fill=factor(trees.bin), color=factor(trees.bin), alpha=0.75)) +
#   geom_circle(data = df, n=20, aes(x0=trees.x, y0=trees.y, r=trees.dbh/200), color="burlywood4", 
#               fill="burlywood4") +
#   scale_fill_brewer(palette = "YlGn", name = "Cluster Size") +
#   scale_colour_brewer(palette = "YlGn", name = "Cluster Size") +
#   geom_sf(data=results[[10]], col="white", linewidth = 0.7, fill= "purple", alpha=0.3) +
#   guides(size = guide_legend(override.aes = list(color ="burlywood4"))) +
#   theme_light()
# 
# ggplot() + # for this one I am trying out the trees.noedge output from #11 of plots_out[[i]] KEEPER
#   geom_sf(data=bound, fill="black") +
#   geom_circle(data = df, n=20, aes(x0 = trees.noedge.x, y0 = trees.noedge.y, r=trees.noedge.crown, x=trees.noedge.x, y=trees.noedge.y, 
#                                    fill=factor(trees.noedge.bin), color=factor(trees.noedge.bin), alpha=0.75)) +
#   geom_circle(data = df, n=20, aes(x0=trees.noedge.x, y0=trees.noedge.y, r=trees.noedge.dbh/200), color="burlywood4", 
#               fill="burlywood4") +
#   scale_fill_brewer(palette = "YlGn", name = "Cluster Size") +
#   scale_colour_brewer(palette = "YlGn", name = "Cluster Size") +
#   geom_sf(data=results[[10]], col="white", linewidth = 0.7, fill= "purple", alpha=0.3) +
#   guides(size = guide_legend(override.aes = list(color ="burlywood4"))) +
#   theme_light()


# edited for loop: map of true-to-scale crown radius, dbh, and gaps
# 12/15 update, using trees.noedge for mapping purposes
# GOOD FOR AFE 2023
# for (i in 1:length(plots_out)){
#   #jpeg(paste("ICO_GapMap",names[i]),700,630)
#   print(ggplot() +
#           geom_sf(data=bound, fill="black") +
#           geom_circle(data = as.data.frame(plots_out[[i]][11]), n=20, # #9 is trees; try 11 for trees.noedge
#                       aes(x0 = trees.noedge.x, y0 = trees.noedge.y, r=trees.noedge.crown, x=trees.noedge.x, y=trees.noedge.y, 
#                                            fill=factor(trees.noedge.bin), color=factor(trees.noedge.bin), alpha=0.75)) +
#           geom_circle(data = as.data.frame(plots_out[[i]][11]), n=20, 
#                       aes(x0=trees.noedge.x, y0=trees.noedge.y, r=trees.noedge.dbh/200), color="burlywood4", fill="burlywood4") +
#           scale_fill_brewer(palette = "YlGn", name = "Cluster Size") +
#           scale_colour_brewer(palette = "YlGn", name = "Cluster Size") +
#           geom_sf(data=results[[i]], col="white", linewidth = 0.7, fill= "purple", alpha=0.3) +
#           guides(size = guide_legend(override.aes = list(color ="burlywood4"))) +
#           #theme_classic(base_size=22) +
#           theme(plot.title=element_text(hjust=0.5)) +
#           labs(title = paste("Living Trees at", names[i], " (1-ha Plot)"),
#                x = "Distance in m",
#                y = "",
#                size = "Crown (m)") +
#           guides(size = guide_legend(override.aes = list(color ="#addd8e"))) +
#           theme_light())#base_size = 22) )
#  # dev.off()
#   }

# requires a data set that contains columns for x, y, stem_diameter, crown_diameter, 
# and categorical cluster size
# head(treeData3) # dbh, spp, Tree.ID
# head(plot.data)
# #    Site Plot   Date Spec   dbh Dec     X     Y    Z CrHt SrchHt Core. Comments  age_est estab_est
# 
# ICO_out[9]
# #       x    y   dbh  spp Tree.ID    crown         ba        sdi clust.sz cluster.membership   bin
# #dataGPT <- as.data.frame(tree.data_out[9])
# dataGPT <- as.data.frame(ICO_out[9])

# # Create the ggplot map
# ggplot(dataGPT, aes(x = trees.x, y = trees.y)) +
#   # Crowns as green circles with width corresponding to crown diameter
#   geom_point(aes(shape = "Crown", size = trees.crown, color = factor(trees.bin), alpha=0.75)) +
#   # Set the size range for stems and crowns separately
#   geom_point(aes(shape = "Stem", size = .5), color = "burlywood4") +
#     scale_size_continuous(range = c(2,10)) +
#   # Set the shape for stems and crowns separately
#   scale_shape_manual(values = c(16, 16)) +
#   # Set the color palette for cluster sizes
#   scale_colour_brewer(palette = "YlGn", name = "Cluster Size") +
#   # Customize the plot appearance
#   labs(title = "Schematic Map of Trees",
#       # x = "X Coordinate",
#       # y = "Y Coordinate",
#        size = "Crown",
#        size = "Diameter",
#        shape = "",
#        color = "black") +
#   theme_classic()

# for (i in 1:length(plots_out)){
#   #jpeg(paste("ICO_Map",names[i]),700,630)
#   print(ggplot(as.data.frame(plots_out[[i]][9]), aes(x = trees.x, y = trees.y)) +
#     # Crowns as green circles with width corresponding to crown diameter
#     geom_point(aes(size = trees.crown, color = factor(trees.bin)), alpha=0.75) +
#     # Set the size range for stems and crowns separately
#     geom_point(aes(size = .5), color = "burlywood4") +
#     scale_size_continuous(range = c(2,10)) +
#     # Set the shape for stems and crowns separately
#     scale_shape_manual(values = c(16, 16)) +
#     # Set the color palette for cluster sizes
#     scale_colour_brewer(palette = "YlGn", name = "Cluster Size") +
#     # Customize the plot appearance
#       theme_classic(base_size=22) +
#       theme(plot.title=element_text(hjust=0.5)) +
#       labs(title = paste("Living Trees at", names[i], " (1-ha Plot)"),
#          x = "Distance in m",
#          y = "",
#          size = "Crown (m)") +
#     guides(size = guide_legend(override.aes = list(color ="#addd8e")))) 
#   #  dev.off() 
# }

# try to make same map with crown projections at same scale as x axis (in m, i.e.)
#install.packages("ggforce")

# old brms
# Trying brms on model code from cluster_stats

(hist_pines <- ggplot(MLR_df, aes(x = clust.sz)) +
    geom_histogram(colour = "#8B5A00", fill = "#CD8500") +
    theme_bw() +
    ylab("Count\n") +
    xlab("\nTree Cluster Size") +  # latin name for red knot
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain")))  

# try centering clust.sz by subtracting the mean from each observation
MLR_df$clust.sz_ctr <- scale(MLR_df$clust.sz, scale = FALSE)

pines3_ctr <- brms::brm(n ~ clust.sz_ctr*Year + (1|Plot.unique),
                        data = MLR_df, family = negbinomial(), chains = 3,
                        iter = 3000, warmup = 1000)
pairs(pines3_ctr)
summary(pines3_ctr)

library(tidybayes)
#pp_check(pines1_mbrms)

pines3_mbrms <- brms::brm(n ~ clust.sz*Year + (1|Plot.unique),
                          data = MLR_df, family = poisson(), chains = 3,
                          iter = 3000, warmup = 1000)
#plot(pines3_mbrms)
#pp_check(pines3_mbrms)

pines4_mbrms <- brms::brm(n ~ clust.sz + Year + (1|Plot.unique),
                          data = MLR_df, family = poisson(), chains = 3,
                          iter = 3000, warmup = 1000)

loo(pines1_mbrms,pines3_mbrms, pines4_mbrms, compare = TRUE)

(location_fit <- MLR_df %>%
    group_by(Year) %>%
    add_predicted_draws(pines3_mbrms) %>%
    ggplot(aes(x = clust.sz, y = n, color = ordered(Year), fill = ordered(Year))) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50), alpha = 1/4) +
    geom_point(data = MLR_df) +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Dark2") +
    theme_bw() +
    ylab("\nCount") +
    xlab("\nTree Cluster Size") +
    theme_bw() +
    theme(legend.title = element_blank()))

# trying to check out nb model
library(rethinking)
summary(pines3_nb)
precis(pines3_nb, depth = 1, ci = TRUE, corr = TRUE)

(location_fit <- MLR_df %>%
    group_by(Year) %>%
    add_predicted_draws(pines3_nb) %>% # add best model here !!!
    ggplot(aes(x = clust.sz, y = n, color = ordered(Year), fill = ordered(Year))) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50), alpha = 1/4) +
    geom_point(data = MLR_df) +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Dark2") +
    theme_bw() +
    ylab("\nCount") +
    xlab("\nTree Cluster Size") +
    theme_bw() +
    theme(legend.title = element_blank()))

# just for fun, check out clust bin data in a plot
ggplot(bin_tests_df, aes(x=bin, y=n)) +
  geom_point(aes(fill= Year, color = Year)) +
  stat_smooth(aes(group = Year, color=Year), method = "glm", method.args = list(family = "poisson")) +
  scale_color_manual(values=c("red","black"))
# just for EXTRA fun, do the same for clust.sz
ggplot(MLR_df, aes(x=clust.sz, y=n, color = Year)) +
  geom_point(aes(fill= Year, color = Year)) +
  geom_smooth(aes(group = Year, color=Year), method = "glm", method.args = list(family = "poisson")) +
  scale_color_manual(values=c("red","black"))
# okay back to zero fun

# write a model where cluster size frequency is predicted by Year

# prepare dataframe
MLR_ICO <- vector(mode='list',length=length(plots_out))
for(i in 1:length(plots_out)) {
  df <- data.frame(clust.sz = plots_out[[i]]$clusters$size, Site = plots_out[[i]]$plot.name) # 'IS1 in 2018'
  df <- df %>% 
    separate(Site, into = c("Site","Plot","Year"), sep=c(2,7)) %>% 
    mutate(Plot = str_remove(Plot, " in ")) %>% 
    mutate(Plot.unique = paste(Site, Plot))
  MLR_ICO[[i]] <- df
}
library(rlist)
MLR_df <- list.rbind(MLR_ICO)

MLR_df <- MLR_df %>% 
  group_by(Site, Plot.unique, Year, clust.sz) %>% 
  tally()
# MLR_41 <- MLR_df %>% 
#   filter(Year == 1941)
# MLR_18 <- MLR_df %>% 
#   filter(Year == 2018)

# write the model
library(lme4)

# Calculate mean and variance of the counts
mean_n <- mean(MLR_df$n) # 8.27
var_n <- var(MLR_df$n) # 157  variance is greater than the mean
# Estimate dispersion parameter using method of moments
theta_est <- var_n/mean_n
GPT_mod <- glm(n ~ clust.sz + Year, data = MLR_df, family = negative.binomial(theta = 19))
GPT_full <- glm(n ~ clust.sz*Year, data = MLR_df, family = negative.binomial(theta = 19))
GPT_glmer <- glmer.nb(n ~ clust.sz + Year + (1|Plot.unique), data = MLR_df) # isSingular
GPT_glmer_full <- glmer.nb(n ~ clust.sz*Year + (1|Plot.unique), data = MLR_df) # iteration limit reached

# Model with negative binomial distribution and unstructured random effects covariance matrix
model <- glmer.nb(n ~ clust.sz + Year + (1 | Plot.unique), 
                  data = MLR_df, 
                  control = glmerControl(optimizer = "bobyqa"),
                  glmerControl(optimizer = "bobyqa"),
                  nAGQ = 10)  # Adjust nAGQ as needed for convergence
# Error: bad name(s) for start vector (optimizer, restart_edge, boundary.tol, calc.derivs, use.last.params, checkControl, checkConv, optCtrl, tolPwrss, compDev, nAGQ0initStep); should be 'theta' and/or 'fixef')

# Model with negative binomial distribution and unstructured random effects covariance matrix
model <- glmer.nb(n ~ clust.sz + Year + (1 | Plot.unique), 
                  data = MLR_df, 
                  theta = 19,  # Specify initial value for theta
                  control = glmerControl(optimizer = "bobyqa"))
# Error: Error in lme4::glmer(n ~ clust.sz + Year + (1 | Plot.unique), data = MLR_df,  : 
# unused argument (theta = 19)
library(glmmTMB)
model <- glmmTMB(n ~ clust.sz + Year + (1 | Plot.unique), 
                 data = MLR_df, 
                 family = nbinom2,
                 start = list(theta = 19))

# opes_sr <- list()
# for (i in 1:length(plots_out)){
#   opes_sr[i] <- st_cast(filter(st_as_sf(as.polygons(patchMorph.SpatRaster(xy_sr(i), buffer = 5, suitThresh = 1, gapThresh = 6, spurThresh = 7, verbose = FALSE)), values = TRUE), focal_max == 1), "POLYGON")
# } # result should be a set of polygons for each of 12 plots, representing the separate openings
# think this one ^ is broken

# test -- not working bc number of items to replace is not a multiple of replacement length
# plot(opes_sr[[2]], col = "lavender")
# ok! I am not happy with the snakiness of the super gap in e.g. plot 2... but moving on for now

# saving a clean version of patchmorph prep loop before I mess with the merge
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
  thingy <- patchMorph.SpatRaster(xy_sr(i), buffer = 5, suitThresh = 1, gapThresh = 8, spurThresh = 6, verbose = FALSE) %>% 
    as.polygons(values = TRUE) %>% 
    st_as_sf() %>% 
    filter(focal_max ==1) %>% 
    st_cast("POLYGON")
  
  st_crs(thingy) <- "local" # attempt to fix ggplot error "cannot transform sfc object with missing crs"
  
  opes_sr[[i]] <- thingy
} 

# I will calculate their associated areas
for (i in 1:length(plots_out)){
  opes_sr[[i]]$area <- st_area(opes_sr[[i]])
}

# clean copy of summarize.clusters before I mess with bins
summarizeClusters.ppp <- function(pointData, treeData, distThreshold=-1, max.bin=-1,edge.cut = c(0,0,0,0), Quickmap=F,plot.name="Name"){
  # can edge.cut be changed for a circular window???
  if ("crown" %in% colnames(treeData)) (treeData=treeData) else (treeData = Crw.rad.pred(treeData))
  
  if(Quickmap == T) (cluster.list = Quickmap.list(treeData[,"Clump.ID"])) else (cluster.list <- clusterByCrown(pointData, treeData, distThreshold))
  
  cluster.size <- unlist(lapply(cluster.list, length))
  names(cluster.size)<-c()  
  
  # Make sure max bin captures. Also sets max.bin to max cluster size for  default -1 setting
  if(max(cluster.size)>max.bin)  (max.bin = max(cluster.size))
  
  # Per-tree variables
  tree.cluster.membership <- c()
  for(i in 1:length(cluster.list))  tree.cluster.membership[unlist(lapply(cluster.list[i], paste))] <- i
  tree.cluster.membership <- as.matrix(tree.cluster.membership[as.character(sort(as.integer(names(tree.cluster.membership))))])
  tree.dbh <- as.matrix(treeData$dbh)
  tree.ba  <- as.matrix(pi * (tree.dbh / 200)^2)
  tree.sdi <- as.matrix((tree.dbh / 25)^1.7)
  tree.clust.sz = cluster.size[tree.cluster.membership]
  tree.bin = lut(c("1","2-4","5-9","10-15","16-29","30+"),breaks=c(0,2,5,10,16,30,200))(tree.clust.sz)
  tree.bin = factor(tree.bin,levels=c("1","2-4","5-9","10-15","16-29","30+"),order=T)
  rownames(tree.dbh) <- rownames(tree.ba) <- rownames(tree.sdi) <- rownames(tree.cluster.membership)
  
  trees <- cbind(data.frame(x=pointData$x,y=pointData$y),as.data.frame(treeData), data.frame(ba=tree.ba, sdi=tree.sdi, clust.sz=tree.clust.sz ,cluster.membership=tree.cluster.membership,bin=tree.bin))
  
  ### Eliminate edge trees from tree list if a cut distances are provided
  noedge.trees= trees
  # SQUARE WINDOW but I don't think this will show up because edge.cut is not specified? Trying with -60,60
  # x.low = -60 #pointData$window$xrange[1] + edge.cut[1] ; 
  # x.high = 60 #pointData$window$xrange[2]- edge.cut[2]
  # y.low = -60 #pointData$window$yrange[1] + edge.cut[3] ; 
  # y.high = 60 #pointData$window$yrange[2] - edge.cut[4]
  
  ## only adjusts trees if any trees are actually out
  #  cut.index = which(trees$y<y.low | trees$x<x.low | trees$x> x.high | trees$y> y.high)
  # cut.index <- trees %>% 
  #   filter((sqrt(((trees$x)^2)+((trees$y)^2))>(sqrt(10000/pi)-5))) %>% # GREATER THAN FOR TESTING PURPOSE
  #   rownames()
  # # filter doesn't work for this on its own because it reassigns row names 1-n; wrap with %in% condition
  # testfilter <- trees %>% 
  #   filter((sqrt(((trees$x)^2)+((trees$y)^2))>(sqrt(10000/pi)-5)))
  
  edgefilter <- trees %>% 
    filter((sqrt(((trees$x)^2)+((trees$y)^2))>(sqrt(10000/pi)-5))) # 5m buffer from plot edge
  cut.index <- which(trees$x %in% edgefilter$x & trees$y %in% edgefilter$y)
  
  
  if(length(cut.index)>0) {
    trees=  trees [-cut.index,] } 
  
  
  ## reset these vectors for calcs later on
  tree.ba = trees$ba
  tree.dbh = trees$dbh
  tree.sdi = trees$sdi
  tree.clust.sz = trees$clust.sz
  tree.cluster.membership =trees$cluster.membership
  
  # recalc cluster size
  new.cl = unique(tree.cluster.membership)
  cluster.size.orig = cluster.size  # store original
  cluster.size=vector()
  #for (i in 1: length(new.cl)) cluster.size[i] = length(which(tree.cluster.membership==new.cl[i]))
  for (i in 1: length(new.cl)) cluster.size[i] = length(which(noedge.trees$cluster.membership==new.cl[i]))
  
  
  ### Adjust x.max & y.max & reset coords
  pointData.orig = 	pointData
  #  trees$x = trees$x -  min(trees$x) # PREVENT SHIFTING PLOT COORDINATES
  #  trees$y = trees$y - min(trees$y)
  if(nrow(trees) > 0) {
    pointData = as.ppp(cbind(trees$x,trees$y,as.numeric(rownames(trees))), W = disc(radius = sqrt(10000/pi)))
  } else {
    pointData = as.ppp(cbind(trees$x,trees$y,as.numeric(rownames(trees))), W = disc(radius = sqrt(10000/pi)))
  }
  
  # General variables
  n.pts <- sum(cluster.size)
  n.clusts <- length(cluster.size)
  n.bins <- max.bin
  mean.clust.size <- sum(cluster.size^2) / n.pts
  norm.mean.clust.size <- mean.clust.size / n.pts
  hectares <- 1
  
  # Per-cluster variables
  cluster.ba  <- rep(0, n.clusts)
  cluster.sdi <- rep(0, n.clusts)
  cluster.qmd <- rep(0, n.clusts)
  cluster.mndbh <- rep(0, n.clusts)
  cluster.lartr <- rep(0, n.clusts)
  cluster.gini = cluster.bin =cluster.mgf <- rep(0, n.clusts)
  
  ## Need to redo this to pull from noedge.trees
  for(i in 1:n.clusts){
    trees.hd = which(noedge.trees$cluster.membership==new.cl[i])
    cluster.ba[i]  <- round(sum(noedge.trees[trees.hd,"ba"]),3)
    cluster.sdi[i] <- round(sum(noedge.trees[trees.hd,"sdi"]),3)
    cluster.qmd[i] <- round(sqrt(sum(noedge.trees[trees.hd,"dbh"]^2) / cluster.size[i]),1)
    cluster.mndbh[i] <- round(mean(noedge.trees[trees.hd,"dbh"]),1)
    cluster.lartr[i] <- round(max(noedge.trees[trees.hd,"dbh"]),1)
    cluster.gini[i] = round(gini(noedge.trees[trees.hd,"dbh"]),3)
    d.class = lut(c(20,40,60,80,100,180),breaks=c(0,20,40,60,80,100,180))(noedge.trees[trees.hd,"dbh"])  
    cluster.mgf[i] = (length(unique(d.class)) - 1) /log(sum(cluster.ba[i]*10))
    cluster.bin[i] = lut(c("1","2-4","5-9","10-15","16-29","30+"),breaks=c(0,2,5,10,16,30,200))(cluster.size[i])
  }
  # Set factor order
  cluster.bin = factor(cluster.bin,levels=c("1","2-4","5-9","10-15","16-29","30+"),order=T)
  clusters <- data.frame(size=cluster.size, tot.ba=cluster.ba, tot.sdi=cluster.sdi,
                         qmd=cluster.qmd,mn.dbh= cluster.mndbh,dbh.lar=cluster.lartr,gini=cluster.gini,mrglf.indx = cluster.mgf,bin=cluster.bin)
  clusters = clusters[sort.list(clusters[,1]), ]                     
  
  # Per-bin variables
  maxbin.num.tree <- maxbin.num.clust <- rep(0, max(cluster.size))
  for(i in 1:max(cluster.size))
  {
    maxbin.num.clust[i] <- length(which(cluster.size == i))
    maxbin.num.tree[i]  <- maxbin.num.clust[i] * i
  }
  n.bins<-length(maxbin.num.clust)
  
  maxbin.clust.ba  <- rep(0, n.bins)
  maxbin.clust.sdi <- rep(0, n.bins)
  maxbin.clust.qmd <- rep(0, n.bins)
  for(i in 1:n.bins)
  {
    maxbin.clust.ba[i]  <- sum(cluster.ba[which(cluster.size == i)])
    maxbin.clust.sdi[i] <- sum(cluster.sdi[which(cluster.size == i)])
    maxbin.clust.qmd[i] <- sqrt(sum(tree.dbh[which(tree.cluster.membership %in% which(cluster.size == i))]^2) / (i * maxbin.num.clust[i]))
  }
  maxbin.clust.qmd[maxbin.num.clust == 0] <- 0
  
  maxbin.pct.clust <- (maxbin.num.clust / sum(maxbin.num.clust)) 
  maxbin.pct.tree  <- (maxbin.num.tree / sum(maxbin.num.tree)) 
  maxbin.pct.ba    <- (maxbin.clust.ba / sum(maxbin.clust.ba)) 
  maxbin.pct.sdi   <- (maxbin.clust.sdi / sum(maxbin.clust.sdi))
  
  # Trim to length max.bin if needed
  if(max(max.bin)<n.bins) {
    maxbin.num.clust  <- c(maxbin.num.clust[1:(max.bin - 1)], sum(maxbin.num.clust[max.bin:n.bins]))
    maxbin.num.tree   <- c(maxbin.num.tree[1:(max.bin - 1)],  sum(maxbin.num.tree[max.bin:n.bins]))
    maxbin.clust.ba   <- c(maxbin.clust.ba[1:(max.bin - 1)],  sum(maxbin.clust.ba[max.bin:n.bins]))
    maxbin.clust.sdi  <- c(maxbin.clust.sdi[1:(max.bin - 1)], sum(maxbin.clust.sdi[max.bin:n.bins]))
    maxbin.clust.qmd  <- c(maxbin.clust.qmd[1:(max.bin - 1)], sum(maxbin.clust.qmd[max.bin:n.bins]))
    maxbin.pct.clust  <- c(maxbin.pct.clust[1:(max.bin - 1)], sum(maxbin.pct.clust[max.bin:n.bins]))
    maxbin.pct.tree   <- c(maxbin.pct.tree[1:(max.bin - 1)],  sum(maxbin.pct.tree[max.bin:n.bins]))
    maxbin.pct.ba     <- c(maxbin.pct.ba[1:(max.bin - 1)],    sum(maxbin.pct.ba[max.bin:n.bins]))
    maxbin.pct.sdi    <- c(maxbin.pct.sdi[1:(max.bin - 1)],   sum(maxbin.pct.sdi[max.bin:n.bins]))
  }
  
  
  maxbin <- data.frame(num.clust=maxbin.num.clust, num.tree=maxbin.num.tree, tot.ba=maxbin.clust.ba, tot.sdi=maxbin.clust.sdi, qmd=maxbin.clust.qmd,
                       pct.clust=maxbin.pct.clust, pct.tree=maxbin.pct.tree, pct.ba=maxbin.pct.ba, pct.sdi=maxbin.pct.sdi)  
  
  ##Expand to max.bin if needed
  if(max(max.bin)>n.bins){ 
    zeross = matrix(0,(max(max.bin)-n.bins),ncol(maxbin));colnames(zeross)=colnames(maxbin)
    maxbin=rbind(maxbin,zeross) }
  
  ### Binned maxbin table in individuals, small clumps (2-4), med (5-9), & large(10-15) & super 16+
  #maxbin.orig = maxbin #save	
  #add.mat = matrix(0,15-nrow(maxbin),9)
  #colnames(add.mat)=colnames(maxbin)
  #maxbin = rbind(maxbin,add.mat)
  
  
  ## Add to maxbin to get right calcs
  maxbin.orig = maxbin #save
  add.mat = matrix(0,500-nrow(maxbin),9)
  colnames(add.mat)=colnames(maxbin)
  maxbin = rbind(maxbin,add.mat)
  
  pct.tree =	round(c(maxbin[1,7],sum(maxbin[2:4,7]),sum(maxbin[5:9,7]),sum(maxbin[10:15,7]),sum(maxbin[16:29,7]),sum(maxbin[30:nrow(maxbin),7])),2)
  pct.ba =	round(c(maxbin[1,8],sum(maxbin[2:4,8]),sum(maxbin[5:9,8]),sum(maxbin[10:15,8]),sum(maxbin[16:29,8]),sum(maxbin[30:nrow(maxbin),8])),2)
  # Get QMD per clump bin
  ba.bin.sum = c(maxbin[1,3],sum(maxbin[2:4,3]),sum(maxbin[5:9,3]),sum(maxbin[10:15,3]),sum(maxbin[16:29,3]),sum(maxbin[30:nrow(maxbin),3]))
  tpa.bin.sum =  c(maxbin[1,2],sum(maxbin[2:4,2]),sum(maxbin[5:9,2]),sum(maxbin[10:15,2]),sum(maxbin[16:29,2]),sum(maxbin[30:nrow(maxbin),2]))
  qmd.bin =  round((((ba.bin.sum/tpa.bin.sum)/pi)^.5)*200,1)
  
  clump.bins = data.frame(pct.tree,pct.ba,qmd.bin,row.names=c("1","2-4","5-9","10-15","16-29","30+"))
  maxbin = maxbin.orig # reset to orig
  
  
  ### Summary Metrics
  BA = sum(trees$ba)/hectares
  TPH = nrow(trees)/hectares
  Mean.dbh = mean(trees$dbh)
  QMD = (((BA/TPH)/pi)^.5)*200
  SDI = TPH*(QMD/25)^1.7
  sum.met = cbind(BA,TPH,Mean.dbh,QMD,SDI,hectares)
  sum.eng = cbind(BA*4.356,TPH/2.47,Mean.dbh/2.54,QMD/2.54,SDI/2.47,hectares*2.47)
  summary = cbind(sum.met,sum.eng)
  colnames(summary)= c("BAH","TPH","Mean.dbh","QMD","SDI","hectares","BAA","TPA","Mean.dbh","QMD","SDI","Acres")
  
  
  
  return(list(plot.name=plot.name, n.pts=nrow(trees), n.clusts=n.clusts, n.bins=max.bin,  summary=round(summary,1), 
              mean.clust.size=mean.clust.size, norm.mean.clust.size=norm.mean.clust.size,
              points=pointData, trees=trees, points.noedge = pointData.orig , trees.noedge = noedge.trees , 
              clusters=clusters, maxbin=maxbin,clump.bins =clump.bins, edge.cut = edge.cut))
}

par(mfrow=c(1,2))
plot(opes_sr[[11]])
plot(xy_sr(11))

##
library(dplyr)
dplyr::select(plots_out[[1]]$trees.noedge, x, y, bin)

## clean copy of patchMorph (or what I have going on now 2/19/24) before I try it from scratch to see if it fixes gap problem
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
  data_in <- terra::distance(data_in, target = data_in[data_in >= suitThresh])#<= suitThresh])
  
  ## Apply a focal maximum
  data_in <- terra::focal(data_in, gapKernel, fun="max", na.policy="omit", na.rm=TRUE)
  # data_in <- terra::mask(data_in, e.mask)
  # a circular window defined  by the gapKernel is centered around each pixel; the values of all pixels within the focal window thus established are considered. The max pixel value is calculated and then assigned to the central pixel. This process is repeated for every pixel in the raster, resulting in a new raster where each pixel value represents the maximum value within its neighborhood.
  
  if(verbose == TRUE)
    cat("Processing gap threshold diameter:", ncol(gapKernel)-1,"pixels\n")
  ## Reclassify based on the gap threshold
  data_in[data_in <= (ncol(gapKernel)+1)/2] <- 1 # original
  data_in[data_in > (ncol(gapKernel)+1)/2] <- 0
  # data_in[data_in <= (ncol(gapKernel)+1)/2] <- 0 
  # data_in[data_in > (ncol(gapKernel)+1)/2] <- 1 # switched to make openings "suitable"
  
  ## Check to see if there's still non-suitable pixels in the raster, otherwise return data_in
  if( (sum(data_in[terra::values(data_in)==1]) + sum(is.na(terra::values(data_in))) ) == ( nrow(data_in)*ncol(data_in)) ) return(data_in)
  
  ## Get the euclidean distances to non-suitable habitat, and ensure the extent is the same as original
  data_in <- terra::distance(data_in, target = data_in[data_in >= suitThresh]) #<= suitThresh])
  
  ## Apply a focal maximum
  data_in <- terra::focal(data_in, spurKernel, fun="max", na.policy="omit", na.rm=TRUE)
  # data_in <- terra::mask(data_in, e.mask)
  
  if(verbose == TRUE)
    cat("Processing spur threshold diameter:",ncol(spurKernel)-1, "pixels\n")
  ## Reclassify based on the spur threshold
  data_in[data_in <= (ncol(spurKernel)+1)/2] <- 0
  data_in[data_in > (ncol(spurKernel)+1)/2] <- 1
  # data_in[data_in <= (ncol(spurKernel)+1)/2] <- 1
  # data_in[data_in > (ncol(spurKernel)+1)/2] <- 0 # switched to make spurs "unsuitable"
  
  # Crop the raster to the original extent
  data_in <- terra::crop(data_in, e.mask, mask=TRUE)
  
  return(data_in)
}
# test patchMorph.SpatRaster on jester2_sr, which has crowns drawn in with row# as attribute, and notcrowns as ... 1?
jester.pm <- patchMorph.SpatRaster(jester2_sr, buffer = 5, suitThres = 1, gapThresh = 8, spurThresh = 10, verbose = TRUE)
jester.pm2 <- patchMorph.SpatRaster(jester2_sr, buffer = 5, suitThres = 1, gapThresh = 8, spurThresh = 12, verbose = TRUE)
plot(jester.pm, main="PatchMorph Results (Gap-8 & Spur-10), 0s and 1s", col = c("white", "#FEC44F"))
plot(jester.pm2, main="PatchMorph Results (Gap-8 & Spur-12), 0s and 1s", col = c("white", "#FEC44F"))

pm.rast <- patchMorph.SpatRaster(rast(tester2), buffer = 5, suitThresh = 1, gapThresh = 8, spurThresh = 10, verbose = TRUE)
plot(pm.rast, main="PatchMorph Results (Gap-8 & Spur-10)", col = c("white", "#FEC44F"))
par(mfrow=c(1,2))
#plot(pm.rast, main="PatchMorph Results (Gap-8 & Spur-10)", col = c("white", "#FEC44F"))
plot(jester.pm, main="PatchMorph Results (Gap-8 & Spur-10), 0s and 1s", col = c("white", "#FEC44F"))
plot(jester.pm2, main="PatchMorph Results (Gap-8 & Spur-12), 0s and 1s", col = c("white", "#FEC44F"))
# 8 and 12 are my new favorite combo, and !! need to add crowns-as-0s into the for loop for opes_sr!

pm.rast <- patchMorph.SpatRaster(xy_sr(9), buffer = 5, suitThresh = 1, gapThresh = 8, spurThresh = 6, verbose = TRUE)
plot(pm.rast, main="PatchMorph Results (Gap-8 & Spur-6)", col = c("white", "#FEC44F"))
pm.rast2 <- patchMorph.SpatRaster(xy_sr(9), buffer = 5, suitThresh = 1, gapThresh = 8, spurThresh = 12, verbose = TRUE)
plot(pm.rast2, main="PatchMorph Results (Gap-8 & Spur-12)", col = c("white", "#FEC44F"))
plot(c(pm.rast, xy_sr(9)))

# summary tables calcs for 1 plot
# Starting with plot 1 (IS1 in 2018), small clusters (2-4)
nrow(plots_out[[1]]$clusters[plots_out[[1]]$clusters$bin == "2-4",])/n.ha  # 17 small (2-4) clusters = n.clust.bin 
round(sum(plots_out[[1]]$trees.noedge$bin == "2-4")*100/nrow(plots_out[[1]]$trees.noedge), 1) # 41.8% = p.trees.bin
round(mean(plots_out[[1]]$trees.noedge[plots_out[[1]]$trees.noedge$bin =="2-4",]$dbh),1) # 47.1 cm = mDBH.bin
round(sum(plots_out[[1]]$clusters[plots_out[[1]]$clusters$bin == "2-4",]$tot.ba)/n.ha,1) # 10.3 (m2/ha) = ba.bin
round(sum(plots_out[[1]]$trees.noedge$bin == "2-4")/(piebins[1,4]/10000),1)  # 469.3 = tph.bin (for this bin (col 4) in this plot (row 1), in ha)
round(piebins[1,4]*100/(10000*n.ha),1) # 9.8% = p.area.bin

nrow(plots_out[[1]]$trees.noedge)/1 # 110 = TPH
plots_out[[1]]$summary[1]/n.ha # 26.7 m2/ha = BAH
round(mean(plots_out[[1]]$trees.noedge$dbh),1) # 49.8 = meanDBH
round(mean(plots_out[[1]]$clusters$size),1) # 2.2 = tpclust.stand
max(plots_out[[1]]$clusters$size) # 13 = maxtpclust.stand
nrow(opes_sr[[1]])/n.ha # 199 = gaps.ha.stand !! not accurate but this will be how to get it when it is

# summary table calcs scratch
# I'm going to see what happens when I call 1, 3, and 5 in to a new large list
# indices <- c(1,3,5)
# IS2018_out <- lapply(indices, function(i) plots_out[[i]]) # it is a list of 3 plots instead of 12, ok
# #now I can do allcrwndf thing to extract different variables I need from all three 
# 
# View(unlist(lapply(lapply(IS2018_out, function(i) i[[12]]), function(x) x[,1]))) # this should get the cluster list for all 3 at once
# okay year but fuck that. [[ syntax can extract nested elements of a list, so
IS2018_out <- plots_out[c(1,3,5)] # just to make smaller for thinking about it; later I can do all calcs starting from plots_out ?

# for n.clust.bin, I called the 12th element (clusters) of the first element (IS1 in 2018) by plots_out[[1]][[12]]
plots_out[[c(1,12)]]
# what if I want the third column of the 11th element of the 1st element, as in mDBH.bin above
plots_out[[c(1,11)]][,3] # great! 49.8, matches stand-level BAH above
mean(filter(plots_out[[c(1,11)]], bin =="2-4")[,3]) # 47.1, matches mDBH.bin !!

# now what if I want to extract the 12th element (clusters) for multiple elements (try 1 and 3, for IS1 and IS2 in 2018)
selected_clusters <- lapply(c(1, 3), function(i) plots_out[[i]][[12]])

selected_data <- lapply(c(1, 3), function(i) {
  clusters <- plots_out[[i]][[12]]
  size_column <- clusters[, 1]
  return(size_column)
})
unlist(selected_data)


# PatchMorph cleaning before sending to Sanches Meador
#values(tester2)[values(tester2) == 1] <- 0
# for tester2, I want 1s in non-crown areas and 0s in crown areas
#values(tester2)[is.na(values(tester2))] <- 0
# this is backwards and should not work
# values(tester2)[!is.na(values(tester2))] <- 0 # order matters here! can't reassign NAs before next step or all are 1
# values(tester2)[is.na(values(tester2))] <- 1
# it did not work; 0s and 1s are assigned the same as before and shapes are still centered around trees, not openings

#levels(bester2)[[1]]$val # Error in .checkLevels(levels(x)[[1]], value) : 
# new raster attributes (factor values) should be in a data.frame (inside a list)
#levels(bester2) <- data.frame(ID=1)
# if you go from crowns, rather than difference of st_union(crowns), you bring along the ID number and radius associated with each crown. If you rasterize (and then spatRaster - ize) the notcrowns object, it all gets assigned a value of 1

# values(jester2)[is.na(values(jester2))] <- 2 # this just confirmed that the NAs are all in the corners
# pm.rast <- patchMorph(rast(tester2), buffer = 5, suitThresh = 1, gapThresh = 6, spurThresh = 7, verbose = TRUE) # on tester2, now with 0s and 1s

# from summary metrics practice

# over here I am writing the functions to do summary metrics on bin 2-4 in multiple plots at once
# I won't actually run anything here this is just for brain space

# indices <- c(1,3,5)
# 
# # n.clust.bin
# selected_data <- lapply(c(1, 3), function(i) {
#   clusters <- plots_out[[i]][[12]] %>% 
#     filter(bin == "2-4")
#   return(nrow(clusters))
# })
# unlist(selected_data)
# 
# 
# n.ha <- 1
# Starting with plot 1 (IS1 in 2018), small clusters (2-4)
# nrow(filter(plots_out[[1]][[12]], bin =="2-4"))/n.ha # 17 small (2-4) clusters = n.clust.bin 
# round(nrow(filter(plots_out[[1]][[11]], bin =="2-4"))*100/nrow(plots_out[[1]][[11]]),1) # 41.8% = p.trees.bin
# round(mean(filter(plots_out[[1]][[11]], bin =="2-4")[,3]),1) # 47.1 cm = mDBH.bin
# round(sum(filter(plots_out[[1]][[12]], bin =="2-4")[,2]),1) # 10.3 (m2/ha) = ba.bin
# round(nrow(filter(plots_out[[1]][[11]], bin == "2-4"))/(piebins[1,4]/10000),1) # 469.3 = tph.bin (for this bin (col 4) in this plot (row 1), in ha)
# round(piebins[1,4]*100/(10000*n.ha),1) # 9.8% = p.area.bin
# 
# nrow(plots_out[[1]][[11]])/1 # 110 = TPH
# plots_out[[1]][[5]][1]/n.ha # 26.7 m2/ha = BAH
# round(mean(plots_out[[1]][[11]][,3]),1) # 49.8 = meanDBH
# round(mean(plots_out[[1]][[12]][,1]),1) # 2.2 = tpclust.stand
# max(plots_out[[1]][[12]][,1]) # 13 = maxtpclust.stand
# nrow(opes_sr[[1]])/n.ha # 199 = gaps.ha.stand !! not accurate but this will be how to get it when it is

## copy of PatchMorph messing before emailing Andrew S-M
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
# prepare data: one plot (plot 9, OH2 in 2018) for testing
pm_df <- data.frame(X=plots_out[[4]]$trees.noedge$x, Y=plots_out[[4]]$trees.noedge$y, crown=plots_out[[4]]$trees.noedge$crown)
# reproducible example:
Xs <- plots_out[[4]]$trees.noedge$x
# [1] -54.3 -53.6 -52.4 -48.5 -47.0 -45.8 -45.6 -40.6 -39.5 -39.2 -36.1 -35.2 -34.6 -34.1 -30.3 -28.2 -27.7 -26.0 -25.9
# [20] -25.9 -23.1 -21.6 -21.2 -20.5 -17.9 -17.3 -15.9 -15.4 -14.6 -10.0  -5.5  -4.9  -2.3  -1.9   0.3   2.2   4.3   4.7
# [39]   6.0   6.9   8.6   9.1  10.4  13.5  13.9  16.8  22.3  28.1  31.4  36.0  37.7  50.0  50.7   5.2  -3.3  27.4  26.6
# [58]  -2.2  -5.4   0.6 -29.9
Ys <- plots_out[[4]]$trees.noedge$y
# [1]   3.2  -5.1   7.2 -20.5 -25.3 -26.0 -21.3  -0.2  12.7 -34.4 -35.6  36.5  23.5  -9.3  -1.9  35.4  18.0 -37.6 -26.9
# [20] -11.9 -41.0  24.3  12.2  30.0   5.4  32.9  -4.3 -28.8  24.1 -23.0 -55.7  14.4  12.2 -53.1  -7.1 -13.8 -13.2  48.1
# [39]   6.5  35.5  10.4  41.7  16.7  53.4  52.0 -12.8 -30.8 -25.0 -40.9 -22.7  41.5 -21.3  12.3  20.3  23.6  40.1 -10.8
# [58] -32.3 -34.9 -36.0  24.0
Cs <- plots_out[[4]]$trees.noedge$crown
# [1] 1.4116 1.1624 1.2336 3.5476 2.4084 1.7676 2.7644 1.1268 3.7612 1.5540 1.2336 3.3340 1.1980 1.1624 2.2304 2.6220
# [17] 3.6188 1.5540 3.4764 3.8324 1.5896 2.4440 1.1268 3.2272 3.7968 3.4764 3.5476 3.7256 4.1528 2.7644 1.5896 2.8000
# [33] 4.2240 1.9812 4.4732 3.4408 2.8356 1.2336 3.6188 1.4828 3.9392 1.8744 5.1140 3.2984 3.5120 3.9748 3.4408 3.0136
# [49] 4.1528 5.2208 4.0460 2.6932 4.3308 2.9424 1.5896 1.5540 4.0104 2.1236 2.2304 2.6576 1.5540
pm_df <- data.frame(X=Xs, Y=Ys, crown=Cs)

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
pm.rast <- patchMorph(sr_test, buffer = 5, suitThresh = 1, gapThresh = 10, spurThresh = 6, verbose = TRUE) # start with logical gap and spur vals
plot(pm.rast,  main="PatchMorph Results (Gap-10 & Spur-6)", col=c("#FEC44F", "forestgreen")) # now the areas near crowns are 1 and openings are 0!
plot(crowns, col="black", add=TRUE)


# pm.rast <- patchMorph(rast(jester2), buffer = 5, suitThresh = 1, gapThresh = 2, spurThresh = 10, verbose = TRUE) # on jester2, the 0s and 1s guy
# pm.rast <- patchMorph.SpatRaster(rast(jester2), buffer = 5, suitThresh = 1, gapThresh = 8, spurThresh = 10, verbose = TRUE) # on jester2, the 0s and 1s guy

jester.pm <- patchMorph.SpatRaster(jester2_sr, buffer = 5, suitThres = 1, gapThresh = 8, spurThresh = 10, verbose = TRUE)
jester.pm2 <- patchMorph.SpatRaster(jester2_sr, buffer = 5, suitThres = 1, gapThresh = 8, spurThresh = 12, verbose = TRUE)
plot(jester.pm, main="PatchMorph Results (Gap-G & Spur-S), 0s and 1s", col = c("white", "#FEC44F"))
plot(jester.pm2, main="PatchMorph Results (Gap-G & Spur-S), 0s and 1s", col = c("white", "#FEC44F"))

# convert to polygons for area calculations and mapping
pm.vect <- as.polygons(pm.rast, values = TRUE) # turns SpatRaster into SpatVector
pm.sf <- st_as_sf(pm.vect) # turns SpatVector into sf with a multipolygon for 0 and for 1
# take the multipolygon for cells with "1" attribute and turn it into lots of polygons
pm.sf1 <- pm.sf %>% filter(focal_max == 1)
pm.sfs <- st_cast(pm.sf1, "POLYGON")
pm.sfs$area <- st_area(pm.sfs)
sum(pm.sfs$area) # 4672.42 

yescrowns <- st_difference(bound, st_union(notcrowns)) 
bester2 <- rasterize(yescrowns, raster(extent(bound), res=0.1))
values(bester2)[values(bester2) == 1] <- 0

tester2 <- rasterize(notcrowns, raster(extent(bound), res = 0.1)) 
bester2 <- rasterize(crowns, raster(extent(bound), res=0.1))
jester2 <- merge(tester2, bester2)
jester2_sr <- rast(jester2)

#

# fn to convert plot x,y coords to not-crowns spatRaster with local crs [and vals for crown, notcrown, commmented out rn]
xy_sr <- function(plot){ 
  df <- data.frame(X=plots_out[[plot]]$trees.noedge$x, Y=plots_out[[plot]]$trees.noedge$y, crown=plots_out[[plot]]$trees.noedge$crown)
  stems <- st_as_sf(df, coords = c("X", "Y")) #, crs = custom_crs) # points for each tree w/dbh attribute
  crowns = st_buffer(stems, dist = stems$crown) 
  notcrowns <- st_difference(bound, st_union(crowns)) # all area in non-crown space
  #yescrowns <- st_difference(bound, st_union(notcrowns)) # all area in crown space (non-overlapping, values = 1)
  #nc_rastL <- rasterize(notcrowns, raster(extent(bound), res = 0.1)) 
  #yc_rastL <- rasterize(yescrowns, raster(extent(bound), res=0.1))
  #values(yc_rastL)[values(yc_rastL) == 1] <- 0
  #spatRast <- rast(merge(nc_rastL, yc_rastL))
  spatRast <- rast(rasterize(notcrowns, raster(extent(bound), res=0.1)))
  crs(spatRast) <- "local"
  return(spatRast)
}
# test
#plot(xy_sr(9)) # works!

# for loop for openings spatrasters from plots, converted to sf polygons and filtered for "1" attribute

opes_sr <- list()
for (i in 1:length(plots_out)){
  thingy <- patchMorph(xy_sr(i), buffer=5, suitThresh=1, gapThresh=6, spurThresh=7, verbose=FALSE) %>%
    as.polygons(values = TRUE) %>% 
    st_as_sf() %>% 
    filter(focal_max ==1) %>% # this part could be a place I can fix the 1/0 issue
    st_cast("POLYGON")
  
  st_crs(thingy) <- "local" # attempt to fix ggplot error "cannot transform sfc object with missing crs"
  
  opes_sr[[i]] <- thingy
} 

unique(values(opes_sr[[5]]))

# I will calculate their associated areas
for (i in 1:length(plots_out)){
  opes_sr[[i]]$area <- st_area(opes_sr[[i]])
}

# clean copy of summary metrics table function and loop, before messing with sd
# make a function (of indices, bin)

summ.bin <- function(indices, bin.i){
  
  n.clust <- lapply(indices, function(i) { # n.clust.bin
    clusters <- plots_out[[i]][[12]] %>% # for every plot in the list "indices", take the 12th element (clusters)
      filter(bin == bin_names[bin.i]) # filter for bin size "2-4"
    return(nrow(clusters)) }) # how many clusters of this size?
  n.clust.bin <- round(sum(unlist(n.clust))/length(indices),1) # 17.7 small clusters/ha at IS in 2018
  
  p.trees <- lapply(indices, function(i) { # p.trees.bin
    trees.bin <- plots_out[[i]][[11]] %>% # for every plot in the list "indices", take the 11th element (trees.noedge)
      filter(bin == bin_names[bin.i]) # filter for bin size "2-4"
    trees.ne <- plots_out[[i]][[11]] # save all the info in trees.noedge
    return(list(nrow(trees.bin), nrow(trees.ne))) }) # gives a list of the trees in this bin, followed by the total trees, for each plot in (indices)
  p.trees.bin <- round(sum(unlist(lapply(c(1:length(indices)), function(x) p.trees[[x]][[1]])))*100/sum(unlist(lapply(c(1:length(indices)), function(x) p.trees[[x]][[2]]))),1) # 45.8% of trees in "indices" are in bin 2-4
  
  mDBH <- lapply(indices, function(i) { # mDBH.bin
    trees.bin <- plots_out[[i]][[11]] %>% 
      filter(bin == bin_names[bin.i])
    dbh <- trees.bin[,3]
    return(dbh) })
  mDBH.bin <- round(mean(unlist(mDBH), na.rm=TRUE),1) # 51.4 cm mean DBH
  
  ba <- lapply(indices, function(i) { # ba.bin
    clusters <- plots_out[[i]][[12]] %>% 
      filter(bin == bin_names[bin.i])
    ba <- clusters[,2]
    return(ba) })
  ba.bin <- round(sum(unlist(ba)/length(indices)),1) # 13.6 m2/ha
  
  tph <- lapply(indices, function(i) { # tph.bin
    trees.bin <- plots_out[[i]][[11]] %>% 
      filter(bin == bin_names[bin.i])
    return(nrow(trees.bin)) })
  tph.bin <- round(sum(unlist(tph), na.rm=TRUE)/((sum(piebins[indices,(bin.i+2)])/10000)),1) # 515.4
  
  p.area.bin <- round(sum(piebins[indices,(bin.i+2)])*100/(10000*length(indices)),1) # 10.7% p.area.bin
  
  output <- c(n.clust.bin, p.trees.bin, mDBH.bin, ba.bin, tph.bin, p.area.bin)
  
  return(output)
}

# need to loop this function over bin size
# summ.bins <- data.frame(matrix(0,length(bin_names),6))
# for (i in 1:length(bin_names)){
#   summ.bins[i,] <- summ.bin(indices,i)
# }
# this gives a df with 6 columns (one per metric) and 4 rows (one per bin)
# want to run this for all 6 plot groups (IS41, OH41, all41; IS18, OH18, all18)

# actually, try looping over indices and then do once for each bin ?
IS41 <- c(2,4,6)
OH41 <- c(8,10,12)
all41 <- c(IS41, OH41)
IS18 <- c(1,3,5)
OH18 <- c(7,9,11)
all18 <- c(IS18, OH18)

mets_names <- c("n.clust.bin", "p.trees.bin", "mDBH.bin", "ba.bin", "tph.bin", "p.area.bin")

indices_list <- list(IS41, OH41, all41, IS18, OH18, all18)
summ.bins1 <- data.frame(matrix(0,length(indices_list),6))
for (i in 1:length(indices_list)){
  summ.bins1[i,] <- summ.bin(indices_list[[i]],1)
  colnames(summ.bins1) <- paste0(mets_names, 1)
} # now it's a df with 6 rows (one per index / plot grouping) and 6 columns (one per metric)

summ.bins2 <- data.frame(matrix(0,length(indices_list),6))
for (i in 1:length(indices_list)){
  summ.bins2[i,] <- summ.bin(indices_list[[i]],2)
  colnames(summ.bins2) <- paste0(mets_names, 2)
}

summ.bins3 <- data.frame(matrix(0,length(indices_list),6))
for (i in 1:length(indices_list)){
  summ.bins3[i,] <- summ.bin(indices_list[[i]],3)
  colnames(summ.bins3) <- paste0(mets_names, 3)
}

summ.bins4 <- data.frame(matrix(0,length(indices_list),6))
for (i in 1:length(indices_list)){
  summ.bins4[i,] <- summ.bin(indices_list[[i]],4)
  colnames(summ.bins4) <- paste0(mets_names, 4)
}

t(summ.bins1) # gives the per-bin chunk of the results table!! 

summary_table <- rbind(t(summ.bins1), t(summ.bins2), t(summ.bins3), t(summ.bins4))
# summary_table <- replace(summary_table, summary_table == "NaN", "-")

# need to loop this function over bin size
# summ.bins <- data.frame(matrix(0,length(bin_names),6))
# for (i in 1:length(bin_names)){
#   summ.bins[i,] <- summ.bin(indices,i)
# }
# this gives a df with 6 columns (one per metric) and 4 rows (one per bin)
# want to run this for all 6 plot groups (IS41, OH41, all41; IS18, OH18, all18)

# actually, (ended up looping by index and applying to bins manually)

# redone summary metrics as averages per plot, averaged over # plots in (index)
# now do it again for all IS in 2018 (plots 1, 3, 5) -- eventually this will have a loop over "bin", and will be a function of "indices" (vector of plot #s)
indices <- c(1,3,5)
n.ha <- length(indices)

selected_data <- lapply(indices, function(i) { # n.clust.bin
  clusters <- plots_out[[i]][[12]] %>% 
    filter(bin == "2-4")
  return(nrow(clusters)) })
round(sum(unlist(selected_data))/n.ha,1) # 17.7 small clusters at IS in 2018 !! double-check this one for n = 6
round(sd(unlist(selected_data)), 1) # sd = 3.1

selected_data <- lapply(indices, function(i) { # p.trees.bin
  trees.bin <- plots_out[[i]][[11]] %>% 
    filter(bin == "2-4")
  trees.ne <- plots_out[[i]][[11]] 
  return(list(nrow(trees.bin), nrow(trees.ne))) })
round(
  mean(
    (unlist(lapply(c(1:length(indices)), function(x) selected_data[[x]][[1]])))*100/ # take trees.bin per plot, x100
      (unlist(lapply(c(1:length(indices)), function(x) selected_data[[x]][[2]])))) # divide each by all trees per plot
  ,1) # 45.6%
round(
  sd(
    (unlist(lapply(c(1:length(indices)), function(x) selected_data[[x]][[1]])))*100/ # take trees.bin per plot, x100
      (unlist(lapply(c(1:length(indices)), function(x) selected_data[[x]][[2]])))) # divide each by all trees per plot
  ,1) # sd = 4.9

selected_data <- lapply(indices, function(i) { # mDBH.bin
  trees.bin <- plots_out[[i]][[11]] %>% 
    filter(bin == "2-4")
  dbh <- trees.bin[,3]
  return(dbh) })
# round(mean(unlist(selected_data)),1) # 51.4 cm mean DBH
# round(sd(unlist(selected_data)),1) # 30.5 ... not sure why this is so high!
# maybe I should try average of averages approach. OK:
round(mean(unlist(lapply(c(1:length(indices)), function(x) mean(selected_data[[x]])))),1) # 51.5
round(sd(unlist(lapply(c(1:length(indices)), function(x) mean(selected_data[[x]])))),1) # 6.2

selected_data <- lapply(indices, function(i) { # ba.bin
  clusters <- plots_out[[i]][[12]] %>% 
    filter(bin == "2-4")
  ba <- clusters[,2]
  return(ba) })
round(sum(unlist(selected_data)/n.ha),1) # 13.6 m2/ha
# redo as average of averages
round(mean(unlist(lapply(c(1:length(indices)), function(x) sum(selected_data[[x]])))),1) # 13.6 m2/ha
round(sd(unlist(lapply(c(1:length(indices)), function(x) sum(selected_data[[x]])))),1) # sd = 3.0

selected_data <- lapply(indices, function(i) { # tph.bin
  trees.bin <- plots_out[[i]][[11]] %>% 
    filter(bin == "2-4")
  return(nrow(trees.bin)) })
round(sum(unlist(selected_data))/((sum(piebins[indices,4])/10000)),1) # 515.4
# redo as average of averages
round(mean(unlist(selected_data)/((piebins[indices,4])/10000)),1) # 536.1
round(sd(unlist(selected_data)/((piebins[indices,4])/10000)),1) # sd = 123.5

round(sum(piebins[indices,4]*100)/(10000*n.ha),1) # 10.7% p.area.bin
# redo as average of averages
round(mean(piebins[indices,4]*100/(10000)),1) # 10.7% p.area.bin
round(sd(piebins[indices,4]*100/(10000)),1) # sd = 3.5


# copy of summ.bin function with avg of avgs, sd

summ.bin <- function(indices, bin.i){
  
  n.clust <- lapply(indices, function(i) { # n.clust.bin
    clusters <- plots_out[[i]][[12]] %>% # for every plot in the list "indices", take the 12th element (clusters)
      filter(bin == bin_names[bin.i]) # filter for bin size
    return(nrow(clusters)) }) # how many clusters of this size?
  n.clust.bin <- round(mean(unlist(n.clust)),1) # 17.7 small clusters/ha at IS in 2018
  n.clust.bin.sd <- round(sd(unlist(n.clust)),1) # sd
  
  p.trees <- lapply(indices, function(i) { # p.trees.bin
    trees.bin <- plots_out[[i]][[11]] %>% # for every plot in the list "indices", take the 11th element (trees.noedge)
      filter(bin == bin_names[bin.i]) # filter for bin size 
    trees.ne <- plots_out[[i]][[11]] # save all the info in trees.noedge
    return(list(nrow(trees.bin), nrow(trees.ne))) }) # gives a list of the trees in this bin, followed by the total trees, for each plot in (indices)
  p.trees.bin <- round(  mean(  (unlist(lapply(c(1:length(indices)), function(x) p.trees[[x]][[1]])))*100/
                                  (unlist(lapply(c(1:length(indices)), function(x) p.trees[[x]][[2]])))  ), 1) # avg % of trees/plot for this bin
  p.trees.bin.sd <- round(  sd(  (unlist(lapply(c(1:length(indices)), function(x) p.trees[[x]][[1]])))*100/
                                   (unlist(lapply(c(1:length(indices)), function(x) p.trees[[x]][[2]])))  ), 1) # sd of % trees/plot for this bin
  
  mDBH <- lapply(indices, function(i) { # mDBH.bin
    trees.bin <- plots_out[[i]][[11]] %>% 
      filter(bin == bin_names[bin.i])
    dbh <- trees.bin[,3]
    return(dbh) })
  mDBH.bin <- round(mean(unlist(lapply(c(1:length(indices)), function(x) mean(mDBH[[x]])))),1) # 51.4 cm mean DBH
  mDBH.bin.sd <- round(sd(unlist(lapply(c(1:length(indices)), function(x) mean(mDBH[[x]])))),1)
  
  ba <- lapply(indices, function(i) { # ba.bin
    clusters <- plots_out[[i]][[12]] %>% 
      filter(bin == bin_names[bin.i])
    ba <- clusters[,2]
    return(ba) })
  ba.bin <- round(mean(unlist(lapply(c(1:length(indices)), function(x) sum(ba[[x]])))),1) # 13.6 m2/ha
  ba.bin.sd <- round(sd(unlist(lapply(c(1:length(indices)), function(x) sum(ba[[x]])))),1) # sd = 3.0
  
  tph <- lapply(indices, function(i) { # tph.bin
    trees.bin <- plots_out[[i]][[11]] %>% 
      filter(bin == bin_names[bin.i])
    return(nrow(trees.bin)) })
  tph.bin <- round(mean(unlist(tph)/((piebins[indices,4])/10000)),1) # 536.1
  tph.bin.sd <- round(sd(unlist(tph)/((piebins[indices,4])/10000)),1) # sd = 123.5
  
  p.area.bin <- round(mean(piebins[indices,(bin.i+2)]*100/(10000)),1) # 10.7% p.area.bin
  p.area.bin.sd <- round(sd(piebins[indices,(bin.i+2)]*100/(10000)),1) # sd = 3.5
  
  # output <- c(n.clust.bin,  p.trees.bin,  mDBH.bin,  ba.bin,  tph.bin,  p.area.bin, 
  #             n.clust.bin.sd, p.trees.bin.sd, mDBH.bin.sd, ba.bin.sd, tph.bin.sd, p.area.bin.sd)
  output <- c(n.clust.bin,  p.trees.bin,  mDBH.bin,  ba.bin,  tph.bin,  p.area.bin, 
              paste0("(", c(n.clust.bin.sd, p.trees.bin.sd, mDBH.bin.sd, ba.bin.sd, tph.bin.sd, p.area.bin.sd),")") ) 
  
  return(output)
}

# Derek C's summary tables
##################################################################################
##
##      Code to process generic stemmap plots
##   		March 2017  Derek Churchill
##  
###     Produces ICO tables & sets up for plotting etc. 
### 
################################################################################


# need to run these functions
#source("Plotkin_Cluster_Crown.R")
# PK: fns from EastPinesICO_edgecorr

### Runs a set of stemmaped plots

##  vector with plot names
# All.plots
# PK: names

# List with a dataframe for each plot x,y,dbh, species, plus clumpID if Quickmap
# All.list.cor
# PK: plots

# vector of T or F for each plot:  T if plot is a quickmap,  F if its a traditional stemmap
#QM = ("F","F",..............)


# matrix with 4 edge cut values for each plot. Edge cut is the buffer you want to come in off the edges. See Churchill 2013 for more on this. 

#edge.ct = matrix (0,length(All.plots),4) # use this if no edge cut
# PK: don't need, only appears within summarizeClusters below

#############################################
###  Run Stemmaps & Quickmaps through cluster function 
# Stem.res = list()
# summary.mets = matrix(0,length(All.plots),12)
# ICO.tr.pct = ICO.ba.pct = matrix(0,length(All.plots),200) 
# can.cov = vector()
# 
# for (w in 1:length(All.plots)){  
#   # Prep input for cluster function & run through function 
#   in.plot = All.list.cor[[w]]
#   
#   clust.in = formatSummaryInput(in.plot, x.min=0, x.max=max(in.plot[,1]), y.min=0, y.max=max(in.plot[,2]), dbh.units="cm")
#   #edge.cut = as.numeric(edge.ct[1,c(6:9)]) ## non-buffer tree method, boundaries are extent of non-buf trees
#   edge.cut = as.numeric(edge.ct[w,c(1:4)]) ## buffer tree method, bndries are extent of buffer trees
#   
#   # Calc canopy cover
#   #can.cov[w] = can.cover.calc(clust.in$pointData,clust.in$treeData,edge.cut=edge.cut)
#   
#   # Run through cluster function
#   Stem.res[[w]] = summarizeClusters.ppp (clust.in$pointData, clust.in$treeData, distThreshold=6, max.bin=-1,edge.cut =edge.cut, Quickmap = QM[w])
# }  
# PK: don't need this, ran cluster function in EastPinesICO_edgecorr


## Sum up and store results
#Plot.names=All.plots
Plot.names=names
n.plots = length(Plot.names)

gap.hold=list()

F.breaks = prop.open = vector()
F.curve = matrix(0,n.plots,1000)
F.bins =  matrix(0,n.plots,9)

nn.hist = matrix(0,n.plots,60)
Mean.clust = vector()
summary.mets = matrix(0,length(Plot.names),12)
ICO.tr.pct = ICO.ba.pct = matrix(0,length(Plot.names),700) 
Clump.bins = as.data.frame(matrix(0,n.plots,18))
gap.size = as.data.frame(matrix(0,n.plots,7))

##
for (w in 1:n.plots)  {
  #Clump metrics
  print(w)
  
  summary.mets[w,] = plots_out[[w]]$summary
  ICO.tr.pct[w,] =  c(plots_out[[w]]$maxbin[,7],rep(0,700-length(plots_out[[w]]$maxbin[,7]))) 
  ICO.ba.pct[w,] =  c(plots_out[[w]]$maxbin[,8],rep(0,700-length(plots_out[[w]]$maxbin[,8])))
  Mean.clust[w]= plots_out[[w]]$mean.clust.size 
  Clump.bins[w,] = c(plots_out[[w]]$clump.bins[,1],plots_out[[w]]$clump.bins[,2],plots_out[[w]]$clump.bins[,3])
  nn.hist[w,] = hist(nndist(plots_out[[w]]$pointData)*3.28084,plot=F,breaks=seq(0,300,5))$counts/sum(hist(nndist(plots_out[[w]]$pointData)*3.28084,plot=F,breaks=seq(0,300,5))$counts)
  # 1m = 3.28084ft  
  
  # ##  Run F test & openings
  F.hold = open.DistM.calc(plots_out[[w]]$points.noedge,Plot.name = Plot.names[w] ,eps = .5,f.break=9)
  F.bins[w,] = F.hold$F.bin[,2]
  F.curve[w,] = c(F.hold$F.cml[,2],rep(0,1000-length(F.hold$F.cml[,2])))
  F.breaks[w]= F.hold$f.break
  
  # Run openings
  # gap.hold[[w]] =gap.quant(plots_out[[w]]$points.noedge,name=Plot.names[w],edge.buf=6,gap.thresh = 9, open.breaks=c(0.04, 0.08, 0.2, 0.4, 0.8, 1.6, 100))
  # prop.open[w]= gap.hold[[w]][[5]]$Prop.open
  # gap.size[w,] = gap.hold[[w]][[2]]
}


## Set up results tables

colnames(summary.mets)=colnames(plots_out[[w]]$summary)

# # If run openings
# summary.mets=data.frame(summary.mets,prop.open)
# colnames(summary.mets)=c(colnames(plots_out[[w]]$summary),"PropOpen")
# colnames(gap.size)=c(0.04, 0.08, 0.2, 0.4, 0.8, 1.6, 100)
# rownames(gap.size)=Plot.names

#
Pattern.df = data.frame(Plot.names,TPA=summary.mets[,"TPA"],Mean.clust,F.breaks)
rownames(summary.mets)=Plot.names
rownames(ICO.tr.pct)=Plot.names
rownames(ICO.ba.pct)=Plot.names

colnames(F.bins) = c(3,6,   9,   12,  15,  18,  21,  24,  "24+")
rownames(F.bins) = Plot.names
colnames(F.curve) = seq(0,99.9,0.1)
rownames(F.curve) = Plot.names
rownames(nn.hist) = Plot.names
rownames(Clump.bins) = Plot.names

colnames(Clump.bins) = rep(rownames(plots_out[[w]]$clump.bins),3)


# 
# # Store
place.name = "EastSide"
#setwd("C:/Users/Derek/Dropbox/ICO/Reference_sites/aaReference summary data/Blues_envelopes")

# write.csv(F.curve,paste(place.name,"_FCurves.csv",sep=""))
# write.csv(F.bins,paste(place.name,"_FBins.csv",sep=""))
# write.csv(summary.mets,paste(place.name,"_summary_metrics.csv",sep=""))
# write.csv(ICO.tr.pct,paste(place.name,"_Cluster_tpa_GT15_6m.csv",sep=""))
# write.csv(ICO.ba.pct,paste(place.name,"_Cluster_ba_GT15_6m.csv",sep=""))
# write.csv(Pattern.df,paste(place.name,"_TPA_AvgClus_F9.csv",sep=""))
# write.csv(Clump.bins,paste(place.name,"_ClumpBins.csv",sep=""))
# # write.csv(gap.size,paste(place.name,"_gapSize.csv",sep=""))  

# _____________________ #
# Practice with summary tables

n.ha <- 1
# Starting with plot 1 (IS1 in 2018), small clusters (2-4)
nrow(filter(plots_out[[1]][[12]], bin =="2-4"))/n.ha # 17 small (2-4) clusters = n.clust.bin 
round(nrow(filter(plots_out[[1]][[11]], bin =="2-4"))*100/nrow(plots_out[[1]][[11]]),1) # 41.8% = p.trees.bin
round(mean(filter(plots_out[[1]][[11]], bin =="2-4")[,3]),1) # 47.1 cm = mDBH.bin
round(sum(filter(plots_out[[1]][[12]], bin =="2-4")[,2]),1) # 10.3 (m2/ha) = ba.bin
round(nrow(filter(plots_out[[1]][[11]], bin == "2-4"))/(piebins[1,4]/10000),1) # 469.3 = tph.bin (for this bin (col 4) in this plot (row 1), in ha)
round(piebins[1,4]*100/(10000*n.ha),1) # 9.8% = p.area.bin

nrow(plots_out[[1]][[11]])/1 # 110 = TPH
plots_out[[1]][[5]][1]/n.ha # 26.7 m2/ha = BAH
round(mean(plots_out[[1]][[11]][,3]),1) # 49.8 = meanDBH
round(mean(plots_out[[1]][[12]][,1]),1) # 2.2 = tpclust.stand
max(plots_out[[1]][[12]][,1]) # 13 = maxtpclust.stand
nrow(opes_sr[[1]])/n.ha # 7 = gaps.ha.stand !! not accurate but this will be how to get it when it is


# now do it again for all IS in 2018 (plots 1, 3, 5) -- eventually this will have a loop over "bin", and will be a function of "indices" (vector of plot #s)
indices <- c(1,3,5)
n.ha <- length(indices)

selected_data <- lapply(indices, function(i) { # n.clust.bin
  clusters <- plots_out[[i]][[12]] %>% 
    filter(bin == "2-4")
  return(nrow(clusters)) })
round(mean(unlist(selected_data)),1) # 17.7 small clusters at IS in 2018 !! double-check this one for n = 6
round(sd(unlist(selected_data)), 1) # sd = 3.1

selected_data <- lapply(indices, function(i) { # p.trees.bin
  trees.bin <- plots_out[[i]][[11]] %>% 
    filter(bin == "2-4")
  trees.ne <- plots_out[[i]][[11]] 
  return(list(nrow(trees.bin), nrow(trees.ne))) })
round(
  mean(
    (unlist(lapply(c(1:length(indices)), function(x) selected_data[[x]][[1]])))*100/ # take trees.bin per plot, x100
      (unlist(lapply(c(1:length(indices)), function(x) selected_data[[x]][[2]])))) # divide each by all trees per plot
  ,1) # 45.6%
round(
  sd(
    (unlist(lapply(c(1:length(indices)), function(x) selected_data[[x]][[1]])))*100/ # take trees.bin per plot, x100
      (unlist(lapply(c(1:length(indices)), function(x) selected_data[[x]][[2]])))) # divide each by all trees per plot
  ,1) # sd = 4.9

selected_data <- lapply(indices, function(i) { # mDBH.bin
  trees.bin <- plots_out[[i]][[11]] %>% 
    filter(bin == "2-4")
  dbh <- trees.bin[,3]
  return(dbh) })
# round(mean(unlist(selected_data)),1) # 51.4 cm mean DBH
# round(sd(unlist(selected_data)),1) # 30.5 ... not sure why this is so high!
# maybe I should try average of averages approach. OK:
round(mean(unlist(lapply(c(1:length(indices)), function(x) mean(selected_data[[x]])))),1) # 51.5
round(sd(unlist(lapply(c(1:length(indices)), function(x) mean(selected_data[[x]])))),1) # 6.2

selected_data <- lapply(indices, function(i) { # ba.bin
  clusters <- plots_out[[i]][[12]] %>% 
    filter(bin == "2-4")
  ba <- clusters[,2]
  return(ba) })
round(sum(unlist(selected_data)/n.ha),1) # 13.6 m2/ha
# redo as average of averages
round(mean(unlist(lapply(c(1:length(indices)), function(x) sum(selected_data[[x]])))),1) # 13.6 m2/ha
round(sd(unlist(lapply(c(1:length(indices)), function(x) sum(selected_data[[x]])))),1) # sd = 3.0

selected_data <- lapply(indices, function(i) { # tph.bin
  trees.bin <- plots_out[[i]][[11]] %>% 
    filter(bin == "2-4")
  return(nrow(trees.bin)) })
round(sum(unlist(selected_data))/((sum(piebins[indices,4])/10000)),1) # 515.4
# redo as average of averages
round(mean(unlist(selected_data)/((piebins[indices,4])/10000)),1) # 536.1
round(sd(unlist(selected_data)/((piebins[indices,4])/10000)),1) # sd = 123.5

round(sum(piebins[indices,4]*100)/(10000*n.ha),1) # 10.7% p.area.bin
# redo as average of averages
round(mean(piebins[indices,4]*100/(10000)),1) # 10.7% p.area.bin
round(sd(piebins[indices,4]*100/(10000)),1) # sd = 3.5


# save all the functions above as output names
# generalize anywhere that calls "2-4" to call bin_names[i]
# write a function that returns a vector of those outputs for each bin size
# repeat these steps for stand-level metrics

indices <- c(1,3,5)
n.ha <- length(indices)

# fixing core-based ages

# IS_coredtrees <- IS_livetrees[!is.na(IS_livetrees$Core.),]
# IS_coredtrees <- IS_coredtrees %>% 
#   mutate(Code = paste0(Plot, Core.))

# IS_livetrees <- IS_livetrees %>%
#   mutate(age_est = predict(IS_exp, newdata = IS_livetrees)) %>% 
#   mutate(estab_est = round(2018 - age_est,0))

# IS_livetrees <- IS_livetrees %>%
#   mutate(age_est = 
#            case_when(Code %in% IS_correction$series.1 ~ 69,  #IS_correction$corrected_age,
#                      TRUE ~ predict(IS_exp, newdata = IS_livetrees)) ) # %>% 
#            mutate(estab_est = round(2018 - age_est,0)))

# IS_livetrees <- IS_livetrees %>%
#   mutate(age_est = 
#            IS_correction[IS_correction$series.1 == Code, 22])

IS_livetrees$age_est <- 0
IS_livetrees[IS_livetrees$Code =="IS110",]$age_est <- IS_correction[IS_correction$Code == "IS110", 22]


#IS_livetrees$age_est <- 0
# IS_livetrees[IS_livetrees$Code =="IS110",]$age_est <- IS_correction[IS_correction$Code == "IS110", 22]
#IS_correction[IS_correction$series.1 == "IS110", 22]
# [1] 115.9594

IS_livetrees <- IS_livetrees %>% 
  mutate(Code = paste0(Plot, Core.))
IS_correction$Code <- gsub("B","", IS_correction$series) # remove all the Bs
sum(IS_livetrees$Code %in% IS_correction$Code) # 68

allcodes <- IS_livetrees$Code
goodcodes <- allcodes[c(which(IS_livetrees$Code %in% IS_correction$Code))]

# now 68 livetrees have a Code I can look up in the IS_correction df to pull corrected_age (and reassign to age_est)

IS_livetrees <- IS_livetrees %>%
  mutate(age_est = predict(IS_exp, newdata = IS_livetrees)) 
# !! need to re-assign age_est with the core-based ages of the trees that we cored
for(i in 1:length(goodcodes)){
  IS_livetrees[IS_livetrees$Code == goodcodes[i],]$age_est <- IS_correction[IS_correction$Code == goodcodes[i], 22]
}

IS_livetrees <- IS_livetrees %>%
  mutate(estab_est = round(2018 - age_est,0))

# clean copy of OH_livetrees before messing with species-specific core age forcing
OH_livetrees <- OH_livetrees %>%
  mutate(PIJE_est = predict(OH_lm, newdata = OH_livetrees)) %>% 
  mutate(JUGR_est = 39.9*log(dbh)+24.2) %>% 
  mutate(ABCO_est = predict(ABCO_lm, newdata = OH_livetrees)) %>% 
  mutate(PICO_est = predict(PICO_lm, newdata = OH_livetrees)) %>% 
  mutate(age_est = case_when(Spec=="PIJE" ~ PIJE_est,
                             Spec=="JUGR" ~ JUGR_est,
                             Spec=="ABCO" ~ ABCO_est,
                             Spec=="PICO" ~ PICO_est)) %>% 
  mutate(estab_est = round(2018 - age_est,0))

# PatchMorph testing reproducible code
# prepare data: one plot (plot 4, IS3 in 1941) for testing
## checking plot 9 (OH2 in 2018) because of the error at PM result stage
pm_df <- data.frame(X=plots_out[[4]]$trees.noedge$x, Y=plots_out[[4]]$trees.noedge$y, crown=plots_out[[4]]$trees.noedge$crown)
#pm_df <- data.frame(X=plots_out[[9]]$trees.noedge$x, Y=plots_out[[9]]$trees.noedge$y, crown=plots_out[[9]]$trees.noedge$crown)
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

#r_notcrowns <- rasterize(notcrowns, raster(extent(bound), res = 1)) 
r_notcrowns <- rasterize(notcrowns, rast(ext(bound), res = 0.1)) # AJSM edit

#r_notcrowns <- rasterize(notcrowns, raster(extent(bound), res = 0.1)) # turns sf into a rasterlayer -- this one is for all non-crown areas
# want now to make a raster with 1s for not-crown and 0s for crown
#r_yescrowns <- rasterize(yescrowns, raster(extent(bound), res=1))
# r_yescrowns <- rasterize(yescrowns, raster(extent(bound), res=0.1)) # sf --> rasterlayer, this time for all crown areas
r_yescrowns <- rasterize(yescrowns, rast(ext(bound), res = 0.1)) # AJSM edit

values(r_yescrowns)[values(r_yescrowns) == 1] <- 0 # reassign all values in crown areas to 0 ("unsuitable")

r_test <- merge(r_notcrowns, r_yescrowns) # creates one rasterlayer with 0s in crown areas, 1s everywhere else
#  sr_test <- rast(r_test) # makes into a spatRaster 
#crs(sr_test) <- "local" # set crs to Cartesian plane in meters
crs(r_test) <- "local" # set crs to Cartesian plane in meters
#r_test <- 1-r_test

plot(r_test, col=c("white", "#FEC44F"), main="Starting SpatRaster") # 0s in crowns (white), 1s everywhere else (yellow)
pm.rast <- patchMorph(r_test, buffer = 5, suitThresh = 1, gapThresh = 6, spurThresh = 6, verbose = TRUE) # 
plot(pm.rast, col=c("#FEC44F", "forestgreen")) # now the areas near crowns are 1 and openings are 0! ## I did not get this on the most recent run but I don't think I've changed anything....except I was using plot 9! also focus for kernel looks like crowns now?
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


# step A: add opes_sr polygons to ICO maps # check!

# step B: add opes_sr summed areas to pie charts (one per site per year = 4)

# step C: make gap size distribution by year. Start with this one
# opes_sr has 12 lists containing 1-10 polygons and their associated areas
# break these out by (site and) year
# names
# [1] "IS1 in 2018" "IS1 in 1941" "IS2 in 2018" "IS2 in 1941" "IS3 in 2018" "IS3 in 1941" "OH1 in 2018" "OH1 in 1941"
# [9] "OH2 in 2018" "OH2 in 1941" "OH3 in 2018" "OH3 in 1941"
# odds are 2018, evens are 1941

# OH_2009
# Size in 2009, 2y after O'Harrell Fire
# need to account for trees that were dead at the time, i.e. all logs Dec 1,2,3 were snags in 2009, logs Dec 4,5 were logs in 2009 (all snags were live trees in 2006)

OH_trees2009 <- OH_trees %>% 
  mutate(age2009 = 2009 - estab_est) %>% 
  mutate(PIJE_2009 = (age2009 - 26.2246)/3.4328) %>% 
  mutate(JUGR_2009 = exp((age2009 - 24.2)/39.9)) %>% 
  mutate(ABCO_2009 = (age2009-65.8335)/0.7771) %>% 
  mutate(PICO_2009 = (age2009-64.0018)/1.1501) %>% 
  mutate(dbh2009 = case_when(Spec=="PIJE" ~ PIJE_2009,
                             Spec=="JUGR" ~ JUGR_2009,
                             Spec=="ABCO" ~ ABCO_2009,
                             Spec=="PICO" ~ PICO_2009))  %>% 
  # need to filter out rows with trees that have NaN or <5 DBH
  filter(!is.na(dbh2009)) %>% # 
  filter(dbh2009>=5)

OH_snags2009 <- OH_trees2009 %>% 
  filter(dec_correction > 9 & dec_correction < 20) # any logs with Dec 1,2,3 were 2009 snags (so dec_correction 10-15y)

OH_trees2009 <- OH_trees2009 %>% 
  filter(dec_correction < 9) # remove trees dead before O'Harrell Fire (snags Dec >1), but keep all live trees as of 2009
#______________________________________________________________________________#

# Prepping all OH sites in 2009
OH1_2009 <- OH_trees2009[OH_trees2009$Plot == "OH1",]
OH2_2009 <- OH_trees2009[OH_trees2009$Plot == "OH2",]
OH3_2009 <- OH_trees2009[OH_trees2009$Plot == "OH3",]
# Need to reassign dbh to 2009 value
OH1_2009 <- rename(OH1_2009, "dbh2018" = "dbh", "dbh" = "dbh2009")
OH2_2009 <- rename(OH2_2009, "dbh2018" = "dbh", "dbh" = "dbh2009")
OH3_2009 <- rename(OH3_2009, "dbh2018" = "dbh", "dbh" = "dbh2009")


# non-pooled TPH 

t.test(dotplots_ISn$TPH.2, dotplots_ISn$TPH) # 1995 v 1941
# t = 0.050903, df = 2.2087, p-value = 0.9636   no sig diff in TPH at IS
t.test(dotplots_OHn$TPH.1, dotplots_OHn$TPH)
# 1.8685, df = 2.0772, p-value = 0.1979    no sig diff in TPH at OH
t.test(c(dotplots_ISn$TPH.1, dotplots_OHn$TPH.1), c(dotplots_ISn$TPH, dotplots_OHn$TPH))
# t = 1.1292, df = 8.5435, p-value = 0.2895; 20.0 ( -20.39653  60.39653 95% CI) *no change* in TPH at p<0.05 level

t.test(dotplots_ISn$BAH.1, dotplots_ISn$BAH)
# t = 7.8478, df = 2.7517, p-value = 0.005828   sig diff in BAH at IS; 2018 value is higher
t.test(dotplots_OHn$BAH.1, dotplots_OHn$BAH)
# t = 2.6861, df = 2.3998, p-value = 0.09491    almost sig diff in BAH at OH; 2018 value is higher
# t.test(c(dotplots_ISn$BAH.1, dotplots_OHn$BH.1), c(dotplots_ISn$baH, dotplots_OHn$BAH))
t.test(c(dotplots_ISn$BAH.1, dotplots_OHn$BAH.1), c(dotplots_ISn$BAH, dotplots_OHn$BAH))
# t = 3.8218, df = 6.7058, p-value = 0.00708; 9.0 (3.387807 14.645526) difference is significant; 2018 BAH is *higher*

###

bin_tests <- vector(mode='list',length=length(plots_out))
for(i in 1:length(plots_out)) {
  df <- data.frame(bin = plots_out[[i]]$clusters$bin, Site = plots_out[[i]]$plot.name) # 'IS1 in 2018'
  df <- df %>% 
    separate(Site, into = c("Site","Plot","Year"), sep=c(2,7)) %>% 
    mutate(Plot = str_remove(Plot, " in "))
  bin_tests[[i]] <- df
}
bin_tests_df <- list.rbind(bin_tests)

## FOR 1941 V PRE-FIRE
# singletons
fisher.test(matrix(c(sum(bin_tests_41[1,3]), sum(bin_tests_41[-1,3]), 
                     sum(bin_tests_prefire[1,3]), sum(bin_tests_prefire[-1,3])), byrow=TRUE, 2, 2))
# p-value = 1.949e-06

# 2-4
fisher.test(matrix(c(sum(bin_tests_41[2,3]), sum(bin_tests_41[-2,3]), 
                     sum(bin_tests_prefire[2,3]), sum(bin_tests_prefire[-2,3])), byrow=TRUE, 2, 2))
# p-value = 0.01042

# 5-9
fisher.test(matrix(c(sum(bin_tests_41[3,3]), sum(bin_tests_41[-3,3]), 
                     sum(bin_tests_prefire[3,3]), sum(bin_tests_prefire[-3,3])), byrow=TRUE, 2, 2))
# p-value = 7.481e-06

# 10+
fisher.test(matrix(c(sum(bin_tests_41[4,3], na.rm = TRUE), sum(bin_tests_41[,3]), sum(bin_tests_prefire[4,3]), sum(bin_tests_prefire[-4,3])), byrow=TRUE, 2, 2))
#fisher.test(matrix(c(0, sum(bin_tests_41[,3]), sum(bin_tests_prefire[4,3]), sum(bin_tests_prefire[-4,3])), byrow=TRUE, 2, 2))
# p-value = 0.01047

# Bonferroni correction: significance level
# 0.05/5 = 0.01


ISdots <- ggplot(dpISn2) +
  geom_pointrange(aes(x=Metrics, y=yN, ymin=yminN, ymax=ymaxN), shape = 23, color="#d8b365", fill="#d8b365", linewidth=1.5, size=1.75) + 
  geom_pointrange(aes(x=Metrics, y=y.1N, ymin=ymin.1N, ymax=ymax.1N), position=position_nudge(0.2,0),linewidth=1.5, size=1.75) + 
  geom_pointrange(aes(x=Metrics, y=y.2N, ymin=ymin.2N, ymax=ymax.2N), position=position_nudge(0.1,0), shape = 23, color = "#5ab4ac", fill= "#5ab4ac",linewidth=1.5, size=1.75) +
  coord_flip() +
  ggtitle("Change in Nonspatial Forest Metrics \nat Indiana Summit") +
  xlab("Metric") + ylab("Percent Change") +
  theme_bw()

# testing new patchMorph code for one plot

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

plot(patchMorph.SpatRaster(xy_sr(4)))

# Assuming 'data' is your data frame with columns: 'Year', 'Plot', 'Bin1_ct', 'Bin2_ct', 'Bin3_ct', 'Bin4_ct'

# Create a multivariate response matrix
#response_matrix <- data[, c("Bin1_ct", "Bin2_ct", "Bin3_ct", "Bin4_ct")]
response_matrix <- obw_1[, gap_bins]
formula <- formula(response_matrix ~ Year)

# Specify the strata for repeated measures (Plot in this case)
strata <- obw_1$Plot
Year <- obw_1$Year

# Perform PERMANOVA using adonis with repeated measures
permanova_result <- adonis(formula, strata = strata)


pointData4 <- ppp(plots[[4]]$X, plots[[4]]$Y, window = disc(radius = sqrt(10000/pi), centre = c(0,0)), 
                 marks = as.numeric(rownames(plots[[4]])))  
treeData4 <- data.frame(dbh=plots[[4]][,"dbh"], spp=plots[[4]]$Spec, Tree.ID=as.numeric(rownames(plots[[4]]))) 
clusterByCrown(pointData4, Crw.rad.pred(treeData4), distThreshold = -1)

pointData4 <- ppp(plots[[4]]$X, plots[[4]]$Y, window = disc(radius = sqrt(10000/pi), centre = c(0,0)), 
                  marks = as.numeric(rownames(plots[[4]])))  
treeData4 <- data.frame(dbh=plots[[4]][,"dbh"], spp=plots[[4]]$Spec, Tree.ID=as.numeric(c(1:nrow(plots[[4]])))) 
clusterByCrown(pointData4, Crw.rad.pred(treeData4), distThreshold = -1)



pointData1 <- ppp(plots[[1]]$X, plots[[1]]$Y, window = disc(radius = sqrt(10000/pi), centre = c(0,0)), 
                  marks = as.numeric(rownames(plots[[1]])))  
treeData1 <- data.frame(dbh=plots[[1]][,"dbh"], spp=plots[[1]]$Spec, Tree.ID=as.numeric(rownames(plots[[1]]))) 
clusterByCrown(pointData1, Crw.rad.pred(treeData1), distThreshold = -1)


## New Fresh Clusters
# 4/17/24
library(spatstat)

##  Functions to predict crown diameter
# returns other other columns past species and dbh # need to change
Crw.rad.pred = function(data,cr.coefs=-1){
  
  ## get dbh and species columns
  colnames(data)[grep("(dbh|dia)",colnames(data),ignore.case=T)] = "dbh"
  dbh = data[,which(colnames(data)=="dbh")]
  
  # Sort out species codes
  Species = as.character(data[,grep("Sp",colnames(data),ignore.case=T)])
  Species[grep("P|p",Species,fixed=T)]="PP"  # fixed=T restricts to only p
  Species[grep("PP|PIPO|PIJE|PIJE*",Species,ignore.case=T)]="PP" # using ponderosa
  Species[grep("PICO",Species,ignore.case=T)]="LP" # lodgepole pine
  Species[grep("G|GF|ABGR|ABCO|WF",Species,ignore.case=T)]="WF" # white fir
  Species[grep("RC|THPL|JUGR",Species,ignore.case=T)]="IC" # using incense cedar
  
  cr.rad = 	rep(0,length(dbh))
  
  ## set up coefs from Gill et al. 2000
  pp.coef = c( 0.9488, 0.0356 )
  lp.coef = c(  0.5230, 0.0440 )
  wf.coef = c(  1.2256, 0.0299 )
  ic.coef = c(  1.2960, 0.0256 )
  
  coefs = data.frame(pp.coef,lp.coef,wf.coef,ic.coef); rownames(coefs)=c("b0","b1")
  if (cr.coefs == -1) (cr.coefs = coefs)
  
  ## run models
  
  cr.rad[which(Species=="PP")] = cr.coefs[1,1] + cr.coefs[2,1]*dbh[which(Species=="PP")]
  cr.rad[which(Species=="LP")] = cr.coefs[1,2] + cr.coefs[2,2]*dbh[which(Species=="LP")]
  cr.rad[which(Species=="WF")] = cr.coefs[1,3] + cr.coefs[2,3]*dbh[which(Species=="WF")]
  cr.rad[which(Species=="IF")] = cr.coefs[1,4] + cr.coefs[2,4]*dbh[which(Species=="IC")]
  
  # Catch all for species with no coeff. PP as default
  cr.rad[which(cr.rad==0)] = cr.coefs[1,1] + cr.coefs[2,1]*dbh[which(Species=="PP")]	
  
  # Output
  return(res=data.frame(data,crown = cr.rad)) # adds crown column to df
}

###################
##  gini coefficient code:  
##   input is dbh list in cm
gini = function(dbh.vec){
  bah = (dbh.vec/200)^2*3.14
  n = length(dbh.vec)
  rank = c(1:n)
  if (n==1) (ginii = -1) else (ginii = sum(((2*rank-n-1)*sort(bah))) / sum(sort(bah)*(n-1))) 
  return (ginii)}

###################

clusterByCrown <- function(pointData, treeData, distThreshold=-1)
{
  # IDs <- rownames(treeData)
  IDs <- c(1:nrow(treeData))
  radii <- ifelse(rep(distThreshold == -1, nrow(treeData)), treeData$crown, rep(distThreshold / 2, nrow(treeData)))
  
  # Make the output list big enough for the theoretical case that every point is a singleton
  cluster.list <- vector(mode="list", length=length(IDs))
  names(cluster.list) <- paste("clust", 1:length(IDs), sep="")
  
  distMat <- as.matrix(dist(matrix(c(pointData$x, pointData$y), ncol=2, dimnames=list(IDs))))
  distMat <- t(apply(distMat, 1, function(x) x - radii))
  distMat <- apply(distMat, 2, function(x) x - radii)
  
  # Set aside the onesies
  singles <- which((nrow(distMat) - apply(distMat, 2, function(x) sum(x > 0))) == 1)
  if(length(singles) > 0)
  {
    cluster.list[1:length(singles)] <- colnames(distMat)[singles]
    distMat <- distMat[-singles,-singles]
    IDs <- IDs[-singles]
  }
  
  # Initializing variables outside of the loop helps speed things up
  index <- length(singles) + 1
  vec <- vector()
  len1 <- len2 <- 0
  
  # Iterate until we've dealt with every ID
  while(length(IDs) > 1)
  {
    # Since we're removing data as we determine clumps, we want the first remaining value every time
    vec <- which(distMat[,1] <= 0)
    
    len1  <-1
    len2 <- 2
    while(len1 < len2)
    {
      len1 <- length(vec)
      vec  <- c(vec, unlist(apply(distMat[,vec] <= rep(0, nrow(distMat)), 2, which)))
      vec  <- vec[!duplicated(vec)]
      len2 <- length(vec)
    }
    
    # Once all points in the current cluster are accounted for, figure out which point IDs belong
    #  to the points belonging to the cluster, and remove them from the IDs list.
    IDs.in.cluster <- colnames(distMat)[sort(vec)]
    cluster.list[[index]] <- IDs.in.cluster 
    
    IDs <- IDs[-which(IDs %in% IDs.in.cluster)]
    distMat <- distMat[-which(rownames(distMat) %in% IDs.in.cluster), -which(colnames(distMat) %in% IDs.in.cluster)]
    
    index <- index + 1
  }
  
  # Remove any slots that we didn't use
  if(sum(as.integer(lapply(cluster.list, is.null))) < nrow(treeData))
    cluster.list <- cluster.list[-which(lapply(cluster.list, length) == 0)]
  
  return(cluster.list)
}

###################

summarizeClusters.ppp <- function(pointData, treeData, distThreshold=-1, max.bin=-1,edge.cut = c(0,0,0,0), Quickmap=F,plot.name="Name"){
  
  if ("crown" %in% colnames(treeData)) (treeData=treeData) else (treeData = Crw.rad.pred(treeData))
  
  if(Quickmap == T) (cluster.list = Quickmap.list(treeData[,"Clump.ID"])) else (cluster.list <- clusterByCrown(pointData, treeData, distThreshold))
  
  cluster.size <- unlist(lapply(cluster.list, length))
  names(cluster.size)<-c()  
  
  # Make sure max bin captures. Also sets max.bin to max cluster size for  default -1 setting
  if(max(cluster.size)>max.bin)  (max.bin = max(cluster.size))
  
  # Per-tree variables
  tree.cluster.membership <- c()
  for(i in 1:length(cluster.list))  tree.cluster.membership[unlist(lapply(cluster.list[i], paste))] <- i
  tree.cluster.membership <- as.matrix(tree.cluster.membership[as.character(sort(as.integer(names(tree.cluster.membership))))])
  tree.dbh <- as.matrix(treeData$dbh)
  tree.ba  <- as.matrix(pi * (tree.dbh / 200)^2)
  tree.sdi <- as.matrix((tree.dbh / 25)^1.7)
  tree.clust.sz = cluster.size[tree.cluster.membership]
  tree.bin = lut(c("1","2-4","5-9","10+"),breaks=c(0,2,5,10,200))(tree.clust.sz)
  tree.bin = factor(tree.bin,levels=c("1","2-4","5-9","10+"),order=T)
  rownames(tree.dbh) <- rownames(tree.ba) <- rownames(tree.sdi) <- rownames(tree.cluster.membership)
  
  trees <- cbind(data.frame(x=pointData$x,y=pointData$y),as.data.frame(treeData), data.frame(ba=tree.ba, sdi=tree.sdi, clust.sz=tree.clust.sz ,cluster.membership=tree.cluster.membership,bin=tree.bin))
  
  ### Eliminate edge trees from tree list if a cut distances are provided
  noedge.trees= trees
  edgefilter <- trees %>% 
    filter((sqrt(((trees$x)^2)+((trees$y)^2))>(sqrt(10000/pi)-5))) # 5m buffer from plot edge
  cut.index <- which(trees$x %in% edgefilter$x & trees$y %in% edgefilter$y)
  
  
  if(length(cut.index)>0) {
    trees=  trees [-cut.index,] } 
  
  
  ## reset these vectors for calcs later on
  tree.ba = trees$ba
  tree.dbh = trees$dbh
  tree.sdi = trees$sdi
  tree.clust.sz = trees$clust.sz
  tree.cluster.membership =trees$cluster.membership
  
  # recalc cluster size
  new.cl = unique(tree.cluster.membership)
  cluster.size.orig = cluster.size  # store original
  cluster.size=vector()
  #for (i in 1: length(new.cl)) cluster.size[i] = length(which(tree.cluster.membership==new.cl[i]))
  for (i in 1: length(new.cl)) cluster.size[i] = length(which(noedge.trees$cluster.membership==new.cl[i]))
  
  
  ### Adjust x.max & y.max & reset coords
  pointData.orig = 	pointData
  
  if(nrow(trees) > 0) {
    pointData = as.ppp(cbind(trees$x,trees$y,as.numeric(rownames(trees))),W=c(min(trees$x),max(trees$x),min(trees$y),max(trees$y)))
  } else {
    pointData = as.ppp(cbind(trees$x,trees$y,as.numeric(rownames(trees))),W=c(-Inf, Inf, -Inf, Inf))
  }
  
  # General variables
  n.pts <- sum(cluster.size)
  n.clusts <- length(cluster.size)
  n.bins <- max.bin
  mean.clust.size <- sum(cluster.size^2) / n.pts
  norm.mean.clust.size <- mean.clust.size / n.pts
  hectares <- diff(pointData$window$xrange) * diff(pointData$window$yrange) / 10000
  
  
  # Per-cluster variables
  cluster.ba  <- rep(0, n.clusts)
  cluster.sdi <- rep(0, n.clusts)
  cluster.qmd <- rep(0, n.clusts)
  cluster.mndbh <- rep(0, n.clusts)
  cluster.lartr <- rep(0, n.clusts)
  cluster.gini = cluster.bin =cluster.mgf <- rep(0, n.clusts)
  
  ## Need to redo this to pull from noedge.trees
  for(i in 1:n.clusts){
    trees.hd = which(noedge.trees$cluster.membership==new.cl[i])
    cluster.ba[i]  <- round(sum(noedge.trees[trees.hd,"ba"]),3)
    cluster.sdi[i] <- round(sum(noedge.trees[trees.hd,"sdi"]),3)
    cluster.qmd[i] <- round(sqrt(sum(noedge.trees[trees.hd,"dbh"]^2) / cluster.size[i]),1)
    cluster.mndbh[i] <- round(mean(noedge.trees[trees.hd,"dbh"]),1)
    cluster.lartr[i] <- round(max(noedge.trees[trees.hd,"dbh"]),1)
    cluster.gini[i] = round(gini(noedge.trees[trees.hd,"dbh"]),3)
    d.class = lut(c(20,40,60,80,100,180),breaks=c(0,20,40,60,80,100,180))(noedge.trees[trees.hd,"dbh"])  
    cluster.mgf[i] = (length(unique(d.class)) - 1) /log(sum(cluster.ba[i]*10))
    cluster.bin[i] = lut(c("1","2-4","5-9","10+"),breaks=c(0,2,5,10,200))(cluster.size[i])
  }
  # Set factor order
  cluster.bin = factor(cluster.bin,levels=c("1","2-4","5-9","10+"),order=T)
  clusters <- data.frame(size=cluster.size, tot.ba=cluster.ba, tot.sdi=cluster.sdi,
                         qmd=cluster.qmd,mn.dbh= cluster.mndbh,dbh.lar=cluster.lartr,gini=cluster.gini,mrglf.indx = cluster.mgf,bin=cluster.bin)
  clusters = clusters[sort.list(clusters[,1]), ]                     
  
  # Per-bin variables
  maxbin.num.tree <- maxbin.num.clust <- rep(0, max(cluster.size))
  for(i in 1:max(cluster.size))
  {
    maxbin.num.clust[i] <- length(which(cluster.size == i))
    maxbin.num.tree[i]  <- maxbin.num.clust[i] * i
  }
  n.bins<-length(maxbin.num.clust)
  
  maxbin.clust.ba  <- rep(0, n.bins)
  maxbin.clust.sdi <- rep(0, n.bins)
  maxbin.clust.qmd <- rep(0, n.bins)
  for(i in 1:n.bins)
  {
    maxbin.clust.ba[i]  <- sum(cluster.ba[which(cluster.size == i)])
    maxbin.clust.sdi[i] <- sum(cluster.sdi[which(cluster.size == i)])
    maxbin.clust.qmd[i] <- sqrt(sum(tree.dbh[which(tree.cluster.membership %in% which(cluster.size == i))]^2) / (i * maxbin.num.clust[i]))
  }
  maxbin.clust.qmd[maxbin.num.clust == 0] <- 0
  
  maxbin.pct.clust <- (maxbin.num.clust / sum(maxbin.num.clust)) 
  maxbin.pct.tree  <- (maxbin.num.tree / sum(maxbin.num.tree)) 
  maxbin.pct.ba    <- (maxbin.clust.ba / sum(maxbin.clust.ba)) 
  maxbin.pct.sdi   <- (maxbin.clust.sdi / sum(maxbin.clust.sdi))
  
  # Trim to length max.bin if needed
  if(max(max.bin)<n.bins) {
    maxbin.num.clust  <- c(maxbin.num.clust[1:(max.bin - 1)], sum(maxbin.num.clust[max.bin:n.bins]))
    maxbin.num.tree   <- c(maxbin.num.tree[1:(max.bin - 1)],  sum(maxbin.num.tree[max.bin:n.bins]))
    maxbin.clust.ba   <- c(maxbin.clust.ba[1:(max.bin - 1)],  sum(maxbin.clust.ba[max.bin:n.bins]))
    maxbin.clust.sdi  <- c(maxbin.clust.sdi[1:(max.bin - 1)], sum(maxbin.clust.sdi[max.bin:n.bins]))
    maxbin.clust.qmd  <- c(maxbin.clust.qmd[1:(max.bin - 1)], sum(maxbin.clust.qmd[max.bin:n.bins]))
    maxbin.pct.clust  <- c(maxbin.pct.clust[1:(max.bin - 1)], sum(maxbin.pct.clust[max.bin:n.bins]))
    maxbin.pct.tree   <- c(maxbin.pct.tree[1:(max.bin - 1)],  sum(maxbin.pct.tree[max.bin:n.bins]))
    maxbin.pct.ba     <- c(maxbin.pct.ba[1:(max.bin - 1)],    sum(maxbin.pct.ba[max.bin:n.bins]))
    maxbin.pct.sdi    <- c(maxbin.pct.sdi[1:(max.bin - 1)],   sum(maxbin.pct.sdi[max.bin:n.bins]))
  }
  
  
  maxbin <- data.frame(num.clust=maxbin.num.clust, num.tree=maxbin.num.tree, tot.ba=maxbin.clust.ba, tot.sdi=maxbin.clust.sdi, qmd=maxbin.clust.qmd,
                       pct.clust=maxbin.pct.clust, pct.tree=maxbin.pct.tree, pct.ba=maxbin.pct.ba, pct.sdi=maxbin.pct.sdi)  
  
  ##Expand to max.bin if needed
  if(max(max.bin)>n.bins){ 
    zeross = matrix(0,(max(max.bin)-n.bins),ncol(maxbin));colnames(zeross)=colnames(maxbin)
    maxbin=rbind(maxbin,zeross) }
  
  ### Binned maxbin table in individuals, small clumps (2-4), med (5-9), & large(10-15) & super 16+
  #maxbin.orig = maxbin #save	
  #add.mat = matrix(0,15-nrow(maxbin),9)
  #colnames(add.mat)=colnames(maxbin)
  #maxbin = rbind(maxbin,add.mat)
  
  
  ## Add to maxbin to get right calcs
  maxbin.orig = maxbin #save
  add.mat = matrix(0,500-nrow(maxbin),9)
  colnames(add.mat)=colnames(maxbin)
  maxbin = rbind(maxbin,add.mat)
  
  pct.tree =	round(c(maxbin[1,7],sum(maxbin[2:4,7]),sum(maxbin[5:9,7]),sum(maxbin[10:nrow(maxbin),7])),2)
  pct.ba =	round(c(maxbin[1,8],sum(maxbin[2:4,8]),sum(maxbin[5:9,8]),sum(maxbin[10:nrow(maxbin),8])),2)
  # Get QMD per clump bin
  ba.bin.sum = c(maxbin[1,3],sum(maxbin[2:4,3]),sum(maxbin[5:9,3]),sum(maxbin[10:nrow(maxbin),3]))
  tpa.bin.sum =  c(maxbin[1,2],sum(maxbin[2:4,2]),sum(maxbin[5:9,2]),sum(maxbin[10:nrow(maxbin),2]))
  qmd.bin =  round((((ba.bin.sum/tpa.bin.sum)/pi)^.5)*200,1)
  
  clump.bins = data.frame(pct.tree,pct.ba,qmd.bin,row.names=c("1","2-4","5-9","10+"))
  maxbin = maxbin.orig # reset to orig
  
  
  ### Summary Metrics
  BA = sum(trees$ba)/hectares
  TPH = nrow(trees)/hectares
  Mean.dbh = mean(trees$dbh)
  QMD = (((BA/TPH)/3.14)^.5)*200
  SDI = TPH*(QMD/25.4)^1.605
  sum.met = cbind(BA,TPH,Mean.dbh,QMD,SDI,hectares)
  sum.eng = cbind(BA*4.356,TPH/2.47,Mean.dbh/2.54,QMD/2.54,SDI/2.47,hectares*2.47)
  summary = cbind(sum.met,sum.eng)
  colnames(summary)= c("BAH","TPH","Mean.dbh","QMD","SDI","hectares","BAA","TPA","Mean.dbh","QMD","SDI","Acres")
  
  
  
  return(list(plot.name=plot.name, n.pts=nrow(trees), n.clusts=n.clusts, n.bins=max.bin,  summary=round(summary,1), 
              mean.clust.size=mean.clust.size, norm.mean.clust.size=norm.mean.clust.size,
              points=pointData, trees=trees, points.noedge = pointData.orig , trees.noedge = noedge.trees , 
              clusters=clusters, maxbin=maxbin,clump.bins =clump.bins, edge.cut = edge.cut))
}

plots <- list(IS1_2018, IS1_1941, IS2_2018, IS2_1941, IS3_2018, IS3_1941, 
              OH1_2018, OH1_1941, OH2_2018, OH2_1941, OH3_2018, OH3_1941, 
              IS1_1995, IS2_1995, IS3_1995, OH1_2006, OH2_2006, OH3_2006) # new line for prefire analysis
names <- c("IS1 in 2018", "IS1 in 1941", "IS2 in 2018", "IS2 in 1941", "IS3 in 2018", "IS3 in 1941", 
           "OH1 in 2018", "OH1 in 1941", "OH2 in 2018", "OH2 in 1941", "OH3 in 2018", "OH3 in 1941",
           "IS1 in 1995", "IS2 in 1995", "IS3 in 1995", "OH1 in 2006", "OH2 in 2006", "OH3 in 2006") # new for prefire 
plots_out <- vector(mode='list',length=length(plots))

for(i in 1:length(plots)){
  pointData <- ppp(plots[[i]]$X, plots[[i]]$Y, window = disc(radius = sqrt(10000/pi), centre = c(0,0)), 
                   marks = as.numeric(rownames(plots[[i]])))  
  treeData <- data.frame(dbh=plots[[i]][,"dbh"], spp=plots[[i]]$Spec, Tree.ID=as.numeric(rownames(plots[[i]]))) 
  plots_out[[i]] <- summarizeClusters.ppp(pointData, treeData, -1, -1, c(0,0,0,0), F, names[i])}


# NEVER NEVER GIVE UP

# NEVER LET GO, NEVER SURRENDER
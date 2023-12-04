# ICO Maps from Scratch/GPT Code from Andrew
# 7/30/23

# Starting with tree.data_out from DChurchill_ICO.R
# or maybe treeData3 ?

library(ggplot2)
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

for (i in 1:length(plots_out)){
  #jpeg(paste("ICO_Map",names[i]),700,630)
  print(ggplot(as.data.frame(plots_out[[i]][9]), aes(x = trees.x, y = trees.y)) +
    # Crowns as green circles with width corresponding to crown diameter
    geom_point(aes(size = trees.crown, color = factor(trees.bin)), alpha=0.75) +
    # Set the size range for stems and crowns separately
    geom_point(aes(size = .5), color = "burlywood4") +
    scale_size_continuous(range = c(2,10)) +
    # Set the shape for stems and crowns separately
    scale_shape_manual(values = c(16, 16)) +
    # Set the color palette for cluster sizes
    scale_colour_brewer(palette = "YlGn", name = "Cluster Size") +
    # Customize the plot appearance
      theme_classic(base_size=22) +
      theme(plot.title=element_text(hjust=0.5)) +
      labs(title = paste("Living Trees at", names[i], " (1-ha Plot)"),
         x = "Distance in m",
         y = "",
         size = "Crown (m)") +
    guides(size = guide_legend(override.aes = list(color ="#addd8e")))) 
  #  dev.off() 
}

# try to make same map with crown projections at same scale as x axis (in m, i.e.)
#install.packages("ggforce")
library(ggforce)
df <- as.data.frame(plots_out[[9]][9])
ggplot(df, aes(x0 = trees.x, y0 = trees.y, r = trees.crown)) +
  ggforce::geom_circle(n = 20) + #5-7x faster than default
  coord_fixed()

ggplot(df, aes(x0 = trees.x, y0 = trees.y, r = trees.crown)) +
  geom_circle(n = 20) + #5-7x faster than default
  coord_fixed()
# this seems to give true-to-size crowns; incorporate into color-coded plots

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
  # Customize the plot appearance
  theme_classic(base_size=22) +
  theme(plot.title=element_text(hjust=0.5)) +
  labs(title = paste("Living Trees at", names[i], " (1-ha Plot)"),
       x = "Distance in m",
       y = "") +
  guides(size = guide_legend(override.aes = list(color ="burlywood4"))) 

# add gap polygons
    # function to create gap polygons -- GapFinder.R
    # for loop to create gap polygons

# loop of ggplot with map of true-to-scale crown radius, dbh, and gaps
  # good map w/ gaps
ggplot() +
  geom_sf(data=bound, fill="black") +
  geom_circle(data = df, n=20, aes(x0 = trees.x, y0 = trees.y, r=trees.crown, x=trees.x, y=trees.y, 
                                   fill=factor(trees.bin), color=factor(trees.bin), alpha=0.75)) +
  geom_circle(data = df, n=20, aes(x0=trees.x, y0=trees.y, r=trees.dbh/200), color="burlywood4", 
              fill="burlywood4") +
  scale_fill_brewer(palette = "YlGn", name = "Cluster Size") +
  scale_colour_brewer(palette = "YlGn", name = "Cluster Size") +
  geom_sf(data=gaps3_noedge, col="white", linewidth = 0.7, fill= "purple", alpha=0.3) +
  guides(size = guide_legend(override.aes = list(color ="burlywood4"))) +
  theme_light()


  # edited for loop: map of true-to-scale crown radius, dbh, and gaps
# GOOD FOR AFE 2023
for (i in 1:length(plots_out)){
  jpeg(paste("ICO_GapMap",names[i]),700,630)
  print(ggplot() +
          geom_sf(data=bound, fill="black") +
          geom_circle(data = as.data.frame(plots_out[[i]][9]), n=20, 
                      aes(x0 = trees.x, y0 = trees.y, r=trees.crown, x=trees.x, y=trees.y, 
                                           fill=factor(trees.bin), color=factor(trees.bin), alpha=0.75)) +
          geom_circle(data = as.data.frame(plots_out[[i]][9]), n=20, 
                      aes(x0=trees.x, y0=trees.y, r=trees.dbh/200), color="burlywood4", fill="burlywood4") +
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
          theme_light(base_size = 22) )
  dev.off()
  }
    


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


# ggplot(dataGPT, aes(x = trees.x, y = trees.y)) +
#   theme(plot.title = element_text(vjust=0.5))+
#   # Crowns as green circles with width corresponding to crown diameter
#   geom_point(aes(size = trees.crown, color = factor(trees.bin)), alpha=0.75) +
#   # Set the size range for stems and crowns separately
#   geom_point(aes(size = .5), color = "burlywood4") +
#   scale_size_continuous(range = c(2,10)) +
#   # Set the shape for stems and crowns separately
#   scale_shape_manual(values = c(16, 16)) +
#   # Set the color palette for cluster sizes
#   scale_colour_brewer(palette = "YlGn", name = "Cluster Size") +
#   # Customize the plot appearance
#   labs(title = paste("Living Trees at PLOT in YEAR"),
#        x = "",
#        y = "",
#        size = "Crown (m)") +
#   guides(size = guide_legend(override.aes = list(color ="#addd8e")))  # "#009E73")))
#   theme_classic()



# next things to try: heatmap for dist-to-tree (spatstat?), superimpose on topo map??
# why are x and y coords in 0-100 range (should be -60-60) 
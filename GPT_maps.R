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
  jpeg(paste("ICO_Map",names[i]),700,630)
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
    dev.off() 
    }

ggplot(OH_livetrees, aes(x=estab_est, fill=Spec))+
  geom_histogram() + ggtitle("Establishment Dates at O'Harrell Canyon")

ggplot(IS_livetrees, aes(x=estab_est, fill=Spec))+
  geom_histogram() + ggtitle("Establishment Dates at Indiana Summit")


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
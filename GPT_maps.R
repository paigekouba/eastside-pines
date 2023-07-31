# ICO Maps from Scratch/GPT Code from Andrew
# 7/30/23

# Starting with tree.data_out from DChurchill_ICO.R
# or maybe treeData3 ?

library(ggplot2)
# requires a data set that contains columns for x, y, stem_diameter, crown_diameter, 
# and categorical cluster size
head(treeData3) # dbh, spp, Tree.ID
head(plot.data)
#    Site Plot   Date Spec   dbh Dec     X     Y    Z CrHt SrchHt Core. Comments  age_est estab_est

tree.data_out[9]
#       x    y   dbh  spp Tree.ID    crown         ba        sdi clust.sz cluster.membership   bin
dataGPT <- as.data.frame(tree.data_out[9])

# Create the ggplot map
ggplot(dataGPT, aes(x = trees.x, y = trees.y)) +
  # Stems as black circles with width corresponding to stem diameter
  geom_point(aes(shape = "Stem", size = trees.dbh, color =factor(trees.bin))) +
  # Crowns as green circles with width corresponding to crown diameter
  geom_point(aes(shape = "Crown", size = trees.crown ),color= "burlywood4", fill="burlywood4") +
  # Set the size range for stems and crowns separately
 # scale_size_continuous(range = c(2, 10)) +
  # Set the shape for stems and crowns separately
  scale_shape_manual(values = c(21, 16)) +
  # Set the color palette for cluster sizes
  scale_colour_brewer(palette = "YlGn", name = "Cluster Size") +
  # Customize the plot appearance
  labs(title = "Schematic Map of Trees",
       x = "X Coordinate",
       y = "Y Coordinate",
       size = "DBH",
       shape = "") +
  theme_classic()

# Create the ggplot map
ggplot(dataGPT, aes(x = trees.x, y = trees.y)) +
  # Crowns as green circles with width corresponding to crown diameter
  geom_point(aes(shape = "Crown", size = trees.crown, color = factor(trees.bin))) +
  # Set the size range for stems and crowns separately
  geom_point(aes(shape = "Stem", size = .5), color = "burlywood4") +
    scale_size_continuous(range = c(2,10)) +
  # Set the shape for stems and crowns separately
  scale_shape_manual(values = c(16, 16)) +
  # Set the color palette for cluster sizes
  scale_colour_brewer(palette = "YlGn", name = "Cluster Size") +
  # Customize the plot appearance
  labs(title = "Schematic Map of Trees",
       x = "X Coordinate",
       y = "Y Coordinate",
       size = "Crown",
       size = "Diameter",
       shape = "",
       color = "black") +
  theme_minimal()

# geom_point(aes(shape = "Stem", size= trees.dbh/1200), color = "blue") +

# next things to try: heatmap for dist-to-tree (spatstat?), superimpose on topo map??
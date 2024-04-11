# ICO Maps from Scratch/GPT Code from Andrew
# 7/30/23

library(ggplot2)
library(ggforce)
library(RColorBrewer)

my.cols <- brewer.pal(5, "YlGn")
my.cols <- my.cols[2:5]

ICO_maps <- list()
for (i in 1:length(plots_out)){
 p <- ggplot() +
          geom_sf(data=bound, fill="white") +
          geom_circle(data = as.data.frame(plots_out[[i]][11]), n=20, # #9 is trees; try 11 for trees.noedge
                      aes(x0 = trees.noedge.x, y0 = trees.noedge.y, r=trees.noedge.crown, x=trees.noedge.x, y=trees.noedge.y, 
                          fill=factor(trees.noedge.bin), color=factor(trees.noedge.bin), alpha=0.75)) +
          geom_circle(data = as.data.frame(plots_out[[i]][11]), n=20, 
       aes(x0=trees.noedge.x, y0=trees.noedge.y, r=trees.noedge.dbh/200), color="burlywood4", fill="burlywood4") +
        #  scale_fill_brewer(palette = "YlGn", name = "Cluster Size") +
        #  scale_colour_brewer(palette = "YlGn", name = "Cluster Size") +
          scale_fill_manual(values = my.cols, name = "Cluster Size") +
          scale_color_manual(values = my.cols, name = "Cluster Size") +
# no gaps for now    # geom_sf(data=results[[i]], col="white", linewidth = 0.7, fill= "purple", alpha=0.3) +
          geom_sf(opes_sr[[i]], mapping = aes(), fill = "#FFFAB2", color= "#FCE879") +        
          guides(size = guide_legend(override.aes = list(color ="burlywood4"))) +
          #theme_classic(base_size=22) +
          theme(plot.title=element_text(size = 10, hjust=0.5)) +
          labs(title = paste("Living Trees at", names[i]),
               x = "",
               y = "",
               size = 10) +
               # size = "Crown (m)") +
          theme(legend.position = "none")
          # guides(shape = guide_legend(override.aes = list(size = 1))) +
          # guides(color = guide_legend(override.aes = list(size = 1))) +
          # theme(legend.title = element_text(size = 5), legend.text = element_text(size = 5))
   #guides(size = guide_legend(override.aes = list(color ="#addd8e"))) +
         # theme_light()#base_size = 22) )
  # dev.off()
 ICO_maps[[i]] <- p
}

# double check the arrangement of the maps; remove per-map labels and add to grid.arrange ?
# goal is two figs: IS 1, 2, 3 in a column for 1941 at left; IS 1, 2, 3 in a column for 2018 at right. Same for OH
#grid.arrange(ICO_maps[[2]], ICO_maps[[1]], ICO_maps[[4]], ICO_maps[[3]], ICO_maps[[6]], ICO_maps[[5]], ncol=2) # IS
grid.arrange(ICO_maps[[2]], ICO_maps[[13]], ICO_maps[[1]], ICO_maps[[4]], ICO_maps[[14]], ICO_maps[[3]], ICO_maps[[6]], ICO_maps[[15]], ICO_maps[[5]], ncol=3) # IS

#grid.arrange(ICO_maps[[8]], ICO_maps[[7]], ICO_maps[[10]], ICO_maps[[9]], ICO_maps[[12]], ICO_maps[[11]], ncol=2) # OH
grid.arrange(ICO_maps[[8]], ICO_maps[[16]], ICO_maps[[7]], ICO_maps[[10]], ICO_maps[[17]], ICO_maps[[9]], ICO_maps[[12]], ICO_maps[[18]], ICO_maps[[11]], ncol=3) # OH


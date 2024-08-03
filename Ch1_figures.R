# Figures for Paper -- Eastside Pines ICO
# Thu 2/15/24

#pdf(file = "Ch1_figures_4.19.24.pdf")

plot(dotplots) + theme(base_size = 18) # percent change in nonspatial metrics, by site. 1941 in red, 2018 in black, with mean and se

# ICO_maps 
png("Ch1_Fig2a.png", width=7, height =5.5, units = "in", res = 300)
  ggarrange(ICO_maps[[2]], ICO_maps[[13]], ICO_maps[[1]], ICO_maps[[4]], ICO_maps[[14]], ICO_maps[[3]], ICO_maps[[6]], ICO_maps[[15]], ICO_maps[[5]], ncol=3, nrow=3, common.legend = T, legend.grob = get_legend(ICO_maps[[13]]), legend = "right")
dev.off()

png("Ch1_Fig2b.png", width=7, height =5.5, units = "in", res = 300)
ggarrange(ICO_maps[[8]], ICO_maps[[16]], ICO_maps[[7]], ICO_maps[[10]], ICO_maps[[17]], ICO_maps[[9]], ICO_maps[[12]], ICO_maps[[18]], ICO_maps[[11]], ncol=3, nrow=3, common.legend = T, legend.grob = get_legend(ICO_maps[[13]]), legend = "right")
dev.off()

# two figs: IS 1, 2, 3 in a column for 1941 at left; IS 1, 2, 3 in a column for 2018 at right. Same for OH
# grid.arrange(ICO_maps[[2]], ICO_maps[[13]], ICO_maps[[1]], ICO_maps[[4]], ICO_maps[[14]], ICO_maps[[3]], ICO_maps[[6]], ICO_maps[[15]], ICO_maps[[5]], ncol=3) # IS
# grid.arrange(ICO_maps[[8]], ICO_maps[[16]], ICO_maps[[7]], ICO_maps[[10]], ICO_maps[[17]], ICO_maps[[9]], ICO_maps[[12]], ICO_maps[[18]], ICO_maps[[11]], ncol=3) # OH

# grid.arrange(pie_IS41, pie_IS18, ncol=2)
# grid.arrange(pie_OH41, pie_OH18, ncol=2) 
png("Ch1_Fig3.png", width=7, height =4.5, units = "in", res = 300)
ggarrange(pie_IS41, pie_IS95, pie_IS18, pie_OH41, pie_OH06, pie_OH18, ncol=3, nrow=2, common.legend = T, legend.grob = get_legend(pie_IS95), legend = "right")
dev.off()

grid.arrange(pie_IS41, pie_IS95, pie_IS18, pie_OH41, pie_OH06, pie_OH18, ncol=3)
# before-and-after pie charts of all structural categories, by site

# summ.all # per-bin and stand-level summary metrics for IS41, OH41, all41, IS18, OH18, and all18

#(gap_distn) # gap size histogram, combined
#grid.arrange(LydersenFig3, LydersenFig3_IS, LydersenFig3_OH, ncol = 2) # bar plots of binned gaps mean and se
#grid.arrange(LydersenFig3, LydersenFig3_clusters, ncol = 1)
png("Ch1_Fig4_Fig5.png", width=12, height =8, units = "in", res = 300)
ggarrange(LydersenFig3, LydersenFig3_clusters, ncol = 1, nrow=2, common.legend = T, labels = c("Fig. 1.4", "Fig. 1.5"), label.y = 1.1)
dev.off()

#(location_fit) # brms model with cluster counts per cluster size
# model_coeffs # inset with model coefficients from brms model
#caterplot(pines3nb_mcmc, parms = c("b_clust.sz", "b_YearPrefire", "b_clust.sz:YearPrefire"), pch=5, cex=1.5, lwd = c(4,8), labels = c("Cluster Size:Year","Year", "Cluster Size"), labels.loc="above", style = "plain")

#Supplementary

# plot(OH_estab_cores) # establishment date distribution based on N = 81 cored trees
# plot(OH_estab_trees) # establishment date distribution based on N = 470 trees, using age-size regression
# plot(IS_estab_cores) # establishment date distribution based on N = 90 cored trees
# plot(IS_estab_trees) # establishment date distribution based on N = 648 trees, using age-size regression
# 
# # size class distn (dbh)
# plot(IS_2018_hist)
# plot(IS_1941_hist)
# plot(OH_2018_hist)
# plot(OH_1941_hist)
# 
# model_v_cores_IS # compares regression-based ages with core-based ages
# model_v_cores_OH

# math on area in interst, gap, size bins
# IS_change
# OH_change

dev.off()

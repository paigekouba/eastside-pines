# Figures for Paper -- Eastside Pines ICO
# Thu 2/15/24

dotplots # percent change in nonspatial metrics, by site. 1941 in red, 2018 in black, with mean and se

ICO_maps # needs updating with PM gap areas, updated color scheme, and arrangement

grid.arrange(pie_IS41, pie_IS18, ncol=2)
grid.arrange(pie_OH41, pie_OH18, ncol=2) 
# before-and-after pie charts of all structural categories, by site


location_fit # brms model with cluster counts per cluster size
model_coeffs # inset with model coefficients from brms model

gap_distn # gap size histogram, combined

plot(OH_estab_cores) # establishment date distribution based on N = 81 cored trees
plot(OH_estab_trees) # establishment date distribution based on N = 470 trees, using age-size regression
plot(IS_estab_cores) # establishment date distribution based on N = 90 cored trees
plot(IS_estab_trees) # establishment date distribution based on N = 648 trees, using age-size regression

# size class distn (dbh)
IS_2018_hist
IS_1941_hist
OH_2018_hist
OH_1941_hist

model_v_cores_IS # compares regression-based ages with core-based ages
model_v_cores_OH

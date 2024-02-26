# brms for cluster size distribution

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

library(brms)

# pines1_mbrms <- brms::brm(n ~ clust.sz,
#                            data = MLR_df, family = poisson(), chains = 3, # need to change this to negative binomial
#                            iter = 3000, warmup = 1000)
#summary(pines1_mbrms)
#plot(pines1_mbrms)
library(tidybayes)

# ______________ Try again with negative binomial _______________ #

pines1_nb <- brms::brm(n ~ clust.sz,
                          data = MLR_df, family = negbinomial(), chains = 3, # need to change this to negative binomial
                          iter = 3000, warmup = 1000)
#summary(pines1_nb)
#plot(pines1_nb) # looks good! caterpillars
#pp_check(pines1_nb)

pines3_nb <- brms::brm(n ~ clust.sz*Year + (1|Plot.unique),
                          data = MLR_df, family = negbinomial(), chains = 3,
                          iter = 3000, warmup = 1000)

#pairs(pines3_nb) # some shenanigans
#plot(pines3_nb) # also looks good?
#pp_check(pines3_nb)

pines4_nb <- brms::brm(n ~ clust.sz + Year + (1|Plot.unique),
                          data = MLR_df, family = negbinomial(), chains = 3,
                          iter = 3000, warmup = 1000)
# There were 10 divergent transitions after warmup.

loo(pines1_nb,pines3_nb, pines4_nb, compare = TRUE) # pines3_nb has the best fit
# Found 2 observations with a pareto_k > 0.7 in model 'pines3_nb'. It is recommended to set 'moment_match = TRUE' in order to perform moment matching for problematic observations.

#loo(pines1_mbrms,pines3_mbrms, pines4_mbrms, compare = TRUE)
#loo(pines1_mbrms,pines3_mbrms, pines4_mbrms, pines1_nb,pines3_nb, pines4_nb, compare = TRUE)

#tidy_stan(pines3_nb)
#performance::r2(pines3_nb)

# try with just 95% CI
(location_fit <- MLR_df %>%
    group_by(Year) %>%
    add_predicted_draws(pines3_nb) %>% # add best model here !!!
    ggplot(aes(x = clust.sz, y = n, color = Year, fill = Year)) +
    stat_lineribbon(aes(y = .prediction), .width = .95, alpha = 1/2) +
    geom_point(data = MLR_df) +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Dark2") +
    theme_bw() +
    ylab("\nFrequency") +
    xlab("\nTrees Per Cluster") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")))
   
    #theme(legend.title = element_blank()))
summary(pines3_nb)
# Family: negbinomial 
# Links: mu = log; shape = identity 
# Formula: n ~ clust.sz * Year + (1 | Plot.unique) 
# Data: MLR_df (Number of observations: 66) 
# Draws: 3 chains, each with iter = 3000; warmup = 1000; thin = 1;
# total post-warmup draws = 6000
# 
# Group-Level Effects: 
#   ~Plot.unique (Number of levels: 6) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.26      0.18     0.02     0.70 1.00     1578     2038
# 
# Population-Level Effects: 
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept             4.51      0.28     3.95     5.06 1.00     3071     3298
# clust.sz             -1.00      0.12    -1.23    -0.76 1.00     2967     3447
# Year2018             -1.18      0.29    -1.75    -0.61 1.00     2880     3380
# clust.sz:Year2018     0.55      0.12     0.30     0.79 1.00     2817     3318
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# shape     7.20      3.96     2.98    16.82 1.00     3060     2419
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).

library(mcmcplots)
pines3nb_mcmc <- as.data.frame(as.matrix(as.mcmc(pines3_nb)))
model_coeffs <- caterplot(pines3nb_mcmc, parms = c("b_clust.sz", "b_Year2018", "b_clust.sz:Year2018"), pch=5, cex=1.5, lwd = c(4,8), labels = c("Cluster Size:Year","Cluster Size", "Year"), labels.loc="above", style = "plain")


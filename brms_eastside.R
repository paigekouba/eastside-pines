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

MLR_df[MLR_df$Year == 1995 | MLR_df$Year == 2006,]$Year <- "Prefire"
#MLR_df[MLR_df$Year == 2018 | MLR_df$Year == 2009,]$Year <- "Postfire"

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
AIC(pines1_nb,pines3_nb, pines4_nb)
# Found 2 observations with a pareto_k > 0.7 in model 'pines3_nb'. It is recommended to set 'moment_match = TRUE' in order to perform moment matching for problematic observations.

#loo(pines1_mbrms,pines3_mbrms, pines4_mbrms, compare = TRUE)
#loo(pines1_mbrms,pines3_mbrms, pines4_mbrms, pines1_nb,pines3_nb, pines4_nb, compare = TRUE)

#tidy_stan(pines3_nb)
#performance::r2(pines3_nb)

# try with just 95% CI
library(tidyverse)
(location_fit <- MLR_df %>%
    group_by(Year) %>%
    add_predicted_draws(pines3_nb) %>% # add best model here !!!
    ggplot(aes(x = clust.sz, y = n, color = Year, fill = Year)) +
    stat_lineribbon(aes(y = .prediction), .width = .95, alpha = 1/2) +
    #geom_point(data = MLR_df) +
    geom_jitter(data = MLR_df, width = 0.2) +
    scale_fill_manual(values = c("coral", "black", "darkgray")) +
    scale_color_manual(values = c("red", "black", "white")) +
    #scale_fill_brewer(palette = "Set2") +
    #scale_color_brewer(palette = "Dark2") +
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
# Data: MLR_df (Number of observations: 118) 
# Draws: 3 chains, each with iter = 3000; warmup = 1000; thin = 1;
# total post-warmup draws = 6000
# 
# Group-Level Effects: 
#   ~Plot.unique (Number of levels: 6) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.23      0.17     0.01     0.65 1.00     1312     2074
# 
# Population-Level Effects: 
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                4.25      0.30     3.66     4.82 1.00     1600     2709
# clust.sz                -0.88      0.13    -1.14    -0.62 1.00     1659     2800
# Year2018                -1.00      0.33    -1.65    -0.36 1.00     1999     2765
# YearPrefire             -0.92      0.32    -1.54    -0.31 1.00     2061     3528
# clust.sz:Year2018        0.46      0.14     0.19     0.73 1.00     1635     2820
# clust.sz:YearPrefire     0.51      0.13     0.25     0.77 1.00     1702     2757
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# shape     4.17      1.22     2.38     7.03 1.00     3804     3683
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).

library(mcmcplots)
pines3nb_mcmc <- as.data.frame(as.matrix(as.mcmc(pines3_nb)))
model_coeffs <- caterplot(pines3nb_mcmc, parms = c("b_clust.sz", "b_YearPrefire", "b_clust.sz:YearPrefire"), pch=5, cex=1.5, lwd = c(4,8), labels = c("Cluster Size:Year","Year", "Cluster Size"), labels.loc="above", style = "plain")


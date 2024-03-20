# trying to get logs and snags modeled based on transition matrix in Raphael and Morrison 1987

library(dplyr)
df <- OH_snags %>% 
  filter(Dec > 1) #%>% 
  #dplyr::select("dbh", "Dec", "age_est", "dec_correction", "estab_est")

df2 <- OH_logs %>% 
  filter(Dec > 1) %>% 
  dplyr::select("dbh", "Dec", "age_est", "dec_correction", "estab_est")

IS_snags %>% 
  group_by(Dec) %>% 
  tally()

IS_logs %>% 
  group_by(Dec) %>% 
  tally()

OH_snags %>% 
  group_by(Dec) %>% 
  tally()

OH_logs %>% 
  group_by(Dec) %>% 
  tally()

ggplot(OH_snags, mapping = aes(x=Dec, y=dbh)) +
  geom_violin(aes(fill = as.factor(Dec)), draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_boxplot(aes(fill = as.factor(Dec)))

ggplot(IS_snags, mapping = aes(x=Dec, y=dbh)) +
  geom_violin(aes(fill = as.factor(Dec)), draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_boxplot(aes(fill = as.factor(Dec)))

OH_t0 <- as.vector(c(27.00, 49.00,5.00, 12.00,30.00, 10.00))
# tm1 <- matrix(c(0,0,0,0,0,0,
#               0.19,0.16,0,0,0,0,
#               0.39,0.35,0.65,0,0,0,
#               0,0,0,0,0,0,
#               0.14,0.16,0,0.33,0.32,0,
#               0.28,0.33,0.35,0.67,0.68,0.5), nrow = 6, ncol = 6, byrow = TRUE)

R <- as.vector(c(23,67,84,0,0,0))

D <- matrix(c(0.00,	0.00,	0.00,	0.00,	0.00,	0.00,
              0.19,	0.16,	0.00,	0.00,	0.00,	0.00,
              0.39,	0.35,	0.65,	0.00,	0.00,	0.00,
              0.00,	0.00,	0.00,	0.00,	0.00,	0.00, # I have qualms about this; shouldn't S1 --> L1? # no it's ok because L1s can just be assumed live as of 4ya
              0.14,	0.16,	0.00,	0.33,	0.32,	0.00,
              0.28,	0.33,	0.35,	0.67,	0.68,	0.50), nrow = 6, ncol = 6, byrow = TRUE)

categories <- c("S1", "S2S3", "S4S5", "L1", "L2L3","L4L5")

# (D %*% OH_t0) + R
# (D %*% ((D %*% OH_t0) + R)) + R

OH_tn <- vector("list",20)
OH_tn[[1]] <- as.vector(c(27.00, 49.00,5.00, 12.00,30.00, 10.00))

for (i in 2:(length(OH_tn))) {
  OH_tnext <- list((D %*% unlist(OH_tn[i-1])) + R)
  OH_tn[i] <- OH_tnext
}

SAD <- as.vector(unlist(OH_tn[[20]]))

# now use D, R, and SAD to calculate backward path probabilities
# in excel this looked like making a table of the ways you get from one stage to another,
# then turning each # into a proportion of the total trees in that stage

# making a table of the ways you get from one stage to another,
N_per <- matrix(nrow=6, ncol=6)
for (i in 1:nrow(N_per)) {
  N_per[,i] <- SAD[i] * D[,i]
}
colnames(N_per) <- categories
rownames(N_per) <- categories

#       S1     S2S3     S4S5 L1      L2L3     L4L5
# S1   0.00  0.00000   0.0000  0  0.000000   0.0000
# S2S3 4.37 13.59429   0.0000  0  0.000000   0.0000
# S4S5 8.97 29.73750 227.8187  0  0.000000   0.0000
# L1   0.00  0.00000   0.0000  0  0.000000   0.0000
# L2L3 3.22 13.59429   0.0000  0  7.912605   0.0000
# L4L5 6.44 28.03821 122.6716  0 16.814286 173.8808

# then turning each # into a proportion of the total trees that came from that stage

pct_per <- N_per/SAD
# add column for % recruits
pct_per <- cbind(pct_per, (R/SAD))
pct_per[4,] <- 0
colnames(pct_per)[7] <- "R"

# round(pct_per,3)
#          S1  S2S3  S4S5 L1  L2L3 L4L5    R   
# S1   0.000 0.000 0.000  0 0.000  0.0 1.000
# S2S3 0.051 0.160 0.000  0 0.000  0.0 0.789
# S4S5 0.026 0.085 0.650  0 0.000  0.0 0.240
# L1   0.000 0.000 0.000  0 0.000  0.0 0.000
# L2L3 0.130 0.550 0.000  0 0.320  0.0 0.000
# L4L5 0.019 0.081 0.353  0 0.048  0.5 0.000

# This table shows the percent of trees in [row] that were [column] one timestep before. E.g. 65% of the S4S5s in t0 were S4S5s in t-1, and 4.71% of the L4L5s in t0 were L2L3 in t-1
# use this to create backwards paths for one decay class (S2S3s was test case)
# then do for one class at a time in an iterable way (S4S5 as test)

# make that a loop!

t0_bycat <- vector("list", 6)
for (i in 1:length(t0_bycat)){ # prep the starting vectors specific to each category (0s for other cats)
  t0_bycat[[i]] <- ifelse(OH_t0 == OH_t0[i], OH_t0[i],0)}

catprob <- vector("list", length(t0_bycat))
recruits <- vector()
# loop below starts at i=2 because the 0s in S1 break the loop
for (i in 2:length(t0_bycat)){ # for each category, get number of timesteps to run out of trees
  j = 1 
  prev_distn <- t0_bycat[[i]]*pct_per # starting vector * transition matrix for t-1
  start_vec <- colSums((prev_distn)[,c(1:6)]) # specify start_vec outside the while loop
  while(sum(start_vec >= 0.5)>=1){ # while at least one decay class at t-n has tree # >= 0.5, do this
        start_vec <- colSums((prev_distn)[,c(1:6)]) # finds start vector of previous timestep
        recruits[j] <- sum(prev_distn[,7]) # finds # recruited into the previous timestep
        prev_distn <- start_vec*pct_per
        j=j+1  } # advance to next iteration: one more timestep backwards
  catprob[[i]] <- data.frame(recruits, c(1:length(recruits))) # store recruitment totals per timestep
  names(catprob[[i]]) <- c("recruits","timesteps")
  catprob[[i]] <- catprob[[i]] %>% 
    mutate(years = 5*timesteps) %>% # convert timesteps to years
    mutate(prob = recruits/sum(catprob[[i]]$recruits))  # calculate probability of each # timesteps
  }
# have to update with S1 info since they're all 0s and break the loop
catprob[[1]] <- data.frame(sum((t0_bycat[[1]]*pct_per)[,7]), 1, 5, 1)
names(catprob[[1]]) <- c("recruits", "timesteps", "years", "prob")

# L1s also have 0 transition probability, AND 0 recruitment. Assign all these time-since-death of 4y
catprob[[4]] <- catprob[[4]][1,]
catprob[[4]]$recruits <- 0
catprob[[4]]$timesteps <- 0.8
catprob[[4]]$years <- 4
catprob[[4]]$prob <- 1

# plot(catprob[[1]]$years,catprob[[1]]$prob)
# plot(catprob[[2]]$years,catprob[[2]]$prob)
# plot(catprob[[3]]$years,catprob[[3]]$prob)
# plot(catprob[[4]]$years,catprob[[4]]$prob)
# plot(catprob[[5]]$years,catprob[[5]]$prob)
# plot(catprob[[6]]$years,catprob[[6]]$prob)

# use these distributions to assign time-since-death (dec_correction) for logs and snags
# test on df and df2 (OH snags and logs, minus Dec = 1)

catdistn <- list()
for(i in c(2,3,5,6)){
  catdistn[[i]] <- sample(seq(5,max(catprob[[i]]$years),5),1000,replace=TRUE,prob=catprob[[i]]$prob)
}

# now I need to assign a random draw from this catdistn[[i]] to each snag in class i
df %>% 
  mutate(S2S3_corr = sample(unlist(catdistn[[2]]),nrow(df), replace = TRUE)) %>% 
  mutate(S4S5_corr = sample(unlist(catdistn[[3]]),nrow(df), replace = TRUE)) %>% 
  mutate(L2L3_corr = sample(unlist(catdistn[[5]]),nrow(df), replace = TRUE)) %>% 
  mutate(L4L5_corr = sample(unlist(catdistn[[6]]),nrow(df), replace = TRUE)) %>% 
  mutate(snag_corr = case_when(Dec==1 ~ 5,
                               Dec==2 | Dec==3 ~ S2S3_corr,
                               Dec==4 | Dec==5 ~ S4S5_corr)) %>%
  mutate(estab_est2 = round(2018 - (age_est+snag_corr),0)) #%>% 
  #mutate(diff = estab_est - estab_est2)

# do the same for logs
df2 %>% 
  mutate(S2S3_corr = sample(unlist(catdistn[[2]]),nrow(df2))) %>% 
  mutate(S4S5_corr = sample(unlist(catdistn[[3]]),nrow(df2))) %>% 
  mutate(L2L3_corr = sample(unlist(catdistn[[5]]),nrow(df2))) %>% 
  mutate(L4L5_corr = sample(unlist(catdistn[[6]]),nrow(df2))) %>% 
  mutate(log_corr = case_when(Dec==1 ~ 4,
                               Dec==2 | Dec==3 ~ L2L3_corr,
                               Dec==4 | Dec==5 ~ L4L5_corr)) %>%
  mutate(estab_est2 = round(2018 - (age_est+log_corr),0)) %>% 
  mutate(diff = estab_est - estab_est2)
    




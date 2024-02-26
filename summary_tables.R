# 2/18/24
# making summary tables code for Ch 1
# start with Derek C's functions above, see if I can modify for gap area function

# goal is a table with 5 rows (one per bin) and 6 columns (for now--IS 1941, OH 1941, 1941_all; IS_2018, OH_2018, 2018_all)
# within each bin's row-grouping, 6 metrics: 

# n.clust.bin = number of clusters per ha (bin tally per bin / total ha considered)
# p.trees.bin = % of total trees (tree tally per bin/total trees)
# mDBH.bin = mean DBH (will probably drop for all except singletons) (average dbh for all trees in this bin)
# ba.bin = BA (m2/ha) (sum BA from this bin and divide by total ha considered (3 or 6))
# tph.bin = stem density (stems/ha) (tree tally per bin/total ha in that bin)
# p.area.bin = proportion of plot area

# stand-level info will include 
# TPH = TPH
# BAH = BAH
# meanDBH = mean dbh
# tpclust.stand = average trees per cluster
# maxtpclust.stand = max trees per cluster
# gaps.ha.stand = gaps per ha

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
# make a function (of indices, bin)

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

# looping over indices and then do once for each bin 
IS41 <- c(2,4,6)
OH41 <- c(8,10,12)
all41 <- c(IS41, OH41)
IS18 <- c(1,3,5)
OH18 <- c(7,9,11)
all18 <- c(IS18, OH18)

mets_names <- c("n.clust.bin",  "p.trees.bin",  "mDBH.bin", "ba.bin",  "tph.bin",  "p.area.bin", 
                "n.clust.bin.sd", "p.trees.bin.sd", "mDBH.bin.sd", "ba.bin.sd", "tph.bin.sd", "p.area.bin.sd")

           

indices_list <- list(IS41, OH41, all41, IS18, OH18, all18)
summ.bins1 <- data.frame(matrix(0,length(indices_list),12))
for (i in 1:length(indices_list)){
  summ.bins1[i,] <- summ.bin(indices_list[[i]],1)
  colnames(summ.bins1) <- paste0(mets_names, 1)
} # now it's a df with 6 rows (one per index / plot grouping) and 6 columns (one per metric)

summ.bins2 <- data.frame(matrix(0,length(indices_list),12))
for (i in 1:length(indices_list)){
  summ.bins2[i,] <- summ.bin(indices_list[[i]],2)
  colnames(summ.bins2) <- paste0(mets_names, 2)
}

summ.bins3 <- data.frame(matrix(0,length(indices_list),12))
for (i in 1:length(indices_list)){
  summ.bins3[i,] <- summ.bin(indices_list[[i]],3)
  colnames(summ.bins3) <- paste0(mets_names, 3)
}

summ.bins4 <- data.frame(matrix(0,length(indices_list),12))
for (i in 1:length(indices_list)){
  summ.bins4[i,] <- summ.bin(indices_list[[i]],4)
  colnames(summ.bins4) <- paste0(mets_names, 4)
}

t(summ.bins1) # gives the per-bin chunk of the results table!! 
#rbind(t(summ.bins1)[1:6,], t(summ.bins1)[7:12,])
#cbind(t(summ.bins1)[1:6,], t(summ.bins1)[7:12,])[,c(1,7,2,8,3,9,4,10,5,11,6,12)] 

#summary_table <- rbind(t(summ.bins1), t(summ.bins2), t(summ.bins3), t(summ.bins4))

summary_table <- rbind( cbind(t(summ.bins1)[1:6,], t(summ.bins1)[7:12,])[,c(1,7,2,8,3,9,4,10,5,11,6,12)],
                        cbind(t(summ.bins2)[1:6,], t(summ.bins2)[7:12,])[,c(1,7,2,8,3,9,4,10,5,11,6,12)],
                        cbind(t(summ.bins3)[1:6,], t(summ.bins3)[7:12,])[,c(1,7,2,8,3,9,4,10,5,11,6,12)],
                        cbind(t(summ.bins4)[1:6,], t(summ.bins4)[7:12,])[,c(1,7,2,8,3,9,4,10,5,11,6,12)])

mets_names_final <- c("IS41", "IS41sd", "OH41", "OH41sd", "all41", "all41sd", "IS18", "IS18sd", "OH18", "OH18sd", "all18", "all18sd")
colnames(summary_table) <- mets_names_final

## stand-level metrics

summ.stand <- function(indices) {
  
# TPH for the whole stand
tph <- lapply(indices, function(i) { # tph.stand
  trees.stand <- plots_out[[i]][[11]] 
  return(nrow(trees.stand)) })
tph.stand <- round(mean(unlist(tph)),1) # 120
tph.stand.sd <- round(sd(unlist(tph)),1) # sd = 10.5

# BAH for the whole stand
ba <- lapply(indices, function(i) { # ba.stand
  clusters.stand <- plots_out[[i]][[12]]
  ba <- clusters.stand[,2]
  return(ba) })
ba.stand <- round(mean(unlist(lapply(c(1:length(indices)), function(x) sum(ba[[x]])))),1) # 26.8 m2/ha
ba.stand.sd <- round(sd(unlist(lapply(c(1:length(indices)), function(x) sum(ba[[x]])))),1) # sd = 1.4

# meanDBH for the whole stand
mDBH <- lapply(indices, function(i) { # mDBH.bin
  trees.stand <- plots_out[[i]][[11]]
  dbh <- trees.stand[,3]
  return(dbh) })
mDBH.stand <- round(mean(unlist(lapply(c(1:length(indices)), function(x) mean(mDBH[[x]])))),1) # 47.4 cm mean DBH
mDBH.stand.sd <- round(sd(unlist(lapply(c(1:length(indices)), function(x) mean(mDBH[[x]])))),1) # sd = 2.8

# avg trees/cluster for the whole stand
tpc <- lapply(indices, function(i) { # tpc.stand
  clusters.stand <- plots_out[[i]][[12]]
  tpc <- clusters.stand[,1]
  return(tpc)})
mean.tpc <- round(mean(unlist(lapply(c(1:length(indices)), function(x) mean(tpc[[x]])))),1)
mean.tpc.sd <- round(sd(unlist(lapply(c(1:length(indices)), function(x) mean(tpc[[x]])))),1)

# max trees/cluster for the whole stand
max.tpc <- round(mean(unlist(lapply(c(1:length(indices)), function(x) max(tpc[[x]])))),1)
max.tpc.sd <- round(sd(unlist(lapply(c(1:length(indices)), function(x) max(tpc[[x]])))),1)

# avg gaps/ha for the whole stand
opes.ha <- lapply(indices, function(i){
  opes.ha <- nrow(opes_sr[[i]]) })
avg.opes <- round(mean(unlist(opes.ha)),1)
avg.opes.sd <- round(sd(unlist(opes.ha)),1)

output <- c(tph.stand,  ba.stand,  mDBH.stand,  mean.tpc,  max.tpc,  avg.opes, 
            paste0("(", c(tph.stand.sd,  ba.stand.sd,  mDBH.stand.sd,  mean.tpc.sd,  max.tpc.sd,  avg.opes.sd),")") ) 

 return(output)    } 

# summ.stand(c(1,3,5))

summ.stands <- data.frame(matrix(0,length(indices_list),12))
for (i in 1:length(indices_list)){
  summ.stands[i,] <- summ.stand(indices_list[[i]])
 # colnames(summ.stands) <- paste0(mets_names, 1)
}
summ.stands <- cbind(t(summ.stands)[1:6,], t(summ.stands)[7:12,])[,c(1,7,2,8,3,9,4,10,5,11,6,12)]
colnames(summ.stands) <- mets_names_final
rownames(summ.stands) <- c("tph.stand",  "ba.stand",  "mDBH.stand",  "mean.tpc",  "max.tpc",  "avg.opes")


summ.all <- rbind(summary_table, summ.stands)

# summary_table <- replace(summary_table, summary_table == "NaN", "-")
# c("n.clust.bin", "n.clust.bin.sd", "p.trees.bin", "p.trees.bin.sd", "mDBH.bin", "mDBH.bin.sd",
# "ba.bin",  "ba.bin.sd", "tph.bin", "tph.bin.sd", "p.area.bin", "p.area.bin.sd")
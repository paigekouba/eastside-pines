##################################################################################
##
##      Code to process generic stemmap plots
##   		March 2017  Derek Churchill
##  
###     Produces ICO tables & sets up for plotting etc. 
### 
################################################################################


# need to run these functions
#source("Plotkin_Cluster_Crown.R")
# PK: fns from EastPinesICO_edgecorr

### Runs a set of stemmaped plots

##  vector with plot names
# All.plots
# PK: names

# List with a dataframe for each plot x,y,dbh, species, plus clumpID if Quickmap
# All.list.cor
# PK: plots

# vector of T or F for each plot:  T if plot is a quickmap,  F if its a traditional stemmap
#QM = ("F","F",..............)


# matrix with 4 edge cut values for each plot. Edge cut is the buffer you want to come in off the edges. See Churchill 2013 for more on this. 

#edge.ct = matrix (0,length(All.plots),4) # use this if no edge cut
# PK: don't need, only appears within summarizeClusters below

#############################################
###  Run Stemmaps & Quickmaps through cluster function 
# Stem.res = list()
# summary.mets = matrix(0,length(All.plots),12)
# ICO.tr.pct = ICO.ba.pct = matrix(0,length(All.plots),200) 
# can.cov = vector()
# 
# for (w in 1:length(All.plots)){  
#   # Prep input for cluster function & run through function 
#   in.plot = All.list.cor[[w]]
#   
#   clust.in = formatSummaryInput(in.plot, x.min=0, x.max=max(in.plot[,1]), y.min=0, y.max=max(in.plot[,2]), dbh.units="cm")
#   #edge.cut = as.numeric(edge.ct[1,c(6:9)]) ## non-buffer tree method, boundaries are extent of non-buf trees
#   edge.cut = as.numeric(edge.ct[w,c(1:4)]) ## buffer tree method, bndries are extent of buffer trees
#   
#   # Calc canopy cover
#   #can.cov[w] = can.cover.calc(clust.in$pointData,clust.in$treeData,edge.cut=edge.cut)
#   
#   # Run through cluster function
#   Stem.res[[w]] = summarizeClusters.ppp (clust.in$pointData, clust.in$treeData, distThreshold=6, max.bin=-1,edge.cut =edge.cut, Quickmap = QM[w])
# }  
# PK: don't need this, ran cluster function in EastPinesICO_edgecorr


## Sum up and store results
#Plot.names=All.plots
Plot.names=names
n.plots = length(Plot.names)

gap.hold=list()

F.breaks = prop.open = vector()
F.curve = matrix(0,n.plots,1000)
F.bins =  matrix(0,n.plots,9)

nn.hist = matrix(0,n.plots,60)
Mean.clust = vector()
summary.mets = matrix(0,length(Plot.names),12)
ICO.tr.pct = ICO.ba.pct = matrix(0,length(Plot.names),700) 
Clump.bins = as.data.frame(matrix(0,n.plots,18))
gap.size = as.data.frame(matrix(0,n.plots,7))

##
for (w in 1:n.plots)  {
  #Clump metrics
  print(w)
  
  summary.mets[w,] = plots_out[[w]]$summary
  ICO.tr.pct[w,] =  c(plots_out[[w]]$maxbin[,7],rep(0,700-length(plots_out[[w]]$maxbin[,7]))) 
  ICO.ba.pct[w,] =  c(plots_out[[w]]$maxbin[,8],rep(0,700-length(plots_out[[w]]$maxbin[,8])))
  Mean.clust[w]= plots_out[[w]]$mean.clust.size 
  Clump.bins[w,] = c(plots_out[[w]]$clump.bins[,1],plots_out[[w]]$clump.bins[,2],plots_out[[w]]$clump.bins[,3])
  nn.hist[w,] = hist(nndist(plots_out[[w]]$pointData)*3.28084,plot=F,breaks=seq(0,300,5))$counts/sum(hist(nndist(plots_out[[w]]$pointData)*3.28084,plot=F,breaks=seq(0,300,5))$counts)
# 1m = 3.28084ft  
  
  # ##  Run F test & openings
   F.hold = open.DistM.calc(plots_out[[w]]$points.noedge,Plot.name = Plot.names[w] ,eps = .5,f.break=9)
   F.bins[w,] = F.hold$F.bin[,2]
   F.curve[w,] = c(F.hold$F.cml[,2],rep(0,1000-length(F.hold$F.cml[,2])))
   F.breaks[w]= F.hold$f.break

  # Run openings
  # gap.hold[[w]] =gap.quant(plots_out[[w]]$points.noedge,name=Plot.names[w],edge.buf=6,gap.thresh = 9, open.breaks=c(0.04, 0.08, 0.2, 0.4, 0.8, 1.6, 100))
  # prop.open[w]= gap.hold[[w]][[5]]$Prop.open
  # gap.size[w,] = gap.hold[[w]][[2]]
}


## Set up results tables

colnames(summary.mets)=colnames(plots_out[[w]]$summary)

# # If run openings
# summary.mets=data.frame(summary.mets,prop.open)
# colnames(summary.mets)=c(colnames(plots_out[[w]]$summary),"PropOpen")
# colnames(gap.size)=c(0.04, 0.08, 0.2, 0.4, 0.8, 1.6, 100)
# rownames(gap.size)=Plot.names

#
Pattern.df = data.frame(Plot.names,TPA=summary.mets[,"TPA"],Mean.clust,F.breaks)
rownames(summary.mets)=Plot.names
rownames(ICO.tr.pct)=Plot.names
rownames(ICO.ba.pct)=Plot.names

colnames(F.bins) = c(3,6,   9,   12,  15,  18,  21,  24,  "24+")
rownames(F.bins) = Plot.names
colnames(F.curve) = seq(0,99.9,0.1)
rownames(F.curve) = Plot.names
rownames(nn.hist) = Plot.names
rownames(Clump.bins) = Plot.names

colnames(Clump.bins) = rep(rownames(plots_out[[w]]$clump.bins),3)


# 
# # Store
place.name = "EastSide"
#setwd("C:/Users/Derek/Dropbox/ICO/Reference_sites/aaReference summary data/Blues_envelopes")

# write.csv(F.curve,paste(place.name,"_FCurves.csv",sep=""))
# write.csv(F.bins,paste(place.name,"_FBins.csv",sep=""))
# write.csv(summary.mets,paste(place.name,"_summary_metrics.csv",sep=""))
# write.csv(ICO.tr.pct,paste(place.name,"_Cluster_tpa_GT15_6m.csv",sep=""))
# write.csv(ICO.ba.pct,paste(place.name,"_Cluster_ba_GT15_6m.csv",sep=""))
# write.csv(Pattern.df,paste(place.name,"_TPA_AvgClus_F9.csv",sep=""))
# write.csv(Clump.bins,paste(place.name,"_ClumpBins.csv",sep=""))
# # write.csv(gap.size,paste(place.name,"_gapSize.csv",sep=""))  

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
round(sum(unlist(selected_data))/n.ha,1) # 17.7 small clusters at IS in 2018

selected_data <- lapply(indices, function(i) { # p.trees.bin
  trees.bin <- plots_out[[i]][[11]] %>% 
    filter(bin == "2-4")
  trees.ne <- plots_out[[i]][[11]] 
  return(list(nrow(trees.bin), nrow(trees.ne))) })
round(sum(unlist(lapply(c(1:length(indices)), function(x) selected_data[[x]][[1]])))*100/sum(unlist(lapply(c(1:length(indices)), function(x) selected_data[[x]][[2]]))),1) # 45.8%

selected_data <- lapply(indices, function(i) { # mDBH.bin
  trees.bin <- plots_out[[i]][[11]] %>% 
    filter(bin == "2-4")
  dbh <- trees.bin[,3]
  return(dbh) })
round(mean(unlist(selected_data)),1) # 51.4 cm mean DBH

selected_data <- lapply(indices, function(i) { # ba.bin
  clusters <- plots_out[[i]][[12]] %>% 
    filter(bin == "2-4")
  ba <- clusters[,2]
  return(ba) })
round(sum(unlist(selected_data)/n.ha),1) # 13.6 m2/ha

selected_data <- lapply(indices, function(i) { # tph.bin
  trees.bin <- plots_out[[i]][[11]] %>% 
    filter(bin == "2-4")
  return(nrow(trees.bin)) })
round(sum(unlist(selected_data))/((sum(piebins[indices,4])/10000)),1) # 515.4

round(sum(piebins[indices,4])*100/(10000*n.ha),1) # 10.7% p.area.bin

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
n.clust.bin <- round(sum(unlist(n.clust))/length(indices),1) # 17.7 small clusters/ha at IS in 2018

p.trees <- lapply(indices, function(i) { # p.trees.bin
  trees.bin <- plots_out[[i]][[11]] %>% # for every plot in the list "indices", take the 11th element (trees.noedge)
    filter(bin == bin_names[bin.i]) # filter for bin size 
  trees.ne <- plots_out[[i]][[11]] # save all the info in trees.noedge
  return(list(nrow(trees.bin), nrow(trees.ne))) }) # gives a list of the trees in this bin, followed by the total trees, for each plot in (indices)
p.trees.bin <- round(sum(unlist(lapply(c(1:length(indices)), function(x) p.trees[[x]][[1]])))*100/sum(unlist(lapply(c(1:length(indices)), function(x) p.trees[[x]][[2]]))),1) # 45.8% of trees in "indices" are in bin 2-4

mDBH <- lapply(indices, function(i) { # mDBH.bin
  trees.bin <- plots_out[[i]][[11]] %>% 
    filter(bin == bin_names[bin.i])
  dbh <- trees.bin[,3]
  return(dbh) })
mDBH.bin <- round(mean(unlist(mDBH), na.rm=TRUE),1) # 51.4 cm mean DBH

ba <- lapply(indices, function(i) { # ba.bin
  clusters <- plots_out[[i]][[12]] %>% 
    filter(bin == bin_names[bin.i])
  ba <- clusters[,2]
  return(ba) })
ba.bin <- round(sum(unlist(ba)/length(indices)),1) # 13.6 m2/ha

tph <- lapply(indices, function(i) { # tph.bin
  trees.bin <- plots_out[[i]][[11]] %>% 
    filter(bin == bin_names[bin.i])
  return(nrow(trees.bin)) })
tph.bin <- round(sum(unlist(tph), na.rm=TRUE)/((sum(piebins[indices,(bin.i+2)])/10000)),1) # 515.4

p.area.bin <- round(sum(piebins[indices,(bin.i+2)])*100/(10000*length(indices)),1) # 10.7% p.area.bin

output <- c(n.clust.bin, p.trees.bin, mDBH.bin, ba.bin, tph.bin, p.area.bin)

return(output)
}

# need to loop this function over bin size
# summ.bins <- data.frame(matrix(0,length(bin_names),6))
# for (i in 1:length(bin_names)){
#   summ.bins[i,] <- summ.bin(indices,i)
# }
# this gives a df with 6 columns (one per metric) and 4 rows (one per bin)
# want to run this for all 6 plot groups (IS41, OH41, all41; IS18, OH18, all18)

# actually, try looping over indices and then do once for each bin ?
IS41 <- c(2,4,6)
OH41 <- c(8,10,12)
all41 <- c(IS41, OH41)
IS18 <- c(1,3,5)
OH18 <- c(7,9,11)
all18 <- c(IS18, OH18)

mets_names <- c("n.clust.bin", "p.trees.bin", "mDBH.bin", "ba.bin", "tph.bin", "p.area.bin")

indices_list <- list(IS41, OH41, all41, IS18, OH18, all18)
summ.bins1 <- data.frame(matrix(0,length(indices_list),6))
for (i in 1:length(indices_list)){
  summ.bins1[i,] <- summ.bin(indices_list[[i]],1)
  colnames(summ.bins1) <- paste0(mets_names, 1)
} # now it's a df with 6 rows (one per index / plot grouping) and 6 columns (one per metric)

summ.bins2 <- data.frame(matrix(0,length(indices_list),6))
for (i in 1:length(indices_list)){
  summ.bins2[i,] <- summ.bin(indices_list[[i]],2)
  colnames(summ.bins2) <- paste0(mets_names, 2)
}

summ.bins3 <- data.frame(matrix(0,length(indices_list),6))
for (i in 1:length(indices_list)){
  summ.bins3[i,] <- summ.bin(indices_list[[i]],3)
  colnames(summ.bins3) <- paste0(mets_names, 3)
}

summ.bins4 <- data.frame(matrix(0,length(indices_list),6))
for (i in 1:length(indices_list)){
  summ.bins4[i,] <- summ.bin(indices_list[[i]],4)
  colnames(summ.bins4) <- paste0(mets_names, 4)
}

t(summ.bins1) # gives the per-bin chunk of the results table!! 

summary_table <- rbind(t(summ.bins1), t(summ.bins2), t(summ.bins3), t(summ.bins4))
# summary_table <- replace(summary_table, summary_table == "NaN", "-")

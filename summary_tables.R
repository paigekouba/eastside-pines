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
# missing open.DistM.calc custom function. Try without

  # Run openings
  # gap.hold[[w]] =gap.quant(plots_out[[w]]$points.noedge,name=Plot.names[w],edge.buf=6,gap.thresh = 9, open.breaks=c(0.04, 0.08, 0.2, 0.4, 0.8, 1.6, 100))
  # prop.open[w]= gap.hold[[w]][[5]]$Prop.open
  # gap.size[w,] = gap.hold[[w]][[2]]
}
# Error in open.DistM.calc(plots_out[[w]]$points.noedge, Plot.name = Plot.names[w],  : 
#                            could not find function "open.DistM.calc"
# stuck here until this function shows up...

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

write.csv(F.curve,paste(place.name,"_FCurves.csv",sep=""))
write.csv(F.bins,paste(place.name,"_FBins.csv",sep=""))
write.csv(summary.mets,paste(place.name,"_summary_metrics.csv",sep=""))
write.csv(ICO.tr.pct,paste(place.name,"_Cluster_tpa_GT15_6m.csv",sep=""))
write.csv(ICO.ba.pct,paste(place.name,"_Cluster_ba_GT15_6m.csv",sep=""))
write.csv(Pattern.df,paste(place.name,"_TPA_AvgClus_F9.csv",sep=""))
write.csv(Clump.bins,paste(place.name,"_ClumpBins.csv",sep=""))
# write.csv(gap.size,paste(place.name,"_gapSize.csv",sep=""))  

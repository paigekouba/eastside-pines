# Using D Churchill's ICO code, minus extras and in runnable order
# 8/2/23

# Want to end up with a for loop that runs below functions for all 12 plots:
# IS1_2018, IS2_2018, IS3_2018; IS1_1941, IS2_1941, IS3_1941; 
# OH1_2018, OH2_2018, OH3_2018; OH1_1941, OH2_1941, OH3_1941
plot.data <- IS1_2018 # for testing code
# data.frame or matrix with column headers x, y, spp, dbh in any order
#               Can also have crown radius, tree tag, but not essential 
#               +  clump Id (Quickmap plots)
#              Any additional columns will be ignored, but passed on

pointData <- ppp(plot.data$X, plot.data$Y, window = disc(radius = sqrt(10000/pi)+0.65, centre = c(0,0)), 
                 marks = as.numeric(rownames(plot.data) ))
# IS1_2018 gets a warning: 1 point was rejected as lying outside the specified window, even after filtering tree_data at start
# But I can't find any x or y coord that lies outside of the window with radius = sqrt(10000/pi)
# Happens for IS1_2018 (1 pt), OH2_2018 (1 pt), and OH3_2018 gets THREE rejected
# For 1941 plots: IS1_1941 (1), OH1_1941 (1), OH2_1941 (1), OH3_1941 (2)
#attr(pointData, "rejects")
# My best guess is the circular window is an approximation, and points close to the edge are cut out by the polygon
# adding 0.65 to the radius seems to keep all points.

treeData <- data.frame(dbh=plot.data[,"dbh"], spp=plot.data$Spec, Tree.ID=as.numeric(rownames(plot.data))) 

# Inputs:
#   
#   Note that the format function above outputs pointData & treeData inputs for this function
#   pointData: ppp object containing xy data, window, and marks that match
#              the row names of treeData
#   treeData:  data.frame with columns named dbh, spp, and crown and row names
#              matching the marks of pointData.
#              If crown is not present, it will run crown prediction model	 
#							 Also, crown is not necessary if distThreshold is specified.
#							  
#   distThreshold: If set, maximum distance between trees in a clump.
#                  If left -1, crown radii are used.
#   max.bin: All clumps with n >= max.bin will be binned together for summary purposes.
#            Leave as -1 to disable.
#		edge.cut is a vector with distances that the plot should be cut for edge effects in this order: xlow,xhigh,ylow,yhigh
#						Default is set to zero with no edge correction. Functions as if there is no edge cut. 	
#   Quickmap = T if data are from a quickmap


###################
##  Functions to predict crown diameter
# returns other other columns past species and dbh # need to change
Crw.rad.pred = function(data,cr.coefs=-1){
  
  ## get dbh and species columns
  colnames(data)[grep("(dbh|dia)",colnames(data),ignore.case=T)] = "dbh"
  dbh = data[,which(colnames(data)=="dbh")]
  
  # Sort out species codes
  Species = as.character(data[,grep("Sp",colnames(data),ignore.case=T)])
  Species[grep("P|p",Species,fixed=T)]="PP"  # fixed=T restricts to only p
  Species[grep("PP|PIPO|PIJE|PICO|PIJE*",Species,ignore.case=T)]="PP"
  Species[grep("G|GF|ABGR|ABCO|WF",Species,ignore.case=T)]="GF"
  Species[grep("RC|THPL|JUGR",Species,ignore.case=T)]="RC"
  
  cr.rad = 	rep(0,length(dbh))
  
  ## set up default coefs from East WA FIA & CVS Data
  pp.coef =c( -1.3992691, 0.6416915)
  gf.coef = c(  -0.882310, 0.526169)
  rc.coef = c(  -0.436856, 0.426431)

  coefs = data.frame(pp.coef,gf.coef,rc.coef); rownames(coefs)=c("Intcpt","log(dia)")
  if (cr.coefs == -1) (cr.coefs = coefs)
  
  ## run models
  cr.rad[which(Species=="PP")] = exp(cr.coefs[1,1] + cr.coefs[2,1]*log(dbh[which(Species=="PP")]))
  cr.rad[which(Species=="GF")] = exp(cr.coefs[1,2] + cr.coefs[2,2]*log(dbh[which(Species=="GF")]))  
  cr.rad[which(Species=="RC")] = exp(cr.coefs[1,3] + cr.coefs[2,3]*log(dbh[which(Species=="RC")]))
  
  # Catch all for species with no coeff. PP as default
  cr.rad[which(cr.rad==0)] = exp(cr.coefs[1,1] + cr.coefs[2,1]*log(dbh[which(cr.rad==0)]))		
  
  # Output
  return(res=data.frame(data,crown = cr.rad))
}

###################
##  gini coefficient code:  
##   input is dbh list in cm
gini = function(dbh.vec){
  bah = (dbh.vec/200)^2*pi
  n = length(dbh.vec)
  rank = c(1:n)
  if (n==1) (ginii = -1) else (ginii = sum(((2*rank-n-1)*sort(bah))) / sum(sort(bah)*(n-1))) 
  return (ginii)}

###################
clusterByCrown <- function(pointData, treeData, distThreshold=-1)
{
  IDs <- rownames(treeData)
  radii <- ifelse(rep(distThreshold == -1, nrow(treeData)), treeData$crown, rep(distThreshold / 2, nrow(treeData)))
  
  # Make the output list big enough for the theoretical case that every point is a singleton
  cluster.list <- vector(mode="list", length=length(IDs))
  names(cluster.list) <- paste("clust", 1:length(IDs), sep="")
  
  distMat <- as.matrix(dist(matrix(c(pointData$x, pointData$y), ncol=2, dimnames=list(IDs))))
  distMat <- t(apply(distMat, 1, function(x) x - radii))
  distMat <- apply(distMat, 2, function(x) x - radii)
  
  # Set aside the onesies
  singles <- which((nrow(distMat) - apply(distMat, 2, function(x) sum(x > 0))) == 1)
  if(length(singles) > 0)
  {
    cluster.list[1:length(singles)] <- colnames(distMat)[singles]
    distMat <- distMat[-singles,-singles]
    IDs <- IDs[-singles]
  }
  
  # Initializing variables outside of the loop helps speed things up
  index <- length(singles) + 1
  vec <- vector()
  len1 <- len2 <- 0
  
  # Iterate until we've dealt with every ID
  while(length(IDs) > 1)
  {
    # Since we're removing data as we determine clumps, we want the first remaining value every time
    vec <- which(distMat[,1] <= 0)
    
    len1  <-1
    len2 <- 2
    while(len1 < len2)
    {
      len1 <- length(vec)
      vec  <- c(vec, unlist(apply(distMat[,vec] <= rep(0, nrow(distMat)), 2, which)))
      vec  <- vec[!duplicated(vec)]
      len2 <- length(vec)
    }
    
    # Once all points in the current cluster are accounted for, figure out which point IDs belong
    #  to the points belonging to the cluster, and remove them from the IDs list.
    IDs.in.cluster <- colnames(distMat)[sort(vec)]
    cluster.list[[index]] <- IDs.in.cluster 
    
    IDs <- IDs[-which(IDs %in% IDs.in.cluster)]
    distMat <- distMat[-which(rownames(distMat) %in% IDs.in.cluster), -which(colnames(distMat) %in% IDs.in.cluster)]
    
    index <- index + 1
  }
  
  # Remove any slots that we didn't use
  if(sum(as.integer(lapply(cluster.list, is.null))) < nrow(treeData))
    cluster.list <- cluster.list[-which(lapply(cluster.list, length) == 0)]
  
  return(cluster.list)
}


summarizeClusters.ppp <- function(pointData, treeData, distThreshold=-1, max.bin=-1,edge.cut = c(0,0,0,0), Quickmap=F,plot.name="Name"){
  # can edge.cut be changed for a circular window???
  if ("crown" %in% colnames(treeData)) (treeData=treeData) else (treeData = Crw.rad.pred(treeData))
  
  if(Quickmap == T) (cluster.list = Quickmap.list(treeData[,"Clump.ID"])) else (cluster.list <- clusterByCrown(pointData, treeData, distThreshold))
  
  cluster.size <- unlist(lapply(cluster.list, length))
  names(cluster.size)<-c()  
  
  # Make sure max bin captures. Also sets max.bin to max cluster size for  default -1 setting
  if(max(cluster.size)>max.bin)  (max.bin = max(cluster.size))
  
  # Per-tree variables
  tree.cluster.membership <- c()
  for(i in 1:length(cluster.list))  tree.cluster.membership[unlist(lapply(cluster.list[i], paste))] <- i
  tree.cluster.membership <- as.matrix(tree.cluster.membership[as.character(sort(as.integer(names(tree.cluster.membership))))])
  tree.dbh <- as.matrix(treeData$dbh)
  tree.ba  <- as.matrix(pi * (tree.dbh / 200)^2)
  tree.sdi <- as.matrix((tree.dbh / 25)^1.7)
  tree.clust.sz = cluster.size[tree.cluster.membership]
  tree.bin = lut(c("1","2-4","5-9","10-15","16-29","30+"),breaks=c(0,2,5,10,16,30,200))(tree.clust.sz)
  tree.bin = factor(tree.bin,levels=c("1","2-4","5-9","10-15","16-29","30+"),order=T)
  rownames(tree.dbh) <- rownames(tree.ba) <- rownames(tree.sdi) <- rownames(tree.cluster.membership)
  
  trees <- cbind(data.frame(x=pointData$x,y=pointData$y),as.data.frame(treeData), data.frame(ba=tree.ba, sdi=tree.sdi, clust.sz=tree.clust.sz ,cluster.membership=tree.cluster.membership,bin=tree.bin))
  
  ### Eliminate edge trees from tree list if a cut distances are provided
  noedge.trees= trees
  # SQUARE WINDOW but I don't think this will show up because edge.cut is not specified? Trying with -60,60
  x.low = -60 #pointData$window$xrange[1] + edge.cut[1] ; 
  x.high = 60 #pointData$window$xrange[2]- edge.cut[2]
  y.low = -60 #pointData$window$yrange[1] + edge.cut[3] ; 
  y.high = 60 #pointData$window$yrange[2] - edge.cut[4]
  
  ## only adjusts trees if any trees are actually out
  cut.index = which(trees$y<y.low | trees$x<x.low | trees$x> x.high | trees$y> y.high)
  
  if(length(cut.index)>0) {
    trees=  trees [-cut.index,] }
  
  
  ## reset these vectors for calcs later on
  tree.ba = trees$ba
  tree.dbh = trees$dbh
  tree.sdi = trees$sdi
  tree.clust.sz = trees$clust.sz
  tree.cluster.membership =trees$cluster.membership
  
  # recalc cluster size
  new.cl = unique(tree.cluster.membership)
  cluster.size.orig = cluster.size  # store original
  cluster.size=vector()
  #for (i in 1: length(new.cl)) cluster.size[i] = length(which(tree.cluster.membership==new.cl[i]))
  for (i in 1: length(new.cl)) cluster.size[i] = length(which(noedge.trees$cluster.membership==new.cl[i]))
  
  
  ### Adjust x.max & y.max & reset coords
  pointData.orig = 	pointData
  trees$x = trees$x -  min(trees$x)
  trees$y = trees$y - min(trees$y)
  if(nrow(trees) > 0) {
    # pointData = as.ppp(cbind(trees$x,trees$y,as.numeric(rownames(trees))),W=c(min(trees$x),max(trees$x),min(trees$y),max(trees$y))) 
    pointData = as.ppp(cbind(trees$x,trees$y,as.numeric(rownames(trees))),W=c(-Inf, Inf, -Inf, Inf))
    #trying with -60,60???
    # pointData = as.ppp(cbind(trees$x,trees$y,as.numeric(rownames(trees))),W=c(-60,60,-60,60))
    # trying with circular window ???
    #  pointData = as.ppp(cbind(trees$x,trees$y,as.numeric(rownames(trees))),W=disc(radius = 60, centre = c(0,0)))
  } else {
    pointData = as.ppp(cbind(trees$x,trees$y,as.numeric(rownames(trees))),W=c(-Inf, Inf, -Inf, Inf))
  }
  
  # General variables
  n.pts <- sum(cluster.size)
  n.clusts <- length(cluster.size)
  n.bins <- max.bin
  mean.clust.size <- sum(cluster.size^2) / n.pts
  norm.mean.clust.size <- mean.clust.size / n.pts
  hectares <- 1
  # hectares <- diff(pointData$window$xrange) * diff(pointData$window$yrange) / 10000
  # changing line above due to circular window ???
  
  # Per-cluster variables
  cluster.ba  <- rep(0, n.clusts)
  cluster.sdi <- rep(0, n.clusts)
  cluster.qmd <- rep(0, n.clusts)
  cluster.mndbh <- rep(0, n.clusts)
  cluster.lartr <- rep(0, n.clusts)
  cluster.gini = cluster.bin =cluster.mgf <- rep(0, n.clusts)
  
  ## Need to redo this to pull from noedge.trees
  for(i in 1:n.clusts){
    trees.hd = which(noedge.trees$cluster.membership==new.cl[i])
    cluster.ba[i]  <- round(sum(noedge.trees[trees.hd,"ba"]),3)
    cluster.sdi[i] <- round(sum(noedge.trees[trees.hd,"sdi"]),3)
    cluster.qmd[i] <- round(sqrt(sum(noedge.trees[trees.hd,"dbh"]^2) / cluster.size[i]),1)
    cluster.mndbh[i] <- round(mean(noedge.trees[trees.hd,"dbh"]),1)
    cluster.lartr[i] <- round(max(noedge.trees[trees.hd,"dbh"]),1)
    cluster.gini[i] = round(gini(noedge.trees[trees.hd,"dbh"]),3)
    d.class = lut(c(20,40,60,80,100,180),breaks=c(0,20,40,60,80,100,180))(noedge.trees[trees.hd,"dbh"])  
    cluster.mgf[i] = (length(unique(d.class)) - 1) /log(sum(cluster.ba[i]*10))
    cluster.bin[i] = lut(c("1","2-4","5-9","10-15","16-29","30+"),breaks=c(0,2,5,10,16,30,200))(cluster.size[i])
  }
  # Set factor order
  cluster.bin = factor(cluster.bin,levels=c("1","2-4","5-9","10-15","16-29","30+"),order=T)
  clusters <- data.frame(size=cluster.size, tot.ba=cluster.ba, tot.sdi=cluster.sdi,
                         qmd=cluster.qmd,mn.dbh= cluster.mndbh,dbh.lar=cluster.lartr,gini=cluster.gini,mrglf.indx = cluster.mgf,bin=cluster.bin)
  clusters = clusters[sort.list(clusters[,1]), ]                     
  
  # Per-bin variables
  maxbin.num.tree <- maxbin.num.clust <- rep(0, max(cluster.size))
  for(i in 1:max(cluster.size))
  {
    maxbin.num.clust[i] <- length(which(cluster.size == i))
    maxbin.num.tree[i]  <- maxbin.num.clust[i] * i
  }
  n.bins<-length(maxbin.num.clust)
  
  maxbin.clust.ba  <- rep(0, n.bins)
  maxbin.clust.sdi <- rep(0, n.bins)
  maxbin.clust.qmd <- rep(0, n.bins)
  for(i in 1:n.bins)
  {
    maxbin.clust.ba[i]  <- sum(cluster.ba[which(cluster.size == i)])
    maxbin.clust.sdi[i] <- sum(cluster.sdi[which(cluster.size == i)])
    maxbin.clust.qmd[i] <- sqrt(sum(tree.dbh[which(tree.cluster.membership %in% which(cluster.size == i))]^2) / (i * maxbin.num.clust[i]))
  }
  maxbin.clust.qmd[maxbin.num.clust == 0] <- 0
  
  maxbin.pct.clust <- (maxbin.num.clust / sum(maxbin.num.clust)) 
  maxbin.pct.tree  <- (maxbin.num.tree / sum(maxbin.num.tree)) 
  maxbin.pct.ba    <- (maxbin.clust.ba / sum(maxbin.clust.ba)) 
  maxbin.pct.sdi   <- (maxbin.clust.sdi / sum(maxbin.clust.sdi))
  
  # Trim to length max.bin if needed
  if(max(max.bin)<n.bins) {
    maxbin.num.clust  <- c(maxbin.num.clust[1:(max.bin - 1)], sum(maxbin.num.clust[max.bin:n.bins]))
    maxbin.num.tree   <- c(maxbin.num.tree[1:(max.bin - 1)],  sum(maxbin.num.tree[max.bin:n.bins]))
    maxbin.clust.ba   <- c(maxbin.clust.ba[1:(max.bin - 1)],  sum(maxbin.clust.ba[max.bin:n.bins]))
    maxbin.clust.sdi  <- c(maxbin.clust.sdi[1:(max.bin - 1)], sum(maxbin.clust.sdi[max.bin:n.bins]))
    maxbin.clust.qmd  <- c(maxbin.clust.qmd[1:(max.bin - 1)], sum(maxbin.clust.qmd[max.bin:n.bins]))
    maxbin.pct.clust  <- c(maxbin.pct.clust[1:(max.bin - 1)], sum(maxbin.pct.clust[max.bin:n.bins]))
    maxbin.pct.tree   <- c(maxbin.pct.tree[1:(max.bin - 1)],  sum(maxbin.pct.tree[max.bin:n.bins]))
    maxbin.pct.ba     <- c(maxbin.pct.ba[1:(max.bin - 1)],    sum(maxbin.pct.ba[max.bin:n.bins]))
    maxbin.pct.sdi    <- c(maxbin.pct.sdi[1:(max.bin - 1)],   sum(maxbin.pct.sdi[max.bin:n.bins]))
  }
  
  
  maxbin <- data.frame(num.clust=maxbin.num.clust, num.tree=maxbin.num.tree, tot.ba=maxbin.clust.ba, tot.sdi=maxbin.clust.sdi, qmd=maxbin.clust.qmd,
                       pct.clust=maxbin.pct.clust, pct.tree=maxbin.pct.tree, pct.ba=maxbin.pct.ba, pct.sdi=maxbin.pct.sdi)  
  
  ##Expand to max.bin if needed
  if(max(max.bin)>n.bins){ 
    zeross = matrix(0,(max(max.bin)-n.bins),ncol(maxbin));colnames(zeross)=colnames(maxbin)
    maxbin=rbind(maxbin,zeross) }
  
  ### Binned maxbin table in individuals, small clumps (2-4), med (5-9), & large(10-15) & super 16+
  #maxbin.orig = maxbin #save	
  #add.mat = matrix(0,15-nrow(maxbin),9)
  #colnames(add.mat)=colnames(maxbin)
  #maxbin = rbind(maxbin,add.mat)
  
  
  ## Add to maxbin to get right calcs
  maxbin.orig = maxbin #save
  add.mat = matrix(0,500-nrow(maxbin),9)
  colnames(add.mat)=colnames(maxbin)
  maxbin = rbind(maxbin,add.mat)
  
  pct.tree =	round(c(maxbin[1,7],sum(maxbin[2:4,7]),sum(maxbin[5:9,7]),sum(maxbin[10:15,7]),sum(maxbin[16:29,7]),sum(maxbin[30:nrow(maxbin),7])),2)
  pct.ba =	round(c(maxbin[1,8],sum(maxbin[2:4,8]),sum(maxbin[5:9,8]),sum(maxbin[10:15,8]),sum(maxbin[16:29,8]),sum(maxbin[30:nrow(maxbin),8])),2)
  # Get QMD per clump bin
  ba.bin.sum = c(maxbin[1,3],sum(maxbin[2:4,3]),sum(maxbin[5:9,3]),sum(maxbin[10:15,3]),sum(maxbin[16:29,3]),sum(maxbin[30:nrow(maxbin),3]))
  tpa.bin.sum =  c(maxbin[1,2],sum(maxbin[2:4,2]),sum(maxbin[5:9,2]),sum(maxbin[10:15,2]),sum(maxbin[16:29,2]),sum(maxbin[30:nrow(maxbin),2]))
  qmd.bin =  round((((ba.bin.sum/tpa.bin.sum)/pi)^.5)*200,1)
  
  clump.bins = data.frame(pct.tree,pct.ba,qmd.bin,row.names=c("I","2-4","5-9","10-15","16-29","30+"))
  maxbin = maxbin.orig # reset to orig
  
  
  ### Summary Metrics
  BA = sum(trees$ba)/hectares
  TPH = nrow(trees)/hectares
  Mean.dbh = mean(trees$dbh)
  QMD = (((BA/TPH)/pi)^.5)*200
  SDI = TPH*(QMD/25)^1.7
  sum.met = cbind(BA,TPH,Mean.dbh,QMD,SDI,hectares)
  sum.eng = cbind(BA*4.356,TPH/2.47,Mean.dbh/2.54,QMD/2.54,SDI/2.47,hectares*2.47)
  summary = cbind(sum.met,sum.eng)
  colnames(summary)= c("BAH","TPH","Mean.dbh","QMD","SDI","hectares","BAA","TPA","Mean.dbh","QMD","SDI","Acres")
  
  
  
  return(list(plot.name=plot.name, n.pts=nrow(trees), n.clusts=n.clusts, n.bins=max.bin,  summary=round(summary,1), 
              mean.clust.size=mean.clust.size, norm.mean.clust.size=norm.mean.clust.size,
              points=pointData, trees=trees, points.noedge = pointData.orig , trees.noedge = noedge.trees , 
              clusters=clusters, maxbin=maxbin,clump.bins =clump.bins, edge.cut = edge.cut))
}

summarizeClusters.ppp(pointData, treeData, -1, -1, c(0,0,0,0), F, "Name")

IS1_2018_out <- summarizeClusters.ppp(pointData, treeData, -1, -1, c(0,0,0,0), F, "IS1 2018")

# for loop to prep all 12 plots for mapping and figures



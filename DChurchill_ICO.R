# Tue 7/18/23
# D. Churchill's code applies Plotkin 2002 ICO algorithm to spatial point pattern; 
# adapting for circular plot area at Indiana Summit

##########################################################################
###
###       Cluster Algorthim for Plotklin et al. 2002 
###       Main cluster function by Sean Jeronimo and Nick Povak 
###	      Summary funcitons by Sean Jeronimo & Derek Churchill
###
###       Last updated:  November-2014
###       
###       Main user funtions are:
###				formatSummaryInput <- function(plot.data, x.min, x.max, y.min, y.max, dbh.units)          
###       summarizeClusters.ppp <- function(pointData, treeData, distThreshold=-1, max.bin=-1,edge.cut = c(0,0,0,0))
###				See below for 
##
##        Added Quick Map cluster summation
###       
##########################################################################

require(spatstat)
require(rgeos)
require(sp)

########################################
#  Use this function to set up data
#  Inputs:
#   plot.data: data.frame or matrix with column headers x, y, spp, dbh in any order; most normal name variations are recognized.
#               Can also have crown radius, tree tag, but not essential 
#               +  clump Id (Quickmap plots)
#              Any additional columns will be ignored, but passed on
#   
# corner.0 = Set to T if you want to reset the plot corner to 0,0 based on min x and y values of trees. 
# x.min, x.max, y.min, y.max: window settings
# x.min=0 :  set if min plot extent is different than 0
# y.min=0 : set if min plot extent is different than 0
# y.max=0 : Will use max x value of tree points if left set to 0
# y.max=0   Will use max y value of tree points if left set to 0
# dbh.units="cm" or "in"
# Species.correct, sets all species codes to 4 letter codes. May not have your species, add as needed. 

plot.data <- IS1_2018 # comes from Pines_ESA2023

#______Found a workaround for the function below, bc it requires square window______
#formatSummaryInput <- function(plot.data, corner.0 = F, x.min=-60, x.max=60, y.min=-60, y.max=60, poly="NA",dbh.units="cm",Species.correct=F)
#{
  
 #  if(corner.0==T & class(poly)=="SpatialPolygonsDataFrame") (stop("corner.0 cannot be T if polygon boundary is used"))
 #  # Looks for x and y as the first or last letter in the column name
 #  # Otherwise we may pick up spurious columns
 #  x.index <- grep("(^x|x$)", colnames(plot.data), perl=TRUE, ignore.case=TRUE)
 #  y.index <- grep("(^y|y$)", colnames(plot.data), perl=TRUE, ignore.case=TRUE)
 #  # If we picked up more than one, we remove any columns that aren't numeric
 #  # Hopefully we don't have anything left...
 #  if(length(x.index) > 1)
 #    x.index <- x.index[which(lapply(plot.data[,x.index], class) == "numeric")]
 #  if(length(y.index) > 1)
 #    y.index <- y.index[which(lapply(plot.data[,y.index], class) == "numeric")]
 #  
 #  dbh.index <- grep("(dbh|dia)", colnames(plot.data), perl=TRUE, ignore.case=TRUE)
 #  spp.index <- grep("(spec|spp|spc|spcs)", colnames(plot.data), perl=TRUE, ignore.case=TRUE)
 #  crown.index <- grep("(crown|crwn|cr.rad)", colnames(plot.data), perl=TRUE, ignore.case=TRUE)
 #  treeid.index <- grep("(plot.id|tree.id|tag)", colnames(plot.data), perl=TRUE, ignore.case=TRUE)
 #  clumpid.index <- grep("(clumpid|clump)", colnames(plot.data), perl=TRUE, ignore.case=TRUE)
 #  
 # 
 # 	# Sorry that this goes so wide...
 #  if(length(c(x.index, y.index, dbh.index, spp.index)) != 4)
 #    stop("The following columns cannot be distinguished in plot.data: ", paste(c("x", "y", "dbh", "spp")[which(c(length(x.index), length(y.index), length(dbh.index), length(spp.index)) == 0)], collapse=", "))
 #   
 #    if(!is.numeric(c(x.min, x.max, y.min, y.max)))
 #    stop("x.min, x.max, y.min, y.max must all be numeric values")
 #  
 #  ## Set to 0,0 if called for by corner.0 = T & deal with min, max values
 #  if(corner.0 == T) (plot.data[,x.index] = plot.data[,x.index] - min(plot.data[,x.index])) & 
 #    (plot.data[,y.index] = plot.data[,y.index] - min(plot.data[,y.index]))
 #  
 #  if(x.max==0) (x.min = min(plot.data[,x.index]))
 #  if(y.max==0) (y.min = min(plot.data[,y.index]))
 #  if(x.max==0) (x.max = max(plot.data[,x.index]))
 #  if(y.max==0) (y.max = max(plot.data[,y.index]))
 #  
 #  ### Set uniform species codes if called by Species.correct = T
 #  Species = as.character(plot.data[,spp.index])
 #  
 #  if (Species.correct==T) {
 #  Species[grep("^P$",Species,fixed=F)]="PIPO"  # fixed=T restricts to only p
 #  Species[grep("^p$",Species,fixed=F)]="PIPO"  # fixed=T restricts to only p
 #  Species[grep("^PP|PIPO$",Species,ignore.case=T)]="PIPO"
 #  Species[grep("^F$",Species,fixed=F)]="PSME"  # fixed=T restricts to only F
 #  Species[grep("^f$",Species,fixed=F)]="PSME"  # fixed=T restricts to only f
 #  Species[grep("^DF|PSME$",Species,ignore.case=T)]="PSME"
 #  Species[grep("^L|WL|LAOC|Lar$",Species,ignore.case=T)]="LAOC"
 #  Species[grep("^G|GF|ABGR$",Species,ignore.case=T)]="ABGR"
 #  Species[grep("^WF|ABCO$",Species,ignore.case=T)]="ABCO"
 #  Species[grep("^S|ES|PIEN$",Species,ignore.case=T)]="PIEN"
 #  Species[grep("^SA|ABLA$",Species,ignore.case=T)]="ABLA"
 #  Species[grep("^RC|THPL$",Species,ignore.case=T)]="THPL"
 #  Species[grep("^WP|WWP|PIMO|PIMO3$",Species,ignore.case=T)]="PIMO"
 #  Species[grep("^LP|PICO$",Species,ignore.case=T)]="PICO"
 #  }
 #  
 #  
 #  # We need to make sure that the tree.ids that are provided are unique since we're using them as our ppp mark
 #  # if not, or if none are provided, then use row names.
 #  if(length(unique(rownames(plot.data))) != nrow(plot.data)) (rownames(plot.data) <- as.character(1:nrow(plot.data)))
 #  
 #  if(length(treeid.index) > 1) stop("Cannot determine tree id column")
 #  if(length(treeid.index) == 1) (Tree.ID = plot.data[,treeid.index]) 
 #  if(length(treeid.index) == 0) (Tree.ID = as.numeric(rownames(plot.data)))    
 #  
 #  class("NA")
 #  if(class(poly)!="character"){ 
 #    windd = poly@polygons[[1]]@Polygons[[1]]@coords
 #    windd = windd[-nrow(windd),]
 #    print("creating ppp with polygon")
 #    pointData <- ppp(plot.data[,x.index], plot.data[,y.index],poly=list(x=rev(windd[,1]),y=rev(windd[,2])),marks=Tree.ID)
 #  }
 #  
 #  if(class(poly)=="character"){
 #    #pointData <- ppp(plot.data[,x.index], plot.data[,y.index],xrange=c(x.min, x.max), yrange=c(y.min, y.max),marks=Tree.ID)
 #  }
  
pointData <- ppp(plot.data$X, plot.data$Y, window = disc(radius = 60, centre = c(0,0)), 
                      marks = as.numeric(rownames(plot.data) ))
  
  #treeData  <- data.frame(dbh=plot.data[,dbh.index], spp=Species, crown=plot.data[,crown.index], 
  #                        Tree.ID=Tree.ID,Clump.ID=plot.data[,clumpid.index])
  
  # leaving out crown for now...
treeData <- data.frame(dbh=plot.data$dbh, spp=plot.data$Spec, Tree.ID=as.numeric(rownames(plot.data)),Clump.ID=as.numeric(rownames(plot.data)) )
treeData2 <- data.frame(dbh=plot.data[,"dbh"], spp=plot.data$Spec, Tree.ID=as.numeric(rownames(plot.data)),Clump.ID=as.numeric(rownames(plot.data)) )  
# I think we needed treeData2 for later issues with plotting function...???
treeData3 <- treeData2 <- data.frame(dbh=plot.data[,"dbh"], spp=plot.data$Spec, Tree.ID=as.numeric(rownames(plot.data))) 
# try without Clump.ID


#   # These possibility lists could probably be expanded
#   metric <- c("cm", "centimeters", "metric")
#   customary <- c("in", "inches", "inch","standard","English", "customary", "imperial", "sae")
#   if(tolower(dbh.units) %in% customary)
#     treeData$dbh <- cm(treeData$dbh)
#   else if(!(tolower(dbh.units) %in% metric))
#     stop("dbh.units must be one of: cm, in")
#   
# #  plot.ha = round(((x.max - x.min) * (y.max - x.min))/10000,2)
# plot.ha = round((pi*57.4^2)/10000,2)  
#   
#   out <- list(pointData=PVKpointData, treeData=PVKtreeData,plot.ha = plot.ha )
#   class(out) <- c(class(out), "summary.input")
#   return(out)
# }

plot.ha = round((pi*57.4^2)/10000,2)  
out <- list(pointData, treeData, plot.ha)
class(out) <- c(class(out), "summary.input")

#
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
#   Note: It is preferred that pointData and treeData inputs come from formatSummaryInput()
#         to ensure proper formatting and naming.
#



# pointData = fw.data.list[[w]]$pointData 
# treeData = fw.data.list[[w]]$treeData  
# distThreshold=6
# max.bin=-1
# edge.cut=fw.edge.cut
# Quickmap = F
# plot.name="PP"


	
#summarizeClusters.ppp <- function(pointData, treeData, distThreshold=-1, max.bin=-1,edge.cut = c(0,0,0,0), Quickmap=F,plot.name="Name"){ # trying with distThreshold=6
summarizeClusters.ppp <- function(pointData, treeData, distThreshold=6, max.bin=-1,edge.cut = c(0,0,0,0), Quickmap=F,plot.name="Name"){
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
  hectares <- round((pi*57.4^2)/10000,2)
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
	qmd.bin =  round((((ba.bin.sum/tpa.bin.sum)/3.14)^.5)*200,1)
  
  clump.bins = data.frame(pct.tree,pct.ba,qmd.bin,row.names=c("I","2-4","5-9","10-15","16-29","30+"))
	maxbin = maxbin.orig # reset to orig
	
  
  ### Summary Metrics
  BA = sum(trees$ba)/hectares
  TPH = nrow(trees)/hectares
  Mean.dbh = mean(trees$dbh)
  QMD = (((BA/TPH)/3.14)^.5)*200
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

summarizeClusters.ppp(pointData, treeData, 6, -1, c(0,0,0,0), F, "IS1")
tree.data_out <- summarizeClusters.ppp(pointData, treeData3, -1, 6, c(0,0,0,0), F, "IS1")

###########################################
##  Functions used by main user functions
###############################################

#
# Inputs:
#   pointData: ppp object containing xy data, window, and marks that match
#              the row names of treeData
#   treeData:  data.frame with columns named dbh, spp, and crown and row names
#              matching the marks of pointData.
#              crown is not necessary if distThreshold is specified.
#   distThreshold: If set, maximum distance between trees in a clump.
#                  If left -1, crown radii are used.
#

# Runs in ~this many seconds given x trees
# 2e-13*x^4 - 4e-10*x^3 + 3e-6*x^2 - .0022*x

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




###################
##  gini coefficient code:  
##   input is dbh list in cm
gini = function(dbh.vec){
	bah = (dbh.vec/200)^2*3.14
	n = length(dbh.vec)
	rank = c(1:n)
	if (n==1) (ginii = -1) else (ginii = sum(((2*rank-n-1)*sort(bah))) / sum(sort(bah)*(n-1))) 
	return (ginii)}


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
    Species[grep("PP|PIPO",Species,ignore.case=T)]="PP"
	  Species[grep("F|f",Species,fixed=T)]="DF"  # fixed=T restricts to only p
		Species[grep(("DF|PSME"),Species,ignore.case=T)]="DF"
		Species[grep("L|WL|LAOC|Lar",Species,ignore.case=T)]="WL"
		Species[grep("G|GF|ABGR|ABCO|WF",Species,ignore.case=T)]="GF"
		Species[grep("S|ES|PIEN",Species,ignore.case=T)]="ES"
		Species[grep("SA|ABLA",Species,ignore.case=T)]="SA"
		Species[grep("RC|THPL",Species,ignore.case=T)]="RC"
		Species[grep("WP|WWP|PIMO|PIMO3",Species,ignore.case=T)]="WP"
    
		cr.rad = 	rep(0,length(dbh))
	
		## set up default coefs from East WA FIA & CVS Data
		pp.coef =c( -1.3992691, 0.6416915)
		df.coef =c( -0.9270301 ,0.5524913)
		wl.coef =c(  -1.2358009, 0.5872084)
    gf.coef = c(  -0.882310, 0.526169)
		es.coef = c(  -1.025246, 0.498273)
		sa.coef = c(  -1.173654, 0.521454)
		rc.coef = c(  -0.436856, 0.426431)
		wp.coef = c(  -1.24609, 0.56432)
    
    
    # Newer coefs from all E Washington
		#pp.coef =c( -1.42100 , 0.64616)
		#df.coef =c(-0.902495,0.542020)
		#wl.coef =c(-1.396547,0.632239)

    
		coefs = data.frame(pp.coef,df.coef,wl.coef,gf.coef,es.coef,sa.coef,rc.coef,wp.coef); rownames(coefs)=c("Intcpt","log(dia)")
		if (cr.coefs == -1) (cr.coefs = coefs)
	
		## run models
		
		cr.rad[which(Species=="PP")] = exp(cr.coefs[1,1] + cr.coefs[2,1]*log(dbh[which(Species=="PP")]))
		cr.rad[which(Species=="DF")] = exp(cr.coefs[1,2] + cr.coefs[2,2]*log(dbh[which(Species=="DF")]))
		cr.rad[which(Species=="WL")] = exp(cr.coefs[1,3] + cr.coefs[2,3]*log(dbh[which(Species=="WL")]))
		cr.rad[which(Species=="GF")] = exp(cr.coefs[1,4] + cr.coefs[2,4]*log(dbh[which(Species=="GF")]))
    
		cr.rad[which(Species=="SA")] = exp(cr.coefs[1,5] + cr.coefs[2,5]*log(dbh[which(Species=="SA")]))
		cr.rad[which(Species=="ES")] = exp(cr.coefs[1,6] + cr.coefs[2,6]*log(dbh[which(Species=="ES")]))
		cr.rad[which(Species=="RC")] = exp(cr.coefs[1,7] + cr.coefs[2,7]*log(dbh[which(Species=="RC")]))
		cr.rad[which(Species=="WP")] = exp(cr.coefs[1,8] + cr.coefs[2,8]*log(dbh[which(Species=="WP")]))
		
    # Catch all for species with no coeff. PP as default
    cr.rad[which(cr.rad==0)] = exp(cr.coefs[1,1] + cr.coefs[2,1]*log(dbh[which(cr.rad==0)]))		
   
    # Output
		return(res=data.frame(data,crown = cr.rad))
}


###############################
### QuickMap Cluster list
###
###  Generates cluster list from quickmap input

Quickmap.list = function(Clump.ID){
  cluster.unq = unique(Clump.ID)
  cluster.list <- vector(mode="list", length=length(cluster.unq))
  names(cluster.list) <- paste("clust", 1:length(cluster.unq), sep="")
  for (w in 1:length(cluster.unq))  cluster.list[[w]] = as.character(which(Clump.ID==cluster.unq[w]))
  return (cluster.list=cluster.list)}


#########################################################
### Function to calc canopy cover from stemmap and crown radii
###

can.cover.calc <- function(pointData, treeData,edge.cut = c(0,0,0,0)){
  
  # create crown radii if they don't already exist
  if ("crown" %in% colnames(treeData)) (treeData=treeData) else (treeData = Crw.rad.pred(treeData))
    
  ## Create point shapefile 
   
  # Set up df with coords & UTM projection
  point.shp = data.frame(x=pointData$x,y=pointData$y,obs=pointData$marks,buf=treeData$crown)
  coordinates(point.shp)<-c('x','y')
  proj4string(point.shp)<-CRS("+proj=utm +zone=11 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  
  # Create buffers for each crown & dissolve
  crown.buf <- gBuffer(point.shp, byid = T, width=treeData$crown)
  crown.buf = gUnionCascaded(crown.buf, id = NULL)

  # Clip by edge.cut buffer
  ext <- extent((pointData$window$xrange[1]+edge.cut[1]), (pointData$window$xrange[2]-edge.cut[2]),
                (pointData$window$yrange[1]+edge.cut[3]),(pointData$window$yrange[2]-edge.cut[4]) ) 
  clipe <- as(ext, "SpatialPolygons") 
  proj4string(clipe) <- CRS("+proj=utm +zone=11 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  crown.clip = crop(crown.buf,clipe)
  
  # Calc area in crowns
  crown.area = gArea(crown.clip)
  
  #Derive total plot area - buffers
  x.dist = (pointData$window$xrange[2]-edge.cut[2]) -  (pointData$window$xrange[1]+edge.cut[1])
  y.dist = (pointData$window$yrange[2]-edge.cut[4]) -  (pointData$window$yrange[1]+edge.cut[3])
  plot.area = x.dist * y.dist
  
  ## Canopy cover
  return (list(can.cover=round(crown.area/plot.area,3),crown.shp =clipe, crown.buf.clip= crown.clip))
}
  



# Inputs:
#   inputData: object returned from formatSummaryInput(), i.e., a list with elements
#              named pointData and treeData:
#       $pointData: ppp object containing xy data, window, and marks that match
#                   the row names of treeData
#       $treeData:  data.frame with columns named dbh, spp, and crown and row names
#                   matching the marks of pointData.
#                   crown is not necessary if distThreshold is specified.
#   distThreshold: If set, maximum distance between trees in a clump.
#                  If left -1, crown radii are used.
#   max.bin: All clumps with n >= max.bin will be binned together for summary purposes.
#            Leave as -1 to disable.
#
#   Note: It is preferred that pointData and treeData inputs come from formatSummaryInput()
#         to ensure proper formatting and naming.
#

summarizeClusters.summary.input <- function(inputData, distThreshold=-1, max.bin=-1)
{
  summarizeClusters(inputData$pointData, inputData$treeData, distThreshold, max.bin)
}

summarizeClusters <- function(x, ...)
{
  UseMethod("summarizeClusters")
}
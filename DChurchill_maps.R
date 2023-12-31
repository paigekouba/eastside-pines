# Tue 7/18/23
# Adapting D. Churchill's stem plot code for Indiana Summit 1

####################################################################################
##
##  Functions for plotting crowns with colors based on cluster size. Figures in Churchill 2017. 
##
##  Derek Churchill
##  May 2012
##
#########################################################################


### Set up two functions
draw.circle2 =function(x, y, radius, nv = 100, border = NULL, col = NA, lty = 1, lwd = 1,density = NULL,angle=45){
  xylim <- par("usr")
  plotdim <- par("pin")
  ymult <- (xylim[4] - xylim[3])/(xylim[2] - xylim[1]) * plotdim[1]/plotdim[2]
  angle.inc <- 2 * pi/nv
  angles <- seq(0, 2 * pi - angle.inc, by = angle.inc)
  xv <- cos(angles) * radius + x
  yv <- sin(angles) * radius * ymult + y
  par(xpd=F)
  polygon(xv, yv, border = border, col = col, lty = lty, lwd = lwd,density=density,angle=angle)
  invisible(list(x = xv, y = yv))
}


### Plot cluster function: 
## data input must have x,y coord in meters plus dbh and spp colums

Plot.cluster = function(data.xy,radi =3, int.dist=6, x.max=142,y.max=142,plot.base="T",clust.breaks=0){
  pattern = #as.ppp(data.xy[,c(1,2)],W=c(-x.max,x.max,-y.max,y.max)) # try with circular window???
    as.ppp(data.xy[,c(1,2)],W=disc(radius = 60, centre = c(0,0)))
  my.clust.table = Clust.table(pattern,maxdist=int.dist)
  clusterss = my.clust.table$cluster.membership
  tree.n= nrow(clusterss)
  clust.mem = matrix(0,tree.n,int.dist)
  for (i in 1:tree.n){
    for (j in 1:int.dist){
      clust.mem[i,j]=length(which(clusterss[,j]==clusterss[i,j]))
    }
  }
  
  ## Set up fixed radius circles for crowns 
  n.pts = length(clust.mem[,1]) 
  clusts = cbind(c(1:n.pts),(clust.mem[,int.dist]))
  clus.sizes= sort(unique(clust.mem[,int.dist])) 
  
  ## set up color ramp for crowns by size
  rgb.palette <- colorRampPalette(c("palegreen","darkgreen"),space = "rgb")
  if (length(clust.breaks) == 1)  (clust.color=rgb.palette(length(clus.sizes)))
  if (length(clust.breaks)>1) {
    clust.lut = lut(rgb.palette(length(clust.breaks)),breaks=c(clust.breaks,100))  
    clust.color = clust.lut(clus.sizes)}
  
  
  ## Set up species codes and colors for tree boles
  # Reset column names to correct ones"  spp & dbh 
  colnames(data.xy)[grep("Sp",colnames(data.xy),ignore.case=T)] = "spp"
  colnames(data.xy)[grep("dia",colnames(data.xy),ignore.case=T)] = "dbh"
  colnames(data.xy)[grep("DBH",colnames(data.xy),ignore.case=T)] = "dbh"
  
  my.cols = factor(data.xy$spp,levels=names(sort(summary(data.xy$spp),decreasing=T)),order=T)
  my.cols = lut(c("orange","brown","black"),inputs=levels(my.cols))(my.cols)
  
  # Plot map and then crowns
if(plot.base == "T") plot(pattern$x,pattern$y,cex=.1,pch=1,xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(-y.max,y.max),xlim=c(-x.max,x.max)) 
  # trying with -60 60
  
#  if(plot.base == "T") plot(pattern$x,pattern$y,cex=.1,pch=1,xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(-60,60),xlim=c(-60,60))
  for (i in 1:length(clus.sizes)){
    tree.hold = subset(clusts,clusts[,2]==clus.sizes[i])
    for(j in 1:nrow(tree.hold)){
      hold = tree.hold[j,1]
      draw.circle2(pattern$x[tree.hold[j,1]],pattern$y[tree.hold[j,1]],radius=radi,
                   nv=1000,col=clust.color[i],lty=0,lwd=1,density=NULL,angle=NULL)
    }
  }  
  
  # Add scaled points by species for tree boles
  points(data.xy[,1],data.xy[,2],pch=21,cex=(data.xy$dbh/150),col=my.cols,bg=my.cols)
  
}


###############################################################################
##  Same plotting function, but use output of Plotkin_Cluster_Crown.R function
##  
##  Edge tree functionality added June 2014
##
##    point.data: point output from Plotkin_Cluster_Crown.R. ppp file.  Use .noedge if buffers desired 
##    tree.data:  tree output from Plotkin_Cluster_Crown.R. Use .noedge if buffers desired 
##    Species.color. data.frame with species codes and corresponding colors for plotting
##         if left as =0, then uses default table inside function
##    radi = crown radius
##    Cluster breaks: clump size breaks for color by clump size , set to 0 if not desired
##    Crown rad:  if clumps are defined by crown radius

###############################################################################

# from DChurchill_ICO.R (aka Plotkin_Cluster_Crown.R)
point.data <- pointData
#tree.data <- treeData did not work, error in l 139
tree.data <- tree.data_out

Plot.pp.cluster = function(point.data,tree.data,species.color="Eastside",radi =3,cm="Y",axes="n",QM=F,newPlot=T,
                           clust.breaks=c(1,2,5,10,16),d.factor=120){
  
  data.xy = point.data
  x.max = point.data$window$xrange[2]
  y.max = point.data$window$yrange[2]
  x.min = point.data$window$xrange[1]
  y.min = point.data$window$yrange[1] #EDIT: y.min and yrange ?
  
# this is causing error:  Error in tree.data[, which(colnames(tree.data) == "dbh")] : 
 # incorrect number of dimensions
 # dbh= tree.data[,which(colnames(tree.data)=="dbh")]
#  Species = tree.data[,which(colnames(tree.data)=="spp")]
  
# trying tree.data --> treeData swap, as treeData actually IS a df
  dbh= treeData[,which(colnames(treeData)=="dbh")]
  Species = treeData[,which(colnames(treeData)=="spp")]
  
  
  ## Set up clump info
  if(QM==F){
    clust.mem = tree.data$Clump.ID
    n.pts = point.data$n
    clusts = tree.data$trees$clust.sz
    clus.sizes= sort(unique(clusts))
  }
  
  if(QM==T){
    clust.mem = tree.data$cluster.membership 
    n.pts = point.data$n
    clusts <- sapply(tree.data$Clump.ID, function(x) length(which(tree.data$Clump.ID == x)))
    clus.sizes= sort(unique(clusts))
  }
  
  data.frame(spcs= c("ABCO","ABGR","PIPO","PSME","LAOC","Unk","CELE3","JUOC","PICO"),colr = c("black","black","orange","brown","red","gray","gray","gray","gray"))
  
  ## set up color ramp for crowns by size
  rgb.palette <- colorRampPalette(c("palegreen","darkgreen"),space = "rgb")
  if (length(clust.breaks) == 1)  (clust.color=rgb.palette(length(clus.sizes)))
  if (length(clust.breaks)>1) {
    clust.lut = lut(rgb.palette(length(clust.breaks)),breaks=c(clust.breaks,100))  
    clust.color = clust.lut(clus.sizes)}
  
  ## Set up species codes and colors for tree boles 
  if(species.color=="PP-DF-WL") (species.color= data.frame(spcs= c("PIPO","PSME","LAOC"),colr = c("orange","black","red")))
# adding in my own color set ???
  else if (species.color =="Eastside") (species.color=data.frame(spcs=c("PIJE", "ABCO", "PICO", "JUGR"), colr = c("black","orange","brown","blue"))) # see if can use viridis or similar?
  else (species.color=species.color)
  
  my.cols = lut(species.color[,2],inputs=species.color[,1])(Species)
  
  # Plot map and then crowns
  if(newPlot == T) plot(point.data$x,point.data$y,cex=.1,pch=1,xlab="",ylab="",xaxt=axes,yaxt=axes,ylim=c(y.min,y.max),xlim=c(x.min,x.max)) 
# trying with -60, 60
#    if(newPlot == T) plot(point.data$x,point.data$y,cex=.1,pch=1,xlab="",ylab="",xaxt#=axes,yaxt=axes,ylim=c(-60,60),xlim=c(-60,60)) 
  
# ERROR col= too short list
  #  for (i in 1:n.pts) draw.circle2(point.data$x[i],point.data$y[i],radius=radi, nv=1000,col=clust.color[i],lty=0,lwd=1,density=NULL,angle=NULL)
  for (i in 1:n.pts) draw.circle2(point.data$x[i],point.data$y[i],radius=radi, nv=1000,col="green",lty=0,lwd=1,density=NULL,angle=NULL)
  
  # Add scaled points by species for tree boles
  if (cm != "Y")  (dbh = dbh*2.54) 
  points(point.data$x,point.data$y,pch=21,cex=(dbh/d.factor),col=as.character(my.cols),bg=as.character(my.cols))
  #return(clust.color)
}

Plot.pp.cluster(point.data, tree.data,species.color="Eastside", radi = 3, cm="Y", axes="n", QM=F, newPlot=T, 
                clust.breaks=c(1,2,5,10,16),d.factor=120)



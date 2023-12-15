# Learning spatstat and K statistic
# the way I got the ppp to test this on specified a square window! 
IS1_2018ppp <- ppp(IS1_2018$X, IS1_2018$Y, window = disc(radius = sqrt(10000/pi), centre = c(0,0)))
#marks(IS1_2018ppp) <- IS1_2018[,5] # plot with dbh

# try again with circular window
plot1_ppp <- ppp(IS1_2018$X, IS1_2018$Y, window = disc(radius = sqrt(10000/pi)+0.69))

# test new plots_out output for Kest
IS1_2018ppp <- ppp(plots[[1]]$X, plots[[1]]$Y, window = disc(radius = sqrt(10000/pi)+0.69, centre = c(0,0)), marks = as.numeric(rownames(plots[[1]])))
# marks(IS1_1941ppp) <- IS1_1941[,13] # NEED TO ADD DBH ?? this might be why the crowns on the eyeball maps aren't changing

# check on OH1 1941, # 8, with illegal point
OH1_1941ppp <- ppp(plots[[8]]$X, plots[[8]]$Y, window = disc(radius = sqrt(10000/pi), centre = c(0,0)), marks = as.numeric(rownames(plots[[8]]))) # weird thing; I think marks here just needs to be a unique ID, but NB some rows are >1000

adriana <- Kest(IS1_2018ppp)
Kest(IS1_2018ppp, correction="isotropic")
adriana <- as.function(Kest(IS1_2018ppp,correction='isotropic')) # change this to have an argument for correction and ppp
# try "best"; it seems to select isotropic
adriana(6)*(IS1_2018ppp$n)/10000 # what is the 6 doing?

# prepping pieces of a for loop

# Define edge trees as those where sqrt(x^2+y^2) + cr.rad > sqrt(10000/pi)

# defining clusters step is different for edge trees; first have to find out-of-window neighbors 
# I have got equation to find the crown area outside the plot for the ith tree, and find as a fraction of crown area
# 
# for cr.rad, x, and y (of a given point); also x2y2 <- (x^2)+(y^2); also R = sqrt(10000/pi) 
# (cr.rad^2)*cos^-1((x2y2 + (cr.rad^2) - (R^2))/(2(sqrt(x2y2))(cr.rad))) +
# (R^2)*cos^-1((x2y2+(R^2)-(cr.rad^2))/(2*sqrt(x2y2)*R)) -
# 0.5*sqrt((-sqrt(x2y2)+cr.rad+R)*(sqrt(x2y2)+cr.rad-R)*(sqrt(x2y2)-cr.rad+R)*(sqrt(x2y2)+cr.rad+R))
# result: area of crown inside plot boundary.
# that value subtracted from ith tree's total crown area
# that value as a fraction of pi(cr.rad^2)
# that fraction *((# points in plot)/10,000)*K(cr.rad)

# Then I multiply that fraction by (# trees in plot / 10,000 m^2) * K(cr.rad) or maybe K(6)
# I should make that a function and apply it to all the edge trees

# test math
# for cr.rad, x, and y (of a given point); also x2y2 <- (x^2)+(y^2); also R = sqrt(10000/pi) 
cr.rad <- 5.913
x <- 59
y <- 1
x2y2 <- (x^2)+(y^2)
R = sqrt(10000/pi)
inside.crown<- (cr.rad^2)*acos((x2y2 + (cr.rad^2) - (R^2))/(2*(sqrt(x2y2))*(cr.rad)))+
(R^2)*acos((x2y2+(R^2)-(cr.rad^2))/(2*sqrt(x2y2)*R)) -
0.5*sqrt((-sqrt(x2y2)+cr.rad+R)*(sqrt(x2y2)+cr.rad-R)*(sqrt(x2y2)-cr.rad+R)*(sqrt(x2y2)+cr.rad+R))
# result: area of crown inside plot boundary. Equation is for area of OVERLAP of two circles

# NON-OVERLAPPING area as a fraction of pi(cr.rad^2)
((pi*cr.rad^2)-inside.crown)/(pi*cr.rad^2)
# that fraction *((# points in plot)/10,000)*K(cr.rad) = 
round(((pi*cr.rad^2)-inside.crown)/(pi*cr.rad^2)*adriana(cr.rad)*(IS1_2018ppp$n)/10000)

# make inside.crown a function of x, y, R, cr.rad
# apply to subset of edge trees


# trying to find out of bounds trees breaking ICO code w circular window? 
# filter(read.csv("Treedata_9-3_Em_DB.csv"), X<((-1)*sqrt(10000/pi)) | X>sqrt(10000/pi) | Y<((-1)*sqrt(10000/pi)) | X>sqrt(10000/pi))
# Site Plot      Date Spec  DBH Dec     X     Y     Z CrHt SrchHt Core.                                                 Comments
# 1   IS  IS1    14-Aug PIJE 28.9  NA 367.0 -18.8   2.9  9.7    3.0    NA                                                         
# 2   OH  OH3    30-Aug PIJE 38.0  NA -57.9  -4.0 -16.3  5.0    0.2    NA                                              may be out 
# 3   OH  OH3    30-Aug PIJE 57.2  NA -57.0   3.5 -13.5  4.0    0.1    NA                                              may be out 
# 4   OH  OH3 8/29/2018 PIJE 57.8  NA  57.4  -6.7  19.3  7.5    5.0    NA near plot boundary - shooting from monopol - might be in


#filter(read.csv("Treedata_9-3_Em_DB.csv"), sqrt((X^2)+(Y^2))<sqrt(10000/pi))
       
#       X<((-1)*sqrt(10000/pi)) | X>sqrt(10000/pi) | Y<((-1)*sqrt(10000/pi)) | X>sqrt(10000/pi))











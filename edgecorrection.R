# Learning spatstat and K statistic

adriana <- Kest(IS1_2018ppp)
Kest(IS1_2018ppp, correction="isotropic")
adriana <- as.function(Kest(IS1_2018ppp,correction='isotropic'))
adriana(6)*(IS1_2018ppp$n)/10000

fempty(IS1_2018ppp)

# prepping pieces of a for loop

# Define edge trees as those where sqrt(x^2+y^2) + cr.rad > sqrt(10000/pi)

# defining clusters step is different for edge trees; first have to find out-of-window neighbors 
# I have got equation to find the crown area outside the plot for the ith tree, and find as a fraction of crown area
# 
# for cr.rad, x, and y (of a given point); also x2y2 <- (x^2)+(y^2); also R = sqrt(10000/pi) 
# (cr.rad^2)*cos^-1((x2y2 + (cr.rad^2) - (R^2))/(2(sqrt(x2y2))(cr.rad))) +
# (R^2)*cos^-1((x2y2+(R^2)-(cr.rad^2))/(2*sqrt(x2y2)*R)) -
# 0.5*sqrt((-sqrt(x2y2)+cr.rad+R)*(sqrt(x2y2)+cr.rad-R)*(sqrt(x2y2)-cr.rad+R)*(sqrt(x2y2)+cr.rad+R))
# result: area of crown outside plot boundary.
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
# result: area of crown inside plot boundary.

# that value as a fraction of pi(cr.rad^2)
((pi*cr.rad^2)-inside.crown)/(pi*cr.rad^2)
# that fraction *((# points in plot)/10,000)*K(cr.rad)
round(((pi*cr.rad^2)-inside.crown)/(pi*cr.rad^2)*adriana(cr.rad)*(IS1_2018ppp$n)/10000)















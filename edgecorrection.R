# Learning spatstat and K statistic

adriana <- Kest(IS1_2018ppp)
Kest(IS1_2018ppp, correction="isotropic")
adriana <- as.function(Kest(IS1_2018ppp,correction='isotropic'))
adriana(6)*(IS1_2018ppp$n)/10000


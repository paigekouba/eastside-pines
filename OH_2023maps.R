# Thu 6/1/23
# Stand Maps of OH for ABCO, PICO, and JUGR Resample
stems23 <- read.csv("stem_mapping.csv")
OHstems23 <- stems23[stems23$Site=="OH",]
treeplots=unique(OHstems23$Plot)

# clean up spec
unique(OHstems23$Spec)
#[1] "PIJE"  "JUN"   "ABCO"  "CELE"  "JUGR"  "PICO"  "PIFL"  "CELE " "PIJE " "BEOC" 
OHstems23 <- OHstems23 %>% 
  filter(Spec != "CELE") %>% 
  filter(Spec != "CELE ") %>%  
  filter(Spec != "BEOC")
OHstems23$Spec <- gsub("PIJE ", "PIJE", OHstems23$Spec)
OHstems23$Spec <- gsub("PIFL", "PIJE", OHstems23$Spec)
OHstems23$Spec <- gsub("JUN", "JUGR", OHstems23$Spec)
unique(OHstems23$Spec)
#[1] "PIJE" "JUGR" "ABCO" "PICO"

# quick and dirty: base R, map of stems scaled by dbh and colored by PIJE or not

OHstems23$color[OHstems23$Spec == "PIJE"]="black"
    OHstems23$color[OHstems23$Spec == "ABCO"]="gray"
      OHstems23$color[OHstems23$Spec == "JUGR"]="gray"
          OHstems23$color[OHstems23$Spec == "PICO"]="gray"
            
par(mfrow=c(1,1))
for(treeplot in treeplots){
  #define inplot as trees within the plot the loop is acting on  
  # for each unique plot name, define inplot as 
  #  trees where plot= current plot
  inplot=OHstems23[OHstems23$Plot==treeplot,]
  plot(inplot$X, inplot$Y, col=inplot$color,pch=19,cex=(inplot$DBH/50),
       asp=1,xlim=c(-60,60),ylim=c(-60,60),xlab="X (meters)", ylab="Y (meters)", 
       main=paste("Trees in",treeplot))
  text(paste("N = ",length(inplot$DBH)), x = 50, y = -55)
}

# Trying again with ggplot
# first, map trees in each site
OHstems23 %>% 
  filter(Plot == "OH1") %>% 
ggplot(aes(label = DBH))+
  geom_point(aes(x=X, y =Y, color = Spec, size = DBH, alpha = 0.5)) +
  xlim(-60, 60) +
  ylim(-60, 60) +
  geom_text(aes(X, Y), check_overlap = TRUE) +
  ggtitle("OH1")

OHstems23 %>% 
  filter(Plot == "OH2") %>% 
  ggplot(aes(label = DBH))+
  geom_point(aes(x=X, y =Y, color = Spec, size = DBH, alpha = 0.5)) +
  xlim(-60, 60) +
  ylim(-60, 60) +
  geom_text(aes(X, Y), check_overlap = TRUE) +
  ggtitle("OH2")

OHstems23 %>% 
  filter(Plot == "OH3") %>% 
  ggplot(aes(label = DBH))+
  geom_point(aes(x=X, y =Y, color = Spec, size = DBH, alpha = 0.5)) +
  xlim(-60, 60) +
  ylim(-60, 60) +
  geom_text(aes(X, Y), check_overlap = TRUE) +
  ggtitle("OH3")

# get UTMs for target trees
# add the offsets to the eastings (x) and northings (y) to get the coordinates of the offset points.
# make a new column for each of Eutm and Nutm
OH1_UTM <- OHstems23 %>% 
  filter(Plot == "OH1") %>% 
  mutate(Eutm = 347137 + X) %>% 
  mutate(Nutm = 4181168 + Y)

OH2_UTM <- OHstems23 %>% 
  filter(Plot == "OH2") %>% 
  mutate(Eutm = 347396 + X) %>% 
  mutate(Nutm = 4180917 + Y)

OH3_UTM <- OHstems23 %>% 
  filter(Plot == "OH3") %>% 
  mutate(Eutm = 347280 + X) %>% 
  mutate(Nutm = 4181093 + Y)

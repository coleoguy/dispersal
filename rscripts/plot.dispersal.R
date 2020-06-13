#Dispersal Proportion Among Lines
dat <- read.csv("../data/beetle.data.csv")

#Add column indicating proportion of the P2 genome
prop.P2 <- c(0,0,1,1,0.5,0.5,0.5,0.5,0.25,0.25,0.77,0.77,0.25,0.25,0.73,0.73)
dat <- cbind(dat,prop.P2)

#Add shape based on sex circles = female and squares = male
shapes <- as.numeric(as.factor(dat$sex))
rshapes <- c(21,22)[shapes]

#Add color based on generation
colors <- as.numeric(as.factor(dat$cross))
rcols <- c("dodgerblue","red","mediumpurple1","lightskyblue",
           "deeppink3","dodgerblue4","red4","darkorchid4")[colors]


#Plot
plot(x = dat$prop.P2, y = dat$mean, col = rcols,
     xlab = "Proportion of P2 (Disperser) Genome",
     ylab = "Proportion of Dispersers",
     xlim = c(0,1), ylim = c(0,1), pch = rshapes,
     cex = 1.2, cex.lab = 1.1, bty = "l", xaxt = "n", yaxt = "n")
segments(0, mean(dat$mean[1:2]),
         1, mean(dat$mean[3:4]))
points(x = dat$prop.P2, y = dat$mean, bg = rcols,
        pch = rshapes, cex = 1.2)

#Change X and Y axis
xtick<-seq(0, 1, by=0.25)
axis(side=1, at=xtick, labels = FALSE)
text(x=xtick,  par("usr")[3],
     labels = xtick, pos = 1, offset = 1, xpd = TRUE)
ytick<-seq(0, 1, by=0.2)
axis(side=2, at=ytick, labels = FALSE)
text(par("usr")[1], ytick,
     labels = ytick, pos = 2, offset = 1, xpd = TRUE)

#Create the ledgend
line <- c("P1", "P2", "F1", "rF1", "BC1", "rBC1", "BC2", "rBC2")
Cols <- c("lightskyblue", "deeppink3", "mediumpurple1", "darkorchid4",
          "dodgerblue", "dodgerblue4", "red", "red4")
ys <- seq(from=1, to=.5, length.out=8)
for(i in 1:8){
        points(x=0, y=ys[i], pch=16, col=Cols[i])
        text(x=0,y=ys[i], labels=line[i], pos=4,cex=.75)
}



#Export Size 4x4

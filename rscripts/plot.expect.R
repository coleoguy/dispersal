dat <- read.csv("../results/expected.comp.csv", row.names = 1)

line <- c("P1", "P2", "F1", "rF1", "BC1", "BC2", "rBC1", "rBC2")
Cols <- c("lightskyblue", "deeppink3", "mediumpurple1", "darkorchid4",
          "dodgerblue", "red", "dodgerblue4", "red4")

#Jitter for points
objitter <- rep(0,8)
objitter[c(4,6)] <- -3.2
jitter <- rep(3.3, 8)
jitter[c(3,8)] <- 7.5
plot(x=dat$perc.p2+objitter, y=dat$obsvd, col=Cols, pch=16,xlim=c(0,103),
     ylim=c(0,1), xlab="Proportion P2 (Disperser) Genome",
     ylab="Proportion of Dispersers", xaxt = "n", bty = "l")
points(x=dat$perc.p2+jitter, y=dat$best, col=Cols, pch=17)

#Change X axis
xtick<-seq(0, 1, by=0.25)
axis(side=1, at=xtick*100, labels = FALSE)
text(x=xtick*100,  par("usr")[3],
     labels = xtick, pos = 1, offset = 1, xpd = TRUE)

#Legend
xvals <- rep(0, 10)
yvals <- seq(from=1, by= -.055, length.out = 10)
points(xvals,yvals, pch=c(16,17,rep(15,8)),
       col=c("black","black",Cols))
text(xvals,yvals, labels=c("observed", "predicted",line), cex=.7,pos=4)

#Export size 5x5


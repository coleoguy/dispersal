#Linear Model and Strip Plot

dat <- read.csv("../data/mal.fem.comp.csv")
disp <- c(dat$males, dat$females)
sex <- rep(c("Male", "Female"), each = 23)
line <- rep(dat$X, times = 2)
dat <- data.frame(disp, sex, line)

#Linear Model of dispersal, sex, and line
library(nlme)
fit <- lme(disp ~ sex, random = list(~1|line), data = dat)
summary(fit)


#Strip Plot 
#dat <- read.csv("../data/saga.beetle.data.csv")

boxplot(dat$sex, dat$disp, pch = " ", frame.plot = F, border = "white", ylim = c(0,1),
     xlab ="", ylab = "Proportion of Dispersers", xaxt = "n", yaxt = "n")

#Females connected to Males 
#Create Line Segements where Females are connected to Males
yf <- dat$disp[dat$sex == "Female"]
ym <- dat$disp[dat$sex == "Male"]
segments(1, yf, 2, ym)

#Colors for the points
colors <- as.numeric(as.factor(dat$line))
rcols <- c("dodgerblue","red","mediumpurple1","lightskyblue",
           "deeppink3","dodgerblue4","red4","darkorchid4")[colors]
points(dat$sex,dat$disp, pch = 16, col = rcols)

#X and Y axis Labels
xtick<-c(1,2)
axis(side=1, at=xtick, labels = FALSE, pos = 0)
text(x=xtick,  par("usr")[3],
     labels = c("Female", "Male"), pos = 1, offset = 1, xpd = TRUE)
abline(h = 0)

ytick<-seq(0, 1, by=0.2)
axis(side=2, at=ytick, labels = FALSE)
text(par("usr")[1], ytick,
     labels = ytick, pos = 2, offset = 1, xpd = TRUE)


#Create the ledgend
line <- c("P1", "P2", "F1", "rF1", "BC1", "rBC1", "BC2", "rBC2")
Cols <- c("lightskyblue", "deeppink3", "mediumpurple1", "darkorchid4",
          "dodgerblue", "dodgerblue4", "red", "red4")
ys <- seq(from=0.98, to=.5, length.out=8)
for(i in 1:8){
  points(x=0.5, y=ys[i], pch=16, col=Cols[i])
  text(x=0.5,y=ys[i], labels=line[i], pos=4,cex=.75)
}

#Export 5x5
#Plot of Generations of Parentals

dat <- read.csv("../data/herit.csv")
pchs <- rep(c(15,16), each=3, times=2)

p2 <- rgb(80,14,46, 50, maxColorValue = 100)
p1 <- rgb(53,81,98, 75, maxColorValue = 100)
cols <- rep(c(p2,p1), each=6)
plot(y=0,x=0, type="b",ylim=c(0,1),xlim=c(0,4),col="white",xlab="Generation",
     ylab="Proportion of Dispersers", bty = "l")
for(i in 1:12){
  lines(y=dat[i,c(4:6,8)],x=c(0:2,4), type="b",pch=pchs[i],col=cols[i])
}

#legend
line <- c("P2 Females", "P2 Males", "P1 Females", "P1 Males")
Cols <- rep(c(rgb(80,14,46, 50, maxColorValue = 100), 
              rgb(53,81,98, 75, maxColorValue = 100)), each = 2)
points <- rep(c(16,15),times=2)
ys <- seq(from=0.98, to=.5, length.out=8)
for(i in 1:4){
  points(x=0, y=ys[i], pch=points[i], col=Cols[i])
  text(x=0,y=ys[i], labels=line[i], pos=4,cex=.75)
}



#Export Size 5x5




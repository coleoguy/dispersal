dat <- read.csv("../results/sim.results.csv", as.is=T)
epi <- dat$sum.epi[1:101]+runif(101,min=-.03, max=.03)
add <- dat$add[1:101]+runif(101,min=-.03, max=.03)
plot(epi~add,
     col=c(viridis(10)[8], rep(rgb(.1,.1,.1,.2),100)),
     pch=16, xlab="additive effect",
     ylab="epistatic effect")



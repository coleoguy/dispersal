library(SAGA2)
########################## Untransformed Data #######################
beetle.dat <- read.csv("../data/saga.single.val.data.csv")
  res2 <- LCA(data=beetle.dat,
                   SCS="XY",parental = "calc",env=FALSE,
                   max.pars = 7, ret.all=F)


  summary(lm(beetle.dat$mean~res2$cmatrix[,3], weights = beetle.dat$SE^-2))
  summary(lm(beetle.dat$mean~as.matrix(res2$cmatrix[,c(3,14)]), weights = beetle.dat$SE^-2))
  summary(lm(beetle.dat$mean~as.matrix(res2$cmatrix[,c(3,14,11)]), weights = beetle.dat$SE^-2))
  summary(lm(beetle.dat$mean~as.matrix(res2$cmatrix[,c(3,14,11,25)]), weights = beetle.dat$SE^-2))


res$best.eqns.w
res$best.models
par(mfcol=c(2,1))
library(viridis)
plot(res, col.ramp = viridis(100))

barplot(res$best.models[[1]]$coefficients[3:6],
        names=(c("Aa","AaAa","AaXd","XaCa")))
#To visualize the model space only works well for low max.pars values
#VisModelSpace(res)
res7<-res
plot(res3, min.vi = .55, main = "")
plot(res4, min.vi = .55, main = "")
plot(res5, min.vi = .55, main = "")
plot(res6, min.vi = .55, main = "")
plot(res7, min.vi = .75, main = "")

cumsum(rev(sort(res$best.eqns.w)))[1:3]

########################## Transformed Data #########################
transformed.dat <- read.csv("../data/saga.transformed.data.csv")

t.res <- LCA(data=transformed.dat,
           SCS="XY",parental = "calc",env=TRUE,
           max.pars = 5, ret.all=T)

#To visualize the model space only works well for low max.pars values
#VisModelSpace(res)

plot(t.res, min.vi = .5, main = "Transformed Data")

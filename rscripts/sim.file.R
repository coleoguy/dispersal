# lets imagine we have a system with 20 loci with equal additive effect
# under this system alleles at a locus contribute either 0% increased
# dispersal or 2.5% increased dispersal. This will yield a system where
# a line fixed for all positive effects has a dispersal tendency of 100%
# and a line fixed for all negative effects has a dispersal tendency of
# 0%. Furthermore we will assume that dispersal alleles are dominant to
# nondispersal alleles.

# we will designate locus 20 as cytotype

# Under this model lets evaluate what happens if our observed dispersals
# are the result of dispersion in high and low dispersal strains. In our
# high line P2 strains we observed dispersal proportion of 59% and just
# 5% in our low line P1 as such we can choose 12 random sites to fix for
# the dispersal allele in P2 and one randomly chosen locus to fix for
# dispersal allele in P1


# lets make a little function that takes two parents and
# makes offspring from them
getOffspring <- function(dam, sire){
  strand <- sample(1:2, 20, replace=T)
  strand[strand==2]<- 21
  allele <- strand + 0:19
  dam.gamete <- c(dam[1,],dam[2,])[allele]
  strand <- sample(1:2, 20, replace=T)
  strand[strand==2]<- 21
  allele <- strand + 0:19
  sire.gamete <- c(sire[1,],sire[2,])[allele]
  sire.gamete[20] <- dam.gamete[20]
  rbind(dam.gamete,sire.gamete)
}

# lets make a little function that takes a genome
# and reports dispersal as mentioned earlier we
# will assume dominance of dispersal alleles to
# attempt to recapitulate data like we observe
getDispersal <- function(genome){
  sum(colSums(genome)>0)*.05
}


phenotypes <- matrix(NA,8,100)
row.names(phenotypes) <- c("P1","P2","F1","rF1","BC1","BC2","rBC1","rBC2")

for(i in 1:100){
  # make an empty genome
  P1genome <- matrix(0, 2, 20)
  P2genome <- matrix(0, 2, 20)
  # fill with appropriate amount of dispersal alleles
  # always fixed for maximum oportunity for overdominance to appear
  # first row comes from dam second row from sire
  P1genome[,sample(1:20, 1)]<-c(1,1)
  P2genome[,sample(1:20, 12)] <- c(1,1)
  pop <- matrix(NA,8,100)
  for(j in 1:100){
    # first parent will be dam second will be sire
    F1genome <- getOffspring(P1genome, P2genome)
    rF1genome <- getOffspring(P2genome, P1genome)
    BC1genome <- getOffspring(F1genome, P1genome)
    BC2genome <- getOffspring(P2genome, F1genome)
    rBC1genome <- getOffspring(P1genome, rF1genome)
    rBC2genome <- getOffspring(rF1genome, P2genome)
    # record phenotypes
    pop[1,j] <- getDispersal(P1genome)
    pop[2,j] <- getDispersal(P2genome)
    pop[3,j] <- getDispersal(F1genome)
    pop[4,j] <- getDispersal(rF1genome)
    pop[5,j] <- getDispersal(BC1genome)
    pop[6,j] <- getDispersal(BC2genome)
    pop[7,j] <- getDispersal(rBC1genome)
    pop[8,j] <- getDispersal(rBC2genome)
  }
  phenotypes[,i] <-  rowMeans(pop)
}

getSE <- function(x){
  sqrt((x*(1-x))/100)
}

# lets evaluate all of these and ask what variables have
# varimp greater than .5
res.list <- list()

beetle.dat <- read.csv("../data/saga.single.val.data.csv")
#original data for comparison
library(SAGA2)
res.list[[1]] <- LCA(data=beetle.dat, max.pars = 4)

# now the 100 simulated datasets
for(i in 2:101){
  print(i)
  beetle.dat$mean <- phenotypes[,(i-1)]
  beetle.dat$SE <- getSE(beetle.dat$mean)
  res.list[[i]] <- LCA(data=beetle.dat, max.pars = 4)
}

# Lets parse these simulations and record the significant effects
# since we are trying to vet our emprical results lets only look
# at those effects with a variable importance score of greater than
# 67% minimum that we found significant.

# This function finds the significant results and stores them for us
getSig <- function(x){
  good.index <- which(as.numeric(x$varimp[,2])>.67)
  if(!is.na(good.index[1])){
    bots <- (as.numeric(x$estimates[1,]) -
               as.numeric(x$estimates[2,]))[good.index+1]
    tops <- (as.numeric(x$estimates[1,]) +
               as.numeric(x$estimates[2,]))[good.index+1]
    diff.pars <- c()
    for(i in 1:length(tops)){
      scheck <- bots[i]*tops[i]
      if(scheck > 0) diff.pars <- c(diff.pars, good.index[i])
    }
    effects <- x$varimp[diff.pars,1]
    estimates <- x$estimates[,(diff.pars+1)]
    return(list(effects, estimates))
  }
}





# this is just a pause function used in visualizing plots of
# all results
pause = function()
{
  if (interactive())
  {
    invisible(readline(prompt = "Press <Enter> to continue..."))
  }
  else
  {
    cat("Press <Enter> to continue...")
    invisible(readLines(file("stdin"), 1))
  }
}
library(viridis)
for(i in 1:101){
  plot(res.list[[i]], min.vi=.5, col=viridis(100))
  pause()
}



sig.results <- as.data.frame(matrix(NA,300,30))
counter <- 1
for(i in 1:101){
  foo <- getSig(res.list[[i]])
  bar <- foo[[1]]
  if(!is.null(bar)){
    sig.results[counter, 1:length(bar)] <- bar
    bar <- foo[[2]]
    if(is.null(nrow(bar))){
      sig.results[((counter+1):(counter+2)),1] <- bar
    }else{
      sig.results[((counter+1):(counter+2)),1:ncol(bar)] <- bar
    }
    counter <- counter + 3
  }
}

# check for transgressive segregation
trans.check <- c()
for(i in 1:101){
  foo <- res.list[[i]]$best.models[[1]]$fitted.values
  trans.check[i] <- max(foo[3],foo[4]) - foo[2]
}





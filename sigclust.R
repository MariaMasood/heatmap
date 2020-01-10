nsim <- 1000
nrep <- 1
icovest <- 1
library("gplots")
pvalue <- sigclust(f,nsim=nsim,nrep=nrep,labflag=0,icovest=icovest)#sigclust plot
plot(pvalue)
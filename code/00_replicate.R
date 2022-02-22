#########################################################
#########################################################

#clear workspace
rm(list=ls())

#load required packages
require(stringr)
require(plyr)
require(dplyr)
require(tidyr)
require(data.table)
require(zoo)
require(rprojroot)

#set dirs
rootdir<-find_root(
  criterion=has_file('china shock.Rproj')
)

#########################################################
#########################################################

#load replication data
setwd(file.path(rootdir,"repdata"))
repdf<-fread('repdata.csv')

#DV is named 'val', for reasons having to do w/ the main code
#rename here for clarity
names(repdf)[names(repdf)=="val"]<-"D.incRate_corrected_estimated_25_ln"

#estimate main model
require(ivpack)
m.tmp <- ivreg(
  data=repdf,
  form=
    D.incRate_corrected_estimated_25_ln ~ 
    factor(state_fips) + #state FE
    factor(periodf) + #period FE
    blackpop_pct + #blackpop_t0
    l_sh_popfborn + #foreignborn_t0
    incRate_corrected_estimated_25_ln + #incrate_t0
    D.emptopop | #emptopop
    factor(state_fips) + 
    factor(periodf) + 
    blackpop_pct + 
    l_sh_popfborn + 
    incRate_corrected_estimated_25_ln +
    otch, #cs instrument
  weights = population
)

#########################################################
#########################################################

#we want to estimate this w/ cluster robust se

### (1) using custom method

#robust se function (http://drewdimmery.com/robust-ses-in-r/)
robust.se <- function(model, cluster){
  require(sandwich)
  require(lmtest)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- model$rank
  dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
  uj <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum));
  rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
  rcse.se <- coeftest(model, rcse.cov)
  return(list(rcse.cov, rcse.se))
}
clustervar<-mapply(
  paste,
  "cz90",
  repdf$cz90,
  sep=""
) %>% unname
tmpoutput<-robust.se(
  m.tmp,
  clustervar
)
myests<-tmpoutput[[2]]

### (2) using built-in function to ivreg, from ivpack
myests2<-cluster.robust.se(
  m.tmp,
  clusterid=repdf$cz90
)

### (1) and (2) are the same
tmp<-row.names(myests)=="D.emptopop"
myests[tmp,1]==myests2[tmp,1]


#########################################################
#########################################################

#get key estimate 
keyest<-myests[tmp,1]

#semi-standardized ( as in fig_ols2sls)
100 * ( keyest * sd(repdf$D.emptopop) ) #21.6
#incRate is in 0-1 units, so multiply by 100 to get %

#as an elasticity
100 * keyest

#standardized (as in fig_rffs)
( keyest * sd(repdf$D.emptopop) ) / 
  sd(repdf$D.incRate_corrected_estimated_25_ln) #0.62



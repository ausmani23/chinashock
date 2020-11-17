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
codedir<-file.path(rootdir,"code")
setwd(codedir); dir()
source('dirs.R')

#load RData
setwd(filesdir); dir()
load("20_cfactuals.RData")

#load a function to get prettynames
setwd(codedir); dir()
source('functions.R')
source('predict_ivreg.R')

#set seed
set.seed(23)

#########################################################
#########################################################

#ACTUAL IMPRATES
#goal is to generate an approximate trajectory
#for the visualization, over the 1991-2011 period

#we right now have a cz-periodf-rep-seq dataset
#of start and endincrates; 

#but (1) b/c of inconsitstent
#sample across the two periods, they don't quite match
#and (2) we don't have intervening years

#so this is what we do:

#(1)
#use a deflator from the observed data to make sure
#observed endpoint in first period matches the
#observed startpoint in the second, and then

#apply this deflator to all predictions in first period

#(2)
#then, we fill in intermediate years by 
#relying on Vera dataset.. 

#########################################################
#########################################################

#make a ratesdf from the predictions
keyvars<-c(
  "cz90",
  "periodf",
  "rep",
  "seq.j",
  "incRate_st",
  "incRate_end"
)
ratesdf<-predictdf[,keyvars]
ratesdf<-gather(
  ratesdf,
  var,
  val,
  incRate_st:incRate_end
)
ratesdf$year<-1991
tmp<-ratesdf$periodf=="first" & 
  ratesdf$var=="incRate_end"
ratesdf$year[tmp]<-"1999a"
tmp<-ratesdf$periodf=="second.alt" & 
  ratesdf$var=="incRate_st"
ratesdf$year[tmp]<-"1999b"
tmp<-ratesdf$periodf=="second.alt" & 
  ratesdf$var=="incRate_end"
ratesdf$year[tmp]<-2011
ratesdf$var<-NULL
ratesdf$periodf<-NULL
ratesdf<-spread(
  ratesdf,
  year,
  val
)
ratesdf<-data.table(ratesdf)

#where we have both periods, no problem
tmp<-complete.cases(ratesdf[,c("1999a","1999b")])
tmpdf<-ratesdf[tmp,]
sum(tmpdf$`1999a`!=tmpdf$`1999b`) #all equal
ratesdf$`1999`[tmp]<-ratesdf$`1999a`[tmp] #doesn't matter which we take
bothp.czs<-unique(ratesdf$cz90[tmp])

#for those which only have first period
tmp<-!is.na(ratesdf$`1991`) & 
  is.na(ratesdf$`2011`)
firstp.czs_only<-unique(ratesdf$cz90[tmp])
ratesdf$`1999`[tmp]<-ratesdf$`1999a`[tmp]

#for those which only have second period
tmp<-is.na(ratesdf$`1991`) & 
  !is.na(ratesdf$`2011`)
secondp.czs_only<-unique(ratesdf$cz90[tmp])
ratesdf$`1999`[tmp]<-ratesdf$`1999b`[tmp]

#keep only 1999
ratesdf$`1999a`<-ratesdf$`1999b`<-NULL

#put this in format ready for merge
tmp<-names(ratesdf)%in%c("1991","1999","2011")
names(ratesdf)[tmp]<-paste0(
  "incRate_hat_",names(ratesdf)[tmp]
)

#########################################################
#########################################################

#bring in actual incrates
setwd(datadir); dir()
veradf<-read.csv(
  'vera_2002.csv',
  stringsAsFactors=F
)
tmpvars<-c(
  "cz90",
  "year",
  "incRate_corrected_estimated_25"
)
veradf<-veradf[,tmpvars]
names(veradf)[3]<-"incRate"

#we don't need data from before 1991
#and missing data won't help us
tmp<-veradf$year>=1991 & 
  !is.na(veradf$incRate)
veradf<-veradf[tmp,]

#need imprates
impdf<-veradf[,c("cz90","year","incRate")]
impdf<-spread(
  impdf,
  year,
  incRate
)
names(impdf)<-paste0(
  "incRate_",names(impdf)
)
names(impdf)[1]<-"cz90"

#merge predictions w/ actual
ratesdf<-merge(
  ratesdf,
  impdf,
  by=c(
    "cz90"
  ),
  all.x=T
)

#gather
tmp<-str_detect(names(ratesdf),"incRate")
gathvars<-names(ratesdf)[tmp]
ratesdf<-gather_(
  ratesdf,
  "var",
  "val",
  gathvars
)

#separate hat/year
ratesdf<-data.table(ratesdf)
ratesdf[,year:=as.numeric(str_extract(var,"[0-9]{4}"))]
ratesdf[,hat:=str_detect(var,"hat")]
ratesdf$var<-NULL
ratesdf<-spread(
  ratesdf,
  hat,
  val
)
names(ratesdf)[names(ratesdf)=="TRUE"]<-"pred"
names(ratesdf)[names(ratesdf)=="FALSE"]<-"raw"

#apply the solution, where we intepolate inflator
#and this will give us the kind of graph that we want..
ratesdf$inflator<-ratesdf$pred/ratesdf$raw

#ipolate this inflator, within cz,rep,seq
ratesdf<-data.table(ratesdf)

#calculate the inflator
ratesdf[
  ,
  inflator:=na.approx(inflator,na.rm=F)
  ,
  by=c(
    "cz90",
    "rep",
    "seq.j"
  )
  ]

#this is the value for plotting
ratesdf$pred_plot<-
  ratesdf$raw * ratesdf$inflator

#finalize the plotting df
ratesdf$raw<-
  ratesdf$pred<-
  ratesdf$inflator<-
  NULL
ratesdf<-ratesdf[!is.na(pred_plot)]

#make a note of availability of which czs
bothp.czs %>% unique %>% length
firstp.czs_only %>% unique %>% length
secondp.czs_only %>% unique %>% length
tmp<-ratesdf$cz90%in%bothp.czs
ratesdf$periods[tmp]<-"both"
tmp<-ratesdf$cz90%in%firstp.czs_only
ratesdf$periods[tmp]<-"firstonly"
tmp<-ratesdf$cz90%in%secondp.czs_only
ratesdf$periods[tmp]<-"secondonly"

#########################################################
#########################################################

#COMPILING AN AVG CZ AND CALCULATING THE DEFLATOR

plotdfs<-list()

#compile avg cz and avgs separately
#for the two periods
#this will allow me to calculate a 
#deflation factor to apply to the secon

#this should be a weighted average..
setwd(file.path(datadir,"prelim")); dir()
tmpdf<-fread("county-cz1990-incarceration-estimates_2002.csv")
popdf<-tmpdf[,c("commuting_zone_1990","year","total_pop_15to64"),with=F]
names(popdf)<-c("cz90","year","pop1564")
popdf<-popdf[
  !is.na(cz90) & !is.na(year)
  ,
  .(pop1564=sum(pop1564))
  ,
  by=c("cz90","year")
  ]
ratesdf<-merge(
  ratesdf,
  popdf,
  by=c('cz90','year'),
  all.x=T
)
tmp<-sum(is.na(ratesdf$pop1564))
if(tmp!=0) stop('missing pop')

periods<-c("first","second")
period_years<-list(1991:1999,1999:2011)
tmpseq.i<-seq_along(periods)
rawratedfs<-lapply(tmpseq.i,function(i) {
  #i<-1
  thisp<-periods[i]
  thisp_years<-period_years[[i]]
  tmp<-ratesdf$periods%in%c(paste0(thisp,"only"),"both") &
    ratesdf$year%in%thisp_years
  tmpdf<-data.table(ratesdf[tmp,])
  tmpdf$periods<-NULL
  avgdf<-tmpdf[
    ,
    list(
      pred_plot=weighted.mean(
        pred_plot,
        w=pop1564
      )
    )
    ,
    by=c(
      "rep",
      "seq.j",
      "year"
    )
    ]
  avgdf$cz90<-"average"
  tmpdf<-rbind.fill(
    tmpdf,
    avgdf
  )
  tmpdf
  # tmpdf<-merge(tmpdf,loopdf)
  # tmpdf
})
names(rawratedfs)<-periods

#to construct the final df,
#we have to deal w/ the fact that
#the two periods are not a consistent
#sample.

#we calculate the val we need to make
#the observed averages match, 
#and we mutliply all second period vals
#by that amount; and then we take the 
#second period vals as our val for 1999

#which is observed? just choose one of them
myseq.j<-loopdf$seq.j[loopdf$cfactual=="observed"][1]

tmpdf<-rawratedfs$first
tmp<-tmpdf$cz90=="average" &
  tmpdf$seq.j==myseq.j & 
  tmpdf$rep==1 &
  tmpdf$year==1999
p1_1999<-tmpdf$pred_plot[tmp]

tmpdf<-rawratedfs$second
tmp<-tmpdf$cz90=="average" &
  tmpdf$seq.j==myseq.j & 
  tmpdf$rep==1 & 
  tmpdf$year==1999
p2_1999<-tmpdf$pred_plot[tmp]

infl_factor <- p2_1999 / p1_1999
#they match closely, which is good!

#remove 1999 from p1
rawratedf_p1<-rawratedfs$first
tmp<-rawratedf_p1$year==1999
rawratedf_p1<-rawratedf_p1[!tmp,]
rawratedf_p2<-rawratedfs$second
rawratedf_p2$pred_plot<-rawratedf_p2$pred_plot / infl_factor

#put these together
rawratedf<-rbind.fill(
  rawratedf_p1,
  rawratedf_p2
)
rm(rawratedf_p1); rm(rawratedf_p2)

#we can summarize to get avg + SE
#across all reps; this is useful for plotting
rawratedf<-data.table(rawratedf)
sumratedf<-rawratedf[
  ,
  list(
    avg=mean(pred_plot),
    med=quantile(pred_plot,0.5),
    min=quantile(pred_plot,0.025),
    max=quantile(pred_plot,0.975)
  )
  ,
  by=c(
    "cz90",
    "year",
    "seq.j"
  )
  ]

#integrate loop info
rawratedf<-merge(
  rawratedf,
  loopdf
)

sumratedf<-merge(
  sumratedf,
  loopdf
)

#########################################################
#########################################################

#GET STATS

#since we extend 90s boom into 2000s,
#natural question is: 
#how much did incarceration increase? 
#how much would it have increased? 

tmpf<-function(base,end) {
  100 * (end-base)/base
}

statsdf<-rawratedf[
  cz90=="average" &
    cfactual%in%c(
      '90spreserved',
      'predicted',
      'observed'
    )
  , 
  list(
    pctchange=tmpf(pred_plot[year==1999],pred_plot[year==2011])
  )
  ,
  by=c(
    'cz90',
    'cfactual',
    'endogenous',
    'rep'
  )
]

sumstatsdf<-statsdf[
  ,
  summarize.distribution2(pctchange)
  ,
  by=c(
    "cz90",
    'cfactual',
    "endogenous"
  )
  ]

#########################################################
#########################################################

#DEPRECATED; THIS WAS THE PREVIOUS CFACTULA EXERCISE
#NO NEED TO GET THIS COMPLICATED, SO WE SIMLPIFIED AS ABOVE

#' #GET STATS 
#' 
#' #one small issue
#' #we only have observed for rep 1
#' #to fix this, I extract observed
#' #and merge it back in w/ reps ignored
#' tmp<-rawratedf$cfactual=="observed"
#' obsdf<-rawratedf[tmp,]
#' obsdf$seq.j<-obsdf$rep<-obsdf$cfactual<-NULL
#' names(obsdf)[names(obsdf)=="pred_plot"]<-"observed"
#' 
#' statsdf<-rawratedf[!tmp,]
#' statsdf$seq.j<-NULL
#' statsdf<-data.table(statsdf)
#' 
#' #make a note of cfactuals
#' base.cfactuals<-c("predicted","observed")
#' tmp<-statsdf$cfactual%in%base.cfactuals
#' oth.cfactuals<-unique(statsdf$cfactual[!tmp])
#' 
#' #spread
#' statsdf<-spread(
#'   statsdf,
#'   cfactual,
#'   pred_plot
#' )
#' 
#' #bring back in observed
#' statsdf<-merge(
#'   statsdf,
#'   obsdf
#' )
#' 
#' #gather
#' statsdf<-gather_(
#'   statsdf,
#'   "cfactual",
#'   "rate_hat",
#'   oth.cfactuals
#' )
#' statsdf<-data.table(statsdf)
#' 
#' #########################################################
#' #########################################################
#' 
#' #CALCULATE STATS
#' 
#' #since we extend 90s boom into 2000s,
#' #natural question is: 
#' #how much did incarceration increase? 
#' #how much would it have increased? 
#' 
#' tmpf<-function(base,end) {
#'   100 * (end-base)/base
#' }
#' 
#' tmpdf<-statsdf[
#'   cz90=="average" 
#'   ,
#'   list(
#'     pctexplained = 
#'       tmpf(
#'         rate_hat[year==2011],
#'         predicted[year==2011] - predicted[year==1991]
#'       ),
#'     pctexplained_observed = 
#'       tmpf(
#'         rate_hat[year==2011] - observed[year==1991],
#'         observed[year==2011] - observed[year==1991]
#'       )
#'   )
#'   ,
#'   by=c(
#'     "cz90",
#'     "cfactual",
#'     "endogenous",
#'     "rep"
#'   )
#'   ]
#' 
#' 
#' 
#' 
#' #this function calcs cfactual chg
#' #as pct of base chg, 
#' #and subtracts from 100 to get
#' #'pctexplained'
#' tmpf<-function(cfactualchg,basechg) {
#'   100 * (1 - cfactualchg/basechg)
#' }
#' 
#' tmpdf<-statsdf[
#'   cz90=="average" 
#'   ,
#'   list(
#'     pctexplained = 
#'       tmpf(
#'         rate_hat[year==2011] - predicted[year==1991],
#'         predicted[year==2011] - predicted[year==1991]
#'       ),
#'     pctexplained_observed = 
#'       tmpf(
#'         rate_hat[year==2011] - observed[year==1991],
#'         observed[year==2011] - observed[year==1991]
#'       )
#'   )
#'   ,
#'   by=c(
#'     "cz90",
#'     "cfactual",
#'     "endogenous",
#'     "rep"
#'   )
#'   ]
#' tmpdf<-gather(
#'   tmpdf,
#'   var,
#'   val,
#'   pctexplained_observed:pctexplained
#' )
#' 
#' tmpdf<-data.table(tmpdf)
#' sumstatsdf<-tmpdf[
#'   cz90=="average"
#'   ,
#'   summarize.distribution2(val)
#'   ,
#'   by=c(
#'     "cz90",
#'     "cfactual",
#'     "endogenous",
#'     "var"
#'   )
#'   ]

#########################################################
#########################################################

setwd(filesdir); dir()
nrow(sumratedf)
nrow(sumstatsdf)
write.csv(
  sumratedf[cz90=="average"],
  'sumratedf.csv',
  row.names=F
)
write.csv(
  sumstatsdf,
  'sumstatsdf.csv',
  row.names=F
)

#takes a bit too much space
# save.image(
#   '23_cfactuals_rates.RData'
# )

tmp<-sapply(ls(),function(x) object.size(get(x)))
names(tmp)<-ls()
abs(sort(-tmp)[1:10])/1000000












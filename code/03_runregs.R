#########################################################
#########################################################

#clear workspace
rm(list=ls())

#load packages
require(stringr)
require(plyr)
require(dplyr)
require(zoo)
require(plm)
require(tidyr)
require(rprojroot)
require(data.table)

#set dirs
rootdir<-find_root(
  criterion=has_file('china shock.Rproj')
)
codedir<-file.path(rootdir,"code")
setwd(codedir); dir()
source('dirs.R')

#########################################################
#########################################################

#load image
setwd(datadir); dir()
load('02_prepped.RData')

#functions
setwd(codedir)
source('functions.R')

#helper function
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

#get pval class
get.pvals.class<-function(pvals) {
  y<-NA
  y[pvals<0.01]<-"at alpha=0.01"
  y[pvals>0.01 & pvals<0.05]<-"at alpha=0.05"
  y[pvals>0.05 & pvals<0.10]<-"at alpha=0.10"
  y[pvals>0.10]<-"not sig"
  return(y)
}

#########################################################
#########################################################

#extra packs
require(ivpack)

#########################################################
#########################################################

#POPULATE THE WHOLE MODEL SPACE

#########################################################
#########################################################

#define all iv strategies

tmpdf<-data.frame(
  name="emptopopc",
  endogenous="D.emptopopc",
  stringsAsFactors=F
)

ends <- c(
  "D.manushare",
  "D.emptopop",
  "D.unemprate",
  tmpdf$endogenous
)
names(ends)<-c(
  "manuf",
  "emptopop",
  "unemp",
  tmpdf$name
)
ivs <- c(
  china="otch"
)
loopdf<-expand.grid(
  end=names(ends),
  iv=names(ivs),
  stringsAsFactors = F
)
tmpseq.i<-1:nrow(loopdf)
tmpoutput<-lapply(tmpseq.i,function(i) {
  #i<-1
  thisrow<-loopdf[i,]
  thisname<-apply(
    thisrow,1,paste0,collapse="_"
  )
  thisname<-str_replace_all(thisname,"(\\_)+$","") %>%
    str_replace_all("\\_{2,10}","_")
  thisendogenous<-list(
    end=unname(ends[names(ends)==thisrow$end]),
    iv=unname(ivs[names(ivs)==thisrow$iv])
  )
  list(
    strategy=thisendogenous,
    name=thisname
  )
})

ivs_list<-lapply(tmpoutput,function(x) x$strategy)
names(ivs_list)<-sapply(tmpoutput,function(x) x$name)
extra_list<-list(
  china=list(
    end="usch",
    iv="otch"
  ),
  china_alt=list(
    end="d_tradeusch_pw",
    iv="d_tradeotch_pw_lag"
  )
)
ivs_list<-append(
  extra_list,
  ivs_list
)

#name this for next step
endogenous<-names(ivs_list)
tmp<-endogenous%in%c(
  "manuf_china",
  "emptopopc_china",
  "unemp_china",
  "emptopop_china"
)
names(endogenous)[tmp]<-"preferred"

#########################################################
#########################################################

#SET UP DVS

tmpdvs<-c(
  #no need for these, 
  #these are just replication
  #of well-established resulst
  # "D.emptopop",
  # "D.emptopopc",
  # "D.manushare",
  # "D.unemprate",
  ###crimevars
  "D.violent_crt",
  "D.property_crt",
  "D.vcrt_fs",
  "D.pcrt_fs",
  "D.murder_crt",
  "D.rape_crt",
  "D.robbery_crt",
  "D.agasslt_crt",
  "D.burglry_crt",
  "D.mvtheft_crt",
  "D.arson_crt",
  ###policevars
  "D.officers",
  "D.employees",
  "D.officers_all",
  "D.employees_all",
  #"D.policesp",
  ###finances
  "D.rev",
  "D.spend",
  "D.eduspend",
  "D.healthspend",
  "D.welfspend",
  "D.policespend",
  "D.jailspend",
  "D.courtspend",
  "D.edushare",
  "D.healthshare",
  "D.welfshare",
  "D.policeshare",
  "D.jailshare",
  "D.courtshare"
)

#get the vera vars
tmp<-str_detect(
  names(covarsdf),
  "corrected"
) & 
  #no need for the 06/19 vintage
  !str_detect(
    names(covarsdf),"1906"
  ) &
  #no need for log versions here
  !str_detect(
    names(covarsdf),"ln"
  )
redodvs<-names(covarsdf)[tmp]
redodvs<-paste0("D.",redodvs)

#name this for next step
tmp<-redodvs=="D.incRate_corrected_estimated_25"
names(redodvs)[tmp]<-"preferred"

#########################################################
#########################################################

#SET UP MOD PERMS

modslist<-list(
  dv=c(
    tmpdvs,
    redodvs
  ),
  logdv=c(
    preferred=T,
    F
  ),
  period=c(
    "first",
    "second",
    "second.alt",
    "stacked",
    preferred="stacked.alt",
    "long",
    "long.alt"
  ),
  model=c(
    "simple",
    #"divisionfe",
    "statefe",
    "periodfe",
    #"dpfe",
    "spfe",
    preferred="preferred",
    "czfe"
  ),
  restriction=c(
    preferred="unrestricted",
    "consistent"
  ),
  endogenous=c(
    endogenous
  ),
  instrumented=c(
    "ols",
    preferred="instrumented"
  ),
  latestart=c(
    preferred="samestart",
    "latestart"
  ),
  weights=c(
    preferred="yes",
    "no"
  ),
  decadal_dv=c(
    preferred="yes",
    "no"
  ),
  decadal_shock=c(
    preferred="yes",
    "no"
  ),
  dropoutliers=c(
    "yes",
    preferred="no",
    "winsorize1",
    "winsorize2"
  ),
  dropsmallczs=c(
    preferred="no",
    "<10k",
    "<100k"
  )
)

#########################################################
#########################################################

#make prefmods
modslist

tmpseq.i<-seq_along(modslist)
prefmodslist<-lapply(tmpseq.i,function(i) {
  #i<-7
  cat<-names(modslist)[i]
  tmp<-names(modslist[[i]])=="preferred" &
    !is.na(names(modslist[[i]]))
  option<-modslist[[i]][tmp] %>% unname
  option
}) 
names(prefmodslist)<-names(modslist)
prefmodsdf<-expand.grid(
  prefmodslist,
  stringsAsFactors=F
)
if(!nrow(prefmodsdf)>0)
  stop('didnt identify a preferred model')

#to get the robmods
#we loop through each condition
#keeping all other choices from prefmods
tmpseq.i<-seq_along(names(prefmodsdf))
robdf<-lapply(tmpseq.i,function(i) {
  #i<-1
  thisname<-names(prefmodsdf)[i]
  othnames<-names(prefmodsdf)[-i]
  #take thisname from modsdf
  #take othnames from prefmodsdf/tmpdf
  thisperm<-lapply(thisname,function(x)
    unique(modslist[[x]])
  )
  othperms<-lapply(othnames,function(x)
    unique(prefmodsdf[[x]])
  )
  allperms<-append(
    thisperm,
    othperms
  )
  #return this
  returndf<-expand.grid(allperms,stringsAsFactors = F)
  names(returndf)<-c(thisname,othnames)
  returndf
}) %>% rbind.fill

#########################################################
#########################################################

#PUT THESE TOGETHER

#robdf shoulnd't have anything that is in prefmodsdf
#to make sure that it doesn't, add prefmods to prefmods
prefmodsdf$prefmods<-T
robdf<-merge(
  robdf,
  prefmodsdf,
  all.x=T
)
tmp<-is.na(robdf$prefmods)
robdf<-robdf[tmp,]
robdf$prefmods<-F

#make each unique
nrow(prefmodsdf)
prefmodsdf<-unique(prefmodsdf)
nrow(prefmodsdf)
nrow(robdf)
robdf<-unique(robdf)
nrow(robdf)

#these are now runmods
runmodsdf<-rbind(
  prefmodsdf,
  robdf
)

#########################################################
#########################################################

#ADJUST/ADD MODELS

#no point doing dv manushare/emptopop/unemprate
#with the manushare/emptopop endogenous
#do china and china_alt, instead
tmp<-runmodsdf$dv%in%c(
  "D.emptopop",
  "D.emptopopc",
  "D.manushare",
  "D.unemprate"
)
runmodsdf$endogenous[tmp & runmodsdf$endogenous=="unemp_china"]<-"china"

#fs data can't be used w/ stacked.long
tmp<-runmodsdf$endogenous=="china_alt"
tmp<-tmp | runmodsdf$dv%in%c(
  "D.vcrt_fs",
  "D.pcrt_fs"
)
runmodsdf$period[tmp]<-"stacked" #b/c no 2011 data
runmodsdf$logdv[tmp]<-F

#our crime vars
#shouldn't be estimated
#on pre-1994 data
tmp<-str_detect(
  runmodsdf$dv,
  "\\_crt$"
)
runmodsdf$latestart[tmp]<-"latestart"
runmodsdf$logdv[tmp]<-F #not logged, b/c of zero's

#my default choice for robustness check
#with decadal rates will be one where i inflate both
tmp<-runmodsdf$prefmods
newrow<-runmodsdf[tmp,]
newrow$decadal_dv<-newrow$decadal_shock<-"yes"
newrow$prefmods<-F
runmodsdf<-rbind.fill(
  runmodsdf,
  newrow
)

#DEPRECTATED B/C ADDED OWN DATA
# tmp<-str_detect(runmodsdf$dv,"rev|spend|share")
# runmodsdf$period[tmp]<-"stacked" #b/c no 2011 data
#share/ratio shouldn't be logged
tmp<-str_detect(runmodsdf$dv,"share")
runmodsdf$logdv[tmp]<-F

#########

#get indices
runmodsdf$i<-1:nrow(runmodsdf)
print(nrow(runmodsdf))

#order
roworder<-runmodsdf$i
runmodsdf<-runmodsdf[roworder,]
row.names(runmodsdf)<-NULL

#trimming?
runmodsdf$i<-1:nrow(runmodsdf)

#########################################################
#########################################################

#run reg
#return sample
#and model

tmpseq.i<-1:nrow(runmodsdf)
fulloutput<-lapply(tmpseq.i,function(i) {
  
  #i<-which(runmodsdf$dropoutliers=="winsorize")
  #i<-263
  
  print(
    paste(
      i,"of",length(tmpseq.i)
    )
  )
  
  ##########
  
  #get params
  thisrow<-runmodsdf[i,]
  thisdv<-thisrow$dv
  thislogdv<-thisrow$logdv
  if(thislogdv)
    thisdv<-paste0(thisdv,"_ln")
  thisperiod<-thisrow$period
  thismodel<-thisrow$model
  thisrestriction<-thisrow$restriction
  thisendogenous<-thisrow$endogenous
  thisinst<-thisrow$instrumented
  thislatestart<-thisrow$latestart
  thisweights<-thisrow$weights
  thisdecadal_dv<-thisrow$decadal_dv
  thisdecadal_shock<-thisrow$decadal_shock
  thisdropoutliers<-thisrow$dropoutliers
  thisdropsmallczs<-thisrow$dropsmallczs
  
  #########
  
  #get the correct dv and main iv dfs
  
  #we do this by creating mydvdf
  #since we don't want to overwrite info in dvdf
  mydvdf <- dvdf
  
  #get mydvdf
  tmprows<-mydvdf$dv==thisdv
  sum(tmprows)
  
  #get period; all are classified
  if(thisperiod=="stacked") {
    myperiods<-c("first","second")
  } else if(thisperiod=="stacked.alt") {
    myperiods<-c("first","second.alt")
  } else {
    myperiods<-thisperiod #anything else, same name
  }
  
  tmprows<-tmprows &
    mydvdf$periodf%in%myperiods
  sum(tmprows)
  
  #get appropriate starttime
  #we pick eithe rlatestart or irrelevant
  #if there is no timiming issue
  tmprows<-tmprows &
    mydvdf$latestart%in%c(
      thislatestart,
      "irrelevant"
    )
  sum(tmprows)

  # #DEPRECATED  
  # #select sample
  # head(mydvdf)
  # tmprows<-tmprows &
  #   mydvdf$sample%in%c(
  #     "prefsample",
  #     "irrelevant"
  #   )
  # sum(tmprows)
  
  #get restriction
  if(thisrestriction=="unrestricted") {
    tmprows<-tmprows & rep(T,nrow(mydvdf))
  } else if(thisrestriction=="consistent") {
    tmptab<-table(mydvdf$cz90[tmprows & !is.na(mydvdf$val)])
    consistentCZs<-names(tmptab==max(tmptab))
    tmprows<-tmprows &
      mydvdf$cz90%in%consistentCZs
  } else if(thisrestriction=="onlyimpRate") {
    tmp<-mydvdf$dv==str_replace(thisdv,"inc|jail","imp") &
      mydvdf$sample=="irrelevant" &
      mydvdf$periodf%in%c("first","second") &
      mydvdf$latestart=="samestart" &
      !is.na(mydvdf$val)
    impCZs<-mydvdf$cz90[tmp] %>% unique
    tmprows<-tmprows &
      mydvdf$cz90%in%impCZs
  }
  
  #get final mydvdf
  mydvdf<-mydvdf[tmprows & !is.na(mydvdf$val),]
  
  #drop outliers from DV (this is b/c of likely errors in vera)
  if(thisdropoutliers=="yes") {
    tmpval<-abs(scale(mydvdf$val))
    summary(tmpval) #some of these are huge; clear errors
    #anything larger than 3 sd's away from mean, drop
    tmp<-abs(scale(mydvdf$val))<=3
    sum(!tmp) #drops about 5
    mydvdf<-mydvdf[tmp,]
  } else if(thisdropoutliers=="winsorize1") {
    mydvdf$val<-winsorize1(mydvdf$val,fraction=0.01)
    table(table(mydvdf$val))
    #transforms about 20
  } else if (thisdropoutliers=="winsorize2") {
    mydvdf$val<-winsorize2(mydvdf$val,multiple=3)
    #transforms about 150
  } else  {
    #nothing
  }
  
  #multiply by 10/X, if decadal growth rates are desired
  if(thisdecadal_dv=="yes") {
    tmpperiods<-mydvdf$period %>% unique
    tmplist<-lapply(str_extract_all(tmpperiods,"[0-9]{4}"),function(xs) {
      diff(as.numeric(xs))
    }); names(tmplist)<-tmpperiods
    tmpseq.s<-seq_along(tmplist)
    for(s in tmpseq.s) {
      mydvdf$val[mydvdf$period==names(tmplist)[s]] <-
        mydvdf$val[mydvdf$period==names(tmplist)[s]] * (10/tmplist[[s]])
    }
  }
  
  #go no further if:
  if(nrow(mydvdf)==0)
    stop('no dv data')
  
  #get shockdf
  myshockdf<-shockdf
  #get cols
  shockvars <- ivs_list[[thisendogenous]]
  shockvars<-str_split(
    shockvars,
    "\\+"
  ) %>% unlist %>%
    str_replace_all(
      "^\\s+|\\s+$",
      ""
    )
  
  tmpvars<-c(
    "cz90",
    "periodf",
    shockvars
  )
  #trim period
  tmprows<-shockdf$periodf%in%myperiods
  #trim
  myshockdf <- myshockdf[tmprows,tmpvars]
  
  #note that Feler and Senses don't multiply the shockvars,
  #they just multiply the DV. I can't see a reason for this.
  #I therefore examine robustness of their results/our results
  #to the choice to multiply the shock by a factor that
  #means that the shock is calibrated to be a 10yr shock
  #this is a bit more complicated than dv, b/c of how I've organized it
  
  #multiply by 10/X, if decadal growth rates are desired
  if(thisdecadal_shock=="yes") {
    for(v in shockvars) {
      #v<-shockvars[1]
      tmp<-sapply(shocklist,function(df) v%in%names(df))
      tmpdf<-shocklist[tmp][[1]]
      tmpdf<-tmpdf[tmpdf$periodf%in%myperiods,]
      tmpperiodfs<-tmpdf$periodf %>% unique
      tmplist<-lapply(tmpperiodfs,function(pf) {
        #pf<-"first"
        p<-tmpdf$period[tmpdf$periodf==pf] %>% unique
        xs<-str_extract_all(p,"[0-9]{4}")[[1]]
        diff(as.numeric(xs))
      }); names(tmplist)<-tmpperiodfs
      tmpseq.r<-seq_along(tmplist)
      for(r in tmpseq.r) {
        #r<-1
        myshockdf[[v]][myshockdf$period==names(tmplist)[r]] <-
          myshockdf[[v]][myshockdf$period==names(tmplist)[r]] * (10/tmplist[[r]])
      }
    }
  }
  
  #########
  
  #get covariatesdf
  
  #subset to appropriate period
  if(str_detect(thisperiod,"stacked")) {
    tmprows<-rep(T,nrow(covarsdf))
  } else if(str_detect(thisperiod,"first|long")) {
    tmprows<-covarsdf$year==1991
  } else if(str_detect(thisperiod,"second")) {
    tmprows<-covarsdf$year==1999
  }
  mycovarsdf<-covarsdf[tmprows,]
  
  #add period info for merge w/ above dfs
  if(thisperiod=="stacked") {
    tmp<-mycovarsdf$year==1991
    mycovarsdf$periodf[tmp]<-"first"
    mycovarsdf$periodf[!tmp]<-"second"
  } else if (thisperiod=="stacked.alt") {
    tmp<-mycovarsdf$year==1991
    mycovarsdf$periodf[tmp]<-"first"
    mycovarsdf$periodf[!tmp]<-"second.alt"
  } else {
    mycovarsdf$periodf <- thisperiod
  }
  
  #########
  
  #merge all of these together
  #to produce regdf
  
  intersect(
    names(mydvdf),
    names(myshockdf)
  )
  mydvdf$period<-NULL
  myshockdf$period<-NULL
  
  thisdf<-merge(
    mydvdf,
    myshockdf,
    by=c(
      "cz90",
      "periodf"
    )
  )
  
  #add covarsdf
  intersect(
    names(thisdf),
    names(mycovarsdf)
  )
  thisdf<-merge(
    thisdf,
    mycovarsdf,
    by=c(
      "cz90",
      "periodf"
    )
  )
  
  #quick check to make sure
  #we don't have too many obs
  if(max(table(thisdf$cz90))>2)
    stop('too many obs')
  
  #########
  
  #set up the 2s formula
  lhs<-"val"
  rhs<-ivs_list[[thisendogenous]]$end
  
  #period FE
  if(thisperiod%in%c("stacked","stacked.alt") &
     !thismodel%in%c("simple","divisionfe","statefe") ) {
    pfe<-"factor(periodf) +"
  } else {
    pfe<-""
  }
  
  #region/state FE
  if( thismodel%in%c("divisionfe","dpfe") ) {
    rfe<-"factor(division) +"
  } else if( thismodel%in%c("simple","periodfe") ) {
    rfe<-""
  } else if(thismodel%in%c("preferred","statefe","spfe") ) {
    rfe<-"factor(state_fips) +"
  } else if(thismodel=="czfe") {
    rfe<-"factor(cz90) +"
  }
  
  #controls
  #get the extra controls,
  #based on endogenous variable
  unemp_tmp<-str_detect(
    thisendogenous,
    "^unemp"
  )
  manu_tmp<-str_detect(
    thisendogenous,
    "^manuf"
  )
  empc_tmp<-str_detect(
    thisendogenous,
    "popc"
  )
  if( manu_tmp ) {
    extra_control <- "manushare"
  } else if ( unemp_tmp ) {
    extra_control <- "unemprate"
  } else if ( empc_tmp) {
    extra_control <- "emptopopc"
  } else {
    extra_control <- "emptopop"
  }
  #based on dv, add start of period dv
  dv_control <- str_replace(
    thisdv,
    "^D\\.",""
  )
  
  if( thismodel%in%c("simple","divisionfe","statefe","periodfe","dpfe","spfe") ) {
    controls<-""
  } else if( thismodel%in%c("preferred","statefe","czfe") ) {
    list_controls<-c(
      #include controls
      #that are correlated w/ shock,
      #AND which could epxlain inc
      #through a non-LM pathway
      "blackpop_pct", #if more black CZs were targeted
      "l_sh_popfborn", #if foreigner-heavy CZs were targeted
      dv_control #if high-inc CZs were targeted
    ) %>% unique
    controls<-paste0(
      list_controls,
      collapse=" + "
    )
    controls<-paste0(
      controls," + "
    )
    #DEPRECATED THE 'FULL' MODEL, B/C POST-TREATMENT BIAS
  # } else if ( thismodel%in%c("statefe","czfe") ) {
  #   list_controls<-c(
  #     "blackpop_pct",
  #     "l_sh_popfborn",
  #     dv_control
  #   ) %>% unique
  #   controls<-paste0(
  #     list_controls,
  #     collapse=" + "
  #   )
  #   controls<-paste0(
  #     controls," + "
  #   )
  } else {
    print( thismodel )
    stop('invalid spec choice')
  }
  
  
  #final formula
  thisform<-paste0(
    lhs,
    " ~ ",
    pfe,
    rfe,
    controls,
    rhs
  ) %>% as.formula
  
  
  #########
  
  ###trim the df
  ###cols
  idvars<-c(
    "cz90",
    "region",
    "state_fips",
    "population"
  )
  #trim
  tmpcols<-c(
    idvars,
    all.vars(thisform),
    shockvars #thisform doesn't include iv
  ) %>%
    unique
  tmp<-tmpcols%in%names(thisdf)
  if(sum(!tmp)>0) {
    print(tmpcols[!tmp])
    stop()
  }
  tmprows<-complete.cases(thisdf[,tmpcols]) &
    is.finite(thisdf$val)
  thisdf<-thisdf[tmprows,tmpcols]
  
  #########
  
  #estimate!
  if(thisinst=="ols") {
    
    #if weights/not
    if(thisweights=='yes') {
      #we don't want to weight 
      #by something endogenous
      #so we want pop in the first period
      #if available
      #this is given by myperiods[1]
      thisdf$pop0<-by(thisdf,thisdf$cz90,function(df) {
        if(myperiods[1]%in%df$periodf) {
          pop_period<-myperiods[1]
        } else {
          pop_period<-myperiods[2]
        }
        rep(df$population[df$periodf==pop_period],nrow(df))
      }) %>% unlist
      myweights<-thisdf$pop0
    } else {
      myweights<-rep(1,nrow(thisdf))
    }
    m.tmp<-lm(
      data=thisdf,
      formula=thisform,
      weights=myweights
    )
    #no reduced form
    m.reducedform<-NULL
    m.firststage<-NULL
    
  } else {
    
    #if IVREG, add to form
    #all exogenous vars except for usch
    oldform<-deparse(
      thisform
    ) %>% paste0(collapse="")
    newbit<-str_replace(
      oldform,
      "val ~ ",
      ""
    ) %>% str_replace(
      ivs_list[[thisendogenous]]$end,
      ivs_list[[thisendogenous]]$iv
    )
    newform<-paste0(
      oldform,
      " | ",
      newbit
    ) %>% as.formula
    #reduced form
    reducedform<-paste0(
      "val ~ ",newbit
    ) %>% as.formula
    #firststage, manual
    firststage<-paste0(
      ivs_list[[thisendogenous]]$end," ~ ",newbit
    ) %>% as.formula
    #if weights/not
    if(thisweights=='yes') {
      myweights<-thisdf$population
    } else {
      myweights<-rep(1,nrow(thisdf))
    }
    m.tmp<-ivreg(
      data=thisdf,
      formula=newform,
      weights=myweights
    )
    m.reducedform<-lm(
      data=thisdf,
      formula=reducedform,
      weights=myweights
    )
    m.firststage<-lm(
      data=thisdf,
      formula=firststage,
      weights=myweights
    )
  }
  
  #get cluster-robust SE's
  thiscluster<-ifelse(
    thisperiod%in%c(
      "stacked",
      "stacked.alt"
    ),
    "cz90",
    "state_fips"
  )
  clustervar<-mapply(
    paste,
    thiscluster,
    thisdf[[thiscluster]],
    sep=""
  ) %>% unname
  tmpoutput<-robust.se(
    m.tmp,
    clustervar
  )
  if(!is.null(m.reducedform)) {
    tmpoutput.reducedform<-robust.se(
      m.reducedform,
      clustervar
    )
  } else {
    tmpoutput.reducedform<-NULL
  }
  
  if(!is.null(m.firststage)) {
    tmpoutput.firststage<-robust.se(
      m.firststage,
      clustervar
    )
  } else {
    tmpoutput.firststage<-NULL
  }
  
  
  #########
  
  if(thisinst=="ols") {
    
    diagdf<-data.frame(
      test=NA
    )
  } else {
    
    #get first-stage diagnostics
    #i try robust SE's, but if can't
    #then I just run w/ standard vcov
    #seems to be collinearity induced by robustness
    
    tmpsum <- try(
      summary(
        m.tmp,
        vcov=tmpoutput[[1]],
        diagnostics=T
      ),
      silent=T
    )
    if( class(tmpsum)=="try-error") {
      
      #if can't do w/ robust vcov
      tmpsum <- summary(
        m.tmp,
        #vcov = tmpoutput[[1]], w/o
        diagnostics = TRUE
      )
      robustSE <- F
      
    } else {
      
      robustSE <- T
      
    }
    
    diagdf <- tmpsum$diagnostics %>%
      as.data.frame
    names(diagdf)<-c(
      "df1",
      "df2",
      "teststat",
      "pval"
    )
    diagdf$i<-i
    diagdf$robustSE <- robustSE
    diagdf$test<-row.names(diagdf) %>%
      tolower
    row.names(diagdf)<-NULL
    #generate pval class
    diagdf$pval.class<-sapply(
      diagdf$pval,
      get.pvals.class
    )
    #get whether it passed or failed
    tests<-c(
      "weak instruments", #this gives F stat from test of restricted first stage vs. unrestricted
      "wu-hausman",
      "sargan"
    )
    pass<-c(
      "reject", #if reject, good instrument
      "reject", #if reject, endogeneity
      "fail to reject" #if fail to reject, not overidentified
    )
    tmpseq.q<-seq_along(tests)
    diagdf$testpass<-
      sapply(tmpseq.q,function(q) {
        #q<-3
        tmp<-diagdf$test==tests[q]
        if (pass[q]=="reject") {
          ifelse(
            diagdf$pval[tmp]>0.10,
            "fail",
            "pass"
          )
        } else {
          ifelse(
            diagdf$pval[tmp]<0.10,
            "fail",
            "pass"
          )
        }
      })
    
  }
  
  #########
  
  #return sds
  
  ms.list<-list(
    m.firststage,
    m.tmp,
    m.reducedform
  )
  formvars<-lapply(ms.list,function(thism) {
    thism %>% formula %>% all.vars
  }) %>% unlist %>% unique
  
  #not of idvars
  idvars<-c(
    "periodf",
    "division",
    "state_fips",
    "cz90"
  )
  tmp<-formvars%in%idvars
  formvars<-formvars[!tmp]
  tmpseq.k<-seq_along(formvars)
  sdsdf<-lapply(tmpseq.k,function(k) {
    data.frame(
      var=formvars[k],
      sd=sd(thisdf[[formvars[k]]]),
      stringsAsFactors=F
    )
  }) %>% rbind.fill
  
  #########
  
  list(
    #main results
    m=m.tmp,
    robvcov=tmpoutput[[1]],
    coefs=tmpoutput[[2]],
    #reduced form, manual
    m.reducedform=m.reducedform,
    robvcov.reducedform=tmpoutput.reducedform[[1]],
    coefs.reducedform=tmpoutput.reducedform[[2]],
    #first stage, manual
    m.firststage=m.firststage,
    robvcov.firststage=tmpoutput.firststage[[1]],
    coefs.firststage=tmpoutput.firststage[[2]],
    #dataframe etc.
    df=thisdf,
    diagdf=diagdf,
    sdsdf=sdsdf
  )
  
})

#name fulloutput by the index
names(fulloutput)<-tmpseq.i


#########################################################
#########################################################

#retrieve coefficients
tmpseq.i<-seq_along(fulloutput)
estsdf<-lapply(tmpseq.i,function(i) {
  #i<-119
  #print(i)
  
  tmpdfs<-list()
  
  #2s estimates
  mycoefs<-fulloutput[[i]]$coefs
  tmpdf<-mycoefs[] %>% as.data.frame
  tmpdf$var<-row.names(tmpdf)
  names(tmpdf)<-c("mu","se","tval","pval","var")
  row.names(tmpdf)<-NULL
  
  #if instrumented, add 1s and rf
  if(runmodsdf$instrumented[i]=="instrumented") {
    
    tmpdf$stage<-"secondstage"
    tmpdfs[["secondstage"]]<-tmpdf
    
    #rf estimates
    mycoefs<-fulloutput[[i]]$coefs.reducedform
    tmpdf<-mycoefs[] %>% as.data.frame
    tmpdf$var<-row.names(tmpdf)
    names(tmpdf)<-c("mu","se","tval","pval","var")
    row.names(tmpdf)<-NULL
    tmpdf$stage<-"reducedform"
    tmpdfs[["reducedform"]]<-tmpdf
    
    #1s estimates
    mycoefs<-fulloutput[[i]]$coefs.firststage
    tmpdf<-mycoefs[] %>% as.data.frame
    tmpdf$var<-row.names(tmpdf)
    names(tmpdf)<-c("mu","se","tval","pval","var")
    row.names(tmpdf)<-NULL
    tmpdf$stage<-"firststage"
    tmpdfs[["firststage"]]<-tmpdf
    
  } else {
    
    tmpdf$stage<-NA
    tmpdfs<-tmpdf
    
  }
  
  #put them together
  tmpdf<-rbind.fill(tmpdfs)
  
  ###
  #put sds in
  mysdsdf<-fulloutput[[i]]$sdsdf
  tmp<-tmpdf$stage%in%c("secondstage","reducedform") | is.na(tmpdf$stage)
  tmpdf$dvsd[tmp]<-mysdsdf$sd[mysdsdf$var=="val"]
  
  if(runmodsdf$instrumented[i]=="instrumented") {
    tmpendogenous<-ivs_list[[runmodsdf$endogenous[i]]]$end
    tmp<-tmpdf$stage=="firststage" 
    tmpdf$dvsd[tmp]<-mysdsdf$sd[mysdsdf$var==tmpendogenous]
  }
  
  #get ivs sds
  mysdsdf<-mysdsdf[mysdsdf$var!="val",]
  names(mysdsdf)<-c("var","ivsd")
  tmpdf<-merge(
    tmpdf,
    mysdsdf,
    all.x=T
  )
  tmpdf$i<-i
  tmpdf
}) %>% rbind.fill

#get pval class
estsdf$pval.class<-sapply(
  estsdf$pval,
  get.pvals.class
)

#retrieve model dets
deetsdf<-lapply(tmpseq.i,function(i) {
  #i<-1
  #print(i)
  m.tmp<-fulloutput[[i]]$m
  N<-nrow(m.tmp$model)
  N.czs<-fulloutput[[i]]$df$cz90 %>%
    unique %>% length
  tmpsum<-summary(m.tmp)
  adjr2<-tmpsum$adj.r.squared
  tmpdf<-data.frame(
    N,
    N.czs,
    adjr2#
  )
  tmpdf$i<-i
  tmpdf
}) %>% rbind.fill
deetsdf$i<-1:nrow(deetsdf)

#bring in mod details
mergelist<-list(
  runmodsdf,
  estsdf,
  #diagsdf,
  deetsdf
)
finaldf<-Reduce(
  function(...)
    merge(..., by="i", all=T),
  mergelist
)

#########################################################
#########################################################

#output 1s results

#retrieve test deets
diagsdf<-lapply(tmpseq.i,function(i) {
  #i<-1
  print(i)
  tmpdf<-fulloutput[[i]]$diagdf
}) %>% rbind.fill
#this can be used to examine
diagsdf<-merge(
  runmodsdf,
  diagsdf,
  by='i'
)

#which ones weren't robustSE?
tmp<-!diagsdf$robustSE
bad.is<-diagsdf$i[tmp]
runmodsdf[bad.is,]
#those w/ state and cz fe's
#can't be estimated w/ robustSE

#########################################################
#########################################################

#display standardized ests, too
tmp<-is.na(finaldf$ivsd)
finaldf$ivsd[tmp]<-1 #make ivsd 1, for inapplicables
finaldf$mu.semisd<-finaldf$mu * finaldf$ivsd
finaldf$se.semisd<-finaldf$se * finaldf$ivsd
finaldf$mu.sd<-(finaldf$mu * finaldf$ivsd)/finaldf$dvsd
finaldf$se.sd<-(finaldf$se * finaldf$ivsd)/finaldf$dvsd
finaldf$mu.sd.min <- finaldf$mu.sd - 1.96 * finaldf$se.sd
finaldf$mu.sd.max <- finaldf$mu.sd + 1.96 * finaldf$se.sd

#########################################################
#########################################################

#OUTPUT

#########################################################
#########################################################

#prefmods
tmp<-runmodsdf$prefmods
these.is<-which(tmp)
#loop through each,
#and produce a list of lists
tmpoutput<-lapply(these.is,function(myi) {
  list(
    prefmod=fulloutput[[myi]],
    dv=runmodsdf$dv[myi],
    endogenous=runmodsdf$endogenous[myi]
  )
})
names(tmpoutput)<-these.is
setwd(filesdir)
saveRDS(
  tmpoutput,
  "prefmods.RDS"
)

#########################################################
#########################################################

#save out fulloutput image
setwd(filesdir)
saveRDS(
  fulloutput,
  'fulloutput.RDS'
)
rm(fulloutput)

#########################################################
#########################################################

#save image w/o fulloutput
#this saves time/space;
#unless I explicitly need fulloutput,
#'03_estimated.RData' won't load it

#save regresults
setwd(filesdir)
write.csv(
  finaldf,
  'regresults.csv',
  row.names=F
)

#save image
setwd(filesdir)
save.image(
  "03_estimated.RData"
)

#########################################################
#########################################################

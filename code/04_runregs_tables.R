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
xdir<-getwd()
load("03_estimated.RData")
setwd(xdir); setwd('..')
rootdir <- getwd()
codedir<-file.path(rootdir,"code")
setwd(codedir); dir()
source('dirs.R')

#load a function to get prettynames
setwd(codedir); dir()
source('functions.R')

#########################################################
#########################################################

#GET REGTABLES

#formatted for list of regtables
regtables<-list()

#these will have to be identified by their model 'i'
tmp<-finaldf$prefmods |
  finaldf$model%in%c("statefe","simple") |
  finaldf$instrumented%in%c("ols")
regdf<-finaldf[tmp,]

#order these properly
tmpvars<-c("i","model","instrumented","endogenous")
tmpdf<-regdf[,tmpvars] %>% unique
tmpdf$model<-factor(
  tmpdf$model,
  c("simple","dpfe","preferred")
)
tmpdf$instrumented<-factor(
  tmpdf$instrumented,
  c("instrumented","ols")
)
tmporder<-order(
  tmpdf$endogenous,
  tmpdf$model,
  tmpdf$instrumented
)
tmpdf<-tmpdf[tmporder,]

#create modnames for easy ordering
regtables[['manuf']]<-c(
  tmpdf$i[tmpdf$endogenous=="manuf_china"]
)

regtables[['emptopop']]<-c(
  tmpdf$i[tmpdf$endogenous=="emptopop_china"]
)

regtables[['emptopopc']]<-c(
  tmpdf$i[tmpdf$endogenous=="emptopopc_china"]
)

regtables[['unemp']]<-c(
  tmpdf$i[tmpdf$endogenous=="unemp_china"]
)

#######

#these will have to be identified by their model 'i'
tmp<-finaldf$prefmods |
  finaldf$endogenous%in%c('manuf_china','unemp_china') |
  finaldf$instrumented%in%c("ols")
regdf<-finaldf[tmp,]
tmpdf<-regdf[,c("i","instrumented","endogenous")] %>% unique
tmpdf$endogenous<-factor(
  tmpdf$endogenous,
  c("emptopopc_china","emptopop_china","manuf_china","unemp_china")
)
tmpdf$instrumented<-factor(
  tmpdf$instrumented,
  c("ols","instrumented")
)
tmporder<-order(
  tmpdf$endogenous,
  tmpdf$instrumented
)
tmpdf<-tmpdf[tmporder,]

#create one w/ all of the vars
regtables[['maintable']]<-c(
  tmpdf$i
)


#########################################################
#########################################################

#LOOP THROUGH, PRODUCE TABLES

#sequence to loop through
tmpseq.j<-seq_along(regtables)
for(j in tmpseq.j) {
  #j<-5
  
  #######################################
  #######################################
  
  #get params
  thistabname<-names(regtables)[j]
  these.is<-regtables[[j]]
  
  #track progress
  print("#####")
  print(j)
  print(thistabname)
  
  #subset estimates df
  #subset big df w/ these models
  #limiting to second stage or two vars
  tmp<-finaldf$i%in%these.is
  ssdf<-finaldf[tmp,]
  thisdv<-unique(ssdf$dv)
  
  #######################################
  #######################################
  
  #get estimates
  
  #we only want to show vars that are in ols
  #or are in 2sls, but second stage vars
  tmp<-ssdf$instrumented=="ols" |
    (
      ssdf$instrumented=="2sls" & 
        ssdf$stage=="secondstage" &
        !is.na(ssdf$stage)
    )
  vars<-ssdf$var[tmp]
  tmp<-str_detect(vars,"factor") |
    vars=="(Intercept)"
  vars<-vars[!tmp] %>% unique
  mods<-these.is
  
  #loop through and get
  tmpseq.k<-seq_along(vars)
  myestsdf<-lapply(seq_along(vars),function(k) {
    #k<-1
    #print(k)
    thisvar<-vars[k]
    #get each estimate
    ivrows<-lapply(seq_along(mods),function(l) {
      #l<-4
      #print(l)
      this.l<-mods[l]
      tmp<-ssdf$i==this.l
      if(unique(ssdf$instrumented[tmp])=="ols") {
        thisrow<-tmp & 
          ssdf$var==thisvar
      } else {
        thisrow<-tmp & 
          ssdf$var==thisvar &
          ssdf$stage=="secondstage" &
          !is.na(ssdf$stage)
      }
      #get estimate
      estdf<-ssdf[thisrow,]
      #use gimmie.est to get the display
      #we want to display semi-standardized ests
      tmp<-apply(estdf[,c("mu.semisd","pval","se.semisd")],1,function(x)
        gimmie.est(x[1],x[2],x[3],nrow=2))
      tmp<-matrix(tmp,ncol=1)
      return(tmp)
    })
    #adjust all these rows to be equal
    maxrows<-max(sapply(ivrows,nrow))
    ivrows<-lapply(ivrows,function(r) {
      if(nrow(r)<maxrows) {
        x<-rep("",maxrows-nrow(r))
        y<-matrix(x,ncol=1)
        r<-rbind(r,y)
      }
      return(r)
    })
    ivrows<-Reduce(cbind,ivrows)
    ivrows<-data.frame(ivrows,stringsAsFactors=F)
    names(ivrows)<-mods
    ivrows$type<-c("est","se")
    ivrows$var<-thisvar
    return(ivrows)
  }) %>% rbind.fill
  
  ###get good names and order the myestsdf
  #goodnames will be propername
  
  #get type of var
  
  myestsdf$label<-sapply(
    myestsdf$var,
    getvarname
  )
  myestsdf$order<-sapply(
    myestsdf$var,
    getvarorder
  )
  myestsdf$type<-sapply(
    myestsdf$var,
    getvartype
  )
  myestsdf$isendogenous<-myestsdf$type=="endogenous"
  myestsdf<-myestsdf[order(myestsdf$order),]
  
  ###finalize
  #make label sparse
  myestsdf$label[1:nrow(myestsdf)%%2!=1]<-""
  
  #denote the start of period controls
  splitter<-which(diff(myestsdf$isendogenous)!=0)
  tmpvars<-c("label",mods)
  myestsdf.endog<-myestsdf[1:splitter,c("label",mods)]
  tmprows<-c(1:nrow(myestsdf))%in%c(splitter+1):nrow(myestsdf)
  myestsdf.speriods<-myestsdf[tmprows,tmpvars]
  
  #######################################
  #######################################
  
  ###GET MOD INFO
  tmpseq.k<-seq_along(mods)
  modelinfo<-lapply(tmpseq.k,function(k) {
    #k<-3
    #print(k)
    thismod.i<-mods[k]
    thismod.row<-runmodsdf$i==thismod.i
    thisdv<-runmodsdf$dv[thismod.row]
    thismodel<-runmodsdf$model[thismod.row]
    tmp<-finaldf$i==thismod.i 
    myfinaldf<-finaldf[tmp,]
    N.czs<-myfinaldf$N.czs %>% unique
    N.obs<-myfinaldf$N %>% unique
    r2<-myfinaldf$r2 %>% unique %>%
      round(2) %>% format(nsmall=2)
    adjr2<-myfinaldf$adjr2 %>% unique %>%
      round(2) %>% format(nsmall=2)
    myinstrumented<-unique(myfinaldf$instrumented)
    if(myinstrumented=="instrumented") {
      est.method<-"2SLS"
      tmp<-diagsdf$i==thismod.i & 
        diagsdf$test=="weak instruments"
      fstat<-round(diagsdf$teststat[tmp],)
    } else {
      est.method<-"OLS"
      fstat<-""
    }
    #fe deets
    mymodtype<-unique(myfinaldf$model)
    if(mymodtype=="simple") {
      divisionfe<-""
    } else {
      divisionfe<-"FE"
    }
    periodfe<-"FE"
    thiscol<-data.frame(
      c(
        est.method,
        N.czs,
        N.obs,
        divisionfe,
        periodfe,
        fstat,
        r2,
        adjr2
      ),
      stringsAsFactors = F
    )
    names(thiscol)<-thismod.i
    return(thiscol)
  }) 
  modelinfo<-Reduce(cbind,modelinfo)
  modelinfo$label<-c(
    "Method",
    "Commuting Zones",
    "Observations",
    "State-Level",
    "Period-Level",
    "First-Stage F-Statistic",
    'R^2',
    "Adjusted-R^2"
  )
  
  #######################################
  #######################################
  
  #PUT ALL TOGETHER
  endfiller<-data.frame(label=c("","\\textit{Measure of Job Loss}",""))
  speriodsfiller<-data.frame(label=c("","\\textit{Start-of-Period Controls}",""))
  infofiller<-data.frame(label=c("","\\textit{Model Info}",""))
  
  regtable<-rbind.fill(
    endfiller,
    myestsdf.endog,
    speriodsfiller,
    myestsdf.speriods,
    infofiller,
    modelinfo
  )
  #make NA's blank and remove leading zeros
  regtable<-apply(regtable,2,function(x) {
    x[is.na(x)]<-""
    x<-str_remove(x,"^0+")
    return(x)
  })

  #write out each of these
  setwd(outputdir)
  filename<-paste0(
    "tab_reg_",
    thistabname,
    ".csv"
  )
  write.csv(
    regtable,
    filename,
    row.names=F
  )
  
}


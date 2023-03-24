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
load("03_estimated.RData")

#load a function to get prettynames
setwd(codedir); dir()
source('functions.R')
source('predict_ivreg.R')

#set seed
set.seed(23)

#set reps
reps<-100

#set percentile for cfactual
ptile<-0.20

#########################################################
#########################################################

#WHAT WOULD HAVE HAPPENED IN COUNTERFACTUALS?
#0. observed
#1. manuf/emp declined as badly as it did in bad CZs
#2. manuf/emp gained as well as it did in healthy CZs
#3. manuf/emp share stable
### ... 

#load the prefmods
setwd(filesdir); dir()
prefmods<-readRDS(
  "prefmods.RDS"
)

#we want model draws, 
#for each
tmpseq.i<-seq_along(prefmods)
prefmods_draws<-lapply(tmpseq.i,function(i) {
  #i<-1
  thismod<-prefmods[[i]]$prefmod
  #get the mod coefs etc.
  mu<-thismod$m$coefficients
  robvcov<-thismod$robvcov
  #for CI's
  betas<-MASS::mvrnorm(
    n=reps,
    mu=mu,
    Sigma=robvcov
  )
})
names(prefmods_draws)<-names(prefmods)

#we want info in a df
tmpseq.i<-seq_along(prefmods)
prefmodsdf<-lapply(tmpseq.i,function(i) {
  data.frame(
    model=names(prefmods)[i],
    dv=prefmods[[i]]$dv,
    endogenous=prefmods[[i]]$endogenous,
    stringsAsFactors=F
  )
}) %>% rbind.fill

#########################################################
#########################################################

#CFACTUALS
#these are cfactuals
cfactuals<-c(
  "observed",
  "predicted", #as observed,
  ####
  #"lowptile", 
  #"stable",
  #"nodecline",
  #"highptile",
  #"90sboom",
  "90spreserved"
)

#this will stipulate
#the dependent variable
#as well as the iv strategy
models<-names(prefmods)

loopdf<-expand.grid(
  cfactual=cfactuals,
  i=models,
  stringsAsFactors=F
)
loopdf$seq.j<-1:nrow(loopdf)

#put model info in here
intersect(
  names(loopdf),
  names(runmodsdf)
)
loopdf<-merge(
  loopdf,
  runmodsdf
)

tmpseq.j<-1:nrow(loopdf)
tmpoutput<-lapply(tmpseq.j,function(j) {
  
  #j<-4
  print(
    paste0(
      j," of ",length(tmpseq.j)
    )
  )
  thiscf<-loopdf$cfactual[j]
  thismod.i<-loopdf$i[j]
  thisoutput<-prefmods[[thismod.i]]
  thismod<-thisoutput$prefmod
  thismod_draw<-prefmods_draws[[thismod.i]]
  thisdv<-thisoutput$dv
  thisendogenous<-thisoutput$endogenous
  endvar<-ivs_list[[thisendogenous]]$end
  
  ###
  #GET DF
  #get the df
  thisdf<-thismod$df
  # tmp<-thisdf$cz90=="22400"
  # thisdf<-thisdf[tmp,]
  thisdf$order<-1:nrow(thisdf) #fixes order
  
  ###
  
  #GET MOD
  #get the mod coefs etc.
  mu<-thismod$m$coefficients
  robvcov<-thismod$robvcov
  #for CI's
  betas<-thismod_draw
  #if we are doing reps?
  if(reps==1) {
    mybetas<-mu
  } else {
    mybetas<-betas
  }
  
  
  ###
  
  #EDIT IVS
  
  if(thiscf=="highptile") {
    #as good as high-end CZ's,
    #period-specific 
    tmp<-tapply(
      thisdf[[endvar]],
      thisdf$periodf,
      function(x) {
        quantile(x,1-ptile)
      }
    )
    tmpseq.k<-seq_along(tmp)
    for(k in tmpseq.k) {
      tmprows<-thisdf$periodf==names(tmp)[k]
      newval<-tmp[k]
      thisdf[[endvar]][tmprows]<-newval
    }
  } else if (thiscf=="lowptile") {
    #as good as low-end CZ's,
    #period-specific
    tmp<-tapply(
      thisdf[[endvar]],
      thisdf$periodf,
      function(x) {
        quantile(x,ptile)
      }
    )
    tmpseq.k<-seq_along(tmp)
    for(k in tmpseq.k) {
      tmprows<-thisdf$periodf==names(tmp)[k]
      newval<-tmp[k]
      thisdf[[endvar]][tmprows]<-newval
    }
  } else if (thiscf=="stable") {
    #all CZ's set to be stable
    thisdf[[endvar]]<-0
  } else if (thiscf=="nodecline") {
    #any CZ's that see deterioration, 
    #we set to be stable
    tmprows<-thisdf[[endvar]]>0
    thisdf[[endvar]][tmprows]<-0
  } else if (thiscf=="90sboom") {
    #continue the 90s boom into the 2000's
    thisdf<-by(thisdf,thisdf$cz90,function(df) {
      #df<-thisdf[thisdf$cz90==100,]
      if(nrow(df)!=1) {
        #make the second period same as the first
        df[[endvar]][df$periodf=="second.alt"]<-
          df[[endvar]][df$periodf=="first"]
      }
      df
    }) %>% rbind.fill 
    
  } else if (thiscf=="90spreserved") {
    tmp<-thisdf$periodf=='second.alt'
    thisdf[[endvar]][tmp]<-0
  } else if (thiscf%in%c("predicted","observed")) {
    #don't do anything
  } else {
    stop('shouldnt end up here')
  }
  
  #make sure thisdf is ordered
  tmporder<-order(thisdf$order)
  thisdf<-thisdf[tmporder,]
  
  ###
  
  #PREDICT
  
  #this is a little tricky, b/c
  #pref spec has start of period dv
  #so we have to predict sequentially
  keyvars<-names(thisdf)
  
  #make the prediction, sequentailly
  returndf<-lapply(1:nrow(mybetas),function(l) {
    # l<-1
    
    #split the dataframe into two periods
    thisdfs<-split(thisdf,thisdf$periodf)
    
    #predict growth rate in first period
    #this is log growth rate under cfactual
    decadalized<-T #T if runregs are decadalized
    if(thiscf=="observed") {
      #if observed, don't predict
      yhat_first<-thisdfs$first$val
    } else {
      yhat_first<-predict_ivreg(
        thismod$m,
        newdata=thisdfs$first,
        mybeta=mybetas[l,]
      ) %>% as.vector
    }
    
    #store in thisdfs
    thisdfs$first$yhat <- yhat_first
    
    #we add this to the SoP value for period 1
    #to get (1) end incRate in first period
    #and to get (2) st incRate in second period
    #(2) is necessary to predict grwothrate in second period
    #one caveat: if these are decadalized models, 
    #the model yields a predicted decade-long growth rate
    #rather than an 8-year growth rate
    #given this, it has to be adjusted slightly to yield start val
    #for second period; no such adjustment necessary for second period
    thisdfs$first$incRate_ln_end <- 
      #if yes, take this
      as.numeric(decadalized) * ( thisdfs$first$yhat * ( (1999-1991)/(2001-1991) ) ) + 
      #if not, then take this
      as.numeric(!decadalized) * thisdfs$first$yhat + 
      #add to base
      thisdfs$first$incRate_corrected_estimated_25_ln
    incRate_ln_st_second <- thisdfs$first$incRate_ln_end
    #for consistent naming
    thisdfs$first$incRate_ln_st <- 
      thisdfs$first$incRate_corrected_estimated_25_ln
    
    #import startincrate into second period df
    tmpdf<-data.frame(
      cz90=thisdfs$first$cz90,
      incRate_ln_st=incRate_ln_st_second
    )
    thisdfs$second.alt<-merge(
      tmpdf,
      thisdfs$second.alt,
      all.y=T
    )
    
    #note some seond periods weren't in the first period
    sum(is.na(thisdfs$second.alt$incRate_ln_st))
    #but where they were and thus I have a prediction
    #replace start of period with that value
    tmp<-!is.na(thisdfs$second.alt$incRate_ln_st)
    thisdfs$second.alt$incRate_corrected_estimated_25_ln[tmp]<-
      thisdfs$second.alt$incRate_ln_st[tmp]
    #do the reverse, too; where we don't have estimated incRate,
    #we take the actual start incRate. this is imp for graphing/representing preds
    thisdfs$second.alt$incRate_ln_st[!tmp]<-
      thisdfs$second.alt$incRate_corrected_estimated_25_ln[!tmp]
    
    #predict growth rate for the second period
    if(thiscf=="observed") {
      yhat_second<-thisdfs$second.alt$val #don't predict
    } else {
      yhat_second<-predict_ivreg(
        thismod$m,
        newdata=thisdfs$second.alt,
        mybeta=mybetas[l,]
      ) %>% as.vector
    }
    thisdfs$second.alt$yhat <- yhat_second
    
    #this yields end incrate for second period
    thisdfs$second.alt$incRate_ln_end <- 
      #if yes, take this
      as.numeric(decadalized) * ( thisdfs$second.alt$yhat * ( (2011-1999)/(2009-1999) ) ) +
      #if not, then take this
      as.numeric(!decadalized) * thisdfs$second.alt$yhat +
      #add to base
      thisdfs$second.alt$incRate_corrected_estimated_25_ln
    
    #return to single df
    newdf<-rbind.fill(thisdfs)
    #exponentiate, since these were in log
    newdf$incRate_st<-exp(newdf$incRate_ln_st)
    newdf$incRate_end<-exp(newdf$incRate_ln_end)
    tmporder<-order(newdf$order)
    newdf<-newdf[tmporder,]
    newdf$seq.j<-j
    newdf$rep<-l
    
    #return only that info which is distinctive
    #to this iteration of the loop
    returnvars<-c(
      "order",
      "yhat",
      "incRate_st",
      "incRate_end",
      "seq.j",
      "rep"
    )
    newdf[,returnvars]
    
    
  }) %>% rbind.fill
  
  #if this was observed, we don't need all the other reps
  if(thiscf=="observed") {
    returndf<-returndf[returndf$rep==1,]
  }
  
  #for returning
  returndf<-merge(
    thisdf[,keyvars],
    returndf,
    by='order'
  )
  returndf$val<-NULL
  
  ####
  #rename the ivs
  #to the appropriate
  names(returndf)[names(returndf)==endvar]<-
    "D.endvar"
  
  ####
  #RETURN
  returndf
  
})

#########################################################
#########################################################

#PUT PREDS TOGETHER

#put the predictions together
predictdf<-rbind.fill(
  tmpoutput
)

nadf<-predictdf[is.na(predictdf$yhat),]
if(nrow(nadf)>0)
  stop('inspect me')

#########################################################
#########################################################

#save out
setwd(filesdir); dir()
save.image(
  "20_cfactuals.RData"
)

#########################################################
#########################################################


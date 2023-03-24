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
source('functions.R')

#load a function to get prettynames
setwd(codedir); dir()
source('functions.R')

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

### why do Feler and Senses (2017) reach a different
### conclusion about the impact on property crime?

# we'll run the different permutations and check
require(ivpack)

# these are our data, from our preferred specification
#load tmpresults
setwd(filesdir); dir()
tmpdf<-fread(
  'regresults.csv'
)

# # load our datasets, from regressions on property crime 
# # we want both stacked and stacked.alt specifications, 
# # so that we have data for all periods
# # commented out so we don't have to
# # read in 'fulloutput.RDS' everytime
# tmpoutput<-readRDS('fulloutput.RDS')
# tmp<-tmpdf$dv=='D.property_crt' &
#   tmpdf$stage=='secondstage' &
#   tmpdf$period%in%c('stacked') &
#   tmpdf$logdv==T &
#   tmpdf$var%in%c('D.emptopopc')
# ourdf<-tmpoutput[[tmpdf$i[tmp]]]$df
# 
# tmp<-tmpdf$dv=='D.property_crt' &
#   tmpdf$stage=='secondstage' &
#   tmpdf$period%in%c('stacked.alt') &
#   tmpdf$logdv==T &
#   tmpdf$var%in%c('D.emptopopc')
# ourdf2<-tmpoutput[[tmpdf$i[tmp]]]$df
# #only second.alt contributes new data
# ourdf2<-ourdf2[ourdf2$periodf=='second.alt',]
# #put these together
# ourdf<-rbind.fill(
#   ourdf,
#   ourdf2
# )
# names(ourdf)[names(ourdf)=='val']<-'dln_property_crt'
# rm(tmpoutput)
# setwd(filesdir); dir()
# fwrite(ourdf,'ourdf_felersenses.csv')
ourdf<-fread('ourdf_felersenses.csv')

# load their data
tmpdir<-file.path(
  prelimdir,
  "feler_senses"
)
setwd(tmpdir); dir()
theirdf<-fread('fs_rep_stata.csv')
#rename idvars
names(theirdf)[names(theirdf)=='czone']<-'cz90'
tmp<-theirdf$t2==0
theirdf$periodf[tmp]<-'first'
theirdf$periodf[!tmp]<-'second'
theirdf$t2<-NULL
#fix regionvar; its division
tmp<-str_detect(names(theirdf),'reg\\_')
regionvars<-names(theirdf)[tmp]
theirdf$division<-apply(theirdf[,regionvars,with=F],1,function(x) {
  tmp<-sum(x==1)
  ifelse(
    tmp==0,
    'reg_newengland', #this is the omitted category
    regionvars[which(x==1)]
  )
})
for(v in regionvars)
  theirdf[[v]]<-NULL

# merge the two
regdf <- merge(
  ourdf,
  theirdf,
  by=c(
    'cz90',
    'periodf'
  ),
  all=T
)
#fborn share is common to both, don't lose it
regdf$l_sh_popfborn<-regdf$l_sh_popfborn.y
tmp<-is.na(regdf$l_sh_popfborn) &
  !is.na(regdf$l_sh_popfborn.x)
regdf$l_sh_popfborn[tmp]<-regdf$l_sh_popfborn.x[tmp]
regdf$l_sh_popfborn.x<-regdf$l_sh_popfborn.y<-NULL

#we also need their start of period dv
#to add as a control when we adopt our controls
require(haven)
tmpdir<-file.path(
  prelimdir,
  "feler_senses",
  "output"
)
setwd(tmpdir); dir()
tmpdf<-read_dta('fs_stage1.dta')
tmpdf<-tmpdf[,c('czone','year','crm_prop_pc1000')]
names(tmpdf)[1]<-'cz90'
#drop extra years, rename period
tmpdf<-tmpdf[tmpdf$year%in%c(1990,2000),]
tmpdf$periodf<-'first'
tmpdf$periodf[tmpdf$year==2000]<-'second'
tmpdf$year<-NULL
tmpdf$ln_crm_prop_pc1000<-
  log(tmpdf$crm_prop_pc1000)

regdf<-merge(
  tmpdf,
  regdf,
  by=c(
    'cz90',
    'periodf'
  ),
  all=T
)

# loop through
loopdf <- expand.grid(
  instrument = c('fs','ours'),
  endogenous = c('fs','ours'),
  controls = c('fs','ours'),
  dv = c('fs','ours'),
  period = c('fs','ours'),
  dropsmall = c('yes','no'),
  #log = c('yes','no'),
  stringsAsFactors = F
)

#we can only use fs vars with stacked spec
tmp<-loopdf$period=='ours' &
  !(
    loopdf$instrument=='ours' &
      loopdf$endogenous=='ours' &
      loopdf$controls=='ours' &
      loopdf$dv=='ours' #&
      #loopdf$log=='yes'
  )
loopdf<-loopdf[!tmp,]

loopdf$i<-1:nrow(loopdf)
tmpseq.i<-loopdf$i
fulloutput <- lapply(tmpseq.i,function(i) {

  #i<-1
  print(
    paste(
      i,"of",length(tmpseq.i)
    )
  )

  ###
  thisrow<-loopdf[i,]
  thisinstrument<-thisrow$instrument
  thiscontrols<-thisrow$controls
  thisdv<-thisrow$dv
  thisendogenous<-thisrow$endogenous
  thisperiod<-thisrow$period
  thisdropsmall<-thisrow$dropsmall
  #thislog<-thisrow$log
  thisdf<-regdf

  ###
  if(thisperiod=='fs') {
    tmprows<-thisdf$periodf%in%c('first','second')
  } else {
    tmprows<-thisdf$periodf%in%c('first','second.alt')
  }

  ###
  if(thisdropsmall=='yes') {
    tmprows<-thisdf$population>10^5 &
      !is.na(thisdf$population)
  } else {
    tmprows<-tmprows
  }

  ###
  if(thisinstrument=='fs') {
    rhs_iv<-'d_tradeotch_pw_lag'
  } else {
    rhs_iv<-'otch'
  }

  ###
  if(thisendogenous=='fs') {
    rhs<-'d_tradeusch_pw'
  } else {
    rhs<-'D.emptopopc'
  }

  ###
  if(thisdv=='fs') {
    #lhs <- ifelse(thislog=='yes','dln_crm_prop_pc1000','d_crm_prop_pc1000')
    lhs<-'dln_crm_prop_pc1000'
  } else {
    #lhs <- ifelse(thislog=='yes','d_property_crt','dln_property_crt')
    lhs<-'dln_property_crt'
  }
  
  ###
  if(thislog=='yes') {
    lhs<-'property_crt'
  }

  ###
  pfe='factor(periodf) +'
  if(thiscontrols=='fs') {
    rfe<-'factor(division) +'
    list_controls<-c(
      "l_shind_manuf_cbp",
      "l_sh_popedu_c",
      "l_sh_popfborn",
      "l_sh_empl_f",
      "l_sh_routine33",
      "l_task_outsource"
    )
    controls<-paste0(
      list_controls,
      collapse=" + "
    )
    controls<-paste0(
      controls," + "
    )
  } else {
    rfe<-'factor(state_fips) +'
    list_controls<-c(
      'blackpop_pct',
      'l_sh_popfborn',
      'genx_noncoll',
      ifelse(thisdv=='fs','ln_crm_prop_pc1000','property_crt_ln')
    )
    controls<-paste0(
      list_controls,
      collapse=" + "
    )
    controls<-paste0(
      controls," + "
    )

  }

  #make the formula
  thisform<-paste0(
    lhs,
    " ~ ",
    pfe,
    rfe,
    controls,
    rhs
  ) %>% as.formula
  oldform<-deparse(
    thisform
  ) %>% paste0(collapse="")
  newbit<-str_replace(
    oldform,
    paste0(lhs," ~ "),
    ""
  ) %>% str_replace(
    rhs,
    rhs_iv
  )
  newform<-paste0(
    oldform,
    " | ",
    newbit
  ) %>% as.formula

  #limit to complete cases
  #and appropriate periods
  tmprows<-tmprows &
    complete.cases(regdf[,all.vars(newform)])
  thisdf<-thisdf[tmprows,]

  if(thisdv=='fs') {
    myweights <- thisdf$timepwt48
  } else {
    myweights <- thisdf$population
  }

  #return the key estimate
  m.tmp<-ivreg(
    data=thisdf,
    formula=newform,
    weights=myweights
  )
  tmpoutput<-cluster.robust.se(
    m.tmp,
    clusterid=thisdf$cz90
  )
  tmp<-row.names(tmpoutput)%in%c(
    'D.emptopopc',
    'd_tradeusch_pw'
  )
  returndf<-data.frame(t(tmpoutput[tmp,1:4]))
  names(returndf)<-c("mu","se","tval","pval")

  #put in sd terms
  ivsd <- sd(thisdf[[rhs]])
  dvsd <- sd(thisdf[[lhs]])
  returndf$mu.sd<-(returndf$mu * ivsd)/dvsd
  returndf$se.sd<-(returndf$se * ivsd)/dvsd
  returndf$mu.sd.min <- returndf$mu.sd - 1.96 * returndf$se.sd
  returndf$mu.sd.max <- returndf$mu.sd + 1.96 * returndf$se.sd
  row.names(returndf)<-NULL

  ##return
  returndf$i<-i
  returndf

}) %>% rbind.fill

#merge
plotdf<-merge(
  loopdf,
  fulloutput
)

#trim for display
loopvars<-names(loopdf)[names(loopdf)!='i']
tmp<-apply(plotdf[,loopvars],1,function(x) sum(x%in%c('fs','no')))>=5 |
  apply(plotdf[,loopvars[loopvars!='period']],1,function(x) sum(x%in%c('ours','no')))>=4
plotdf<-plotdf[tmp,]
tmp<-apply(plotdf[,loopvars],1,function(x) sum(x%in%c('fs','no')))>=5
plotdf$facet[tmp]<-"Feler and Senses' Approach"
tmp<-apply(plotdf[,loopvars],1,function(x) sum(x%in%c('ours','no')))>=5 |
  apply(plotdf[,loopvars[loopvars!='period']],1,function(x) sum(x%in%c('ours','no')))>=4
plotdf$facet[tmp]<-'Our Approach'
plotdf<-plotdf[!is.na(plotdf$facet),]

#fix model names
plotdf$modelname<-''
tmp<-plotdf$facet=="Feler and Senses' Approach"
plotdf$modelname[tmp]<-apply(plotdf[tmp,loopvars],1,function(x) {
  tmplog<-x%in%c('ours','yes')
  if(sum(tmplog)==0) {
    y<-'Their Preferred Estimate'
  } else {
    y<-paste0("+ our ",loopvars[which(x%in%c('ours','yes'))])
  }
}) %>% unlist

tmp<-plotdf$facet=='Our Approach'
plotdf$modelname[tmp]<-apply(plotdf[tmp,loopvars],1,function(x) {
  tmplog<-x%in%c('fs','yes')
  if(sum(tmplog)==0) {
    y<-'Our Preferred Estimate'
  } else {
    y<-paste0("+ their ",paste0(loopvars[which(x%in%c('fs','yes'))],collapse=', '))
  }
}) %>% unlist

#fix dropsmall
tmp<-plotdf$modelname%in%c('+ our dropsmall','+ their dropsmall')
plotdf$modelname[tmp]<-'+ drop small CZs'

#add color
plotdf$color<-'black'
tmp<-str_detect(plotdf$model,'Preferred Estimate')
plotdf$color[tmp]<-'red'
plotdf$color<-factor(plotdf$color)
tmpcolors<-c('black','red')
tmpsizes<-c(1,10)
names(tmpsizes)<-names(tmpcolors)<-c('black','red')

#order factors
plotdf<-plotdf[order(-plotdf$mu.sd),]
plotdf$modelname <- factor(
  plotdf$modelname,
  levels=unique(plotdf$modelname)
)

#add shape et al to plotdf
plotdf$pval.class <- sapply(
  plotdf$pval,
  get.pvals.class
)

#add pval info to shape of point
plotdf$pval.shp<-NA
plotdf$pval.shp[plotdf$pval.class=="at alpha=0.01"]<-1
plotdf$pval.shp[plotdf$pval.class=="at alpha=0.05"]<-2
plotdf$pval.shp[plotdf$pval.class=="at alpha=0.10"]<-3
plotdf$pval.shp[plotdf$pval.class=="not sig"]<-4
plotdf$pval.shp<-factor(
  plotdf$pval.shp,
  levels=c(1,2,3,4),
  labels=c(
    "at alpha=0.01",
    "at alpha=0.05",
    "at alpha=0.10",
    "not sig"
  )
)
#tmpshapes
tmpshapes<-c(8,4,16,1)
names(tmpshapes)<-levels(plotdf$pval.shp)
shp.labels<-c(
  bquote(alpha == 0.01),
  bquote(alpha == 0.05),
  bquote(alpha == 0.10)
)

require(ggplot2)
g.tmp <- ggplot(
  plotdf,
  aes(
    x=modelname,
    y=mu.sd,
    ymin=mu.sd.min,
    ymax=mu.sd.max,
    shape=pval.shp,
    color=color,
    size=color
  )
) +
  geom_point(
    size=2
  ) +
  geom_errorbar(
    size=0.4,
    width=0.2
  ) +
  geom_hline(
    yintercept=0,
    linetype='dashed',
    color='black'
  ) +
  scale_shape_manual(
    name="",
    values=tmpshapes,
    labels=shp.labels,
    drop=F
  ) +
  scale_color_manual(
    guide='none',
    values=tmpcolors
  ) +
  coord_flip() +
  facet_wrap(
    ~ facet,
    ncol=1,
    scales='free_y'
  ) +
  xlab("") +
  ylab("\nStandardized Estimate") +
  theme_bw()

setwd(outputdir)
tmpname<-"fig_felersenses.png"
ggsave(
  plot=g.tmp,
  filename=tmpname,
  width=6,
  height=8
)

#summary:
#the standout issue is that their estimate
#is not robust to:
#changing the dv (ours is preferable)
#droppoing small CZs (which is concerning)
#this is reason to think that theirs is not robust..

#########################################################
#########################################################



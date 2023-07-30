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

setwd(cwdir)
source('gencrosswalks.R')

setwd(codedir); dir()
source('functions.R')

#########################################################
#########################################################

#plotting prelims
require(ggplot2)
require(ggthemes)
require(extrafont)
require(RColorBrewer)
require(scales)
# #load fonts
# loadfonts(quiet=T) #register w/ pdf
# loadfonts(device = "win",quiet=T) #register w/ windows
# #fonts()
# #ghostscript
# Sys.setenv(
#   R_GSCMD = gsdir_full
# )
# #initialize graphlist
# gs.list<-list()

#quick function to outputdfs
output <- function(df,tmpname,fig=NULL) {
  
  setwd(outputdir)
  if( str_detect(tmpname,"\\.pdf$|\\.png$") ) 
    tmpname<-str_replace(tmpname,"\\.pdf$|\\.png$",".csv")
  
  #get just the vars used for plotting
  if(is.null(fig)) {
    keyvars<-names(df)
  } else {
    keyvars<-sapply(fig$mapping,deparse) %>%
      str_replace("\\~","") 
    rowvar<-names(g.tmp$facet$params$rows)
    colvar<-names(g.tmp$facet$params$cols)
    keyvars<-c(rowvar,colvar,keyvars)
  }
  
  #save out
  df<-data.table(df)
  write.csv(
    df[,keyvars,with=F],
    tmpname,
    row.names=F
  )
  
}

#########################################################
#########################################################

#load prepped dataframes
setwd(datadir); dir()
xdir<-getwd()
load(
  '02_prepped.RData'
)
setwd(xdir); setwd('..')
rootdir <- getwd()
codedir<-file.path(rootdir,"code")
setwd(codedir); dir()
source('dirs.R')
source('functions.R')

#########################################################
#########################################################

#examine correlations of shocks
#w/ candidate start of period controls

#shock
tmprows<-shockdf$periodf=="first" |
  shockdf$periodf=="second.alt"
tmpcols<-c(
  "cz90",
  "periodf",
  "D.manushare",
  "D.emptopop",
  "D.emptopopc",
  "D.unemprate",
  "otch"
)
tmpshockdf<-shockdf[tmprows,tmpcols]
tmp<-tmpcols%in%names(shockdf)
tmpcols[!tmp]

#covars
head(covarsdf)
tmpcols<-c(
  "cz90",
  "year",
  "blackpop_pct",
  "manushare",
  "emptopop",
  "emptopopc",
  "unemprate",
  "incRate_corrected_estimated_25_ln",
  "l_sh_popedu_c",
  "l_sh_popfborn",
  "l_sh_empl_f",
  "l_sh_routine33",
  "l_task_outsource",
  "genx_noncoll",
  "genx_hsdrop"
)
tmpcovarsdf<-covarsdf[,tmpcols]
tmp<-tmpcovarsdf$year==1991
tmpcovarsdf$periodf[tmp]<-"first"
tmp<-tmpcovarsdf$year==1999
tmpcovarsdf$periodf[tmp]<-"second.alt"
tmpcovarsdf$year<-NULL
tmp<-tmpcols%in%names(covarsdf)
tmpcols[!tmp]

tmpdf<-merge(
  tmpshockdf,
  tmpcovarsdf
)

shockvars<-c(
  "otch"
)

plotdf<-lapply(shockvars,function(v) {
  #v<-shockvars[1]
  stvars<-c(
    "blackpop_pct",
    "manushare",
    "emptopop",
    "emptopopc",
    "unemprate",
    "incRate_corrected_estimated_25_ln",
    "l_sh_popedu_c",
    "l_sh_popfborn",
    "l_sh_empl_f",
    "l_sh_routine33",
    "l_task_outsource",
    "genx_noncoll",
    "genx_hsdrop"
  )
  returndf<-lapply(stvars,function(sv) {
    #sv<-stvars[1]
    tmpoutput<-cor.test(
      tmpdf[[v]],
      tmpdf[[sv]],
      use='complete.obs'
    )
    data.frame(
      stvar=sv,
      cor=tmpoutput$estimate %>% unname,
      cor.min=tmpoutput$conf.int[1],
      cor.max=tmpoutput$conf.int[2],
      pval=tmpoutput$p.value,
      stringsAsFactors=F
    )
  }) %>% rbind.fill
  returndf$shockvar<-v
  returndf
}) %>% rbind.fill

tmpdf<-data.frame(
  stvar=c(plotdf$stvar),
  group=c(
    'Controls',
    'Extra',
    'Extra',
    'Extra',
    'Extra',
    'Controls',
    'Controls',
    'Controls',
    'Extra',
    'Extra',
    'Extra',
    'Controls',
    'Extra'
  )
)

plotdf<-merge(
  plotdf,
  tmpdf
)

tmplevels<-c('Controls','Extra')
plotdf$group<-factor(
  plotdf$group,
  tmplevels
)

tmplevels<-sapply(plotdf$shockvar,getvarorder) %>%
  sort %>% names %>% unique
tmplabels<-sapply(tmplevels,getvarname)
plotdf$shockvar<-factor(
  plotdf$shockvar,
  tmplevels,
  tmplabels
)

tmplevels<-sapply(plotdf$stvar,getvarorder) %>%
  sort %>% names %>% unique %>% rev
tmplabels<-sapply(tmplevels,getvarname)
plotdf$stvar<-factor(
  plotdf$stvar,
  tmplevels,
  tmplabels
)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=stvar,
    y=cor,
    ymin=cor.min,
    ymax=cor.max
  )
) +
  geom_point() +
  geom_errorbar(
    width=0.3
  ) +
  coord_flip() +
  geom_hline(
    yintercept=0,
    linetype='dashed',
    color='darkblue',
    alpha=0.5
  ) +
  facet_wrap(
    ~ group,
    scales='free_y',
    ncol=1
  ) +
  ylab("\nCorrelation with Instrument") +
  xlab("") +
  ylim(-1,1) +
  theme_bw()

tmpname<-"fig_otch_corrs.png"
setwd(outputdir)
ggsave(
  plot=g.tmp,
  filename=tmpname,
  width=10/2,
  height=10/2
)
# gs.list[[tmpname]]<-list(
#   graph=g.tmp,
#   filename=tmpname,
#   width=10/2,
#   height=10/2
# )

output(plotdf,tmpname)


#########################################################
#########################################################

#PLOT OF DESCRIPTIVE STATISTICS

setwd(filesdir); dir()
fulloutput<-readRDS('fulloutput.RDS')
setwd(metadir); dir()
runmodsdf<-fread('runmodsdf.csv')
#get just the mods you want, which are the four main ones
mymodsdf <- runmodsdf[
  (runmodsdf$prefmods==T) | (
    runmodsdf$endogenous%in%c('unemp_china','manuf_china') & 
      runmodsdf$instrumented=='instrumented'
  ),'i'
]
mymods <- fulloutput[mymodsdf$i]
ddf <-lapply(mymods,function(x) {
  #x<-mymods[[1]]
  #get mean and sd of vars in sdsdf
  returndf <- lapply( x$sdsdf$var,function(y) {
    if(y=="incRate_corrected_estimated_25_ln")
      x$df[[y]]<-exp(x$df[[y]])
    data.frame(
      var=y,
      mean=mean( x$df[[y]] ),
      sd=sd( x$df[[y]] )
    )
  }) %>% rbind.fill
  returndf$var[returndf$var=='val']<-"D.incRate_corrected_estimated_25"
  returndf$var[returndf$var=='incRate_corrected_estimated_25_ln']<-"incRate_corrected_estimated_25"
  returndf
}) %>% rbind.fill %>% unique

ddf$varname<-sapply(
  ddf$var,
  getvarname
)
ddf$order<-sapply(
  ddf$var,
  getvarorder
)
ddf$type<-sapply(
  ddf$var,
  getvartype
)
ddf$isendogenous<-ddf$type=="endogenous"

#make the table
ddf<-ddf[order(ddf$order),]
ddf$varname[ddf$var=='D.incRate_corrected_estimated_25']<-'Increase in Incarceration Rate'
ddf <- ddf[,c('varname','mean','sd')]
ddf$mean <- ddf$mean %>% round(2) %>% format(nsmall=2)
ddf$sd <- ddf$sd %>% round(2) %>% format(nsmall=2)

# require(xtable)
# temptable<-xtable(
#   ddf,
#   caption='Descriptive Statistics',
#   label='tab_descriptive',
#   align= rep("c",ncol(ddf) + 1 )
# )
#print table
#(note this prints table without styling)
# setwd(outputdir)
# print(
#   temptable,
#   file='tab_descriptive.tex',
#   ###optional commands
#   add.to.row=NULL,
#   tabular.environment='tabular',
#   include.colnames=F,
#   floating=T,
#   ##preset commands, same for all
#   append=T,
#   caption.placement="top",
#   booktabs=T,
#   include.rownames=F,
#   sanitize.text.function=identity
# )



#########################################################
#########################################################

#TILEPLOT OF CZS IN THE PREF SAMPLE
#load sample

setwd(filesdir)
prefmods<-readRDS('prefmods.RDS')
tmpdf<-prefmods[[1]]$prefmod$df
tmpdf<-tmpdf[,c("cz90","periodf")]

#load cz info
plotdf<-crosswalks$cz90_region
newdf<-lapply(plotdf$cz90,function(thiscz) {
  #
  #thiscz<-"09800" %>% as.numeric
  data.frame(
    first=sum(tmpdf$cz90==thiscz & tmpdf$periodf=="first"),
    second.alt=sum(tmpdf$cz90==thiscz & tmpdf$periodf=="second.alt"),
    stringsAsFactors=F
  )
}) %>% rbind.fill
plotdf<-cbind(
  plotdf,
  newdf
)
#how many in each
table(plotdf$first)
table(plotdf$second.alt)

#merge statename
plotdf$state_fips<-plotdf$state_fips %>% as.numeric
plotdf<-merge(
  plotdf,
  crosswalks$sfips_salpha2,
  by='state_fips'
)

#merge placename
tmpdf<-crosswalks$fips_cz90
tmpdf<-tmpdf[,c("largestplace","cz90")] %>% unique
tmpdf$cz90 <- as.numeric(tmpdf$cz90)
plotdf<-merge(
  plotdf,
  tmpdf,
  by='cz90'
) 

#clean and fix dups in largestplace
plotdf$largestplace<-str_replace(
  plotdf$largestplace,
  "\\s(city|town|CDP|borough)",
  ""
) %>% str_replace(
  "(remainder|remainde|remaind|remain)",""
) %>% str_replace_all("\\(|\\)","") %>% str_replace("\\s,",",")

dups<-names(which(table(plotdf$largestplace)>1))
tmp<-plotdf$largestplace%in%dups
plotdf[tmp,]<-by(plotdf[tmp,],plotdf$largestplace[tmp],function(df) {
  df$largestplace <- paste0(df$largestplace," (",1:nrow(df),")")
  df
}) %>% rbind.fill


#order dataset
tmporder<-order(plotdf$state_alpha2,plotdf$largestplace)
plotdf<-plotdf[tmporder,]

#levels for largetsplace
tmplevels<-unique(plotdf$largestplace)
plotdf$largestplace<-factor(
  plotdf$largestplace,
  tmplevels %>% rev
)

#split into groups of 20
plotdf$group <- factor(sort(rank(1:nrow(plotdf))%%20))
plotdf<-by(plotdf,plotdf$group,function(df) {
  df$groupname<-rep(paste0(unique(df$state_alpha2),collapse=" / "),length(df$state_alpha2))
  df
}) %>% rbind.fill

#gather
plotdf<-gather(
  plotdf,
  periodf,
  insample,
  first:second.alt
)

#output for sample map
setwd(filesdir); dir()
write.csv(
  plotdf,
  'regsample_deets.csv',
  row.names=F
)

#names
tmplevels<-c(
  "first",
  "second.alt"
)
tmplabels<-c(
  "1991-1999",
  "1999-2011"
)
plotdf$periodf<-factor(
  plotdf$periodf,
  tmplevels,
  tmplabels
)

tmplevels<-plotdf$cz90 %>% unique
tmplevels<-tmplevels[order(tmplevels)]
plotdf$cz90<-factor(
  plotdf$cz90,
  tmplevels,
  tmplevels
)

tmplevels<-c(
  0,1
)
tmplabels<-c(
  "Out of Sample",
  "In Sample"
)
plotdf$insample<-factor(
  plotdf$insample,
  tmplevels,
  tmplabels
)
table(plotdf$insample)

#colors
brewer.pal.info
tmpcolors<-c('white','darkgreen')
names(tmpcolors)<-levels(plotdf$insample)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=largestplace,
    y=periodf,
    fill=insample
  )
) +
  geom_tile(
    color='black'
  ) +
  scale_fill_manual(
    name="",
    values=tmpcolors
  ) +
  facet_wrap(
    ~ groupname,
    scales='free'
  ) +
  coord_flip() +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(
    axis.title.y=element_blank(),
    axis.text.y=element_text(size=6),
    axis.ticks.y=element_blank(),
    axis.text.x=element_text(size=10)
  ) +
  theme(
    legend.position='bottom',
    legend.direction='horizontal'
  )

tmpname<-"fig_sample.png"
setwd(outputdir)
ggsave(
  plot=g.tmp,
  filename=tmpname,
  width=8*2,
  height=8*2
)
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8*2,
  height=8*2
)

#########################################################
#########################################################

#PLOT NATIONAL IMPRISONMENT RATE
#in the country
#all vera data
#czs in the regression sample
plotdfs<-list()

#get vera data
setwd(datadir); dir()
veradf<-read.csv(
  'vera_2002.csv',
  stringsAsFactors=F
)

#for writeup, for how many CZs do we have full incrate data
tapply(veradf$incRate_corrected_estimated_0,veradf$cz90,function(x) sum(!is.na(x))>0) %>% sum #525
tapply(veradf$incRate_corrected_estimated_25,veradf$cz90,function(x) sum(!is.na(x))>0) %>% sum #536 is the gain
veradf$incRate<-veradf$incRate_corrected_estimated_25
veradf<-veradf[,c("cz90","year","incRate")]

#we need pop info, which we otherwise discarded
setwd(prelimdir); dir()
tmpdf<-read.csv(
  'county-cz1990-incarceration-estimates_2002.csv',
  stringsAsFactors=F
)
tmpdf$cz90<-tmpdf$commuting_zone_1990
tmpdf$pop1564<-tmpdf$total_pop_15to64
tmpdf<-data.table(tmpdf)
tmpdf<-tmpdf[
  !is.na(cz90) & !is.na(year)
  ,
  .(pop1564=sum(pop1564))
  ,
  by=c("cz90","year")
]
veradf<-merge(
  veradf,
  tmpdf[,c('cz90','year','pop1564')]
)

#how many czs in the vera data?
head(veradf)
tmp<-!is.na(veradf$incRate) 
veradf$cz90[tmp] %>% unique %>% length

#what is avaialbility like
tapply(veradf$incRate,veradf$year,function(x) sum(!is.na(x)))
#until 1983, there are only 22 CZ's in thes sample.
#avability is much better starting in 1983,
#so we drop years before 1983
tmp<-veradf$year>=1983
veradf<-veradf[tmp,]

tmpdf<-by(veradf,veradf$year,function(df) {
  data.frame(
    year=unique(df$year),
    incRate=weighted.mean(
      df$incRate,
      df$pop1564,
      na.rm=T
    )
  )
}) %>% rbind.fill
tmpdf$source<-"raw"
plotdfs[["raw"]]<-tmpdf

#get vera data in regsample
setwd(filesdir)
prefmods<-readRDS('prefmods.RDS')
tmpdf<-prefmods[[1]]$prefmod$df
tmpdf<-tmpdf[,c("cz90","periodf")]
tmpdf<-by(tmpdf,tmpdf$cz90,function(df) {
  #df<-tmpdf[tmpdf$cz90==100,]
  if("first"%in%df$periodf) {
    fyears<-1991:2000
  } else {
    fyears<-NA
  }
  if("second.alt"%in%df$periodf) {
    syears<-2001:2011
  } else {
    syears<-NA
  }
  years<-c(fyears,syears)
  years<-years[!is.na(years)]
  data.frame(
    cz90=unique(df$cz90),
    year=years,
    stringsAsFactors=F
  )
}) %>% rbind.fill

#count how many are in pref, for writeup
unique(tmpdf$cz90) %>% length 

intersect(
  names(tmpdf),
  names(veradf)
)
tmpdf<-merge(
  tmpdf,
  veradf,
  all.x=T
)
tmpdf<-by(tmpdf,tmpdf$year,function(df) {
  data.frame(
    year=unique(df$year),
    incRate=weighted.mean(
      df$incRate,
      df$pop1564,
      na.rm=T
    )
  )
}) %>% rbind.fill
tmpdf$source<-"reg"

plotdfs[["reg"]]<-tmpdf

#get state and local jail incrate
setwd(prelimdir); dir()
tmpdf<-read.csv(
  'imprates_historic.csv',
  stringsAsFactors=F
)
keyvars<-c(
  "year",
  "state",
  "jail",
  "uspop"
)
tmpdf<-tmpdf[,keyvars]

#quick calc of amount of increase in our sample
qdf<-tmpdf
qdf$state<-as.numeric(qdf$state)
qdf$jail<-as.numeric(qdf$jail)
qdf$uspop<-as.numeric(qdf$uspop)
qdf$incrate<-10^5 * 
  (qdf$state + qdf$jail)/qdf$uspop
tmin<-1973
t0<-1991
tmax<-2007
totinc<-qdf$incrate[qdf$year==tmax] - 
  qdf$incrate[qdf$year==tmin]
sampinc<-qdf$incrate[qdf$year==tmax] - 
  qdf$incrate[qdf$year==t0]
#total amount that we don't see
100 - (100 * sampinc/totinc)
#total that we do
100 * sampinc/totinc

these.years<-c(
  plotdfs$raw$year,
  plotdfs$reg$year
) %>% unique
tmprows<-tmpdf$year%in%these.years
tmpdf<-tmpdf[tmprows,]

#we need population 15to65 in US at large
#get this from the CZ-level data from vera
head(veradf)
tmp<-tapply(veradf$pop1564,veradf$year,sum,na.rm=T)
popdf<-data.frame(
  year=names(tmp),
  pop1564=tmp,
  stringsAsFactors=F
)
tmpdf<-merge(
  tmpdf,
  popdf
)
tmpdf$state<-as.numeric(tmpdf$state)
tmpdf$jail<-as.numeric(tmpdf$jail)
tmpdf$incRate <- 10^5 * (tmpdf$state + tmpdf$jail)/tmpdf$pop1564
tmpdf$state<-tmpdf$jail<-tmpdf$uspop<-tmpdf$pop1564<-NULL
tmpdf$source<-"nat"
plotdfs[['nat']]<-tmpdf

plotdf<-rbind.fill(plotdfs)
tmp<-!is.na(plotdf$incRate)
plotdf<-plotdf[tmp,]

tmplevels<-c(
  "raw",
  "reg",
  "nat"
)
tmplabels<-c(
  "CZs with Data",
  "CZs in Regression",
  "National Rate"
)
plotdf$source<-factor(
  plotdf$source,
  tmplevels,
  tmplabels
)
tmptypes<-c(3,2,1)
names(tmptypes)<-levels(plotdf$source)

#stop where national rate stops
maxyear<-max(plotdf$year[plotdf$source=='National Rate'])
plotdf<-plotdf[plotdf$year<=maxyear,]

g.tmp<-ggplot(
  plotdf,
  aes(
    x=year,
    y=incRate,
    group=source,
    linetype=source#,
    #shape=source
  )
) +
  geom_line() +
  #geom_point() +
  scale_linetype_manual(
    name="",
    values = tmptypes
  ) +
  # scale_shape_discrete(
  #   name=""
  # ) +
  xlab("") +
  ylab("State and Jail Incarceration per 100,000 Adults\n") +
  theme_bw() +
  theme(
    legend.position='bottom',
    legend.direction = 'horizontal'
  )

tmpname<-"fig_incrates.png"
setwd(outputdir)
ggsave(
  plot=g.tmp,
  filename=tmpname,
  width=10,
  height=6
)
# gs.list[[tmpname]]<-list(
#   graph=g.tmp,
#   filename=tmpname,
#   width=10,
#   height=6
# )

#########################################################
#########################################################

#NATIONAL-LEVEL, INCARCERATION VS. LM

#nationa-level incrate
incdf<-plotdfs$nat

#vs. unemployment and emptopop
#get and fix national level stats from BLS
setwd(datadir); dir()
tmp<-str_detect(dir(),"^bls")
tmpfiles<-dir()[tmp]
tmpdf<-lapply(tmpfiles,function(thisfile) {
  #thisfile<-tmpfiles[1]
  tmpdf<-readxl::read_xlsx(
    thisfile,
    skip=11
  )
  names(tmpdf)<-tolower(names(tmpdf))
  tmpdf$val<-apply(tmpdf[,-1],1,mean,na.rm=T) #take avg
  tmpdf$var<-str_replace(thisfile,"bls\\_","") %>%
    str_replace("\\.xlsx","")
  tmpdf[,c("year","var","val")]
}) %>% rbind.fill
tmpdf<-spread(
  tmpdf,
  var,
  val
)
tmpdf$manushare<-100 * tmpdf$manuf/(tmpdf$private + tmpdf$govt)
tmpdf$govt<-tmpdf$manuf<-tmpdf$private<-NULL

#we need to do a clever merge,
#b/c we want to plot this in facets

tmpdf<-gather(
  tmpdf,
  var,
  lmind,
  emptopop:manushare
)
plotdf<-merge(
  incdf,
  tmpdf
)
plotdf$source<-NULL

#omit manushare
tmp<-plotdf$var%in%c("unemployment","emptopop")
plotdf<-plotdf[tmp,]

tmpvars<-c("lmind","incRate")
plotdf<-gather_(
  plotdf,
  "var2",
  "val",
  tmpvars
)

plotdf<-data.table(plotdf)
plotdf<-plotdf[
  ,
  .(
    year=year,
    val=scale(val)[,1])
  ,
  by=c(
    "var",
    "var2"
  )
]

corrsdf<-by(plotdf,plotdf$var,function(df) {
  #df<-plotdf[plotdf$var=="manushare",]
  tmpdf<-spread(df,var2,val)
  tmpoutput<-cor.test(
    tmpdf$lmind,
    tmpdf$incRate,
    use='complete.obs'
  )
  data.frame(
    var=unique(df$var),
    cor=tmpoutput$estimate,
    pval=tmpoutput$p.value
  )
}) %>% rbind.fill
corrsdf$text

corrsdf$cor<-format(corrsdf$cor,digits=2)
#hack to get 0.00
options(scipen=99)
corrsdf$pval<-prettyNum(corrsdf$pval,digits=2) %>%
  str_extract("[0-9]\\.[0-9]{2}")
options(scipen=0)
corrsdf$textdisp<-paste0(
  corrsdf$cor,
  " (pval=",
  corrsdf$pval,")"
)

#show just unemploiyment rate
plotdf<-plotdf[var=='unemployment']
corrsdf<-corrsdf[corrsdf$var=='unemployment',]

# tmplevels<-c(
#   "emptopop",
#   "unemployment",
#   "manushare"
# )
# tmplabels<-c(
#   "Emp-to-Pop Ratio (CPS)",
#   "Unemployment Rate (CPS)",
#   "Manufacturing Share (CES)"
# )
# plotdf$var<-factor(
#   plotdf$var,
#   tmplevels,
#   tmplabels
# )
# corrsdf$var<-factor(
#   corrsdf$var,
#   tmplevels,
#   tmplabels
# )
# 
# tmplevels<-c(
#   "incRate",
#   "lmind"
# )
# tmplabels<-c(
#   "Incarceration Rate",
#   "Labor Market Indicator"
# )
# plotdf$var2<-factor(
#   plotdf$var2,
#   tmplevels,
#   tmplabels
# )

tmplevels<-c(
  "incRate",
  "lmind"
)
tmplabels<-c(
  "Incarceration Rate",
  "Unemployment Rate"
)
plotdf$var2<-factor(
  plotdf$var2,
  tmplevels,
  tmplabels
)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=year,
    y=val
  )
) +
  geom_line(
    aes(
      linetype=var2,
      group=var2
    )
  ) +
  scale_linetype_discrete(name="") +
  geom_text(
    data=corrsdf,
    aes(
      label=textdisp
    ),
    x=1990,
    y=1.75,
    size=4
  ) +
  # facet_wrap(
  #   ~ var,
  #   ncol=1
  # ) +
  xlab("") +
  ylab("Standardized Level\n") +
  theme_bw() +
  theme(
    legend.position='bottom',
    legend.direction='horizontal'
  )

setwd(outputdir)
tmpname<-"fig_inclm_corrs.png"
ggsave(
  plot=g.tmp,
  filename=tmpname,
  width=5,
  height=4
)

gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=5,
  height=4
)


#########################################################
#########################################################

#DENSITY AND CORRELATION PLOTS
#of the shocks, for the cfactuals
#where is 20th, where is 80th, where is stability

setwd(filesdir); dir()
fulloutput<-readRDS('fulloutput.RDS')
tmpdf<-fread('regresults.csv')
tmp<-tmpdf$prefmods | (
  tmpdf$endogenous%in%c(
    'manuf_china',
    'unemp_china'
  ) & tmpdf$instrumented=='instrumented'
)
prefmods<-fulloutput[unique(tmpdf$i[tmp])]

myvars<-c(
  "D.manushare",
  "D.emptopop",
  "D.emptopopc",
  "D.unemprate"
)
tmpseq.i<-1:length(myvars)
tmpdfs<-lapply(tmpseq.i,function(i) {
  #i<-1
  modname<-i
  tmpdf<-prefmods[[i]]$df
  
  thisvar<-myvars[myvars%in%names(tmpdf)]
  tmpvars<-c(
    "cz90",
    "periodf",
    thisvar
  )
  tmpdf<-tmpdf[,tmpvars]
  # #if this is manushare or emptopop, 
  # #it will be more intuitive for someone
  # #to read these as chg. in the endogenous variable
  # #rather than as -/+
  # if(thisvar!='D.unemprate')
  #   tmpdf[[thisvar]]<- -1 * tmpdf[[thisvar]]
  tmpdf
})

plotdf<-Reduce(
  function(...)
    merge(..., by=c("cz90","periodf"), all=T),
  tmpdfs
)

#get correlation plot
cordf<-expand.grid(
  var1=myvars,
  var2=myvars,
  stringsAsFactors=F
)
cordf$rho<-NA
for (i in 1:nrow(cordf) ) {
  #i<-2
  tmpoutput<-cor.test(plotdf[[cordf$var1[i]]],plotdf[[cordf$var2[i]]] )
  cordf$rho[i]<-tmpoutput$estimate
}
cordf$rho.disp<-format(cordf$rho,digits=2)
cordf$rho.disp[cordf$rho.disp=="1.00"]<-""

plotdf<-gather_(
  plotdf,
  "var",
  "val",
  myvars
)

tmplevels<-c(
  "D.unemprate",
  "D.manushare",
  "D.emptopop",
  "D.emptopopc"
) %>% rev
tmplabels<-c(
  "Increase in Unemployment Rate",
  "Decline in Manufacturing Share",
  "Decline in Emp-to-Pop Ratio (CBP)",
  "Decline in Emp-to-Pop Ratio (IPUMS)"
) %>% rev
plotdf$var<-factor(
  plotdf$var,
  tmplevels,
  tmplabels
)
cordf$var2<-factor(
  cordf$var2,
  tmplevels %>% rev,
  tmplabels %>% rev
)
cordf$var1<-factor(
  cordf$var1,
  tmplevels,
  tmplabels
)

tmplist<-list(
  plotdf$var,
  plotdf$periodf
)
sumdf<-by(plotdf,tmplist,function(df) {
  #df<-plotdf[plotdf$var=="D.manushare",]
  data.frame(
    var=unique(df$var),
    periodf=unique(df$periodf),
    lowptile=quantile(df$val,0.2),
    median=quantile(df$val,0.5),
    highptile=quantile(df$val,0.8),
    maxy=max(density(df$val)$y),
    stringsAsFactors=F
  )
}) %>% rbind.fill
sumdf<-gather(
  sumdf,
  stat,
  val,
  lowptile:highptile
)

tmplevels<-c(
  "lowptile",
  "median",
  "highptile"
)
tmplabels<-c(
  "20th Percentile",
  "50th Percentile",
  "80th Percentile"
)
sumdf$stat<-factor(
  sumdf$stat,
  tmplevels,
  tmplabels
)

tmplevels<-c(
  "first",
  "second.alt"
)
tmplabels<-c(
  "1991-1999",
  "1999-2011"
)
plotdf$periodf<-factor(
  plotdf$periodf,
  tmplevels,
  tmplabels
)
sumdf$periodf<-factor(
  sumdf$periodf,
  tmplevels,
  tmplabels
)

textdf<-sumdf[sumdf$stat=="50th Percentile",]
textdf$vald<-format(textdf$val,digits=1)
textdf$vald[!str_detect(textdf$vald,"-")] <-
  paste0("+",str_replace(textdf$vald[!str_detect(textdf$vald,"-")],"\\s",""))

g.tmp<-ggplot(
  plotdf,
  aes(
    x=val
  )
) +
  geom_density(
    fill='grey'
  ) +
  geom_vline(
    xintercept=0,
    alpha=0.2,
    linetype='longdash',
    color='darkblue'
  ) +
  geom_text(
    data=textdf,
    aes(
      x=val,
      y=maxy+0.025,
      label=vald
    ),
    fontface='bold'
  ) +
  scale_linetype_discrete(
    name=""
  ) +
  facet_grid(
    periodf ~ var,
    #scales='free'
  ) +
  xlab("") +
  ylab("") +
  theme_bw()

tmpname<-"fig_densities.png"
setwd(outputdir)
ggsave(
  plot=g.tmp,
  filename=tmpname,
  width=12,
  height=6
)
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=12,
  height=6
)
output(sumdf,tmpname)


g.tmp<-ggplot(
  cordf,
  aes(
    x=var1,
    y=var2,
    label=rho.disp,
    fill=rho
  )
) +
  geom_tile() +
  geom_text() +
  scale_x_discrete(position='top') +
  scale_fill_gradient(
    low="#deebf7",
    high="#3182bd"
  ) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

tmpname<-"fig_endog_corrs.png"
setwd(outputdir)
ggsave(
  plot=g.tmp,
  filename=tmpname,
  width=10,
  height=4
)
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=10,
  height=4
)
output(cordf,tmpname)

#########################################################
#########################################################

#PRISONS VS POLICE, SOCIAL VS PENAL

setwd(prelimdir); dir()
pbdf<-fread('penalbalance_chinashock.csv')
tmp<-!is.na(pbdf$penalbalance)
pbdf<-pbdf[tmp,]
spdf<-fread('ratiosp_chinashock.csv')
spdf$countryname[spdf$countryname=='United States']<-'USA'
#use E+W as stand-in for UK
spdf$countryname[spdf$countryname=='United Kingdom']<-'England and Wales'

tmp<-pbdf$countryname%in%spdf$countryname
pbdf$countryname[!tmp]
tmp<-spdf$countryname%in%pbdf$countryname
spdf$countryname[!tmp]

plotdf<-merge(
  spdf,
  pbdf,
  by='countryname'
)
plotdf$cownum<-NULL


plotdf<-gather(
  plotdf,
  var,
  val,
  ratio_sp:penalbalance
)
plotdf$usa<-factor(
  plotdf$countryname=='USA',
  c(T,F)
)
tmpcolors<-c('red','grey')
tmpsizes<-c(4,2)
names(tmpsizes)<-names(tmpcolors)<-c(T,F)

tmporder<-order(-plotdf$val[plotdf$var=="penalbalance"])
tmplevels <- plotdf$countryname[plotdf$var=="penalbalance"][tmporder]
plotdf$countryname <- factor(
  plotdf$countryname,
  tmplevels
)

#s3p: social protection + eductation
#s2p: social protection
#sp: social protection + education + health
#we exclude healthcare spending b/c of strange inefficiencies of US system
plotdf<-plotdf[plotdf$var%in%c('penalbalance','ratio_sp'),] 

tmplevels<-c(
  'penalbalance',
  'ratio_sp'
)
tmplabels<-c(
  "Prisoners / Police",
  "Social Spending / Penal Spending"
)
plotdf$var <- factor(
  plotdf$var,
  tmplevels,
  tmplabels
)

g.tmp <- ggplot(
  plotdf,
  aes(
    x=countryname,
    y=val,
    fill=usa
  )
) +
  geom_bar(
    stat='identity',
    color='black',
    width=0.75
  ) +
  scale_fill_manual(
    values=tmpcolors,
    guide='none'
  ) +
  facet_wrap(
    ~ var,
    scales = 'free_x'
  ) +
  coord_flip() +
  ylab("") +
  xlab("") +
  theme_bw()

tmpname<-"fig_exceptionalism.png"
setwd(outputdir)
ggsave(
  plot=g.tmp,
  filename=tmpname,
  width=10,
  height=5
)
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=10,
  height=5
)
output(plotdf,tmpname,g.tmp)


#########################################################
#########################################################

# #CERTAINTY VS. SEVERITY
# #and SOCIAL VS. PENAL
# 
# setwd(prelimdir); dir()
# policedf<-fread(
#   'wiki_police.csv'
# )
# wpbdf<-fread(
#   'wpb.csv'
# )
# homdf<-fread(
#   'wb_homicides.csv'
# )
# tmpdf<-rbind.fill(
#   policedf,
#   wpbdf,
#   homdf
# )
# tmp<-tmpdf$statistic%in%c(
#   'prisoners',
#   'police',
#   'homicides'
# )
# tmpvars<-c(
#   'cowcode',
#   'countryname',
#   'year',
#   'statistic',
#   'value'
# )
# tmpdf<-tmpdf[tmp,tmpvars] %>%
#   data.table
# 
# #fix value
# tmpnum<-str_replace_all(
#   tmpdf$value,
#   "c|c\\.|\\s|,|\\*",
#   ""
# ) %>% as.numeric
# tmpdf$value[is.na(tmpnum)] #some additions, fix later
# tmpdf[is.na(tmpnum),] #all in Rwanda
# tmpdf$value<-tmpnum
# 
# #fix so we get UK also
# tmp<-tmpdf$countryname%in%c("England and Wales","Scotland")
# ukdf<-tmpdf[tmp,]
# ukdf$countryname<-"United Kingdom"
# ukdf$cowcode<-"200"
# ukdf<-ukdf[
#   ,
#   .(
#     value=sum(value)
#   )
#   ,
#   by=c(
#     'cowcode',
#     'countryname',
#     'year',
#     'statistic'
#   )
# ]
# tmpdf<-tmpdf[!tmp,] #drop england/wales & scotland
# tmpdf<-rbind.fill( #add uk
#   tmpdf,
#   ukdf
# ) %>% data.table
# 
# #make sure each is unique
# tmpdf$decade<-floor(tmpdf$year/10) * 10
# tmpdf<-tmpdf[
#   decade==2010
#   ,
#   .(
#     value=mean(value)
#   )
#   ,
#   by=c(
#     'cowcode',
#     'countryname',
#     'decade',
#     'statistic'
#   )
# ]
# tmpdf<-spread(
#   tmpdf,
#   statistic,
#   value
# )
# tmpdf<-tmpdf[complete.cases(tmpdf),]
# 
# #add population
# dir()
# popdf<-fread(
#   'mpd2018df.csv'
# )
# tmpvars<-c('cowcode','year','population','gdppc')
# popdf$decade<-floor(popdf$year/10) * 10
# popdf<-popdf[
#   decade==2010
#   ,
#   .(
#     pop=mean(population,na.rm=T),
#     gdppc=mean(gdppc,na.rm=T)
#   )
#   ,
#   by=c(
#     'cowcode',
#     'countryname',
#     'decade'
#   )
# ]
# tmpvars<-c('cowcode','decade','pop','gdppc')
# popdf<-popdf[,tmpvars,with=F]
# 
# #merge
# tmpdf<-merge(
#   tmpdf,
#   popdf,
#   both=T
# )
# 
# #fix countrynames
# tmpdf$countryname<-str_replace(
#   tmpdf$countryname,
#   "\\s[0-9]{4}\\-[0-9]{4}",
#   ""
# )
# tmp<-tmpdf$countryname=="United States of America"
# tmpdf$countryname[tmp]<-"USA"
# 
# #add spending for some oced countries
# setwd(prelimdir); dir()
# spdf <- fread(
#   'oecd_govspending_2017.csv'
# )
# tmp<-spdf$countryname=="Slovak Republic"
# spdf$countryname[tmp]<-"Slovakia"
# tmp<-spdf$countryname=="United States"
# spdf$countryname[tmp]<-"USA"
# tmpsubjects<-c("TOT","PUBORD","SOCPROT","HEALTH","EDU")
# spdf<-spdf[
#   time==2017 & 
#     measure=="value" & 
#     subject%in%tmpsubjects &
#     advanced
#   ,
#   ]
# spdf<-spread(
#   spdf,
#   subject,
#   value
# ) %>% data.table
# names(spdf)<-tolower(names(spdf))
# spdf$ratio_sp<-(spdf$edu + spdf$health + spdf$socprot)/spdf$pubord
# keepvars<-c(
#   "countryname",
#   "ratio_sp"
# )
# spdf<-spdf[,keepvars,with=F]
# spdf$decade<-2010
# 
# fulldf<-merge(
#   tmpdf,
#   spdf,
#   by=c(
#     'countryname',
#     'decade'
#   ),
#   all.x=T
# )
# 
# #make vars
# fulldf$pop <- fulldf$pop*10^3
# fulldf$homicides <- fulldf$homicides * fulldf$pop/10^5
# fulldf$policeperhom <- log(fulldf$police/fulldf$homicides)
# fulldf$prisonersperhom <- log(fulldf$prisoners/fulldf$homicides)
# fulldf$policepercap <- log(fulldf$police/fulldf$pop)
# fulldf$prisonerspercap <- log(fulldf$prisoners/fulldf$pop)
# 
# #quick browse of advanced countries only
# tmp<-complete.cases(fulldf)
# fulldf<-fulldf[tmp,]
# fulldf
# fulldf$policeperhom <- exp(fulldf$policeperhom)
# fulldf$prisonersperhom <- exp(fulldf$prisonersperhom)
# fulldf$policepercap <- exp(fulldf$policepercap)*10^5
# 
# fulldf$policepercap[fulldf$cowcode==2]
# mean(fulldf$policepercap[fulldf$cowcode!=2])
# fulldf$policeperhom[fulldf$cowcode==2]
# mean(fulldf$policeperhom[fulldf$cowcode!=2])
# fulldf$prisonersperhom[fulldf$cowcode==2]
# mean(fulldf$prisonersperhom[fulldf$cowcode!=2])
# 
# 
# #########################################################
# #########################################################
# 
# #POLICE VS. PRISONS
# 
# plotdf<-fulldf
# bigcountries <- plotdf$pop > quantile(plotdf$pop,0.5)
# plotdf<-plotdf[bigcountries,]
# 
# #standardize in this sample
# vars<-c('policeperhom','prisonersperhom','policepercap','prisonerspercap')
# for(v in vars) {
#   plotdf[[v]] <- scale(plotdf[[v]])[,1]
# }
# 
# #gather, percap,perhom separately
# plotdf<-gather_(
#   plotdf,
#   "var",
#   "val",
#   vars
# )
# tmp<-str_detect(plotdf$var,"perhom")
# plotdf$group<-"percap"
# plotdf$group[tmp]<-"perhom"
# plotdf$var<-str_extract(plotdf$var,"police|prisoners")
# plotdf<-spread(
#   plotdf,
#   var,
#   val
# )
# 
# plotdf$usa<-factor(
#   plotdf$cowcode==2,
#   c(T,F)
# )
# tmpcolors<-c('red','black')
# tmpsizes<-c(4,2)
# names(tmpsizes)<-names(tmpcolors)<-c(T,F)
# 
# plotdf$group<-factor(
#   plotdf$group,
#   c('percap','perhom'),
#   c('...capita','...homicide')
# )
# 
# g.tmp <- ggplot(
#   plotdf,
#   aes(
#     x=prisoners,
#     y=police,
#     label=countryname,
#     color=usa,
#     size=usa
#   )
# ) +
#   geom_text(
#     fontface='bold',
#     #size=2
#   ) +
#   geom_hline(yintercept=0) +
#   geom_vline(xintercept=0) +
#   scale_color_manual(
#     values=tmpcolors,
#     guide=F
#   ) +
#   scale_size_manual(
#     values=tmpsizes,
#     guide=F
#   ) +
#   stat_smooth(
#     size=0.5,
#     method='lm',
#     alpha=0.05,
#     linetype='dashed'
#   ) +
#   xlab("\nPrisoners per... (log)") +
#   ylab("Police per... (log)\n") +
#   facet_wrap(
#     ~ group
#   ) +
#   theme_bw() +
#   theme(
#     #axis.line=element_blank(),
#     axis.text.x=element_blank(),
#     axis.text.y=element_blank(),
#     axis.ticks=element_blank(),
#     #axis.title.x=element_blank(),
#     #axis.title.y=element_blank(),
#     legend.position="none",
#     panel.background=element_blank(),
#     panel.border=element_blank(),
#     panel.grid.major=element_blank(),
#     panel.grid.minor=element_blank(),
#     plot.background=element_blank()
#   )
# 
# 
# tmpname<-"fig_policevsprisons.png"
# setwd(outputdir)
# ggsave(
#   plot=g.tmp,
#   filename=tmpname,
#   width=10,
#   height=4
# )
# gs.list[[tmpname]]<-list(
#   graph=g.tmp,
#   filename=tmpname,
#   width=10,
#   height=4
# )
# output(plotdf,tmpname,g.tmp)
# 
# 
# #########################################################
# #########################################################
# 
# #AMERICAN EXCEPTIONALISM
# 
# plotdf<-fulldf
# tmp<-complete.cases(plotdf)
# plotdf<-plotdf[tmp,]
# 
# #police vs. prisons
# plotdf$prizpolratio <- plotdf$police/plotdf$prisoners
# #plotdf$prizpolratio <- scale(plotdf$prizpolratio)[,1]
# 
# 
# 
# #social penal
# plotdf$spratio <- plotdf$ratio_sp
# #plotdf$spratio <- scale(plotdf$ratio_sp)[,1]
# 
# plotdf<-plotdf[,c('countryname','cowcode','prizpolratio','spratio'),with=F]
# plotdf<-gather(
#   plotdf,
#   var,
#   val,
#   prizpolratio:spratio
# )
# plotdf$usa<-factor(
#   plotdf$cowcode==2,
#   c(T,F)
# )
# tmpcolors<-c('red','grey')
# tmpsizes<-c(4,2)
# names(tmpsizes)<-names(tmpcolors)<-c(T,F)
# 
# tmporder<-order(plotdf$val[plotdf$var=="prizpolratio"])
# tmplevels <- plotdf$countryname[plotdf$var=="prizpolratio"][tmporder]
# plotdf$countryname <- factor(
#   plotdf$countryname,
#   tmplevels
# )
# 
# tmplevels<-c(
#   'prizpolratio',
#   'spratio'
# )
# tmplabels<-c(
#   "Police / Prisoners",
#   "Social Spending / Penal Spending"
# )
# plotdf$var <- factor(
#   plotdf$var,
#   tmplevels,
#   tmplabels
# )
# 
# g.tmp <- ggplot(
#   plotdf,
#   aes(
#     x=countryname,
#     y=val,
#     fill=usa
#   )
# ) +
#   geom_bar(
#     stat='identity',
#     color='black',
#     width=0.75
#     ) + 
#   scale_fill_manual(
#     values=tmpcolors,
#     guide=F
#   ) +
#   facet_wrap(
#     ~ var,
#     scales = 'free_x'
#   ) +
#   coord_flip() +
#   ylab("") +
#   xlab("") +
#   theme_bw()
# 
# tmpname<-"fig_exceptionalism.png"
# setwd(outputdir)
# ggsave(
#   plot=g.tmp,
#   filename=tmpname,
#   width=10,
#   height=5
# )
# gs.list[[tmpname]]<-list(
#   graph=g.tmp,
#   filename=tmpname,
#   width=10,
#   height=5
# )
# output(plotdf,tmpname,g.tmp)


#########################################################
#########################################################

# #OUTPUT
# #output graphlist
# setwd(outputdir)
# this.sequence<-seq_along(gs.list)
# for(i in this.sequence) {
#   print(
#     paste0(
#       "saving ",i," of ",length(this.sequence)
#     )
#   )
#   thiselement<-gs.list[[i]]
#   ggsave(
#     #filename="tmp.pdf",
#     filename=thiselement$filename,
#     plot=thiselement$graph,
#     width=thiselement$width,
#     height=thiselement$height
#   )
#   # #embed font
#   # embed_fonts(
#   #   file="tmp.pdf",
#   #   outfile=thiselement$filename
#   # )
#   # file.remove(
#   #   "tmp.pdf"
#   # )
#   #Sys.sleep(0.5)
# }
# 


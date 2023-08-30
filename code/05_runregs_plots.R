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
thiscompdir<-codedir
setwd(codedir); dir()
source('dirs.R')
source('functions.R')

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
setwd(thiscompdir)
source('dirs.R')
source('functions.R')

#########################################################
#########################################################

#plotting prelims
require(ggplot2)
require(ggthemes)
require(extrafont)
require(RColorBrewer)
require(scales)
#load fonts
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
  write.csv(
    df[,keyvars],
    tmpname,
    row.names=F
  )
  
}

#########################################################
#########################################################

#these are the keyvars
keyvars<-sapply(
  ivs_list,
  function(x) x$end
)

#for plotting semi-standardized estimates, mult by 100
#since this makes chg in incarceration rate interpertable
finaldf$mu.semisd<-finaldf$mu.semisd * 100
finaldf$mu.semisd.min<-finaldf$mu.semisd - (1.96 * 100 * finaldf$se.semisd)
finaldf$mu.semisd.max<-finaldf$mu.semisd + (1.96 * 100 * finaldf$se.semisd)

#add shape et al to finaldf
#add pval info to shape of point
finaldf$pval.shp<-NA
finaldf$pval.shp[finaldf$pval.class=="at alpha=0.01"]<-1
finaldf$pval.shp[finaldf$pval.class=="at alpha=0.05"]<-2
finaldf$pval.shp[finaldf$pval.class=="at alpha=0.10"]<-3
finaldf$pval.shp[finaldf$pval.class=="not sig"]<-4
finaldf$pval.shp<-factor(
  finaldf$pval.shp,
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
names(tmpshapes)<-levels(finaldf$pval.shp)
shp.labels<-c(
  bquote(alpha == .01),
  bquote(alpha == .05),
  bquote(alpha == .10)
)

#add fill oclors, for tileplots
#get pval fill, for tile
finaldf$pval.fill<-NA
finaldf$pval.fill[finaldf$pval.class=="at alpha=0.01"]<-4
finaldf$pval.fill[finaldf$pval.class=="at alpha=0.05"]<-3
finaldf$pval.fill[finaldf$pval.class=="at alpha=0.10"]<-2
finaldf$pval.fill[finaldf$pval.class=="not sig"]<-1
negmu<-ifelse(finaldf$mu<0,-1,1)
finaldf$pval.fill<-finaldf$pval.fill * negmu
pval.labels<-c("at alpha=0.01","at alpha=0.05","at alpha=0.10","")
tmplabels<-c(
  paste0("- /",pval.labels),
  paste0("+ /",rev(pval.labels))
)
#assign levels,colors
finaldf$pval.fill<-factor(
  finaldf$pval.fill,
  levels=c(-4,-3,-2,-1,1,2,3,4),
  labels=tmplabels
)
#for colors, consult brewer
brewer.pal.info
tmpcolors<-brewer.pal(8,"RdYlGn")
names(tmpcolors)<-levels(finaldf$pval.fill)
fill.labels<-c(
  expression(paste(alpha==.01,", ",beta<0)),
  expression(paste(alpha==.05,", ",beta<0)),
  expression(paste(alpha==.10,", ",beta<0)),
  expression(paste(beta<0)),
  expression(paste(beta>0)),
  expression(paste(alpha==.10,", ",beta>0)),
  expression(paste(alpha==.05,", ",beta>0)),
  expression(paste(alpha==.01,", ",beta>0))
)

#########################################################
#########################################################

#for writeup
calcs_list<-list()

finaldf[
  finaldf$prefmods==T & 
    finaldf$var%in%c('D.emptopopc','D.emptopop'),
  ]

tmpdf<-finaldf[
  finaldf$instrumented=='instrumented' & finaldf$var%in%c('D.unemprate'),
]
tmpdf
finaldf[
  finaldf$stage=='firststage' & 
    finaldf$var=='otch' & 
    (
      finaldf$prefmods==T |
        finaldf$endogenous%in%c('manuf_china','unemp_china')
    )
  ,
  c('endogenous','mu.sd')
]

finaldf[
  finaldf$dv=='D.violent_crt' & 
    finaldf$var%in%c('D.emptopopc','D.emptopop'),
]

#########################################################
#########################################################

#MAKE 1S PLOTS
#rf, 1s estimate
#for preferred spec

tmp<-(
  finaldf$prefmods |
    finaldf$endogenous%in%c(
      #'emptopopc_china',
      'manuf_china',
      'unemp_china'
    )
) &
  finaldf$var%in%c(
    'otch',
    "D.emptopop",
    "D.manushare",
    "D.emptopopc",
    "D.unemprate"
  ) &
  finaldf$stage%in%c("firststage","reducedform","secondstage")
plotdf<-finaldf[tmp,]

tmplevels<-c(
  "reducedform",
  "firststage",
  "secondstage"
) %>% rev
tmplabels<-c(
  "Reduced Form",
  "First Stage",
  "Second Stage"
) %>% rev
plotdf$stage <- factor(
  plotdf$stage,
  tmplevels,
  tmplabels
)

tmplevels<-c(
  "unemp_china",
  "manuf_china",
  "emptopopc_china",
  "emptopop_china"
) %>% rev
tmplabels<-c(
  "+ Unemployment Rate",
  "- Manufacturing Share",
  "- Emp-to-Pop Ratio (IPUMS)",
  "- Emp-to-Pop Ratio (CBP)"
) %>% rev
plotdf$endogenous<-factor(
  plotdf$endogenous,
  tmplevels,
  tmplabels
)

g.tmp <- ggplot(
  plotdf,
  aes(
    x=stage,
    y=mu.sd,
    ymin=mu.sd.min,
    ymax=mu.sd.max,
    shape=pval.shp
  )
) +
  geom_point() +
  geom_errorbar(
    width=0.3
  ) +
  geom_hline(
    yintercept=0,
    linetype='dashed',
    color='red',
    alpha=0.5
  ) +
  scale_shape_manual(
    name="",
    values=tmpshapes,
    labels=shp.labels,
    drop=F
  ) +
  facet_wrap(
    ~ endogenous,
    ncol=1
  ) +
  coord_flip() +
  xlab("") +
  ylab("\nStandardized Estimate") +
  theme_bw()

setwd(outputdir)
tmpname<-"fig_rffs.png"
ggsave(
  plot=g.tmp,
  filename=tmpname,
  width=3.75 * 1.5,
  height=4.94 * 1.5
)

plotdf[plotdf$stage=='First Stage',c('endogenous','mu.sd')]

# gs.list[[tmpname]]<-list(
#   graph=g.tmp,
#   filename=tmpname,
#   width=3.75 * 1.5,
#   height=4.94 * 1.5
# )
output(plotdf,tmpname)

#########################################################
#########################################################

#MAINREGS PLOT

#this compares the OLS to 2SLS results
#across the prefmods

tmp<-finaldf$prefmods
tmp<-tmp | finaldf$endogenous%in%c(
  #'emptopopc_china',
  'manuf_china',
  'unemp_china'
)
tmp<-tmp | finaldf$instrumented=='ols'
#tmp<-tmp & finaldf$endogenous!='unemp_china'
tmp<-tmp & finaldf$var%in%c(
  "D.emptopop",
  "D.manushare",
  "D.emptopopc",
  "D.unemprate"
)
tmp<-tmp & finaldf$dv=="D.incRate_corrected_estimated_25"
plotdf<-finaldf[tmp,]

tmplevels<-c(
  "D.incRate_corrected_estimated_25"
)
tmplabels<-c(
  "Incarceration Rate"
)
plotdf$dv<-factor(
  plotdf$dv,
  tmplevels,
  tmplabels
)

tmplevels<-c(
  "ols","instrumented"
) %>% rev
tmplabels<-c(
  "OLS","2SLS"
) %>% rev
plotdf$instrumented<-factor(
  plotdf$instrumented,
  tmplevels,
  tmplabels
)

tmplevels<-c(
  "unemp_china",
  "manuf_china",
  "emptopopc_china",
  "emptopop_china"
) %>% rev
tmplabels<-c(
  "+ Unemployment Rate",
  "- Manufacturing Share",
  "- Emp-to-Pop Ratio (IPUMS)",
  "- Emp-to-Pop Ratio (CBP)"
) %>% rev
plotdf$endogenous<-factor(
  plotdf$endogenous,
  tmplevels,
  tmplabels
)

plotdf[,c('instrumented','dv','endogenous','N','N.czs')]

g.tmp<-ggplot(
  plotdf,
  aes(
    x=instrumented,
    y=mu.semisd,
    ymin=mu.semisd.min,
    ymax=mu.semisd.max,
    shape=pval.shp
  )
) +
  geom_point(
    size=2
  ) +
  geom_errorbar(
    linewidth=0.4,
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
  scale_color_discrete(
    name=""
  ) +
  ylab("\nEstimated Effect on Incarceration Rate (%)") +
  xlab("") +
  coord_flip() +
  facet_wrap(
    ~ dv + endogenous,
    ncol=1
  ) +
  theme_bw() +
  theme(
    text = element_text(family='serif')
  )
g.tmp

setwd(outputdir)
tmpname<-"fig_ols2sls.pdf"
ggsave(
  g.tmp,
  filename=tmpname,
  width=6,
  height=5
)
output(plotdf,tmpname)

#########################################################
#########################################################

#INCARCERATION, JAIL, IMPRISONMENT, CRIME

tmpvars<-c(
  ###crime
  #"D.pcrt_fs",                           
  "D.property_crt",
  "D.burglry_crt",
  "D.mvtheft_crt",
  "D.arson_crt",
  #"D.vcrt_fs",
  "D.violent_crt",
  "D.murder_crt",
  "D.rape_crt",
  "D.robbery_crt",
  ##punishment
  "D.impRate_corrected_estimated_25",
  "D.jailRate_corrected_estimated_25",
  ##policing
  "D.officers",
  "D.employees",
  ##spending
  "D.rev",
  #total
  "D.spend",
  "D.policespend",
  "D.jailspend",
  "D.courtspend",
  "D.eduspend",
  "D.welfspend",
  "D.healthspend"#,
  # #direct
  # "D.spend_d",
  # "D.policespend_d",
  # "D.jailspend_d",
  # "D.courtspend_d",
  # "D.eduspend_d",
  # "D.welfspend_d",
  # "D.healthspend_d"
)
tmp<-(
  finaldf$dv%in%tmpvars |
    finaldf$prefmods 
) & finaldf$endogenous=="emptopopc_china" & 
  finaldf$var%in%c("D.emptopopc","otch") 
tmp<- tmp & 
  #limit the crime vars to exclude the non-log specification
  !(str_detect(finaldf$dv,"crt") & (finaldf$logdv==F | finaldf$period=='stacked'))
tmp<- tmp & 
  #limit the spend variables to exclud non-log
  !(str_detect(finaldf$dv,"spend") & finaldf$logdv==F)
plotdf<-finaldf[tmp,]

#examine spending on welfare, etc.
tmp<-str_detect(finaldf$dv,'welfspend|healthspend') &
  finaldf$stage=='secondstage' &
  finaldf$endogenous=="emptopopc_china" & 
  finaldf$var%in%c("D.emptopopc","otch") 
finaldf[tmp,c('dv','logdv','mu.sd','mu.sd.min','mu.sd.max','pval','pval.shp')]


#sort into facets
plotdf$facet<-"Crime"
tmp<-str_detect(plotdf$dv,"Rate")
plotdf$facet[tmp]<-"Punishment"
tmp<-plotdf$dv%in%c("D.officers","D.employees")
plotdf$facet[tmp]<-"Policing"
tmp<-plotdf$dv%in%c(
  ##spending
  "D.rev",
  #total
  "D.spend",
  "D.policespend",
  "D.jailspend",
  "D.courtspend",
  "D.eduspend",
  "D.welfspend",
  "D.healthspend",
  #direct
  "D.spend_d",
  "D.policespend_d",
  "D.jailspend_d",
  "D.courtspend_d",
  "D.eduspend_d",
  "D.welfspend_d",
  "D.healthspend_d"
)
plotdf$facet[tmp]<-"Spending"
tmplevels<-c(
  "Crime",
  "Policing",
  "Spending",
  "Punishment"
)
plotdf$facet <- factor(
  plotdf$facet,
  tmplevels
)

tmplevels<-c(
  ###crime
  "D.property_crt",
  #"D.pcrt_fs",   
  "D.burglry_crt",
  "D.mvtheft_crt",
  "D.arson_crt",
  "D.violent_crt",
  #"D.vcrt_fs",
  "D.murder_crt",
  "D.rape_crt",
  "D.robbery_crt",
  ##others
  "D.incRate_corrected_estimated_25",
  "D.impRate_corrected_estimated_25",
  "D.jailRate_corrected_estimated_25",
  ###
  "D.officers",
  "D.employees",
  ##spending
  "D.rev",
  #total
  "D.spend",
  "D.policespend",
  "D.jailspend",
  "D.courtspend",
  "D.eduspend",
  "D.welfspend",
  "D.healthspend",
  #direct
  "D.spend_d",
  "D.policespend_d",
  "D.jailspend_d",
  "D.courtspend_d",
  "D.eduspend_d",
  "D.welfspend_d",
  "D.healthspend_d"
) %>% rev
tmplabels<-c(
  "Property Crime",
  #"Property Crime (FS)",
  "Auto Theft",
  "Burglary",
  "Arson",  
  "Violent Crime",
  #"Violent Crime (FS)",
  "Murder",
  "Rape",
  "Robbery",
  ####
  "Incarceration",
  "Prison",
  "Jail",
  ###
  "Police Officers",
  "Police Employees",
  ###
  "Local Revenue",
  "Local Spending",
  "Police Spending",
  "Jail Spending",
  "Court Spending",
  "Education Spending",
  "Welfare Spending",
  "Health Spending",
  ###
  "Local Spending (Direct)",
  "Police Spending (Direct)",
  "Jail Spending (Direct)",
  "Court Spending (Direct)",
  "Education Spending (Direct)",
  "Welfare Spending (Direct)",
  "Health Spending (Direct)"
) %>% rev
plotdf$dv<-factor(
  plotdf$dv,
  tmplevels,
  tmplabels
)

tmplevels<-c(
  "reducedform",
  "firststage",
  "secondstage"
)
tmplabels<-c(
  "Reduced Form",
  "First Stage",
  "Second Stage"
)
plotdf$stage<-factor(
  plotdf$stage,
  tmplevels,
  tmplabels
)

plotdf[,c('dv','N','N.czs')] %>% unique

g.tmp<-ggplot(
  plotdf[plotdf$stage%in%c('Second Stage'),],
  aes(
    x=dv,
    y=mu.sd,
    ymin=mu.sd.min,
    ymax=mu.sd.max,
    shape=pval.shp
  )
) +
  geom_point(
    size=2
  ) +
  geom_errorbar(
    linewidth=0.4,
    width=0.2
  ) +
  geom_hline(
    yintercept=0,
    linetype='dashed',
    color='black'
  ) +
  scale_y_continuous(labels = no_zeros) +
  scale_shape_manual(
    name="",
    values=tmpshapes,
    labels=shp.labels,
    drop=F
  ) +
  scale_color_discrete(
    name=""
  ) +
  ylab("\nStandarized Estimate") +
  xlab("") +
  coord_flip() +
  facet_grid(
    facet ~ stage,
    scales='free',
    space='free_y'
  ) +
  theme_bw() +
  theme(
    text=element_text(family='serif')
  )
 
setwd(outputdir)
tmpname<-"fig_otherdvs.pdf"
ggsave(
  g.tmp,
  filename=tmpname,
  width=5,
  height=8
)
output(plotdf,tmpname,g.tmp)

#########################################################
#########################################################

#RACE-BASED INCARCERATION RATES

tmp<-str_detect(
  finaldf$dv,
  'white|black|aapi|native|latinx'
) & !str_detect(finaldf$dv,'estimated0')

tmp<-tmp & 
  finaldf$endogenous=="emptopopc_china" & 
  finaldf$var%in%c("D.emptopopc","otch") &
  (finaldf$stage=='secondstage' & !is.na(finaldf$stage))
plotdf<-finaldf[tmp,]


#get the race and institution from the name
plotdf$race <- str_extract(
  plotdf$dv,
  'white|black|aapi|native|latinx'
)
tmplevels<-c(
  'white',
         'black',
         'latinx',
         'native',
         'aapi'
) %>% rev
tmplabels<-c(
  'White',
  'Black',
  'Hispanic',
  'Native',
  'AAPI'
) %>% rev
plotdf$race <- factor(plotdf$race,tmplevels,tmplabels)
plotdf$institution <- str_extract(
  plotdf$dv,
  'impRate|incRate|jailRate'
)
tmplevels<-c(
  'incRate','impRate','jailRate'
)
tmplabels<-c(
  'Incarceration','Prison','Jail'
)
plotdf$institution <- factor(plotdf$institution,tmplevels,tmplabels)

#imputation
plotdf$threshold <- str_extract(
  plotdf$dv,
  "25|75"
)
plotdf$threshold <- paste0("<=",plotdf$threshold,'%')

#add the main estimates
tmp<-(
  finaldf$dv%in%c(
    'D.impRate_corrected_estimated_25',
    'D.jailRate_corrected_estimated_25',
    #'D.incRate_corrected_estimated_25',
    'D.impRate_corrected_estimated_75',
    'D.jailRate_corrected_estimated_75',
    'D.incRate_corrected_estimated_75'
  ) | finaldf$prefmods
) & finaldf$endogenous=="emptopopc_china" & 
  finaldf$var%in%c("D.emptopopc")
extradf<-finaldf[tmp,]
extradf$threshold <- str_extract(
  extradf$dv,
  "25|75"
)
extradf$threshold <- paste0("<=",extradf$threshold,'%')
extradf$institution <- str_extract(
  extradf$dv,
  'impRate|incRate|jailRate'
)
tmplevels<-c(
  'incRate','impRate','jailRate'
)
tmplabels<-c(
  'Incarceration','Prison','Jail'
)
extradf$institution <- factor(extradf$institution,tmplevels,tmplabels)

#limit threshold
plotdf<-plotdf[plotdf$threshold=="<=75%",]
extradf<-extradf[extradf$threshold=="<=75%",]

Ndf<-plotdf[,c('race','threshold','institution','N','N.czs')]
Ndf[order(Ndf$N),]

g.tmp<-ggplot(
  plotdf,
  aes(
    x=race,
    y=mu.sd,
    ymin=mu.sd.min,
    ymax=mu.sd.max,
    shape=pval.shp
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
  geom_hline(
    data=extradf,
    aes(yintercept=mu.sd),
    linetype='dashed',
    color='red'
  ) +
  scale_shape_manual(
    name="",
    values=tmpshapes,
    labels=shp.labels,
    drop=F
  ) +
  scale_color_discrete(
    name=""
  ) +
  scale_y_continuous(labels = no_zeros) +
  ylab("\nStandarized Estimate") +
  xlab("") +
  coord_flip() +
  facet_grid(
    . ~ institution,
    #threshold ~ institution,
    scales='free_y',
    space='free_y'
  ) +
  theme_bw() +
  theme(
    text = element_text(family='serif')
  )

setwd(outputdir)
tmpname<-"fig_effectsbyrace.pdf"
ggsave(
  g.tmp,
  filename=tmpname,
  width=9,
  height=4.5/1.5
)
output(plotdf,tmpname,g.tmp)

#what are the results in the 25 case?
tmp<-str_detect(
  finaldf$dv,
  'white|black|aapi|native|latinx'
) &
  finaldf$endogenous=="emptopopc_china" & 
  finaldf$var%in%c("D.emptopopc","otch") &
  finaldf$stage=='secondstage' &
  !str_detect(finaldf$dv,'estimated0') & 
  str_detect(finaldf$dv,'25$')
plotdf<-finaldf[tmp,]
plotdf<-plotdf[,c('dv','mu.sd','pval.class')]
plotdf[order(plotdf$mu.sd),]

#these are the results where adding 0.1 makes a big difference
#in other words, native and aapi results are not robust to zero coding
#so we will not include these.. 
tmp<-str_detect(
  finaldf$dv,
  'white|black|aapi|native|latinx'
) &
  finaldf$endogenous=="emptopopc_china" & 
  finaldf$var%in%c("D.emptopopc","otch") &
  finaldf$stage=='secondstage' &
  str_detect(finaldf$dv,'estimated0') & 
  str_detect(finaldf$dv,'75$')
plotdf<-finaldf[tmp,]
# plotdf$zero<-ifelse(str_detect(plotdf$dv,'estimated0'),'smallval','zero')
# plotdf$dv <- str_replace(plotdf$dv,'estimated0','estimated')
plotdf<-plotdf[,c('dv','mu.sd','pval.class')]
plotdf[order(plotdf$mu.sd),]

#what are the results when we instrument for black/white emptopop directly? 
tmp<-str_detect(
  finaldf$dv,'black|white'
) &
  str_detect(
    finaldf$endogenous,'black|white'
  ) & (
    str_detect(
      finaldf$var,'D.emptopopc'
    ) | finaldf$var=='otch'
  )
finaldf[tmp,c('endogenous','var','mu.sd','pval','pval.class','stage')]
diagsdf[diagsdf$i%in%unique(finaldf$i[tmp]) & diagsdf$test=='weak instruments',]

#########################################################
#########################################################

#ROBUSNTESS TO DV CHOICES

#estimated or discarded missing counties, given threshold
#corrected jail/inc populations for double-counting

tmp<-str_detect(finaldf$dv,"D.incRate") &
  !str_detect(finaldf$dv,'vera2002') & #not the old vera var
  !str_detect(finaldf$dv,'black|white|aapi|native|latinx') & #none of the race vars
  (
    finaldf$dv!="D.incRate_corrected_estimated_25" |
      finaldf$prefmods
  ) &
  finaldf$endogenous=="emptopopc_china" & 
  finaldf$var%in%c("otch","D.emptopopc") 
plotdf<-finaldf[tmp,]

#quick plot of how N changes as threshold changed
plotdf$corrected<-str_extract(
  plotdf$dv,
  "\\_corrected|\\_uncorrected"
)
tmplevels<-c(
  "_corrected",
  "_uncorrected"
)
tmplabels<-c(
  "Adjusted for Double-Counting",
  "Unadjusted for Double-Counting"
)
plotdf$corrected<-factor(
  plotdf$corrected,
  tmplevels,
  tmplabels
)

plotdf$estimated<-str_extract(
  plotdf$dv,
  "\\_estimated|\\_discarded"
)
tmplevels<-c(
  "_estimated",
  "_discarded"
)
tmplabels<-c(
  "Imputed NA's",
  "Discarded NA's"
)
plotdf$estimated<-factor(
  plotdf$estimated,
  tmplevels,
  tmplabels
)

plotdf$threshold<-100 - str_extract(
  plotdf$dv,
  "[0-9]{1,3}$"
) %>% as.numeric


tmplevels<-c(
  "reducedform",
  "firststage",
  "secondstage"
)
tmplabels<-c(
  "Reduced Form",
  "First Stage",
  "Second Stage"
)
plotdf$stage<-factor(
  plotdf$stage,
  tmplevels,
  tmplabels
)

tmplevels<-c(
  T,
  F
)
tmplabels<-c(
  "Preferred",
  "Robustness"
)
plotdf$prefmods<-factor(
  plotdf$prefmods,
  tmplevels,
  tmplabels
)
tmpcolors<-c(
  "red",
       "black"
)
names(tmpcolors)<-tmplabels

g.tmp<-ggplot(
  plotdf,
  aes(
    x=threshold,
    y=mu.sd,
    ymin=mu.sd.min,
    ymax=mu.sd.max,
    shape=pval.shp,
    color=prefmods
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
    name="",
    values=tmpcolors,
    drop=T
  ) +
  ylab("\nStandardized Estimate") +
  xlab("% of CZ Population With Data\n") +
  coord_flip() +
  facet_grid(
    stage ~ corrected + estimated
  ) +
  theme_bw()

tmpname<-"fig_datachoices.png"
ggsave(
  plot=g.tmp,
  filename=tmpname,
  width=10,
  height=6
)
output(plotdf,tmpname)


#also make a quick plot of how N.CZs changes
# tmpvars<-c(
#   "threshold",
#   "N.czs",
#   "corrected",
#   "estimated"
# )
# plotdf<-unique(plotdf[,tmpvars])
# 
# # ggplot(
# #   plotdf,
# #   aes(
# #     x=threshold,
# #     y=N.czs,
# #     label=N.czs
# #   )
# # ) +
# #   geom_bar(stat='identity') +
# #   geom_label() +
# #   facet_grid(
# #     estimated ~ corrected
# #   )


#########################################################
#########################################################

#ROBUSTNESS TO MODELING, SAMPLE, PERIOD

mostprefs<-c('emptopopc_china')#,'emptopopc_china')
prefvars<-c('D.emptopopc')#,'D.emptopopc')

tmp<-robdf$endogenous%in%mostprefs &
  robdf$dv=="D.incRate_corrected_estimated_25" &
  #don't need these, they're for other figs
  !(robdf$instrumented=="ols") 
tmpdf<-robdf[tmp,]
tmpdf$endogenous<-NULL
tmpdf<-unique(tmpdf)

#write out and get info
setwd(metadir)
write.csv(
  tmpdf,
  "robmodsdf_EDIT.csv",
  row.names=F
)

#the way this plot will work is that
#it will have columns and multiple rows
#first column plots the 1s estimate
#second column plots the rf estimate
#third column plots the 2sls estimate w/ correcet sampling error
#all standardized. 

tmpdf<-read.csv(
  'robmodsdf_EDITED.csv',
  stringsAsFactors=F
)

plotdf<-merge(
  finaldf,
  tmpdf
)
tmp<-plotdf$var%in%prefvars
plotdf<-plotdf[tmp,]

#get rf/1s estimates
tmp<-finaldf$i%in%plotdf$i & 
  finaldf$stage%in%c('reducedform','firststage') &
  !is.na(finaldf$stage) &
  finaldf$var=='otch'
tmpcols<-c(
  'i',
  'stage',
  'pval',
  'pval.shp',
  'mu.sd',
  'mu.sd.min',
  'mu.sd.max',
  'N.czs',
  'N'
)
tmpdf<-finaldf[tmp,tmpcols]
tmpdf<-merge(
  tmpdf,
  plotdf[,c('i','propername','group','order','endogenous')],
  by='i'
)
plotdf<-rbind.fill(
  tmpdf,
  plotdf
)

#order the propernames
tmplevels<-unique(plotdf$propername[order(-plotdf$order)])
plotdf$propername<-factor(
  plotdf$propername,
  tmplevels
)

#order the stages
tmplevels<-c(
  "reducedform",
  "firststage",
  "secondstage"
)
tmplabels<-c(
  "Reduced Form",
  "First Stage",
  "Second Stage"
)
plotdf$stage<-factor(
  plotdf$stage,
  tmplevels,
  tmplabels
)

# #sort out the endogenous
# tmplevels<-c(
#   "unemp_china",
#   "manuf_china",
#   "emptopop_china",
#   "emptopopc_china"
# ) %>% rev
# tmplabels<-c(
#   "+ Unemployment Rate",
#   "- Manufacturing Share",
#   "- Emp-to-Pop Ratio (CBP)",
#   "- Emp-to-Pop Ratio (IPUMS)"
# ) %>% rev
# plotdf$endogenous<-factor(
#   plotdf$endogenous,
#   tmplevels,
#   tmplabels
# )

#get the underlying estimate from pref
tmp<-finaldf$var%in%c(prefvars,"otch") &
  finaldf$prefmods & 
  finaldf$endogenous%in%mostprefs
tmpdf<-finaldf[tmp,]
tmplevels<-c(
  "reducedform",
  "firststage",
  "secondstage"
)
tmplabels<-c(
  "Reduced Form",
  "First Stage",
  "Second Stage"
)
tmpdf$stage<-factor(
  tmpdf$stage,
  tmplevels,
  tmplabels
)
# tmplevels<-c(
#   "unemp_china",
#   "manuf_china",
#   "emptopop_china",
#   "emptopopc_china"
# ) %>% rev
# tmplabels<-c(
#   "+ Unemployment Rate",
#   "- Manufacturing Share",
#   "- Emp-to-Pop Ratio (CBP)",
#   "- Emp-to-Pop Ratio (IPUMS)"
# ) %>% rev
# tmpdf$endogenous<-factor(
#   tmpdf$endogenous,
#   tmplevels,
#   tmplabels
# )

#sample sizes
plotdf[,c('propername','N'),] %>% unique

g.tmp<-ggplot(
  plotdf,
  aes(
    x=propername,
    y=mu.sd,
    ymin=mu.sd.min,
    ymax=mu.sd.max,
    shape=pval.shp
  )
) +
  geom_hline(
    data=tmpdf,
    aes(
      yintercept=mu.sd
    )
    ,
    color='red',
    linetype='dashed',
    alpha=0.5
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
  scale_y_continuous(
    labels=no_zeros
  ) +
  facet_grid(
    group ~ stage,
    scales='free',
    space = 'free_y'
  ) +
  coord_flip() +
  xlab("") + 
  ylab("\nStandardized Estimate") +
  theme_bw() +
  theme(
    text = element_text(family='serif')
  )
g.tmp

setwd(outputdir)
tmpname<-"fig_robustness.pdf"
ggsave(
  plot=g.tmp,
  filename=tmpname,
  width=10,
  height=7
)
# gs.list[[tmpname]]<-list(
#   graph=g.tmp,
#   filename=tmpname,
#   width=10,
#   height=7
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
#     filename=thiselement$filename,
#     plot=thiselement$graph,
#     width=thiselement$width,
#     height=thiselement$height
#   )
#   Sys.sleep(0.5)
# }


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
# load("23_cfactuals_rates.RData") #this is too big
sumratedf<-read.csv(
  'sumratedf.csv',
  stringsAsFactors=F
)
sumstatsdf<-read.csv(
  'sumstatsdf.csv',
  stringsAsFactors=F
)
sumstatsdf2<-read.csv(
  'sumstatsdf2.csv',
  stringsAsFactors=F
)

#load a function to get prettynames
setwd(codedir); dir()
source('functions.R')
source('predict_ivreg.R')

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
  if( str_detect(tmpname,"\\.pdf$") ) 
    tmpname<-str_replace(tmpname,"\\.pdf$",".csv")
  
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

#PLOT RAW RATES
#in cfactual scenarios

tmp<-sumratedf$cz90=='average' & 
  sumratedf$cfactual%in%c(
    "observed",
    "predicted",
    #"stable",
    #"nodecline",
    #"lowptile",
    #"90sboom",
    "90spreserved"#,
    #"highptile"
  ) &
  sumratedf$endogenous%in%c("emptopopc_china")
plotdf<-sumratedf[tmp,] 


#edits
plotdf$year<-as.numeric(plotdf$year)

tmplevels<-c(
  "observed",
  "predicted",
  "stable",
  "nodecline",
  "highptile",
  "lowptile",
  "90sboom",
  "90spreserved"
)
tmplabels<-c(
  "Observed",
  "Predicted",
  "Stable",
  "No Decline",
  "As if 80th",
  "As if 20th",
  "As if 1990s",
  "90s Gains Preserved"
)
plotdf$cfactual<-factor(
  plotdf$cfactual,
  tmplevels,
  tmplabels
)

tmptypes<-c(1,2,3)
names(tmptypes)<-c(
  "90s Gains Preserved",
  "Predicted",
  "Observed"
)

tmplevels<-c(
  "unemp_china",
  "manuf_china",
  "emptopop_china",
  "emptopopc_china"
) %>% rev
tmplabels<-c(
  "+ Unemployment Rate",
  "- Manufacturing Share",
  "- Emp-to-Pop Ratio (CBP)",
  "- Emp-to-Pop Ratio (IPUMS)"
) %>% rev
plotdf$endogenous<-factor(
  plotdf$endogenous,
  tmplevels,
  tmplabels
)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=year,
    y=avg,
    group=cfactual,
    linetype=cfactual,
    shape=cfactual
  )
) +
  geom_line() +
  #geom_point() +
  facet_wrap(
    ~ endogenous,
    ncol=1
  ) +
  scale_linetype_manual(
    name="",
    values=tmptypes
  ) +
  xlab("") +
  ylab("Incarceration Rate \n") +
  theme_bw() +
  theme(
    legend.position = 'bottom'
  ) +
  theme(
    text = element_text(family='serif')
  )

setwd(outputdir)
tmpname<-"fig_cfactuals_rates.pdf"
ggsave(
  plot=g.tmp,
  filename=tmpname,
  width=8,
  height=6
)

# gs.list[[tmpname]]<-list(
#   graph=g.tmp,
#   filename=tmpname,
#   width=8,
#   height=6
# )
output(plotdf,tmpname)

#get actual rates; start, peak and end
plotdf<-data.table(plotdf)
plotdf[
  ,
  .(
    startrate=avg[year==1991],
    peakrate=max(avg),
    endrate=avg[year==2011]
  )
  ,
  by=c(
    'endogenous',
    'cfactual'
  )
]

#get change in rates
sumstatsdf

#########################################################
#########################################################

#PLOT CHG ACROSS SCENARIOS

# #i want 'increase averted' and not amount explained.. 
# tmp<-sumstatsdf$cz90=="average" & 
#   sumstatsdf$var=="pctexplained" &
#   sumstatsdf$cfactual%in%c(
#     #"stable",
#     #"nodecline",
#     #"lowptile",
#     #"90sboom",
#     "90spreserved"
#     #"highptile"
#   ) &
#   sumstatsdf$endogenous=="emptopop_china"
# plotdf<-sumstatsdf[tmp,]
# 
# 
# tmplevels<-c(
#   "observed",
#   "predicted",
#   "stable",
#   "nodecline",
#   "highptile",
#   "lowptile",
#   "90sboom",
#   "90spreserved"
# )
# tmplabels<-c(
#   "Observed",
#   "As Observed",
#   "Stable",
#   "No Decline",
#   "As if 80th",
#   "As if 20th",
#   "As if 1990s",
#   "90s Gains Preserved"
# )
# plotdf$cfactual<-factor(
#   plotdf$cfactual,
#   tmplevels,
#   tmplabels
# )
# 
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
# 
# 
# #add shape et al to plotdf
# #add pval info to shape of point
# plotdf$pval.shp<-NA
# plotdf$pval.shp[plotdf$pval.class=="at alpha=0.01"]<-1
# plotdf$pval.shp[plotdf$pval.class=="at alpha=0.05"]<-2
# plotdf$pval.shp[plotdf$pval.class=="at alpha=0.10"]<-3
# plotdf$pval.shp[plotdf$pval.class=="not sig"]<-4
# plotdf$pval.shp<-factor(
#   plotdf$pval.shp,
#   levels=c(1,2,3,4),
#   labels=c(
#     "at alpha=0.01",
#     "at alpha=0.05",
#     "at alpha=0.10",
#     "not sig"
#   )
# )
# #tmpshapes
# tmpshapes<-c(8,4,16,1)
# names(tmpshapes)<-levels(plotdf$pval.shp)
# shp.labels<-c(
#   bquote(alpha == 0.01),
#   bquote(alpha == 0.05),
#   bquote(alpha == 0.10)
# )
# 
# g.tmp<-ggplot(
#   plotdf,
#   aes(
#     x=cfactual,
#     y=mu,
#     ymin=mu.min,
#     ymax=mu.max,
#     shape=pval.shp
#   )
# ) + 
#   geom_point() +
#   geom_errorbar(
#     width=0.2
#   ) +
#   geom_hline(
#     yintercept=0,
#     linetype='dashed'
#   ) +
#   scale_shape_manual(
#     name="",
#     values=tmpshapes,
#     labels=shp.labels,
#     drop=F
#   ) + 
#   facet_wrap(
#     ~ endogenous,
#     ncol=1
#   ) +
#   coord_flip() +
#   xlab("") + 
#   ylab("\n% of Increase Explained") +
#   theme_bw()
# 
# tmpname<-"fig_cfactuals_stats.pdf"
# gs.list[[tmpname]]<-list(
#   graph=g.tmp,
#   filename=tmpname,
#   width=6/1.1,
#   height=8/1.1
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
#     filename="tmp.pdf",
#     plot=thiselement$graph,
#     width=thiselement$width,
#     height=thiselement$height
#   )
#   #embed font
#   embed_fonts(
#     file="tmp.pdf",
#     outfile=thiselement$filename
#   )
#   file.remove(
#     "tmp.pdf"
#   )
#   Sys.sleep(0.5)
# }
# 
# 




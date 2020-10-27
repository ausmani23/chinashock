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

#set dirs
rootdir<-find_root(
  criterion=has_file('china shock.Rproj')
)
codedir<-file.path(rootdir,"code")
setwd(codedir); dir()
source('dirs.R')
source('functions.R')

#########################################################
#########################################################

#LOAD DV'S

setwd(datadir) 

crimedf<-read.csv(
  "Dcrimedf.csv",
  stringsAsFactors=F
)
head(crimedf)

crimedf_fs<-read.csv(
  "Dcrimedf_fs.csv",
  stringsAsFactors=F
)
head(crimedf_fs)

incdf<-read.csv(
  'Dincdf.csv',
  stringsAsFactors=F
)
head(incdf)
#unneeded
incdf$incrate_end<-
  incdf$incrate_st<-NULL

veradf<-read.csv(
  'Dveradf.csv',
  stringsAsFactors=F
)
head(veradf)
#unneeeded 
veradf$rate_st<-
  veradf$rate_end<-NULL

prisjaildf<-read.csv(
  'Dprisjaildf.csv',
  stringsAsFactors=F
)
head(prisjaildf)

empdf<-read.csv(
  'Dempdf.csv',
  stringsAsFactors=F
) 
empdf2<-read.csv(
  'Dempdf2.csv',
  stringsAsFactors=F
)
#only emptopopc
tmp<-empdf2$dv%in%c("D.emptopop","D.emptopop_ln")
empdf2<-empdf2[tmp,]
empdf2$dv<-str_replace(empdf2$dv,"emptopop","emptopopc")

#unemp
unempdf<-read.csv(
  'Dunempdf.csv',
  stringsAsFactors=F
)
#these can be DVs
#or endogenous IV (in shockdf)

#new vera data (2/2019)
verarevdf<-read.csv(
  'Dverarevdf.csv',
  stringsAsFactors=F
)
head(verarevdf)

#even newer vera data (6/2019)
vera1906df<-read.csv(
  'Dvera_1906.csv',
  stringsAsFactors=F
)
head(vera1906df)

#doubly newer vera data (02/2020)
vera2002df<-read.csv(
  'Dvera_2002.csv',
  stringsAsFactors=F
)
head(vera2002df)

#add police data
policedf<-read.csv(
  'Dpolicedf.csv',
  stringsAsFactors = F
)
policespdf<-read.csv(
  'Dpolicespdf.csv',
  stringsAsFactors = F
)
financesdf<-read.csv(
  'Dfinancesdf.csv',
  stringsAsFactors=F
)
findf<-read.csv(
  'Dfindf.csv',
  stringsAsFactors=F
)
certdf<-read.csv(
  'Dcertaintydf.csv',
  stringsAsFactors=F
)


#put them together
dvdf<-rbind.fill(
  crimedf,
  crimedf_fs,
  incdf,
  veradf,
  vera1906df,
  vera2002df,
  prisjaildf,
  empdf,
  empdf2,
  unempdf,
  policedf,
  policespdf,
  financesdf,
  findf,
  certdf
)

#a note on finites
#especially when taking logs, 
#this generates Inf values
#here I convert all to NA
tmp<-!is.finite(dvdf$val) & !is.na(dvdf$val)
table(dvdf$dv[tmp]) %>% sort 
#bigger problem w/ crt's, 
#which is why I run these in raw
dvdf$val[tmp] <- NA

#a note on inflation factors.
#the two periods are not exactly equivalent. 
#1991-1999 spans 9 years, 1999 to 2011 spans 13 years

#if it is desired that both periods are on the same scale, 
#one can apply an inflation factor to each of these periods
#i include this in runregs.R rather than mess w/ it here

#########################################################
#########################################################

#LOAD INSTRUMENTS/ENDOGENOUS IVS

setwd(datadir); dir()
shockdf<-read.csv(
  'shockdf.csv',
  stringsAsFactors=F
)

#i also want shocklist to be accessible
shocklist<-readRDS('shocklist.RDS')

#########################################################
#########################################################

#LOAD START OF PERIOD COVARS

#add population info 
#for yrs 1991 and 1999
setwd(prelimdir); dir()
popdf<-read.csv(
  "czone_pop_1990_2012.csv",
  stringsAsFactors=F
)

names(popdf)<-c(
  "year",
  "cz90",
  "population",
  "wagepop"
)

tmp<-popdf$year%in%c(1991,1999)
popdf<-popdf[tmp,]
covarsdf<-popdf

#add region info, via state
setwd(cwdir)
source('gencrosswalks.R')

#harmonize cz90's
crosswalks$cz90_region$cz90<-
  crosswalks$cz90_region$cz90 %>% 
  as.character %>%
  as.numeric

covarsdf<-merge(
  covarsdf,
  crosswalks$cz90_region,
  by=c(
    "cz90"
  ),
  all=T
)

#######

#get pct black 
setwd(prelimdir); dir()
tmpdf<-read.csv(
  'blackpop_1990s.csv',
  stringsAsFactors=F
)
covarsdf<-merge(
  covarsdf,
  tmpdf,
  by=c(
    "cz90",
    "year"
  ),
  all=T
)

######

#add other start of period ivs
#emptopop
setwd(prelimdir); dir()
tmpdf<-read.csv(
  'emptopop_czdf.csv',
  stringsAsFactors=F
)
tmp<-tmpdf$year%in%c(1991,1999)
tmpdf<-tmpdf[tmp,]

covarsdf<-merge(
  covarsdf,
  tmpdf,
  by=c(
    "cz90",
    "year"
  ),
  all=T
)

#emptopopc
setwd(prelimdir); dir()
tmpdf<-read.csv(
  'emptopops_census.csv',
  stringsAsFactors=F
)
tmp<-tmpdf$year%in%c(1990,2000)
tmpcols<-c(
  "czone",
  "year",
  "emptopop"
)
tmpdf<-tmpdf[tmp,tmpcols]
names(tmpdf)[names(tmpdf)=="czone"]<-"cz90"
names(tmpdf)[names(tmpdf)=="emptopop"]<-"emptopopc"
tmpdf$year[tmpdf$year==1990]<-1991
tmpdf$year[tmpdf$year==2000]<-1999
covarsdf<-merge(
  covarsdf,
  tmpdf,
  by=c(
    "cz90",
    "year"
  ),
  all=T
)

#check cor in levels
cor(
  covarsdf$emptopop,
  covarsdf$emptopopc,
  use = 'complete.obs'
) #0.6 im levels

#unemployment rate
setwd(prelimdir); dir()
tmpdf<-read.csv(
  'unemprates_czdf.csv',
  stringsAsFactors=F
)
tmp<-tmpdf$year%in%c(1991,1999)
tmpdf<-tmpdf[tmp,]
covarsdf<-merge(
  covarsdf,
  tmpdf,
  by=c(
    "cz90",
    "year"
  ),
  all=T
)

#####

#add other controls
#taken from feler_senses,
#who get it from Autor..
tmpdir<-file.path(
  prelimdir,
  'feler_senses',
  '20150578R1Data'
)
setwd(tmpdir); dir()
tmpdf<-read.csv(
  'workfile_china.csv',
  stringsAsFactors=F
)
keyvars<-c(
  "czone",
  "yr",
  "l_shind_manuf_cbp",
  "l_sh_popedu_c",
  "l_sh_popfborn",
  "l_sh_empl_f",
  "l_sh_routine33",
  "l_task_outsource"
)
tmpdf<-tmpdf[,keyvars]
names(tmpdf)[1:2]<-c("cz90","year")
tmp<-names(tmpdf)=="l_shind_manuf_cbp"
names(tmpdf)[tmp]<-"manushare_fs"

#1990 and 2000 are our covars
#for 1991 and 2000, so fix to merge
tmpdf$year[tmpdf$year==1990]<-1991
tmpdf$year[tmpdf$year==2000]<-1999

covarsdf<-merge(
  covarsdf,
  tmpdf,
  by=c(
    "cz90",
    "year"
  ),
  all=T
)

#the two manufshares correlated at 0.98,
#and they're even not drawn from identical years..
cor(
  covarsdf$manushare,
  covarsdf$manushare_fs,
  use='complete.obs'
)

######

###this is the 02/2020 update
setwd(datadir); dir()
tmpdf<-read.csv(
  'vera_2002.csv',
  stringsAsFactors=F
)
tmp<-tmpdf$year%in%c(1991,1999)
tmpdf<-tmpdf[tmp,]

covarsdf<-merge(
  covarsdf,
  tmpdf,
  by=c(
    "cz90",
    "year"
  ),
  all=T
)

#generate log versions of all the thresh vars
tmp<-!names(tmpdf)%in%c("cz90","year")
threshvars<-names(tmpdf)[tmp]
for(v in threshvars) {
  newname<-paste0(v,"_ln")
  covarsdf[[newname]]<-log(covarsdf[[v]])
}

########

##UCR crime
setwd(datadir); dir()
tmpdf<-read.csv(
  'crimedf.csv',
  stringsAsFactors=F
)
tmp<-tmpdf$year%in%c(1994,1999)
tmpdf<-tmpdf[tmp,]
tmpdf$year[tmpdf$year==1994]<-1991
tmpdf$popcrime<-NULL

#merge and add log vars
covarsdf<-merge(
  covarsdf,
  tmpdf,
  by=c(
    "cz90",
    "year"
  ),
  all=T
)
tmp<-!names(tmpdf)%in%c("cz90","year")
spvars<-names(tmpdf)[tmp]
for(v in spvars) {
  newname<-paste0(v,"_ln")
  covarsdf[[newname]]<-log(covarsdf[[v]])
}
  
########

###FS crime
tmpdir<-file.path(
  prelimdir,
  "feler_senses",
  "20150578R1Data"
)
setwd(tmpdir); dir()
tmpdf<-read.csv(
  'PublicFinancesGoods.csv',
  stringsAsFactors=F
)
keepvars<-c(
  "czone",
  "year",
  "crm_violent_pc1000",
  "crm_prop_pc1000"
)
tmprows<-(
  !is.na(tmpdf$crm_violent_pc1000) |
  !is.na(tmpdf$crm_prop_pc1000)
) &
  tmpdf$year%in%c(1990,2000)
tmpdf<-tmpdf[tmprows,keepvars]
names(tmpdf)<-c(
  "cz90",
  "year",
  "vcrt_fs",
  "pcrt_fs"
)
tmpdf$year[tmpdf$year==1990]<-1991
tmpdf$year[tmpdf$year==2000]<-1999

#merge and add log vars
covarsdf<-merge(
  covarsdf,
  tmpdf,
  by=c(
    "cz90",
    "year"
  ),
  all=T
)
tmp<-!names(tmpdf)%in%c("cz90","year")
spvars<-names(tmpdf)[tmp]
for(v in spvars) {
  newname<-paste0(v,"_ln")
  covarsdf[[newname]]<-log(covarsdf[[v]])
}

###policing
setwd(datadir); dir()
tmpdf<-read.csv(
  'policedf.csv',
  stringsAsFactors=F
)
tmpdf<-spread(
  tmpdf,
  var,
  val
)
tmp<-!names(tmpdf)%in%c("cz90","year")
spvars<-names(tmpdf)[tmp]
for(v in spvars) {
  newname<-paste0(v,"_ln")
  tmpdf[[newname]]<-log(tmpdf[[v]])
}
tmp<-tmpdf$year%in%c(1991,1999)
tmpdf<-tmpdf[tmp,]
covarsdf<-merge(
  covarsdf,
  tmpdf,
  by=c(
    "cz90",
    "year"
  ),
  all=T
)

###police spendfin
setwd(datadir); dir()
tmpdf<-read.csv(
  'policespdf.csv',
  stringsAsFactors=F
)
tmpdf<-spread(
  tmpdf,
  var,
  val
)
tmp<-!names(tmpdf)%in%c("cz90","year")
spvars<-names(tmpdf)[tmp]
for(v in spvars) {
  newname<-paste0(v,"_ln")
  tmpdf[[newname]]<-log(tmpdf[[v]])
}
tmp<-tmpdf$year%in%c(1991,1999)
tmpdf<-tmpdf[tmp,]
covarsdf<-merge(
  covarsdf,
  tmpdf,
  by=c(
    "cz90",
    "year"
  ),
  all=T
)


###govt finances, feler and senses
setwd(datadir); dir()
tmpdf<-read.csv(
  'financesdf.csv',
  stringsAsFactors=F
)
tmpdf<-spread(
  tmpdf,
  var,
  val
)
tmp<-!names(tmpdf)%in%c("cz90","year")
spvars<-names(tmpdf)[tmp]
for(v in spvars) {
  newname<-paste0(v,"_ln")
  tmpdf[[newname]]<-log(tmpdf[[v]])
}
tmp<-tmpdf$year%in%c(1990,2000)
tmpdf<-tmpdf[tmp,]
tmpdf$year[tmpdf$year==1990]<-1991
tmpdf$year[tmpdf$year==2000]<-1999
covarsdf<-merge(
  covarsdf,
  tmpdf,
  by=c(
    "cz90",
    "year"
  ),
  all=T
)

###govt finances, our own data
setwd(datadir); dir()
tmpdf<-read.csv(
  'findf.csv',
  stringsAsFactors=F
)
tmpdf<-spread(
  tmpdf,
  var,
  val
)
tmp<-!names(tmpdf)%in%c("cz90","year")
spvars<-names(tmpdf)[tmp]
for(v in spvars) {
  newname<-paste0(v,"_ln")
  tmpdf[[newname]]<-log(tmpdf[[v]])
}
tmp<-tmpdf$year%in%c(1991,1999)
tmpdf<-tmpdf[tmp,]
covarsdf<-merge(
  covarsdf,
  tmpdf,
  by=c(
    "cz90",
    "year"
  ),
  all=T
)
covarsdf$cz90 %>% table %>% unique

###certainty/severity
setwd(datadir); dir()
tmpdf<-read.csv(
  'certaintydf.csv',
  stringsAsFactors=F
)
tmpdf<-spread(
  tmpdf,
  var,
  val
)
tmp<-!names(tmpdf)%in%c("cz90","year")
spvars<-names(tmpdf)[tmp]
for(v in spvars) {
  newname<-paste0(v,"_ln")
  tmpdf[[newname]]<-log(tmpdf[[v]])
}
tmp<-tmpdf$year%in%c(1991,1999)
tmpdf<-tmpdf[tmp,]
covarsdf<-merge(
  covarsdf,
  tmpdf,
  by=c(
    "cz90",
    "year"
  ),
  all=T
)

########################################################
#########################################################

#save out
setwd(datadir)
save.image(
  '02_prepped.RData'
)

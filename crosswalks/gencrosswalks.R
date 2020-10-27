#CROSSWALKS
#load crosswalks for countyid to commuting zone
mydir<-getwd()
crosswalks<-list()

###CZ90
#conversion to CZ90
#load the CZ90 file
setwd(mydir); dir()
tmpdf<-read.csv(
  'czlma903.csv',
  colClasses = "character"
)
head(tmpdf)
names(tmpdf)<-c(
  "county_fips",
  "cz90",
  "countyname",
  "distance",
  "pop90",
  "cz80",
  "beale_code",
  "msa93",
  "msaname",
  "stateplace_code",
  "largestplace"
)

#is each row unique?
tmptab<-table(tmpdf$county_fips)
tmp<-sum(tmptab>1)
if(tmp!=0)
  stop('not unique')

#get weights; although:
#given that they're unique,
#this exercise is not very important..
tmpdf$pop90<-as.numeric(tmpdf$pop90)
#collapse by county_fips
roworder<-order(
  tmpdf$county_fips
)
tmpdf<-tmpdf[roworder,]
tmpdf$sh_fips_cz90<-by(tmpdf,tmpdf$county_fips,function(df) {
  #df<-tmpdf[tmpdf$county_fips==unique(tmpdf$county_fips)[1],]
  df$pop90/sum(df$pop90)
}) %>% unlist
#collapse by cz90
roworder<-order(
  tmpdf$cz90
)
tmpdf<-tmpdf[roworder,]
tmpdf$sh_cz90_fips<-by(tmpdf,tmpdf$cz90,function(df) {
  #df<-tmpdf[tmpdf$cz90==unique(tmpdf$cz90)[1],]
  df$pop90/sum(df$pop90)
}) %>% unlist
#now, identify
crosswalks[["fips_cz90"]]<-tmpdf

#############

###CZ00
#conversion to CZ00
#load the CZ00 file
tmpdf<-read.csv(
  'cz00_eqv_v1.csv',
  colClasses = "character"
)
head(tmpdf)
names(tmpdf)<-c(
  "county_fips",
  "cz00",
  "cz90",
  "cz80",
  "countyname",
  "msa03",
  "pop00",
  "czpop00"
)

#is each row unique?
tmptab<-table(tmpdf$county_fips)
tmp<-sum(tmptab>1)
if(tmp!=0)
  stop('not unique')

#get weights; although:
#given that they're unique,
#this exercise is not very important..
tmpdf$pop00<-str_replace_all(
  tmpdf$pop00,",",""
) %>% as.numeric
#collapse by county_fips
roworder<-order(
  tmpdf$county_fips
)
tmpdf<-tmpdf[roworder,]
tmpdf$sh_fips_cz00<-by(tmpdf,tmpdf$county_fips,function(df) {
  #df<-tmpdf[tmpdf$county_fips==unique(tmpdf$county_fips)[1],]
  df$pop00/sum(df$pop00)
}) %>% unlist
#collapse by cz90
roworder<-order(
  tmpdf$cz00
)
tmpdf<-tmpdf[roworder,]
tmpdf$sh_cz00_fips<-by(tmpdf,tmpdf$cz90,function(df) {
  #df<-tmpdf[tmpdf$cz00==unique(tmpdf$cz00)[1],]
  df$pop00/sum(df$pop00)
}) %>% unlist
#add to list
crosswalks[["fips_cz00"]]<-tmpdf

#############

#CZ90 to state_fips to region
#assign each cz to state w/ most pop
tmpdf<-crosswalks$fips_cz90
tmpdf<-by(tmpdf,tmpdf$cz90,function(df) {
  #df<-tmpdf[tmpdf$cz90=="35500",]
  df$state_fips<-str_extract(
    df$county_fips,
    "^[0-9]{2}"
  )
  tmpsum<-tapply(
    df$sh_cz90_fips,
    df$state_fips,
    sum
  )
  sfips<-names(which(tmpsum==max(tmpsum)))
  #return
  returndf<-data.frame(
    cz90=unique(df$cz90),
    state_fips=sfips,
    stringsAsFactors=F
  )
  returndf
}) %>% rbind.fill

crosswalks[["cz90_sfips"]]<-tmpdf

#############

#state_fips to region
dir()
tmpdf<-read.csv(
  'state-geocodes-v2011.csv',
  skip=5,
  colClasses = "character"
)
names(tmpdf)<-c(
  "region",
  "division",
  "state_fips",
  "statename"
)
tmp<-tmpdf$state_fips!="00"
tmpdf<-tmpdf[tmp,]
crosswalks[["fips_region"]]<-tmpdf

#############

#cz90 to region
#put these together
tmpdf<-merge(
  crosswalks$cz90_sfips,
  crosswalks$fips_region,
  all=T
)

tmpdf<-by(tmpdf,tmpdf$cz90,function(df) {
  #df<-tmpdf[tmpdf$cz90=="12701",]
  region<-unique(df$region)
  division<-unique(df$division)
  returndf<-data.frame(
    cz90=unique(df$cz90),
    state_fips=unique(df$state_fips),
    region=region,
    division=division,
    stringsAsFactors=F
  )
  if(nrow(returndf)>1) {
    #if there is more than 1
    print(unique(df$cz90))
    stop()
  }
  returndf
}) %>% rbind.fill
crosswalks[["cz90_region"]]<-tmpdf

#############

#state_fips to statename/state_alpha2
###CZ90
#conversion to CZ90
#load the CZ90 file
setwd(mydir); dir()
tmpdf<-read.csv(
  'statenames.csv',
  stringsAsFactors=F
)
crosswalks[['sfips_salpha2']]<-tmpdf

#############

#get alterantive bignames

#better names available from IPUMS
require(rvest)
tmpurl <- 'https://usa.ipums.org/usa/volii/1990lma.shtml'
raw <- tmpurl %>%
  read_html %>%
  html_nodes("table") %>%
  html_table(header=F)
tmpdf<-raw[[1]]

#all we need are the lines with 
#lma and the one after
tmpdf$group <- str_detect(
  tmpdf$X1,
  '^Labor(\\s)?Market'
) %>% as.numeric %>% cumsum
newdf <- by(tmpdf,tmpdf$group,function(df) {
  #df<-tmpdf[tmpdf$group==3,]
  czs <- df[5:(nrow(df))-1,1] %>% unique
  bigname <- df[2,1]
  data.frame(
    cz90 = czs,
    bigname = bigname,
    stringsAsFactors=F
  )
}) %>% rbind.fill
crosswalks[['cz90_bigname']] <- newdf


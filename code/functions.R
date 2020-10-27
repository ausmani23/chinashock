olddir<-getwd()

######################################################
######################################################

#load varsdf
setwd(metadir); dir()
prettyvarsdf<-read.csv(
  'prettyvars.csv',
  stringsAsFactors=F
)

getvarname<-function(x) {
  #x<-"D.violent_crt_ln"
  ###
  #fix for ln
  suffix<-str_extract(
    x,
    "\\_ln$"
  )
  x<-str_replace(
    x,
    "\\_ln$",
    ""
  )
  if(!is.na(suffix)) {
    prettysuffix<-" (Ln)"
  } else {
    prettysuffix<- ""
  }
  ###
  tmp<-prettyvarsdf$var==x
  if(sum(tmp)==1) {
    y<-prettyvarsdf$prettyname[tmp]
    y<-paste0(y,prettysuffix)
  } else {
    y<-NA
  }
  ###
  y
}

getvarorder<-function(x) {
  x<-str_replace(
    x,
    "\\_ln$",
    ""
  )  
  tmp<-prettyvarsdf$var==x
  if(sum(tmp)==1) {
    y<-prettyvarsdf$order[tmp]
  } else {
    y<-NA
  }
  ###
  y
}

getvartype<-function(x) {
  x<-str_replace(
    x,
    "\\_ln$",
    ""
  )  
  tmp<-prettyvarsdf$var==x
  if(sum(tmp)==1) {
    y<-prettyvarsdf$type[tmp]
  } else {
    y<-NA
  }
  ###
  y
}


######################################################
######################################################

#load varsdf
setwd(metadir); dir()
prettyivsdf<-read.csv(
  'prettyendogenous.csv',
  stringsAsFactors=F
)

getivname<-function(x) {
  ###
  tmp<-prettyivsdf$endogenous==x
  if(sum(tmp)==1) {
    y<-prettyivsdf$prettyname[tmp]
  } else {
    y<-NA
  }
  ###
  y
}

getivorder<-function(x) {
  tmp<-prettyivsdf$endogenous==x
  if(sum(tmp)==1) {
    y<-prettyivsdf$order[tmp]
  } else {
    y<-NA
  }
  ###
  y
}

######################################################
######################################################

exportData<-function(g) {
  tmpdir<-file.path(
    outputdir,
    "plotdata"
  )
  setwd(tmpdir)
  tmpdata<-ggplot_build(g)$data
  newtmpname<-str_replace(
    tmpname,
    "\\.pdf",
    ""
  )
  i<-1
  for(tmp in tmpdata) {
    write.csv(
      tmp,
      paste0(
        newtmpname,
        "_",i,".csv"
      ),
      row.names=F
    )
    i<-i+1
  }
}

########################################################################
########################################################################

summarize.distribution2<-function(ests.distribution) {
  #ests.distribution<-lrm.distribution
  #ests.distribution <- rep(0,10)
  #get quantiles
  quantiles<-quantile(
    ests.distribution,
    c(
      0.01,
      0.025,
      0.05,
      0.5,
      0.95,
      0.975,
      0.99
    )
  )
  #return mu, mu.min, mu.max
  mu<-quantiles["50%"]
  mu.min<-quantiles["2.5%"]
  mu.max<-quantiles["97.5%"]
  #and also a pval classification
  if(mu>=0) {
    if(quantiles["1%"]>0) {
      pval.class<-'at alpha=0.01'
    } else if(quantiles["2.5%"]>0) {
      pval.class<-'at alpha=0.05'
    } else if(quantiles["5%"]>0) {
      pval.class<-'at alpha=0.10'
    } else {
      pval.class<-'not sig'
    }
  } else if(mu<0) {
    if(quantiles["99%"]<0) {
      pval.class<-'at alpha=0.01'
    } else if(quantiles["97.5%"]<0) {
      pval.class<-'at alpha=0.05'
    } else if(quantiles["95%"]<0) {
      pval.class<-'at alpha=0.10'
    } else {
      pval.class<-'not sig'
    }
  }
  # #se
  # #est of se explodes when lagdv coef is over 1
  # #so need something that is robust to that scenario
  # tmpboot<-boot(
  #   ests.distribution,
  #   f.sd,
  #   R=500
  # )
  # se<-mean(tmpboot$t)
  # se.q <- ( quantiles[3] - quantiles[1] ) / 4
  #SE is less rather than more helpful
  se<-NA 
  #se.q<-NA
  #get something like a two-sided pval test
  #pval<-ecdf(ests.distribution)(0)
  #pval<-ifelse(mu<0,(1-pval)*2,pval*2)
  pval<-NA
  #return me
  data.frame(
    mu,
    mu.min,
    mu.max,
    se=se,
    #se.q=se.q,
    pval=pval,
    pval.class=pval.class,
    stringsAsFactors=F
  )
}

########################################################################
########################################################################

#a function to add Latex stars to estimates, based on pval
#obviously, will return a string and not a number
apply.pvals<-function(ests,pvals,markers=c("**","*","+")) {
  if(class(ests)=="numeric")
    ests<-sprintf("%.3f",ests)
  ests[pvals<0.1 & pvals>=0.05]<-
    paste0(
      ests[pvals<0.1 & pvals>=0.05],
      paste0("\\textsuperscript{",markers[3],"}")
    )
  ests[pvals<0.05 & pvals>=0.01]<-
    paste0(
      ests[pvals<0.05 & pvals>=0.01],
      paste0("\\textsuperscript{",markers[2],"}")
    )  
  ests[pvals<0.01]<-
    paste0(
      ests[pvals<0.01],
      paste0("\\textsuperscript{",markers[1],"}")
    )
  return(ests)
}

#test
apply.pvals(ests=rnorm(30),pvals=rep(c(0.11,0.06,0.01),10))
apply.pvals(ests=sprintf("%.2f",rnorm(30)),pvals=rep(c(0.11,0.06,0.01),10))

#get 
get.pvals.class<-function(pvals) {
  y<-NA
  y[pvals<0.01]<-"at alpha=0.01"
  y[pvals>0.01 & pvals<0.05]<-"at alpha=0.05"
  y[pvals>0.05 & pvals<0.10]<-"at alpha=0.10"
  y[pvals>0.10]<-"not sig"
  return(y)
}

#this function uses class rather than numeric pval
apply.pvals.class<-function(ests,pvals.class,markers=c("**","*","+")) {
  if(class(ests)=="numeric")
    ests<-sprintf("%.3f",ests)
  ests[pvals.class=="at alpha=0.10"]<-
    paste0(
      ests[pvals.class=="at alpha=0.10"],
      paste0("\\textsuperscript{",markers[3],"}")
    )
  ests[pvals.class=="at alpha=0.05"]<-
    paste0(
      ests[pvals.class=="at alpha=0.05"],
      paste0("\\textsuperscript{",markers[2],"}")
    )  
  ests[pvals.class=="at alpha=0.01"]<-
    paste0(
      ests[pvals.class=="at alpha=0.01"],
      paste0("\\textsuperscript{",markers[1],"}")
    )
  return(ests)
}

#this function takes an est, pval, and se, 
#and gives something for a regtable
gimmie.est<-function(mu,pval,se,nrow=4) {
  tmp1<-apply.pvals(mu,pval)
  tmp2<-paste0("(",sprintf("%.3f",se),")")
  matrix(c(tmp1,tmp2),nrow=nrow)
}

#this function takes an est, a pval class and CI
#and gives something for a regtable
#to be used with simulated long run multipliers
gimmie.est2<-function(mu,pval.class=NULL,mu.min,mu.max,nrow=4) {
  # mu<-estdf$mu
  # pval.class<-estdf$pval.class
  # mu.min<-estdf$mu.min
  # mu.max<-estdf$mu.max
  # nrow<-2
  if(length(mu)==0) {
    tmp1<-tmp2<-""
  } else {
    if(!is.null(pval.class)) {
      tmp1<-apply.pvals.class(mu,pval.class)
    } else {
      tmp1<-format(round(mu,2),2)
    }
    tmp2<-paste0(
      "[",
      format(round(mu.min,2),2),
      ",",
      format(round(mu.max,2),2),
      "]"
    )
  }
  matrix(
    c(tmp1,tmp2),
    nrow=nrow
  )
}


########################################################################
########################################################################

summarize.distribution2<-function(ests.distribution) {
  #ests.distribution<-lrm.distribution
  #get quantiles
  quantiles<-quantile(
    ests.distribution,
    c(
      0.01,
      0.025,
      0.05,
      0.5,
      0.95,
      0.975,
      0.99
    )
  )
  #return mu, mu.min, mu.max
  mu<-quantiles["50%"]
  mu.min<-quantiles["2.5%"]
  mu.max<-quantiles["97.5%"]
  #and also a pval classification
  if(mu>0) {
    if(quantiles["1%"]>0) {
      pval.class<-'at alpha=0.01'
    } else if(quantiles["2.5%"]>0) {
      pval.class<-'at alpha=0.05'
    } else if(quantiles["5%"]>0) {
      pval.class<-'at alpha=0.10'
    } else {
      pval.class<-'not sig'
    }
  } else if(mu<0) {
    if(quantiles["99%"]<0) {
      pval.class<-'at alpha=0.01'
    } else if(quantiles["97.5%"]<0) {
      pval.class<-'at alpha=0.05'
    } else if(quantiles["95%"]<0) {
      pval.class<-'at alpha=0.10'
    } else {
      pval.class<-'not sig'
    }
  }
  # #se
  # #est of se explodes when lagdv coef is over 1
  # #so need something that is robust to that scenario
  # tmpboot<-boot(
  #   ests.distribution,
  #   f.sd,
  #   R=500
  # )
  # se<-mean(tmpboot$t)
  # se.q <- ( quantiles[3] - quantiles[1] ) / 4
  #SE is less rather than more helpful
  se<-NA 
  #se.q<-NA
  #get something like a two-sided pval test
  #pval<-ecdf(ests.distribution)(0)
  #pval<-ifelse(mu<0,(1-pval)*2,pval*2)
  pval<-NA
  #return me
  data.frame(
    mu,
    mu.min,
    mu.max,
    se=se,
    #se.q=se.q,
    pval=pval,
    pval.class=pval.class,
    stringsAsFactors=F
  )
}

#########################################################
#########################################################

#taken from https://www.r-bloggers.com/winsorization/

winsorize1 <- function (x, fraction=.05)
{
  if(length(fraction) != 1 || fraction < 0 ||
     fraction > 0.5) {
    stop("bad value for 'fraction'")
  }
  lim <- quantile(x, probs=c(fraction, 1-fraction))
  x[ x < lim[1] ] <- lim[1]
  x[ x > lim[2] ] <- lim[2]
  x
}

winsorize2<-function (x, multiple=3)
{
  if(length(multiple) != 1 || multiple <= 0) {
    stop("bad value for 'multiple'")
  }
  med <- median(x)
  y <- x - med
  sc <- mad(y, center=0) * 3
  y[ y > sc ] <- sc
  y[ y < -sc ] <- -sc
  y + med
}

#########################################################
#########################################################


setwd(olddir)
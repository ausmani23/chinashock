#this is a predict function for ivreg objects
#written to take a whole df, and return
#predictions in the same order in which they came

predict_ivreg <- function(
  object, 
  newdata, 
  mybeta=NULL
) 
{
  
  ###
  #DEBUGGING
  # object<-thismod$m
  # newdata<-thisdfs$second.alt
  # mybeta<-mybetas[l,]
  
  ###
  
  #check the model na's
  tmp<-attr( terms(object)$regressors , 'factors')
  tmpvars<-row.names(tmp) %>% 
    str_replace('factor\\((.*)\\)',"\\1") 
  tmp<-complete.cases(newdata[,tmpvars])
  if(sum(!tmp)>0)
    stop('You cant pass me NAs')
  
  ####
  #MODEL MATRIX
  
  #model matrix w/o response var wanted
  tt <- terms(object)$regressors #regressors
  if(is.null(tt))
    tt<-terms(object)
  Terms <- delete.response(tt)
  #get the xlevels from the object matrix
  obj.model<-object$model
  tmp<-names(object$model)
  tmp<-tmp[str_detect(tmp,"factor")]
  xlevels<-lapply(tmp,function(v) {
    levels(obj.model[,v])
  })
  names(xlevels)<-tmp
  #get the model frame
  m <- model.frame(
    formula=Terms, 
    newdata,
    xlev = xlevels
  )
  #make the model matrix
  X <- model.matrix(
    Terms, 
    m,
    contrasts.arg = 
      object$contrasts$regressors
  )
  
  ###
  #SET UP COMPUTATION
  
  #get coefs
  if(is.null(mybeta))
    mybeta <- coef(object)
  
  #make sure beta and X matrix 
  #are in the same order
  myX<-as.matrix(X[,])
  match.order<-match(
    colnames(myX),
    names(mybeta)
  )
  myX<-myX[,match.order]
  
  ###
  
  #get the cross product
  Yhat<-myX %*% mybeta
  Yhat
  
}



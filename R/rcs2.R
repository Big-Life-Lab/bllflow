
#Modified RCS function
#Modified to allow for naming of the new rcs variables
#Original rcs() function in github repo: harrelfe/rms/rms.trans.s 

#Requires Hmisc and rms package
library(Hmisc)
library(rms)

des.args <- function(x,parms.allowed,call.args) {
  nam <- names(x)
  if(! length(nam)) nam <- rep("",5)
  name <- nam[1]
  if(name=="") {
    form <- formula(call("~",as.name("...y..."),call.args[[2]]))
    name <- var.inner(form)
  }
  pa <- parms.allowed
  argu <- function(x,karg, arg.name, parms.all, nm)  {
    if(! parms.all) karg <- karg-1
    k <- charmatch(arg.name,nm,0)  #k>0 : named arg found
    ## Added karg <= length(x) 9Apr02 for R; R doesn't return NULL
    ## like S+
    if(k>0) x[[k]] else 
      if(length(nm) < karg || nm[karg] != "") NULL else
        if(karg <= length(x)) x[[karg]] else NULL
  }
  if(parms.allowed) parms <- argu(x,2,"parms",pa,nam) else {
    parms <- NULL
    if(charmatch("parms",nam,0)>0)
      stop(paste("parms not allowed for",as.character(call.args[1])))
  }
  
  nm <- argu(x,5,"name",pa,nam)
  if(length(nm)) name <- nm
  if(length(.Options$Design.attr)) {
    atr <- .Options$Design.attr
    i <- charmatch(name, atr$name, 0)
    if(! length(i))stop("program logic error for options(factor.number)")
    parmi <- atr$parms[[name]]
    return(list(name=atr$name[i],parms=parmi,label=atr$label[i],
                units=atr$units[i]))		# added units 9Jun99
  }
  
  label <- argu(x,3,"label",pa,nam)
  atx <- attributes(x[[1]])  # 9Jun99
  if(! length(label)) label <- atx$label   # 9Jun99 attr(x[[1]],"label")
  if(! length(label)) label <- name
  
  list(name=name,parms=parms,label=label,units=atx$units)  #9Jun99
  
}


## Function to list all attributes of new sub-design matrix
set.atr <- function(xd, x, z, colnames, assume, code, parms, nonlinear) {
  ##Note: x argument isn't used
  if(is.matrix(xd))
    list(dim=dim(xd),dimnames=list(NULL,colnames),class="rms",
         name=z$name, label=z$label, assume=assume, assume.code=code,
         parms=parms, 
         nonlinear=nonlinear,colnames=colnames,units=z$units)
  else list(dim=dim(xd), class="rms",
            name=z$name, label=z$label, assume=assume, assume.code=code,
            parms=parms, 
            nonlinear=nonlinear,colnames=colnames,units=z$units)
}


## Restricted cubic spline expansion
rcs2 <- function(..., rcs_names) {
  
  cal <- sys.call()
  xx  <- list(...)
  z   <- des.args(xx, TRUE, cal)
  x   <- xx[[1]]
  if(! is.numeric(x)) stop(paste(z$name, "is not numeric"))
  
  nknots <- .Options$nknots
  if(! length(nknots)) nknots <- 5
  
  parms <- z$parms
  if(! length(parms)) parms <- nknots
  
  if(length(parms)==1) {
    nknots <- parms
    knots <- NULL
    if(nknots == 0) {
      attributes(x) <- set.atr(x, x, z, z$name, "asis", 1, NULL, FALSE)
      return(x)
    }
  }
  else {
    nknots <- length(parms)
    knots <- parms
  }
  
  pc <- length(.Options$rcspc) && .Options$rcspc
  fractied <- .Options$fractied
  if(! length(fractied)) fractied <- 0.05
  
  if(! length(knots)) {
    xd <- rcspline.eval(x, nk=nknots, inclx=TRUE, pc=pc, fractied=fractied)
    knots <- attr(xd,"knots")
  }
  else xd <- rcspline.eval(x, knots=knots, inclx=TRUE, pc=pc, fractied=fractied)
  
  parms  <- knots
  nknots <- length(parms)
  nam    <- z$name
  name   <- rcs_names
  
  if(pc) attr(parms, 'pcparms') <- attr(xd, 'pcparms')
  attributes(xd) <-
    set.atr(xd, x, z, name, "rcspline", 4, parms,
            if(pc) rep(TRUE, nknots-1) else c(FALSE,rep(TRUE,nknots-2)))
  xd
}

#Test modified RCS function (rcs2)
#Age_rcs1 <- as.numeric(sample(18:100, 3000, replace=TRUE))
#DrinksLastWeek_rcs1 <- as.numeric(sample(0:20, 3000, replace=TRUE))
#data1 <- data.frame(Age_rcs1, DrinksLastWeek_rcs1)
#rm(Age_rcs1, DrinksLastWeek_rcs1)

#attach(data1)
#rcs(Age_rcs1, 5)
#rcs2(Age_rcs1, 5, rcs_names=c('Age_rcs1', 'Age_rcs2', 'Age_rcs3', 'Age_rcs4'))
#detach(data1)

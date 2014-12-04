## This functionn is to perform group coordinate descent regression

gcdreg2=function (x,y,groupInfo,penalty=c("MCP", "SCAD", "lasso"),gamma,lambda,nlambda=100,delta,maxIter=100,...)
{
  ##error checking
  if (class(x) != "matrix") 
  {
    tmp <- try(x <- as.matrix(x), silent=TRUE)
    if (class(tmp)[1] == "try-error") 
      stop("x must be a matrix or able to be coerced to a matrix")
  }
  if (class(y) != "numeric") 
  {
    tmp <- try(y <- as.numeric(y), silent=TRUE)
    if (class(tmp)[1] == "try-error") 
      stop("y must numeric or able to be coerced to numeric")
  }
  penalty <- match.arg(penalty)
  if (gamma <= 1 & penalty=="MCP") 
    stop("gamma must be greater than 1 for the MC penalty.")
  if (gamma <= 2 & penalty=="SCAD") 
    stop("gamma must be greater than 2 for the SCAD penalty.")
  if (nlambda < 2) 
    stop("nlambda must be at least 2")
  if (any(is.na(y)) | any(is.na(x))) 
    stop("Missing data (NA's) detected.Take actions to eliminate missing data before passing X and y to gcdreg.")
  
  ##group standardize
  ##std <- .Call("GroupStandardize", x,y)
  ##check GroupStandardize
  std<-GroupStandardize2(x,y)
  
  XX <- std[[1]]
  yy <- std[[2]]
  scale <- std[[3]]  
  #nz <- which(scale > 1e-6)
  #if (length(nz) != ncol(XX)) XX <- XX[ ,nz, drop=FALSE]
  
  
  
  ##setup parameter
  if (missing(lambda)) 
  {
    lambda <- SetupParameter(XX, yy, groupInfo,nlambda)
  } 
  else 
  {o
    nlambda <- length(lambda)
  }
  
  ##Fit
  
  # cfunction
  #res <- .Call("GCDReg", XX, yy,groupInfo,penalty,gamma,lambda, delta,maxIter) 
  #beta1=matrix(res[[1]],nrow=nlambda,ncol=dim(XX)[2],byrow=TRUE)  
  res=GCDReg2(XX, yy,groupInfo,penalty,gamma,lambda, delta,maxIter)
  m<-ncol(XX)
  b<-matrix(res[[1]],nlambda,m,byrow=TRUE)
  loss<-res[[2]]
  iter<-res[[3]]
  
  ##unstandardize
  beta<-matrix(0,m,nlambda)
  beta<-b*scale
  
  ##output
  val <- structure(list(beta = beta,
                        iter = iter,
                        lambda = lambda,
                        gamma=gamma,
                        penalty = penalty,
                        loss = loss,
                        groupInfo=groupInfo,
                        n = n),
                   class = "gcdreg")
  val
}
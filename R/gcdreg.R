## This functionn is to perform group coordinate descent regression

gcdreg=function (x,y,groupInfo,penalty=c("MCP", "SCAD", "lasso"),gamma,lamda,nlamda=100,delta,maxInter=1000,...)
{
  ##error checking
  if (class(X) != "matrix") 
  {
    tmp <- try(X <- as.matrix(X), silent=TRUE)
    if (class(tmp)[1] == "try-error") 
      stop("X must be a matrix or able to be coerced to a matrix")
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
  if (any(is.na(y)) | any(is.na(X))) 
    stop("Missing data (NA's) detected.Take actions to eliminate missing data before passing X and y to gcdreg.")
  
  ##group standardize
  std <- .Call("GroupStandardize", X,y)
  XX <- std[[1]]
  yy <- std[[2]]
  scale <- std[[3]]  
  #nz <- which(scale > 1e-6)
  #if (length(nz) != ncol(XX)) XX <- XX[ ,nz, drop=FALSE]


  
  ##setup parameter
  
  ##Fit
  
  ##unstandardize
  
  ##output
}
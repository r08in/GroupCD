#Generate random data from mvn for linear model

GenerateData = function (n,p,pNum,dataSetNum=1,r=0.9,errorSigma=1,offSet=0)
{
  #for test
  set.seed(120)
  
  #check data
  if(n<=0||p<=0||pNum<=0)
    stop("n or p or pNum cannot smaller than 0.")
  if(p<pNum)
    stop("p cannot be smaller than pNum.")  
  if(dataSetNum<=0)
    stop("dataSetNum should be positive integer.")
  ##generate design matrix x 
  #normal parameter for design matrix
  mu = rep(0,p)
  sigma=matrix(0,nrow =p, ncol = p)  
  xx=matrix(0,nrow =n, ncol = p)
  for(i in 1:p)
    for(j in 1:p)
    {
      sigma[i,j]=r^abs(i-j)   
    }
  tempx=array(0,dim=c(dataSetNum,n,p))
  
  xx=mvrnorm(n,mu,sigma)
  
  
  #generate beta  
  tempBeta=array(0,dim=c(dataSetNum,p))
  if(length(offSet)!=dataSetNum)
  {
    offSet=rep(0,dataSetNum)
  }
  for(j in 1:dataSetNum)
  {
    tempBeta[j,] =c(rep(0,offSet[j]),rep(1,pNum),rep(0,p-offSet[j]-pNum))
  }
  
  #generate observation y
  tempy=array(0,dim=c(dataSetNum,n))
  for(j in 1:dataSetNum)
  {
    if(errorSigma==0)
    {
      error=rep(0,n);
    }
    else
    {
      error=rnorm(n,0,errorSigma);
    }
    tempy[j,]=tempx[j,,]%*%tempBeta[j,]+error
  }
  
  if(dataSetNum==1)
  {
    x<-tempx[1,,]
    beta<-tempBeta[1,]
    y<-tempy[1,]
  }
  else 
  {
    x=tempx
    beta=tempBeta
    y=tempy
  }
  
  list(x=x,y=y,beta=beta)
  
}

GenerateGroupData=function(groupSize,groupNum,validGroupNum,dataSize,offSet=0)
{
  n=dataSize
  p=groupNum
  pNum=validGroupNum
  dataSetNum=groupSize
  data=GenerateData(n,p,pNum,dataSetNum,errorSigma=0.001,offSet=rep(offSet,dataSetNum))
  out=CombineMultiLm(data$x,data$y)
}

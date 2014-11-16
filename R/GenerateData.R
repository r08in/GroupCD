#Generate random data from mvn for linear model

GenerateData = function (n,p,pNum,dataSetNum=1,r=0.5,errorSigma=0.01)
{
  #for test
  #set.seed(120)
  
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
  for(j in 1:dataSetNum)
  {     
    for(i in 1:n)
    {
      xx[i,]=rnorm(p,mu,sigma)
    }    
    tempx[j,,]=xx
  }
  
  
  #generate beta  
  tempBeta=array(0,dim=c(dataSetNum,p))
  for(j in 1:dataSetNum)
  {
    tempBeta[j,] =c(rep(1,pNum),rep(0,p-pNum))
  }
  
  #generate observation y
  tempy=array(0,dim=c(dataSetNum,n))
  for(j in 1:dataSetNum)
  {
    #error=rnorm(n,0,errorSigma)
    error=rep(0,n)
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

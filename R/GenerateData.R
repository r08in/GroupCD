##Generate random data from mvn for linear model

#Generate multi-dataset. each one is in linear model
# r paramter of variance in generating X matrix
# errorSigma variance of ramdom error
# offSet indicate the offset of position of non-zero beta
GenerateData = function (n,p,pNum,dataSetNum=1,r=0.9,errorSigma=1,offSet=0,outlier.op="NONE",outlier.pro=0.1,outlier.r=10)
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
  if(outlier.op!="NONE"&&(outlier.pro>1||outlier.pro<0))
    stop("the proportion of outlier is illegal!")
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
    tempx[j,,]=mvrnorm(n,mu,sigma)
  }
  
  
  #generate beta  
  tempBeta=array(0,dim=c(dataSetNum,p))
  if(length(offSet)!=dataSetNum)
  {
    offSet=rep(0,dataSetNum)
  }
  for(j in 1:dataSetNum)
  {
    tempBeta[j,] =c(rep(0,offSet[j]),rep(2,pNum),rep(0,p-offSet[j]-pNum))
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
    if(outlier.op!="NONE") #generate outliers
    {
      oNum=round(n*outlier.pro)
      if(outlier.op=="MEANSHIFT")
      {
        shift=c(rep(outlier.r,oNum),rep(0,n-oNum))
        error=error+shift
      }
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

## Generate group data
GenerateGroupData=function(groupSize,groupNum,validGroupNum,dataSize,offSet=0)
{
  n=dataSize
  p=groupNum
  pNum=validGroupNum
  dataSetNum=groupSize
  data=GenerateData(n,p,pNum,dataSetNum,errorSigma=0.001,offSet=rep(offSet,dataSetNum))
  out=CombineMultiLm(data$x,data$y)
}


##Generate linear model data
##The design matrix is composed of dummy data
GenerateDummyData=function(n,groupInfo,validGroupNum,errorSigma=1,offSet=0)
{
  #check data
  p=length(groupInfo)
  pNum=validGroupNum
  if(p<pNum||pNum<=0)
    stop("The number of valid group is illegal!")
  if(offSet+pNum>p)
    stop("The offSet is illegal!")
  #for test
  set.seed(120)
  
  #generate design matrix x
  m=sum(groupInfo)
  x=matrix(0,n,m)
  left=0
  for(j in 1:p)
  {    
    for(i in 1:n)
    {
      index=sample(0:groupInfo[j],1)
      if(index==0) next
      x[i,(left+index)]=1
    }
    left=left+groupInfo[j]
  }

  #generate beta
  beta=rep(0,m)
  if(offSet==0)
  {
    start=1
    end=sum(groupInfo[1:pNum])
  }
  else
  {
    start=sum(groupInfo[1:offSet])+1
    end=sum(groupInfo[1:(offSet+pNum)])
  }
  beta[start:end]=2
  
  #generate y
 
 # x[,1]=0
  if(errorSigma==0)
  {
    error=rep(0,n)
  }
  else
  {
    error=rnorm(n,0,errorSigma)
  }
  y=x%*%beta+error
  #return
  list(x=x,y=y,beta=beta)
}

##Generate Dummy model
GenerateDummyModel=function(sizeInfo,groupInfo,validGroupNumInfo,offSet=0,errorSigma)
{
  ##intial
  m=length(sizeInfo) #dataset Num
  p=sum(groupInfo)
  
  ##generate design matrix
  X=matrix(0,0,p)
  Y=NULL
  if(length(offSet)!=m)
  {
    offSet=rep(0,m)
  }
  for(i in 1:m)
  {
    #generate group dummy data  GenerateDummyData=function(n,groupInfo,validGroupNum,errorSigma=1,offSet=0)
    out=GenerateDummyData(sizeInfo[i],groupInfo,validGroupNumInfo[i],offSet=offSet[i],errorSigma=errorSigma)
    X=rbind(X,out$x)
    Y=c(Y,out$y)
  }
  
  #combine each dataSet
  CombineDataset(X,sizeInfo,groupInfo,Y)
}
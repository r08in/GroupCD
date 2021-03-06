IsSuccess=function(n,p,pNum,dataSetNum)
{
  groupInfo=rep(dataSetNum,p)
  data=GenerateData(n,p,pNum,dataSetNum,errorSigma=1)
  out=CombineMultiLm(data$x,data$y)
  res2=gcdreg(out$x,out$y,groupInfo,penalty="MCP",gamma=3,nlambda=300,delta=0.000001,maxIter=1000)
  final=BICSelect2(res2$loss,res2$n,res2$beta,res2$lambda,inv=0.9)
  #final=GreedySelect(res2$beta,res2$lambda,inv=0.95)
  ##plot(final$res[,1],final$res[,2])
  success=0
  if(final$df==pNum*dataSetNum&&final$df==sum(final$beta[1:final$df]!=0))
  {
    
    success=1
  }
  
  success
}

TestRate=function(t)
{
  sum=0
  res=matrix(0,t,5)
  for(i in 1:t)
  {
    n=as.double(sample(30:60,1))
    p=as.double(sample(200:300,1))
    pNum=as.double(sample(5:10,1))
    dataSetNum=as.double(sample(4:7,1))
    out=IsSuccess(n,p,pNum,dataSetNum)
    sum=sum+(out==1)
    res[i,1]=n
    res[i,2]=p
    res[i,3]=pNum
    res[i,4]=dataSetNum
    res[i,5]=out  
    
  }
  list(rate=(sum/t),res=res)
  
}

GenerateBindedGroupData=function(m,groupSize,groupNum,validGroupNumList,dataSizeList,offSet=0)
{ 
  
  ##generate group data for each data set
  XX=matrix(0,0,groupSize*groupNum)
  YY=NULL
  if(length(offSet)!=m)
  {
    offSet=rep(0,m)
  }
  for(i in 1:m)
  {
    #generate group data 8,200,20,31
    out=GenerateGroupData(groupSize,groupNum,validGroupNumList[i],dataSizeList[i],offSet=offSet[i])
    XX=rbind(XX,out$x)
    YY=c(YY,out$y)
  }
  
  #combine each dataSet
  CombineDataset(XX,dataSizeList*groupSize,rep(groupSize,groupNum),YY)
}
## This file is for tunning parameter selection

##BIC

BICSelect2=function(rss,n,betas,lambdas)
{
  #declare and initial
  m=length(lambdas)
  indexSelected=1
  df=sum(betas[1,]!=0)
  BicPre=log(rss[1])+log(n)*df/n
  res=matrix(0,m,3)
  res[1,1]=lambdas[1]
  res[1,2]=BicPre
  res[1,3]=df
  BicPre=1000000 # not use the  first lambda(all zero)
  for(i in 2:m)
  {
    df=sum(betas[i,]!=0)
    BicTemp=log(rss[i])+log(n)*df/n
    res[i,1]=lambdas[i]
    res[i,2]=BicTemp
    res[i,3]=df
    if(BicTemp<BicPre)
    {
      indexSelected=i
      BicPre=BicTemp
    }
  }
  
  list(lambda=lambdas[indexSelected],beta=betas[indexSelected,],rss=rss[indexSelected],
       df=sum(betas[indexSelected,]!=0),index=indexSelected,res=res)
}
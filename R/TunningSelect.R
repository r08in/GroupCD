## This file is for tunning parameter selection

##BIC

BICSelect2=function(rss,n,betas,lambdas)
{
  #declare and initial
  m=length(lambdas)
  indexSelected=1
  BicPre=log(rss[1])+log(n)*sum(betas[1,]!=0)/n
  
  for(i in 2:m)
  {
    BicTemp=log(rss[i])+log(n)*sum(betas[i,]!=0)/n
    if(BicTemp<BicPre)
    {
      indexSelected=i
      BicPre=BicTemp
    }
  }
  
  list(lambda=lambdas[indexSelected],beta=betas[indexSelected,],rss=rss[indexSelected],
       df=sum(betas[indexSelected,]!=0),index=indexSelected)
}
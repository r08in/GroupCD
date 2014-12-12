## The function in this file is the R function for testing the corresponding c function


GroupStandardize2=function(x,y)
{
  ##declaration
  
  ##scale each collum of x
  n=dim(x)[1]
  p=dim(x)[2]
  scale=rep(0,p)
  tempx=x
  for(i in 1: p)
  {
    scale[i]=sqrt(x[,i]%*%x[,i]/n)
    tempx[,i]=x[,i]/scale[i]
  }

  ## Return list
  list(tempx,y,scale)
  
}

##calculate the value of Xj'Y/n
CrossProduct2=function(x,y,begin,end)
{
  n=length(y)
  t(x[,begin:end])%*%y/n
}

CrossProductL2Norm2=function(x,y,begin,end)
{
  out=CrossProduct2(x,y,begin,end)
  sqrt(t(out)%*%out)
}


##find the max ||Xj'Y/n|| for max lamda
MaxProduct2=function(x,y,groupInfo)
{

##declaration  
p=length(groupInfo)  
n=length(y)
maxVal=0
begin=0
end=0

for(j in 1:p)
{
  begin=end+1;
  end=end+groupInfo[j];
  val= CrossProductL2Norm2(x,y,begin,end);
  if(maxVal<val)
 {
   maxVal=val;
 }     
} 

maxVal

}



GCDReg2=function(x, y, groupInfo, penalty,gamma, lamda, delta, maxIter)
{  
  ##declaration
  n=length(y)
  m=dim(x)[2]
  p=length(groupInfo)
  L=length(lamda)
  begin=0
  end=0
  lstart=1 ##since lamda[1] will give all beta 0
  
  ##reslut to be returned
  beta=matrix(ncol=m,nrow=L,0)
  loss=rep(0,L)
  iter=rep(0,L)
  
  ##temp
  r=rep(0,n)
  betaPre=rep(0,m)
  betaShift=rep(0,m)
  z=rep(0,m)
  tempb=rep(0,m)
  
  ##initial
  r=y    
  for(j in 1:p) ##initial z
{
  begin=end+1
  end=end+groupInfo[j]
  tempz=CrossProduct2(x,y,begin,end)  ## division by n included
  for(i in begin:end)
  {
    z[i]=tempz[i-begin+1]
  }
}

  loss[1]=t(y)%*%y ##initial loss[1]

##iteration for each lamda
for(l in lstart:L)
{  
  if(l>=2)
  {
    betaPre=beta[l-1,] ##assign previous beta to betaPre
  }

##iteration for all covariates
while(iter[l]<maxIter)
{
  iter[l]=iter[l]+1
  begin=end=0
  
  ##iteration for each covariate group
  for(j in 1:p)
  {
    begin=end+1
    end=end+groupInfo[j]
    
    ##(1)calculate z j group
    tempz=CrossProduct2(x,r,begin,end)
    for(i in begin:end)
    {
      z[i]=tempz[i-begin+1]+betaPre[i]
    }
    
    ##(2)update beta j group
    if (penalty=="MCP")
    {
      tempb=McPGroup2(z,begin,end,lamda[l],gamma)
      beta[l,begin:end]=tempb
    }
    
    ##(3)update r
    betaShift[begin:end]=beta[l,begin:end]-betaPre[begin:end]
    r=r-x[,begin:end]%*%betaShift[begin:end]  
    
  }
  
  ##update betaPre for next iteration
  betaPre=beta[l,]
  
  ## Check for convergence
  if(t(betaShift)%*%betaShift<delta)
  {
    break;
  }
  
}
##compute square of loss
loss[l]=t(r)%*%r
}

list(beta,loss,iter)
}


McPGroup2=function(z, begin,end,lamda,gamma)
{
  ##error checking
  #if(end<begin||end<0||begin<0)
  
  size=end-begin+1
  tempz=rep(0,size)
  multiplier=sqrt(size)
  znorm=sqrt(t(z[begin:end])%*%z[begin:end])
  lamda2=multiplier*lamda
  
  if(znorm<=lamda2) ## set all to zero
{
   tempz=rep(0,size)
}
else if(lamda2<znorm && lamda2*gamma>=znorm)
{
  s=gamma/(gamma-1)*(1-lamda2/znorm)
  tempz=z[begin:end]*s
}
else
{
  tempz=z[begin:end]
}

tempz
}


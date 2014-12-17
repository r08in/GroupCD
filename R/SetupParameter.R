SetupParameter = function(X, y,groupInfo,nlamda,multiplier=1) 
{
   lamdaMax=(.Call("MaxProduct", X, y, groupInfo)/sqrt(min(groupInfo)))  
   #lamdaMin=1e-5
   #lamda=seq(lamdaMax,lamdaMin,length.out=nlamda)
  
  lamda=lamdaMax
  xmin=log(1)
  xmax=log(nlamda)
  for(i in 2:nlamda)
  {
    lamdaTemp=lamdaMax/(xmax-xmin)*(xmax-log(i))
    lamda=c(lamda,lamdaTemp)
  }
  lamda 
  
  
}



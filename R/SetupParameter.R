SetupParameter = function(X, y,groupInfo,nlamda) 
{
   lamdaMax=(.Call("MaxProduct", X, y, groupInfo)/sqrt(min(groupInfo)));
   #lamdaMax2=MaxProduct2(X,y,groupInfo)
   lamdaMin=0.0005*lamdaMax;
   lamda=seq(lamdaMax,lamdaMin,length.out=nlamda-1)
   lamda=c(lamda,0)

}


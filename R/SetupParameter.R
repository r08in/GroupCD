SetupParameter = function(X, y,groupInfo,nlamda) 
{
   lamdaMax=.Call("MaxProduct", X, y, groupInfo);
   lamdaMin=0.01*lamdaMax;
   seq(lamdaMax,lamdaMin,length.out=nlamda)

}


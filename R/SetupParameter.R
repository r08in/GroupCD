SetupParameter = function(X, y,groupInfo,nlamda) 
{
   lamdaMax=.Call("MaxProduct", X, y, groupInfo);
   #lamdaMax2=MaxProduct2(X,y,groupInfo)
   lamdaMin=0.001*lamdaMax;
   seq(lamdaMax,lamdaMin,length.out=nlamda)

}


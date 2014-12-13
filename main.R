##This is a test
n=31
p=200
pNum=20
dataSetNum=9
nlambda=1000
groupInfo=rep(dataSetNum,p)
data=GenerateData(n,p,pNum,dataSetNum,errorSigma=0.001)
out=CombineMultiLm(data$x,data$y)
#std <- .Call("GroupStandardize", out$x,out$y)
res=gcdreg(out$x,out$y,groupInfo,penalty="MCP",gamma=3,nlambda=nlambda,delta=0.000001,maxIter=1000)
#res2=gcdreg2(out$x,out$y,groupInfo,penalty="MCP",gamma=3,nlambda=1000,delta=0.000001,maxIter=1000)
final=BICSelect2(res$loss,res$n,res$beta,res$lambda)
#final2=BICSelect2(res2$loss,res2$n,res2$beta,res2$lambda)
#final2=.Call("BICSelect",res$loss,res$n,res$beta,res$lambda)

plot(final$res[,1],final$res[,2])
x11()
plot(final$res[1:(nlambda-5),1],final$res[1:(nlambda-5),3])

#test rate
res=TestRate(10)



#function
n=31
p=136
pNum=24
dataSetNum=7
outPut=IsSuccess(n,p,pNum,dataSetNum)




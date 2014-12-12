##This is a test
n=30
p=100
pNum=10
dataSetNum=5
groupInfo=rep(dataSetNum,p)
data=GenerateData(n,p,pNum,dataSetNum)
out=CombineMultiLm(data$x,data$y)
#std <- .Call("GroupStandardize", out$x,out$y)
#res=gcdreg(out$x,out$y,groupInfo,penalty="MCP",gamma=3,nlambda=200,delta=0.000001,maxIter=1000)
res2=gcdreg2(out$x,out$y,groupInfo,penalty="MCP",gamma=3,nlambda=200,delta=0.000001,maxIter=1000)
final=BICSelect2(res2$loss,res2$n,res2$beta,res2$lambda)
final2=.Call("BICSelect",res2$loss,res2$n,res2$beta,res2$lambda)



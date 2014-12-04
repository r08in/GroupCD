##This is a test
n=30
p=100
pNum=10
dataSetNum=5
groupInfo=rep(dataSetNum,p)
data=GenerateData(n,p,pNum,dataSetNum)
out=CombineMultiLm(data$x,data$y)
#std <- .Call("GroupStandardize", out$x,out$y)
res=gcdreg(out$x,out$y,groupInfo,penalty="MCP",gamma=3,nlambda=200,delta=0.000001,maxIter=1000)


#.Call("test1", std[[1]],std[[2]],0,1,4)

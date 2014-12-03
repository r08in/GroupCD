##This is a test
n=2
p=3
pNum=1
dataSetNum=2
groupInfo=rep(dataSetNum,p)
data=GenerateData(n,p,pNum,dataSetNum)
out=CombineMultiLm(data$x,data$y)
std <- .Call("GroupStandardize", out$x,out$y)
#max<-.Call("MaxProduct",std[[1]],std[[2]],groupInfo)
#lamda=SetupParameter(std[[1]], std[[2]],groupInfo,100) 
res=gcdreg2(out$x,out$y,groupInfo,penalty="MCP",gamma=3,nlambda=10,delta=0.000001,maxIter=1000)


#.Call("test1", std[[1]],std[[2]],0,1,4)

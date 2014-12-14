##This is a test
n=31
p=200
pNum=20
dataSetNum=8
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


##This is a test2
n=31
p=200
pNum=20
dataSetNum=8
groupInfo=rep(dataSetNum,p)
data=GenerateData(n,p,pNum,dataSetNum,errorSigma=0.001)
out1=CombineMultiLm(data$x,data$y)

out2=GenerateGroupData(8,200,20,31)
GenerateGroupData(groupSize,groupNUm,validGroupNumList[i],dataSizeList[i])

GenerateBindedGroupData()

##Testing
##data setting
m=3
groupSize=3
groupNum=3 #num of the group 
validGroupNumList=rep(1,m) #the valid group start from col 1
dataSizeList=rep(2,m) # the actual num for each dataset would be obtain by multiplier groupsize
GenerateBindedGroupData(m,groupSize,groupNum,validGroupNumList,dataSizeList)

##final test
##This is a test
m=4
groupSize=2
groupNum=100 #num of the group 
validGroupNumList=rep(5,m) #the valid group start from col 1
dataSizeList=rep(60,m) # the actual num for each dataset would be obtain by multiplier groupsize
out=GenerateBindedGroupData(m,groupSize,groupNum,validGroupNumList,dataSizeList,offSet=c(0,0,10,10))
nlambda=1000
groupInfo=rep(groupSize*m,groupNum)
#std <- .Call("GroupStandardize", out$x,out$y)
res=gcdreg(out$x,out$y,groupInfo,penalty="MCP",gamma=3,nlambda=nlambda,delta=0.000001,maxIter=1000)
#res2=gcdreg2(out$x,out$y,groupInfo,penalty="MCP",gamma=3,nlambda=1000,delta=0.000001,maxIter=1000)
final=BICSelect2(res$loss,res$n,res$beta,res$lambda)
plot(final$res$lambda,final$res$BIC)
x11()
plot(final$res$lambda,final$res$df)
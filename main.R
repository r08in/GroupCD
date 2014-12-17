##This is a test  
library("MASS") #try to use onevent later
n=40 
p=200 
pNum=5
dataSetNum=5
nlambda=300
groupInfo=rep(dataSetNum,p)
data=GenerateData(n,p,pNum,dataSetNum,errorSigma=1)
out=CombineMultiLm(data$x,data$y)
res=gcdreg(out$x,out$y,groupInfo,penalty="MCP",gamma=3,nlambda=nlambda,delta=0.000001,maxIter=1000)
#res=gcdreg2(out$x,out$y,groupInfo,penalty="MCP",gamma=3,nlambda=nlambda,delta=0.000001,maxIter=1000)
final=BICSelect2(res$loss,res$n,res$beta,res$lambda,inv=1)
#final2=GreedySelect(res$beta,res$lambda,inv=0.95)
start=1
end=nlambda-5
plot(final$res$lambda[start:end],final$res$BIC[start:end],main='BIC-lambda')
x11()
plot(final$res$lambda[start:end],final$res$df[start:end],main='df-bambda')
betas=res$beta
beta=final$beta
(beta!=0)+0

##setting for test(bad)
n=44 
p=221 
pNum=37 
dataSetNum=7

n=as.double(sample(30:60,01))
p=as.double(sample(200:300,1))
pNum=as.double(sample(30:40,1))
dataSetNum=as.double(sample(5:10,1))

##setting for test(good)
n=30 
p=260
pNum=15
dataSetNum=10

n=30 
p=260
pNum=15
dataSetNum=10

n=as.double(sample(30:60,01))
p=as.double(sample(200:300,1))
pNum=as.double(sample(10:20,1)) # smaller valid p
dataSetNum=as.double(sample(5:10,1))

#test rate
res=TestRate(10)

#test with offset
n=50
p=200
pNum=10
dataSetNum=3
groupInfo=rep(dataSetNum,p)
data=GenerateData(n,p,pNum,dataSetNum,errorSigma=0.001,offSet=c(0,0,20))
out=CombineMultiLm(data$x,data$y)
res=gcdreg(out$x,out$y,groupInfo,penalty="MCP",gamma=3,nlambda=nlambda,delta=0.000001,maxIter=1000)
final=BICSelect2(res$loss,res$n,res$beta,res$lambda,inv=0.95)
final2=GreedySelect(res$beta,res$lambda,inv=0.95)
plot(final$res$lambda[10:(nlambda-10)],final$res$BIC[10:(nlambda-10)],main='BIC-lambda')
x11()
plot(final$res$lambda[10:(nlambda-10)],final$res$df[10:(nlambda-10)],main='df-bambda')
betas=res$beta
beta=final$beta
(beta!=0)+0


##final test
##This is a test
m=4
groupSize=4
groupNum=180 #num of the group 
validGroupNumList=rep(10,m) #the valid group start from col 1
dataSizeList=rep(40,m) # the actual num for each dataset would be obtain by multiplier groupsize
out=GenerateBindedGroupData(m,groupSize,groupNum,validGroupNumList,dataSizeList,offSet=c(0,0,15,15))
nlambda=1000
groupInfo=rep(groupSize*m,groupNum)
res=gcdreg(out$x,out$y,groupInfo,penalty="MCP",gamma=3,nlambda=nlambda,delta=0.000001,maxIter=1000)
final=BICSelect2(res$loss,res$n,res$beta,res$lambda,inv=0.9)
plot(final$res$lambda[10:(nlambda-10)],final$res$BIC[10:(nlambda-10)],main='BIC-lambda')
x11()
plot(final$res$lambda[10:(nlambda-10)],final$res$df[10:(nlambda-10)],main='df-bambda')

betas=res$beta
beta=final$beta
(beta!=0)+0
(betas[nlambda-100,]!=0)+0
m=4
groupSize=5
groupNum=180 #num of the group 
validGroupNumList=rep(5,m) #the valid group start from col 1
dataSizeList=rep(20,m) # the actual num for each dataset would be obtain by multiplier groupsize
out=GenerateBindedGroupData(m,groupSize,groupNum,validGroupNumList,dataSizeList)


















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
m=3
groupSize=3
groupNum=3 #num of the group 
validGroupNumList=rep(1,m) #the valid group start from col 1
dataSizeList=rep(2,m) # the actual num for each dataset would be obtain by multiplier groupsize
GenerateBindedGroupData(m,groupSize,groupNum,validGroupNumList,dataSizeList)




#function
        
n=42
p=239
pNum=39
dataSetNum=7
outPut=IsSuccess(n,p,pNum,dataSetNum)



##This is a test  
library("MASS") #try to use onevent later
n=41 
p=282  
pNum=5
dataSetNum=6
nlambda=300
groupInfo=rep(dataSetNum,p)
data=GenerateData(n,p,pNum,dataSetNum,errorSigma=1)
out=CombineMultiLm(data$x,data$y)
res=gcdreg(out$x,out$y,groupInfo,penalty="MCP",gamma=3,nlambda=nlambda,delta=0.000001,maxIter=1000)
#res=gcdreg2(out$x,out$y,groupInfo,penalty="MCP",gamma=3,nlambda=nlambda,delta=0.000001,maxIter=1000)
final=BICSelect2(res$loss,res$n,res$beta,res$lambda,inv=0.9)
#final2=GreedySelect(res$beta,res$lambda,inv=0.95)
start=1
end=nlambda-15
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

#test3
#function(sizeInfo,groupInfo,validGroupNumInfo,offSet=0)
m=2
p=3
sizeInfo=rep(4,m)
groupInfo=rep(2,p)
validGroupNumInfo=rep(1,m)

GenerateDummyModel(sizeInfo,groupInfo,validGroupNumInfo,offSet=c(0,1))

##########################
dataSetNum=4
p=60
pNum=5
groupSize=5
n=120  #data size for each dataset
sizeInfo=rep(n,dataSetNum)
groupInfo=rep(groupSize,p)
validGroupNumInfo=rep(pNum,dataSetNum)
out=GenerateDummyModel(sizeInfo,groupInfo,validGroupNumInfo,errorSigma=1)
nlambda=300
res=gcdreg(out$x,out$y,groupInfo*dataSetNum,penalty="MCP",gamma=3,nlambda=nlambda,delta=0.000001,maxIter=1000)
final=BICSelect2(res$loss,res$n,res$beta,res$lambda,inv=0.95)
start=1
end=nlambda-15
plot(final$res$lambda[start:end],final$res$BIC[start:end],main='BIC-lambda')
x11()
plot(final$res$lambda[start:end],final$res$df[start:end],main='df-lambda')
betas=res$beta
beta=final$beta
(beta!=0)+0

#test data for dataset note:the result is weird
dataSetNum=4
p=2
pNum=1
groupSize=2
n=5  #data size for each dataset


#Real dada analysis
start=40:45
end=5:10
xlst=list(HIVDAT1APV$X[start,end],HIVDAT2ATV$X[start,end],HIVDAT3IDV$X[start,end],HIVDAT4LPV$X[start,end],
         HIVDAT5NFV$X[start,end],HIVDAT6RTV$X[start,end],HIVDAT7SQV$X[start,end])
ylst=list(HIVDAT1APV$Y[start],HIVDAT2ATV$Y[start],HIVDAT3IDV$Y[start],HIVDAT4LPV$Y[start],
          HIVDAT5NFV$Y[start],HIVDAT6RTV$Y[start],HIVDAT7SQV$Y[start])
xxlst=FindCommonFeature(xlst)
res0=CentralizeMultiData(xxlst,ylst)
out0=EncodeDesignMatrix(res$x,res$standard)
out=CombineDataset(out0$x,res0$sizeInfo,out0$groupInfo,res0$y)
groupInfo=out0$groupInfo*length(xlst)
nlambda=300
res=gcdreg(out$x,out$y,groupInfo,penalty="MCP",gamma=3,nlambda=nlambda,delta=0.000001,maxIter=1000)
final=BICSelect2(res$loss,res$n,res$beta,res$lambda,inv=1)
start=1
end=nlambda-15
plot(final$res$lambda[start:end],final$res$BIC[start:end],main='BIC-lambda')
x11()
plot(final$res$lambda[start:end],final$res$df[start:end],main='df-lambda')
betas=res$beta
beta=final$beta
(beta!=0)+0

##
load('C:/Users/Administrator/Desktop/caspar/data/HIVDAT1APV.rda')
load('C:/Users/Administrator/Desktop/caspar/data/HIVDAT2ATV.rda')
load('C:/Users/Administrator/Desktop/caspar/data/HIVDAT3IDV.rda')
load('C:/Users/Administrator/Desktop/caspar/data/HIVDAT4LPV.rda')
load('C:/Users/Administrator/Desktop/caspar/data/HIVDAT5NFV.rda')
load('C:/Users/Administrator/Desktop/caspar/data/HIVDAT6RTV.rda')
load('C:/Users/Administrator/Desktop/caspar/data/HIVDAT7SQV.rda')
xlst=list(HIVDAT1APV$X,HIVDAT2ATV$X,HIVDAT3IDV$X,HIVDAT4LPV$X,
          HIVDAT5NFV$X,HIVDAT6RTV$X,HIVDAT7SQV$X)
ylst=list(HIVDAT1APV$Y,HIVDAT2ATV$Y,HIVDAT3IDV$Y,HIVDAT4LPV$Y,
          HIVDAT5NFV$Y,HIVDAT6RTV$Y,HIVDAT7SQV$Y)
out=PrepareLM(xlst,ylst)
#save(out,file = "out.rda")
#load("out.rda")
nlambda=300
res=gcdreg(out$x,out$y,out$groupInfo,penalty="MCP",gamma=3,nlambda=nlambda,delta=0.000001,maxIter=1000)
final=BICSelect2(res$loss,res$n,res$beta,res$lambda,inv=1)
start=1
end=nlambda-15
plot(final$res$lambda[start:end],final$res$BIC[start:end],main='BIC-lambda')
x11()
plot(final$res$lambda[start:end],final$res$df[start:end],main='df-lambda')
betas=res$beta
beta=final$beta
(beta!=0)+0
SelectGroup(beta,out$groupInfo)


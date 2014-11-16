##This is a test
n=2
p=4
pNum=2

data=GenerateData(n,p,pNum,dataSetNum=2)

out=CombineMultiLm(data$x,data$y)
std <- .Call("GroupStandardize", out$x)

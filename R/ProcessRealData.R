##This is file is for real data processing

##This function is to find the common feature among dataset
FindCommonFeature=function(lst)
{
  #setup the feature list
  m=length(lst)
  nameLst=NULL
  lengths=NULL
  for(i in 1:m)
  {
    name=names(lst[[i]])
    names(lst[[i]])=sapply(name,toupper)
    tempLst=list(strtoi(substring(name,2)))
    nameLst=c(nameLst,tempLst)
    lengths=c(lengths,length(tempLst[[1]]))
  }
  
  #declare and initial
  cur=rep(1,m)
  
  #find the minimumn dataset
  colNum=lengths[1]
  target=nameLst[[1]]
  for(i in 2:m)
  {
    if(colNum>lengths[i])
    {
      colNum=lengths[i]
      target=nameLst[[i]]
    }
  }
  
  #find common feature
  common=NULL
  for(j in 1:colNum)
  {
    neq=0
    for(i in 1:m)
    {
      #move the cursor to appropriate position
      while((cur[i]<=lengths[i])&&(nameLst[[i]][cur[i]]<target[j])) 
      {
        cur[i]=cur[i]+1
      }
      if(cur[i]>lengths[i]) next
      if(nameLst[[i]][cur[i]]==target[j])
      {
        cur[i]=cur[i]+1
        neq=neq+1
      }    
    }
    if(neq==m) #all the same
    {
      common=c(common,target[j])
    }
  }
  common=paste("P",sapply(common,toString),sep="")
  #return
  p=length(common)
  x=NULL
  for(i in 1:m)
  {
    x=c(x,list(lst[[i]][common]))
  }
  x
}


##find the common row for each design matrix x and centralize the data
CentralizeMultiData=function(xlst,ylst)
{
  #arrage design matrix xlst
  m=length(xlst)
  xxlst=NULL
  for(i in 1:m)
  {
    xxlst=c(xxlst, list(xlst[[i]][do.call(order,as.list(as.data.frame(xlst[[i]]))),]))
  }
  
  #find minimun dataset as target  
  #target=xxlst[[1]]
  index=1
  rowNum=dim(xxlst[[1]])[1]
  rows=rowNum
  for(i in 2:m)
  {
    r=dim(xxlst[[i]])[1]
    rows=c(rows,r) #find the row num for each dataset
    if(rowNum>r)
    {
      rowNum=r
      #target=xxlst[[i]]
      index=i
    }
    
  }
  
  cur=rep(1,m)
  #find the most common row for each dataset
  #comRow=NULL
  comRowIndex=0
  comTime=0
  preRow=NULL
  for(i in 1:rowNum)
  {
    if(!is.null(preRow)&&all((preRow)==as.matrix((xxlst[[index]][i,])))) next
    preRow=as.matrix(xxlst[[index]][i,])
    iTime=rowNum  # the common times for row i, initialed as maximun
    for(k in 1:m)
    {
      kTime=0 #the common times of dataset k
      while((cur[k]<=rows[k])&&paste(as.matrix(xxlst[[k]][cur[k],]),collapse="")<paste(as.matrix(xxlst[[index]][i,]),collapse=""))
      {
        cur[k]=cur[k]+1
      }
      while((cur[k]<=rows[k])&&all(as.matrix(xxlst[[k]][cur[k],])==as.matrix((xxlst[[index]][i,]))))
      {
        cur[k]=cur[k]+1
        kTime=kTime+1
      }
      
      if(iTime>kTime)
      {
        iTime=kTime
      }
      if(iTime==0)
      {
        break;
      }
    }
    if(comTime<iTime)
    {
      comTime=iTime
      #comRow=xxlst[[index]][i,]
      comRowIndex=i
    }
    
  }
  
  #centralize dataset respectively
  y=NULL
  for(i in 1:m)
  {
    num=0
    sum=0
    for(j in 1:rows[i])
    {
      if(all(as.matrix(xlst[[i]][j,])==as.matrix(xxlst[[index]][comRowIndex,])))
      {
        num=num+1
        sum=sum+ylst[[i]][j]
      }
    }
    y=c(y,ylst[[i]]-sum/num)
  }
  #return
  x=xlst[[1]]
  for(i in 2:m)
  {
    x=rbind(x,xlst[[i]])
  }
  row.names(x)<-1:dim(x)[1]
  list(y=y,x=x,sizeInfo=rows,standard=as.matrix(xxlst[[index]][comRowIndex,]),datasetIndex=index,comRowIndex=comRowIndex,comTime=comTime)
}


##encode design matrix
EncodeDesignMatrix=function(x,standard)
{
  #data initial 
  p=dim(x)[2]
  n=dim(x)[1]
  
  #find the levels list of x
  lx=sapply(droplevels(x),levels)
  
  #create encoding table
  tEncode=NULL
  for(j in 1:p)
  {
    temp=NULL
    num=length(lx[[j]])
    label=0:(num-1)
    for(i in 1:num)
    {
      if(lx[[j]][i]==standard[j]) break;
    }
    label[1]=label[i]
    label[i]=0
    temp=as.list(label)
    names(temp)=lx[[j]]
    tEncode=c(tEncode,list(temp))
  }
  
  #encode design matrix
  xx=NULL
  for(j in 1:p)
  {
    xj=NULL
    num=length(lx[[j]])
    for(i in 1:n)
    {
      tempx=rep(0,num-1)
      v=as.numeric(tEncode[[j]][as.character(x[i,j])])
      if(v!=0)
      {
        tempx[v]=1
      }
      xj=rbind(xj,tempx)
    }
    xx=cbind(xx,xj)
  }
  
  #return
  row.names(xx)<-1:n
  groupInfo=sapply(lx,length)-1
  list(x=xx,groupInfo=groupInfo)
}


#prepare linear mode
PrepareLM=function(xlst,ylst)
{
  xxlst=FindCommonFeature(xlst)
  res0=CentralizeMultiData(xxlst,ylst)
  out0=EncodeDesignMatrix(res0$x,res0$standard)
  out=CombineDataset(out0$x,res0$sizeInfo,out0$groupInfo,res0$y)
  list(x=out$x,y=out$y,groupInfo=(out0$groupInfo*length(xlst)))
}
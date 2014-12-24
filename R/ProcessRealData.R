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
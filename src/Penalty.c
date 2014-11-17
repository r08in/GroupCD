#include "Rinternals.h"
#include "R_ext/Rdynload.h"
#include <R.h>
#include <R_ext/Applic.h>

#include <math.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

double L2Norm(double *z, int begin,int end)
{
  double val=0;
  for(int i=begin;i<=end;i++)
  {
    val+=z[i]*z[i];
  }
  return val;
}

double * McPGroup(double *z, int begin,int end,double lamda,double gamma,int size)
{
  //error checking
  if(size<0)
  {
    printf("inside McpGroup:the size of group cannot be smaller than 0.");
    abort();
  }
  if(end<begin||end<0||begin<0)
  {
    printf("inside McpGroup:begin or end index is not legal.");
    abort();
  }
  
  double *tempz=Calloc(end-begin+1, double);
  double multiplier=sqrt(size);
  double znorm=L2Norm(z,begin,end);
  double lamda2=multiplier*lamda;
  
  if(znorm<=lamda2)// set all to zero
  {
    for(int i=0;i<end-begin+1;i++)
    {
      tempz[i]=0;
    }
  }
  else if(lamda2<znorm && lamda2*gamma>=znorm)
  {
    double s=gamma/(gamma-1)*(1-lamda2/znorm);
    for(int i=0;i<end-begin+1;i++)
    {
      tempz[i]=s*z[i+begin];
    }
  }
  else
  {
    for(int i=0;i<end-begin+1;i++)
    {
      tempz[i]=z[i+begin];
    }
  }
  
  return tempz;
  
}
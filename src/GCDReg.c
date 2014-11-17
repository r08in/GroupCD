#include "Rinternals.h"
#include "R_ext/Rdynload.h"
#include <R.h>
#include <R_ext/Applic.h>

#include <math.h>
#include <string.h>
#define false 0
#define true 1
typedef int bool; 

// Gaussian loss
double gLoss(double *r, int n) {
  double l = 0;
  for (int i=0;i<n;i++) l = l + pow(r[i],2);
  return(l);
}

bool IsConvergent (double * shift, int size,double delta)
{
  double sumSq=0;
  for(int i=0;i<size;i++)
  {
    sumSq+=shift[i]*shift[i];
  }
  
  if(sumSq<delta)
  {
    return true;
  }
  else
  {
    return false;
  }
}

SEXP CleanupG(double *r, double *betaPre, double * betaShift, double *z, SEXP beta, SEXP loss, SEXP iter) {
  Free(r);
  Free(betaPre);
  Free(betaShift);
  Free(z);
  SEXP res;
  PROTECT(res = allocVector(VECSXP, 3));
  SET_VECTOR_ELT(res, 0, beta);
  SET_VECTOR_ELT(res, 1, loss);
  SET_VECTOR_ELT(res, 2, iter);
  UNPROTECT(4);
  return(res);
}

SEXP GCDReg(SEXP X_, SEXP Y_, SEXP GroupInfo_, SEXP Penalty_, 
            SEXP Gamma_, SEXP Lamda_, SEXP Delta_, SEXP MaxIter_)
{
  
  //data convertion
  double *x=REAL(X_);
  double *y=REAL(Y_);
  double *groupInfo=REAL(GroupInfo_);
  const char *penalty = CHAR(STRING_ELT(Penalty_, 0));
  double gamma=REAL(Gamma_)[0];
  double *lamda=REAL(Lamda_);
  double delta=REAL(Delta_)[0];
  int maxIter = INTEGER(MaxIter_)[0];
  
  //declaration
  int n=nrows(X_);
  int m=ncols(X_);
  int p=length(GroupInfo_);
  int L=length(Lamda_);
  int begin=-1,end=-1;
  
  //reslut to be returned
  SEXP res_, beta_, loss_, iter_;
  PROTECT(beta_ = allocVector(REALSXP, m*L));
  PROTECT(loss_ = allocVector(REALSXP, L));
  PROTECT(iter_ = allocVector(REALSXP, L)); 
  double * beta=REAL(beta_);
  double *loss=REAL(loss_);
  double * iter=REAL(iter_);
  
  //temp
  double *r = Calloc(n, double);
  double *betaPre=Calloc(m, double);
  double * betaShift=Calloc(m, double);
  double *z=Calloc(m, double);
  double *tempz;
  double * tempb;
  
  //initial
  for(int i=0;i<n;i++)
  {
    r[i]=y[i];
  }
  for(int i=0;i<m*L;i++)
  {
    beta[i]=0;
  }
  for(int i=0;i<m;i++)
  {
    betaPre[i]=0;
  }
  for(int i=0;i<L;i++)
  {
    iter[i]=0;
  }
  for(int j=0;j<p;j++) //initial z
  {
    begin=end+1;
    end+=groupInfo[j];
    tempz=CrossProduct(x,y,begin,end,n);// division by n included
    for(int i=begin;i<=end;i++)
    {
      z[i]=tempz[i-begin];
    }
  }
  int lstart=1;//since lamda[0] will give all beta 0;
  
  //iteration for each lamda
  for(int l=lstart;l<L;l++)
  {
    for(int i=0;i<m;i++) //assign previous beta to betaPre
    {
      betaPre[i]=beta[(l-1)*m+i];
    }
    
     //iteration for all covariates
    while(iter[l]<maxIter)
    {
      iter[l]++;     
      begin=end=-1;
      
      //iteration for each covariate group
      for(int j=0;j<p;j++)
      {
        begin=end+1;
        end+=groupInfo[j];
        
        //(1)calculate z j group
        tempz=CrossProduct(x,y,begin,end,n);
        for(int i=begin;i<=end;i++)
        {
          z[i]=tempz[i-begin]+betaPre[i];
        }
        
        //(2)update beta j group
        if (strcmp(penalty,"MCP")==0)
        {
          tempb=McPGroup(z,begin,end,lamda[l],gamma,groupInfo[j]);
          for(int i=begin;i<=end;i++)
          {
            beta[l*m+i]=tempb[i-begin];
          }
        }
        
        //(3)update r
        double tempSum=0;
        for(int j=begin;j<=end;j++)
        {
          betaShift[j]=beta[l*m+j]-betaPre[j];
        }
        for(int i=0;i<n;i++)
        {
          tempSum=0;
          for(int j=begin;j<=end;j++)
          {
            tempSum+=x[j*n+i]*betaShift[j];
          }
          r[i]-=tempSum;
        }
      
       
      }
       // Check for convergence
       if(IsConvergent(betaShift,m,delta))
       {
         //update betaPre for next iteration
         for(int i=0;i<m;i++)
         {
           betaPre[i]=beta[l*m+i];
         }
         break;
       }
       
    }
    loss[l]=gLoss(r,n);
  }
  
  //clean and return result
  res_=CleanupG(r,betaPre,betaShift,z,beta_,loss_,iter_);
  return res_;
            
}

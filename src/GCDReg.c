#include "Rinternals.h"
#include "R_ext/Rdynload.h"
#include <R.h>
#include <R_ext/Applic.h>

#include <math.h>
#include <string.h>
/*
SEXP GCDReg(SEXP X_, SEXP Y_, SEXP GroupInfo_, SExP Penalty_, 
            SEXP Gamma_, SEXP Lamda_, SEXP Delta_, SEXP MaxInter_=1000)
{
  
  //data convertion
  double *x=REAL(X_);
  double *y=REAL(Y_);
  double *groupInfo=REAL(GroupInfo_);
  const char *penalty = CHAR(STRING_ELT(penalty_, 0));
  double gamma=REAL(Gamma_)[0];
  double *lamda=REAL(Lamda_);
  double delta=REAL(Delta_)[0];
  int max_iter = INTEGER(MaxInter_)[0];
  
  //declaration
  int n=nrows(X_);
  int m=ncols(X_);
  int p=length(GroupInfo_);
  int L=length(Lamda_);
  int begin=-1,end=-1;
  
  //reslut to be returned
  SEXP res, beta, loss, iter;
  PROTECT(beta = allocVector(REALSXP, m));
  PROTECT(loss = allocVector(REALSXP, L));
  PROTECT(iter = allocVector(REALSXP, L));  
  
  //temp
  double *r = Calloc(n, double);
  double *betaPre=Calloc(m, double);
  double *z=Calloc(m, double);
  
  //initial
  for(int i=0;i<n;i++)
  {
    r[i]=y[i];
  }
  for(int i=0;i<m;i++)
  {
    betaPre[i]=0;
  }
  for(int j=0;j<p;j++)
  {
    begin=end+1;
    end+=groupInfo[j];
    
  }
            
}
*/
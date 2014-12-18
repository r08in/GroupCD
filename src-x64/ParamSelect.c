#include "Rinternals.h"
#include "R_ext/Rdynload.h"
#include <R.h>
#include <R_ext/Applic.h>
#include <math.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

double* GetNozeroNum(double * Betas, int p, int m)
{
  double*num=Calloc(m,int);

  for(int i=0;i<m;i++)
  {
    int n=0;
    for(int j=0;j<p;j++)
    {
      if(Betas[j*m+i]!=0)
      {
        n++;
      }
    }
    num[i]=n;
  }
  return num;
}

SEXP BICSelect(SEXP Rss_, SEXP N_, SEXP Betas_, SEXP Lambdas_)
{
  //data conversion
  double * rss=REAL(Rss_);
  int n = asInteger(N_);
  double * betas=REAL(Betas_);
  double * lambdas=REAL(Lambdas_);
  
  //declaration and initial
  int m = length(Lambdas_);
  int p = ncols(Betas_);
  int index=0;
  double* df=GetNozeroNum(betas,p,m);
  double bicMin=log(rss[0])+log(n)*df[0]/n;
  
  //minimize BIC
  double bicTemp=0;
  for(int i=1;i<m;i++)
  {
    bicTemp=log(rss[i])+log(n)*df[i]/n;
    if(bicTemp<bicMin)
    {
      bicMin=bicTemp;
      index=i;
    }
  }
  
  //return result
  SEXP beta_,df_,rss_,index_,res_,lambda_;
  
  PROTECT(beta_ = allocVector(REALSXP, p));
  PROTECT(df_ = allocVector(REALSXP, 1));
  PROTECT(rss_ = allocVector(REALSXP, 1));
  PROTECT(lambda_= allocVector(REALSXP, 1));
  PROTECT(index_= allocVector(REALSXP, 1));
  PROTECT(res_ = allocVector(VECSXP, 5));
  double *beta=REAL(beta_);
  for(int i=0;i<p;i++)
  {
    beta[i]=betas[i*m+index];
  }
  
  REAL(df_)[0]=df[index];
  REAL(rss_)[0]=rss[index];
  REAL(index_)[0]=index+1;
  REAL(lambda_)[0]=lambdas[index];

  SET_VECTOR_ELT(res_, 0,lambda_);
  SET_VECTOR_ELT(res_, 1,beta_);
  SET_VECTOR_ELT(res_, 2,rss_ );
  SET_VECTOR_ELT(res_, 3,df_);
  SET_VECTOR_ELT(res_, 4,index_);
  UNPROTECT(6);
  //Free(df);
  return res_;
}
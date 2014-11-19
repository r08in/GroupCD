#include "Rinternals.h"
#include "R_ext/Rdynload.h"
#include <R.h>
#include <R_ext/Applic.h>

#include <math.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>



void test1 (SEXP x_,SEXP y_, SEXP begin_, SEXP end_,SEXP n_ )
{
  double *x = REAL(x_);
  double *y=REAL(y_);
  double begin=REAL(begin_)[0];
  double end= REAL(end_)[0];
  double n=REAL(n_)[0];
  printf("mm");
  double * tempz=CrossProduct(x,y,(int)begin,(int)end,(int)n);// division by n included
  printf("from test1: \n");
  for(int i=0;i<n;i++)
  {
    printf("i:%d, tempz:%f",i,tempz[i]);
  }
}


void test (SEXP x1_,SEXP x2_, SEXP x3_, SEXP x4_,SEXP x5_ )
{
  double *x1 = REAL(x1_);
  double *x2 = REAL(x2_);
  double *x3 = REAL(x3_);
  double *x4 = REAL(x4_);
  double *x5 = REAL(x5_);
  
  //replace the method you want
  //double * out=
  printf("from test: \n");
}


/*****************************************************************************
* This file contains a number of routines for doing generic numerical
* tasks like computing integrals and finding the peak of a function. 
* These are all taken from Numerical Recipies
*****************************************************************************/


#include <stdio.h>
#include <math.h>
#include "rdd.h"

#define RDD_SPLIT 10.

/****************************************************************************
*****************************************************************************
* integrate a function from a to +Inf
****************************************************************************/
double rdd_integral_to_infinity(double func(double, void*), void* param, 
                                double a, double accuracy ) {
double middle,right;

if(a<RDD_SPLIT) middle=rdd_romberg(func,param, a,RDD_SPLIT,accuracy); 
else            middle=0.;

right =rdd_open_romberg(func,param, 0.,1./RDD_SPLIT,accuracy);

return(middle+right);

} /* end of rdd_infinite_integral */


/****************************************************************************
*****************************************************************************
* integrate a function from -Inf to +Inf
****************************************************************************/
double rdd_infinite_integral(double func(double, void*), void* param,
                             double accuracy ) {
double left,middle,right;

left  =rdd_open_romberg(func,param,-1./RDD_SPLIT,0.,accuracy);
middle=rdd_romberg(     func,param,-RDD_SPLIT,RDD_SPLIT,accuracy); 
right =rdd_open_romberg(func,param, 0.,1./RDD_SPLIT,accuracy);

return(left+middle+right);

} /* end of rdd_infinite_integral */

#undef RDD_SPLIT 


#define RDD_JMAX 20 /* maximum number of iterations */
#define RDD_K 4 /* order of extrapolating polynomial */
/****************************************************************************
*****************************************************************************
* perform a "Romberg" integral. This method performs trapeziod rule
* integrations with sucessively smaller step sizes and extrapolates to 
* a step size of zero.
****************************************************************************/
double rdd_romberg(double func(double, void*), void* param,
                    double a, double b, double toll) {

double ss,dss;
double s[RDD_JMAX+2],h[RDD_JMAX+2];
int j;
double remember;

/*
printf("integrating between %g and %g\n",a,b);
*/

  h[1]=1.0;
  for (j=1;j<=RDD_JMAX;j++) {

    s[j]=rdd_trapezoid(func,param, a,b,j,&remember);

    if (j >= RDD_K) {
        /*************************************************************
        * extrapolate to zero step size using the last RDD_K results *
        *************************************************************/
        rdd_poly_interpolate(&h[j-RDD_K],&s[j-RDD_K],RDD_K,0.0,&ss,&dss);

/*
if (fabs(dss) < toll*fabs(ss)+toll) printf("closed: %d\n",j);
*/

        if (fabs(dss) < toll*fabs(ss)+toll) return ss;
    }

    s[j+1]=s[j];
    h[j+1]=0.25*h[j];
  }
  fprintf(stderr,"Warning: RDD integration only accurate to %g\n",
          fabs(dss/ss));
  return ss;
}

/****************************************************************************
*****************************************************************************
* perform a "Romberg" integral using the "open" midpoint rule. 
* This method should be used when you don't want to evaluate the 
* integrand at the endpoints
* note we actually integrate over the function func(1./x)/(x*x)
****************************************************************************/
double rdd_open_romberg(double func(double, void*), void* param,
                        double a, double b, double toll) {
int j;
double ss,dss,h[RDD_JMAX+2],s[RDD_JMAX+2];
double remember;

/*
printf("rdd_open_romberg: start a=%g b=%g\n",a,b);
*/

h[1]=1.0;
for(j=1;j<=RDD_JMAX;j++) {
    s[j]=rdd_midpoint(func,param, a,b,j,&remember);
    if(j>=RDD_K) {
        rdd_poly_interpolate(&h[j-RDD_K],&s[j-RDD_K],RDD_K,0.0,&ss,&dss);
/*
printf("rdd_open_romberg: %d ss=%g dss=%g\n",j,ss,dss);
*/
/*
if (fabs(dss) < toll*fabs(ss)+toll) printf("open: %d\n",j);
*/
        if(fabs(dss)<=toll*fabs(ss)) return ss;
    }
    s[j+1]=s[j];
    h[j+1]=h[j]/9.0;
}

fprintf(stderr,"Warning: RDD integration (open) only accurate to %g\n",
          fabs(dss/ss));
return ss;

} /* end of rdd_open_romberg function */
#undef RDD_JMAX
#undef RDD_K

/************************************************************************
*************************************************************************
* Simple trapezoid integration routine 
* returns the trapezoid rule integral with n steps, provided 
* "remember" is set to the trabezoid integral with n-1 steps.
************************************************************************/
double rdd_trapezoid(double func(double, void*), void* param,
                     double a,  double b,  int n, double* remember) {
double x,tnm,sum,del;
int it,j;

if (n == 1) {
    /**********************************************
    * initialize "remember" and return simple sum *
    **********************************************/
    *remember=0.5*(b-a)*(func(a,param)+func(b,param));
    return (*remember);
} else {
    /****************************************
    * determine the number of points to add *
    ****************************************/
    for (it=1,j=1;j<n-1;j++) it <<= 1;

    tnm=it;
    del=(b-a)/tnm;
    x=a+0.5*del;

    for (sum=0.0,j=1;j<=it;j++,x+=del) sum += func(x,param);

    *remember=0.5*(*remember+(b-a)*sum/tnm);
    return (*remember);

} /* end if n>1 */

} /* end of rdd_trapezoid function*/

/****************************************************************************
*****************************************************************************
* Midpoint rule integration routine to be used on open integrals
* Note that we actually integrate over the function func(1./x)/(x*x)
*****************************************************************************/
double rdd_midpoint(double func(double, void*), void* param,
                    double a,  double b,  int n, double* remember) {

double x,tnm,sum,del,ddel;
int it,j;

if(n==1) {
    x=0.5*(a+b);
    *remember=(b-a)*func(1./x, param)/(x*x);
    return(*remember);

} else {

    for(it=1,j=1;j<n-1;j++) it*=3;

    tnm=it;
    del=(b-a)/(3.*tnm);
    ddel=del+del;
    x=a+.5*del;

    sum=0.;
    for(j=1;j<=it;j++) {

        sum += func(1./x, param)/(x*x);
        x += ddel;

        sum += func(1./x, param)/(x*x);
        x += del;
    }
    *remember=(*remember+(b-a)*sum/tnm)/3.0;
    return(*remember);
} /* end if more than one step */

} /* end of rdd_midpoint function */


/****************************************************************************
*****************************************************************************
* polynomial interpolation (and extrapolation) routine. This is needed
* for Romberg integration
*****************************************************************************/
void rdd_poly_interpolate(double* xa, double* ya,  int n,  double x,  
                double* y,  double* dy){

  int i,m,ns=1;
  double den,dif,dift,ho,hp,w;
  double *c,*d;

  dif=fabs(x-xa[1]);

  c=(double*)malloc(sizeof(double)*(n+1));
  d=(double*)malloc(sizeof(double)*(n+1));

  for (i=1;i<=n;i++) {
    if ( (dift=fabs(x-xa[i])) < dif) {
      ns=i;
      dif=dift;
    }
    c[i]=ya[i];
    d[i]=ya[i];
  }
  *y=ya[ns--];
  for (m=1;m<n;m++) {
    for (i=1;i<=n-m;i++) {
      ho=xa[i]-x;
      hp=xa[i+m]-x;
      w=c[i+1]-d[i];
      if ( (den=ho-hp) == 0.0) fprintf(stderr,"Error in routine polint");
      den=w/den;
      d[i]=hp*den;
      c[i]=ho*den;
    }
    *y += (*dy=(2*ns < (n-m) ? c[ns+1] : d[ns--]));
  }

  free(c);
  free(d);
}

#define ITMAX 100
#define RDD_GOLDEN 0.3819660
#define RDD_SHIFT(a,b,c,d) (a)=(b);(b)=(c);(c)=(d);
#define RDD_SIGN(a,b) ((b) >= 0.0 ? fabs(a) : -fabs(a))

/****************************************************************************
*****************************************************************************
* routine for bracketing a function minimum.
***************************************************************************/
void rdd_bracket_minimum(double func(double, void*), void* param,
                         double* a, double* c, double* b ) {
double fa,fb,fc;
double dum;
double ulim,u,r,q,fu;

fa=func(*a, param);
fb=func(*b, param);

if(fb>fa) {
    RDD_SHIFT(dum,*a,*b,dum);
    RDD_SHIFT(dum,fb,fa,dum);
}

*c=(*b)+RDD_GOLDEN*(*b-*a);
fc=func(*c, param);
while(fb>fc) {
    r=(*b-*a)*(fb-fc);
    q=(*b-*c)*(fb-fa);
    if(q!=r) u=(*b)-((*b-*c)*q-(*b-*a)*r)/(2.0*(q-r));
    else     u=(*b)-((*b-*c)*q-(*b-*a)*r)*1e20;
    ulim=(*b)+100.*(*c-*b);

    if((*b-u)*(u-*c)>0) {
        fu=func(u, param);
        if(fu<fc) {
            *a=(*b);
            *b=u;

            if(*b<*a) { RDD_SHIFT(dum,*a,*b,dum); }

            return;
        }
        u=(*c)+RDD_GOLDEN*(*c-*b);
        fu=func(u,param);
    } else if((*c-u)*(u-ulim)>0.0) {
        fu=func(u,param);
        if(fu<fc) {
            RDD_SHIFT(*b,*c,u,*c+RDD_GOLDEN*(*c-*b));
            RDD_SHIFT(fb,fc,fu,func(u,param));
        }
    } else {
        u=(*c)+RDD_GOLDEN*(*c-*b);
        fu=func(u,param);
    }
    RDD_SHIFT(*a,*b,*c,u);
    RDD_SHIFT(fa,fb,fc,fu);

}

} /* end of rdd_bracket_minimum function */

        
 

/************************************************************************
*************************************************************************
* OK, so this isn't an integration routine, but it's another Numerical
* Recipies routine. It uses Brent's method to find the minimum
* of a function.
* min and max must bracket the minimum and min<mid<max.
************************************************************************/
double rdd_brent_minimum(double func(double, void*), void* param,
                         double min, double mid, double max, 
                         double accuracy) {
int iteration;
double a,b,d,etemp,fu,fv,fw,fx,p,q,r,tol1,tol2,u,v,w,x,xm;
double e=0.0;

a=min;
b=max;
d=0.0;
x=w=v=mid;

fw=fv=fx=func(x,param);

for (iteration=1;iteration<=ITMAX;iteration++) {

    xm=0.5*(a+b);

    tol1=accuracy*fabs(x)+accuracy;
    tol2=2.*tol1;

    /*******************
    * are we done yet? *
    *******************/
    if (fabs(x-xm) <= (tol2-0.5*(b-a)))  return(x);


    if (fabs(e) > tol1) {

    	r=(x-w)*(fx-fv);
    	q=(x-v)*(fx-fw);
    	p=(x-v)*q-(x-w)*r;
    	q=2.0*(q-r);

    	if (q > 0.0) p = -p;
    	q=fabs(q);
    	etemp=e;
    	e=d;
        if (fabs(p) >= fabs(0.5*q*etemp) || p <= q*(a-x) || p >= q*(b-x)) {
            d=RDD_GOLDEN*(e=(x >= xm ? a-x : b-x));
        } else {
            d=p/q;
            u=x+d;
            if (u-a < tol2 || b-u < tol2)
                d=RDD_SIGN(tol1,xm-x);
        }
    } else {
        d=RDD_GOLDEN*(e=(x >= xm ? a-x : b-x));
    }
    u=(fabs(d) >= tol1 ? x+d : x+RDD_SIGN(tol1,d));
    fu=func(u,param);
    if (fu <= fx) {
        if (u >= x) a=x; else b=x;
        RDD_SHIFT(v,w,x,u)
        RDD_SHIFT(fv,fw,fx,fu)
    } else {
        if (u < x) a=u; else b=u;
        if (fu <= fw || w == x) {
            v=w;
            w=u;
            fv=fw;
            fw=fu;
        } else if (fu <= fv || v == x || v == w) {
            v=u;
            fv=fu;
        }
    }
}

fprintf(stderr,"Peak position not as accurate as %g\n",accuracy);

return(x);

} /* end of rdd_brent_minimum function */
#undef ITMAX
#undef RDD_GOLDEN
#undef RDD_SHIFT
#undef RDD_SIGN



#include "xrt_gammq.h"

#define XRT_GAMMQ_C
#define XRT_GAMM_VERSION  "0.1.1"

int xrt_gammq(float a, float x, float * gammcf)
{
  float gamser, gln, tmp_gammcf;
  char taskname[MAXFNAME_LEN];
  
  get_toolnamev(taskname);

  if(x < 0.0 || a <= 0.0)
    {
      headas_chat(NORMAL, "%s: Error: xrt_gammq: Invalid arguments in input.\n", taskname);
      goto gamm_end;
    }
  if(x < (a + 1.0))
    {
      if(xrt_gser(&gamser, a, x, &gln))
	{
	  headas_chat(NORMAL, "%s: Error: xrt_gammq: Problem in 'xrt_gser' routine.\n",taskname);
	  /*goto gamm_end; */

	  printf("gamser == %f \n", gamser);
	}

      /*  printf("gamser == %f \n", gamser); */

      *gammcf=(1.0 - gamser);
    }
  else
    {
      if(xrt_gcf(&tmp_gammcf, a, x, &gln))
	{
	  headas_chat(NORMAL, "%s: Error: xrt_gammq: Problem in 'xrt_gcf' routine.\n",taskname);
	  /*goto gamm_end;*/
	}
      *gammcf=tmp_gammcf;
    }
  return OK;

 gamm_end:
  return NOT_OK;

} /* xrt_gammq */ 

int xrt_gser(float *gamser, float a, float x, float *gln)
{

  int     n;
  float sum, del, ap;
  char taskname[MAXFNAME_LEN];
  
  get_toolnamev(taskname);
  *gln=xrt_gammln(a);
  if(x <= 0.0)
    {
      if( x < 0)
	headas_chat(NORMAL, "%s: Warning: xrt_gser: x less than 0 in routine gser.\n",taskname);
      *gamser=0.0;
      return OK;
    }
  else
    {
      ap=a;
      del=sum=1.0/a;
      for (n=1; n<=ITMAX; n++)
	{
	  ++ap;
	  del *= x/ap;
	  sum += del;
	  if(fabs(del)<fabs(sum)*EPS)
	    {
	      *gamser=sum*exp(-x+a*log(x)-(*gln));
	      return OK;
	    }
	}
      
      *gamser=sum*exp(-x+a*log(x)-(*gln));
      headas_chat(CHATTY, "%s: Warning: xrt_gser: 'a' too large, ITMAX too small.\n",taskname);
      return OK;
    } 
} 
  
int xrt_gcf(float *gammcf, float a, float x, float *gln)
{
  int    i;
  float  an, b, c, d, del, h;
  char taskname[MAXFNAME_LEN];
  
  get_toolnamev(taskname);

  *gln = xrt_gammln(a);
  b = x + 1.0 - a;
  c = 1.0/FPMIN;
  d = 1.0/b;
  h = d;
  for (i=1; i <= ITMAX; i++)
    {
      an = -i *(i-a);
      b += 2.0;
      d = an*d+b;
      if (fabs(d) < FPMIN)
	d = FPMIN;
      c = b+an/c;
      if(fabs(c) < FPMIN)
	c=FPMIN;
      d = 1.0/d;
      del = d*c;
      h *= del ;
      if(fabs(del - 1.0) < EPS)
	break;
    }
  if (i > ITMAX)
    headas_chat(CHATTY, "%s: Warning: xrt_gcf: 'a' too large and ITMAX too small.\n",taskname);
    

  *gammcf=exp(-x+a*log(x)-(*gln))*h;
  return OK;
}/* xrt_gcf */


float xrt_gammln(float xx)
{

  int             j;
  float           ret;
  double          x, y, tmp, ser;
  static double   cof[6]={76.18009172947146, -86.50532032941677, 24.01409824083091, -1.231739572450155,
			  0.1208650973866179e-2, -0.5395239384953e-5};
  

  y=x=xx;
  tmp=x+5.5;
  tmp-=(x+0.5)*log(tmp);
  ser=1.000000000190015;
  for(j=0; j <=5 ; j++)
    ser+=cof[j]/++y;

  ret=-tmp+log(2.5066282746310005*ser/x);

  return ret;

}
	

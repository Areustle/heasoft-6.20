/* replace #include <> -> #include "" for local include files
 * Wed Nov  3 22:49:52 1999 by E. Miyata*/
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include "atFunctions.h"
#include "aste_time.h"

#ifndef AE_EPOCH		/* 2000/1/1 0:0:0 �Ρ�1970/1/1 0:0:0 ����� */
#define AE_EPOCH 946684800	/* �в��ÿ� (���뤦�ôޤޤ�)		    */
#endif

/*****************************************************************
  timeval ����ä� aetime�֤��ؿ�
******************************************************************/
double timeval2aetime (struct timeval t);

/*****************************************************************
  time_t ����ä� aetime�֤��ؿ�
******************************************************************/
double time2aetime (time_t t);

/*****************************************************************
  time_t ����ä� localtime �� aetime �֤��ؿ�
  �Ĥޤꡢlocaltime �� 1993/01/01 ����ηв����ȸ�����̣
******************************************************************/
double time2aetimeLocal (time_t t);

/*****************************************************************
  time_t ����ä����դ��֤��ؿ���
     UT ���֤��Τ� time2utCtime
     local ���� ���֤��Τ� time2localCtime
******************************************************************/
char *time2utCtime    (time_t t, char *c);
char *time2localCtime (time_t t, char *c);

/*****************************************************************
  timeval ����ä����դ��֤��ؿ���
    UT ���֤��Τ� timeval2utCtime
    local ���֤��֤��Τ� timeval2localCtime
******************************************************************/
char *timeval2utCtime    (struct timeval t, char *c);
char *timeval2localCtime (struct timeval t, char *c);

/*****************************************************************
  aetime ����ä����դ��֤��ؿ���
    UT ���֤��Τ� aetime2utCtime
    local ���֤��֤��Τ� aetime2localCtime
******************************************************************/
char *aetime2utCtime    (double t, char *c);
char *aetime2localCtime (double t, char *c);

int  aetime2aedate (double aetime);

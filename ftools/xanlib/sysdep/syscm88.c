/* C support functions for REAL/IX */

#include <sys/types.h>
#include <stdio.h>
#include <sys/stat.h>
#include <string.h>

#define DECFortran
#include "cfortran.h"


getcwd_(str,len)
char *str;
int len;
{
  char cwd[1000];
  int i;

  if (NULL == getcwd(str,len)) return 1;
  for (i=strlen(str);i<len+1;i++)
    {
      str[i] = ' ';
    }
  return 0;
  
}

stat_(str,arr,len)
char *str;
int *arr;
int len;
{
  struct stat buf;

  stat (str,&buf);


  arr[0] = buf.st_dev;
  arr[1] = buf.st_ino;
  arr[2] = buf.st_mode;
  arr[3] = buf.st_nlink;
  arr[4] = buf.st_uid;
  arr[5] = buf.st_gid;
  arr[6] = buf.st_rdev;
  arr[7] = buf.st_size;
  arr[8] = buf.st_atime;
  arr[9] = buf.st_mtime;
  arr[10] = buf.st_ctime;
  arr[11] = -1;
  arr[12] = -1;

}

FCALLSCFUN1(INT,system,SYSTEM,system,STRING)

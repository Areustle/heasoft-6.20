#include<bcont.h>
void getwtFlg(char *wtFlg, char *rcFlg)
 {
  int   status=0, BufLen_2=9;
  char msg[50]="";

   strcpy(msg, "wtFlg");
   Uclgst(msg, wtFlg, &status);
   if (status != 0)
      {
       printf ("Problem getting wtFlg!\n");
       exit (1);
      }

   strcpy(msg, "rcFlg");
   Uclgst(msg, rcFlg, &status);
   if (status != 0)
     {
      printf ("Problem getting rcFlg!\n");
      exit (1);
     }
 }

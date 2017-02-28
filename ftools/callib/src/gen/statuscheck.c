/* StatusChk.c */

/*----------------------------------------------------------
  authors/modifications:
   Banashree M Seifert (Oct 21, 1997):
            c_fcecho in two places changed to c_fcerr
-------------------------------------------------------------*/

void StatusChk(int status, char *message)
{
      if (status){
          c_fcerr(" ");
          c_fcerr(message);
          exit(1);
      }

   return ;

}

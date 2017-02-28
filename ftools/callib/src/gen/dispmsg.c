/* 
Function:

     DispMsg


Description:

     This routine displays the input message on screen if the chatter is
     set to some big enough value.


Functions called:

   genereal utility functions:
     c_fcecho -- writes the input message to the screen


Author and modification history:

     Sandhia Bansal (July, 1997)


Usage:

     void DispMsg(int chatter, int wtchatter, char *message)

     Input:
        chatter   -- int    -- input chatter value
	wtchatter -- int    -- input lower limit on chatter
	message   -- char * -- input string to be displayed on the screen
 
     Include Files:
        general.h
	

----------------------------------------------------------------------------- */


#include "general.h"


void DispMsg(int chatter, int wtchatter, char *message)
{
   if (chatter >= wtchatter)
   {
      c_fcecho(" ");
      c_fcecho(message);
   }
}

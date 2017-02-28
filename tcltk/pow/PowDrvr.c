/************************************************************************
 *
 *   POW C socket/XPA driver
 *
 *        Use this code to make an XPA connection to a POW process and
 *        send pow scripting commands.
 *
 ***********************************************************************/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <float.h>

#include <xpa.h>

int  openPowConnection  ( void );
void closePowConnection ( void );
int  sendSetCmdToPow    ( char *cmd, char  *buf,    int    buflen   );
int  sendGetCmdToPow    ( char *cmd, char **rtnBuf, int   *bufLen   );
int  postProcessCall    ( int   got, char **names,  char **messages );

#define NXPA 1

static XPA   powConnection = NULL;
static char *powAddress    = NULL;

int openPowConnection()
{
   extern XPA powConnection;
   char *rtnBuf;
   int bufLen;

   if( !powAddress ) {
      powAddress = getenv( "POW_DISPLAY" );
      if( !powAddress || strlen(powAddress)==0 ) {
         /*  POW_DISPLAY not defined, must depend on a name server  */
         powAddress = "pow";
      }
   }

   powConnection = XPAOpen( NULL );
   if( sendGetCmdToPow( "version", &rtnBuf, &bufLen )<0 ) {
      system("POWplot &");
      sendSetCmdToPow( "scope 0", NULL, 0);
      sleep(3);
      if( sendGetCmdToPow( "version", &rtnBuf, &bufLen )<0 ) {
         sleep(3);
         if( sendGetCmdToPow( "version", &rtnBuf, &bufLen )<0 ) {
            return 1;
         }
      }
   }
   free( rtnBuf );
   return 0;
}

void closePowConnection()
{
   extern XPA powConnection;

   XPAClose( powConnection );
   powConnection = NULL;
}


int sendGetCmdToPow( char *cmd, char **rtnBuf, int *bufLen)
{
   int  got, stat=0;
   char *bufs[NXPA];
   char *names[NXPA];
   char *messages[NXPA];
   int  lens[NXPA];
   extern XPA powConnection;
   extern char *powAddress;
   
   /*  Flush 'sendSet' command cache  */
   stat = sendSetCmdToPow( NULL, NULL, -1 );
   if( !stat ) {
      got = XPAGet(powConnection, powAddress, cmd, "",
                   bufs, lens, names, messages, NXPA);
      if( got==0 )
         stat = -1;
      else
         stat = postProcessCall(got, names, messages);
      *rtnBuf = bufs[0];
      *bufLen = lens[0];
   }
   return stat;
}

int sendSetCmdToPow( char *cmd, char *buf, int buflen )
{
   int  got, len, stat=0;
   char *names[NXPA];
   char *messages[NXPA];
   extern XPA  powConnection;
   extern char *powAddress;

   static char *cache     = NULL;
   static int   cachePos  = 0;
   static int   cacheSize = 0;

   switch ( buflen ) {

   case -2:      /*  Free the cache  */

      free(cache);
      cache = NULL;
      cachePos = cacheSize = 0;
      break;

   case -1:      /*  Flush the cache  */

      if( cachePos ) {
         len      = cachePos;
         cachePos = 0;
         stat = sendSetCmdToPow( "tcl", cache, len );
      }
      break;

   case 0:      /*  No data, so just cache command  */

      len = strlen(cmd);
      if( cachePos + len + 2 > cacheSize ) {
         cacheSize += 4096 + len;
         if( cache ) {
            cache = (char*) realloc( cache, sizeof(char) * cacheSize );
         } else {
            cache = (char*) malloc( sizeof(char) * cacheSize );
         }
      }
      cache[cachePos++] = '\n';
      strcpy( cache+cachePos, cmd );
      cachePos += len;
      if( cachePos>60000 ) {
         /*  Flush the cache  */
         stat = sendSetCmdToPow( NULL, NULL, -1 );
      }         
      break;

   default:      /*  Data being sent  */

      if( cachePos ) {
         /*  Flush cache first  */
         stat = sendSetCmdToPow( NULL, NULL, -1 );
      }
      if( !stat ) {
         got = XPASet(powConnection, powAddress, cmd, "", buf, buflen,
                      names, messages, NXPA);
         if( got==0 )
            stat = -1;
         else
            stat = postProcessCall(got, names, messages);
      }
      break;
   }
   return stat;
}


int postProcessCall( int got, char **names, char **messages )
{
   int i, status=0;

   for(i=0; i<got; i++){
      if( messages[i] != NULL ) {
         /* error processing */
         fprintf(stderr, "ERROR: %s (%s)\n", messages[i], names[i]);
         status = 1;
      }
      if( names[i]    ) free(names[i]);
      if( messages[i] ) free(messages[i]);
   }
   return status;
}

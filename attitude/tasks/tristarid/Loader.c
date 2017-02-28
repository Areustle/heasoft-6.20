/*
 * $Source: /headas/headas/attitude/tasks/tristarid/Loader.c,v $
 * $Revision: 1.6 $
 * $Date: 2006/04/18 19:50:37 $
 * 
 *   Triplet based star identification algorithm
 *
 * $Log: Loader.c,v $
 * Revision 1.6  2006/04/18 19:50:37  miket
 * Merging Swift Build18 branch
 *
 * Revision 1.5.4.1  2006/04/04 14:22:54  miket
 * changing name of 'getline' to avoid conflict with stdio.h (Cygwin)
 *
 * Revision 1.5  2005/10/10 12:14:25  rwiegand
 * Allow reference to observation matches.
 *
 * Revision 1.4  2005/08/27 12:51:14  wiegand
 * Bob-ified.  Primarily consists of formatting.  Also reworked memory
 * management and renamed object allocation, initialization, deallocation
 * functions.
 *
 * Revision 1.3  2005/08/09 18:57:19  drhunter
 * Considering final.
 *
 * Revision 1.2  2005/08/03 13:05:57  drhunter
 * Tristarid as of 9:00 AM, 8/3/05
 *
 * Revision 1.1  2005/07/25 18:11:24  drhunter
 * Initial revision
 */

#include <stdlib.h>
#include <string.h>

#include "report.h"

#include "Loader.h"
#include "MUtil.h"
#include "Source.h"
#include "SUtil.h"



static int trigetline (char s[], int lim, FILE *stream)
{
   int c = 0, i;

   i = 0;
   while (--lim > 0 && (c=fgetc(stream)) != EOF && c != '\n')
      s[i++] = c;
   if (c == '\n')
      s[i++] = c;
   s[i] = '\0';
   return i;
}


int loadSources (List *sources, char *name, int flags)
{
   char input[250];
   FILE *inStream;
   inStream = fopen(name, "r");

   if (inStream == NULL) {
      report_error("unable to open file %s\n", name);
      return 0;
   }

   while (trigetline(input, sizeof(input), inStream) > 0) {
      char *fields[5];
      char *id;
      char *type;
      double ra;
      double dec;
      float mag;
      int f;
      Source *s;

      if (input[0] == '\r') continue;

      if (input[0] == '#') continue;

      if (string_split(input, fields, 5, ",") != 5) {
         report_error("wrong number of fields in %s\n", input);
         return 0;
      }

      for (f = 0; f < 5; f++)
         fields[f] = strchr(fields[f], '=');

      id = string_copy(&fields[0][1]);
      ra = atof(&fields[1][1]);
      dec = atof(&fields[2][1]);
      if (strstr(&fields[3][1], "NULL") != NULL)
         mag = MAG_NULL;
      else
         mag = atof(&fields[3][1]);
      type = string_copy(&fields[4][1]);


      if (flags & LOADER_DEGREES) {
         ra  = Math_toRadians(ra);
         dec = Math_toRadians(dec);
      }

      s = Source_create(id, ra, dec, mag, type);
	  if (flags & LOADER_OBSERVATION)
         s->observation = 1;
      List_push(sources, s);
   }

   fclose(inStream);
   return 1;
}


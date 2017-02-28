/*
 * $Source: /headas/headas/attitude/tasks/tristarid/Catalog.c,v $
 * $Revision: 1.4 $
 * $Date: 2005/08/27 12:50:32 $
 * 
 *   Triplet based star identification algorithm
 *
 * $Log: Catalog.c,v $
 * Revision 1.4  2005/08/27 12:50:32  wiegand
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

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "report.h"

#include "Catalog.h"
#include "SourceMap.h"
#include "MUtil.h"

#define DEBUG_CATALOG (1 << 16)


static int n;
static Position *center;
static SourceMap *map;

static double nx[3], ny[3];
static double k, q, scale;

static double uhat[3];

static double mincos;


void Catalog_describe ()
{
   int i, mapSize = List_size(map->keys);
   if (mapSize == 0)
      printf("catalog is empty\n");
   for (i = 0; i < mapSize; i++) {
      char *key = (char *) List_get(map->keys, i);
      List *data = (List *) List_get(map->data, i);
      printf("entry %s has %d objects\n", key, List_size(data));
   }
}


void Catalog_initialize2 (Position *p)
{
   double rd[3];
   double rhat;
   center = p;

   Math_v3rdl(center->unit, rd);

   report_verbose("catalog: center ra %f, dec %f, radius %f arcmin\n",
            Math_toDegrees(rd[0]), Math_toDegrees(rd[1]),
			60 * Math_toDegrees(center->radius));

   Math_createSystem(center->unit, nx, ny);

   rhat = tan(center->radius);
   k = rhat / n;
   q = n / rhat;
   scale = n / center->radius;

   map = SourceMap_allocate(0);

   if (debug_test(0x2))
      report_verbose("catalog: k %f, q %f, scale %f\n", k, q, scale);
}


void Catalog_initialize (List *objects, int _n)
{
   Position * p;

   double ra, dec, radius;
   double unit[3];
   double mincos = 1;
   double testcos;

   double minra = 1000;
   double maxra = -1000;
   double mindec = 1000;
   double maxdec = -1000;

   int i;
   for (i = 0; i < List_size(objects); i++) {
      Source *o = (Source *) List_get(objects, i);
      if (o->ra < minra)
         minra = o->ra;
      if (o->ra > maxra)
         maxra = o->ra;
      if (o->dec < mindec)
         mindec = o->dec;
      if (o->dec > maxdec)
         maxdec = o->dec;
   }

   dec = (mindec + maxdec) / 2;
   if (maxra - minra < Math_PI)
      ra = (minra + maxra) / 2;
   else
      ra = fmod(minra + maxra + Math_PI * 2, Math_PI * 2);

   Math_rd2unit(ra, dec, unit);

   for (i = 0; i < List_size(objects); i++) {
      Source *o = (Source *) List_get(objects, i);
      if ((testcos = Math_v3cosangle(unit, o->unit)) < mincos)
         mincos = testcos;
   }

   radius = acos(mincos) * 1.1;

   p = malloc(sizeof(Position));
   Position_construct(p, unit, radius);
   n = _n;

   Catalog_initialize2(p);
}


void Catalog_deconstruct ()
{
   SourceMap_deallocate(map);
   map = 0;

   free(center);
   center = 0;
}


void Catalog_query (Position *spec, List *out)
{
   int i, j;

   double ox = Math_v3dot(nx, spec->unit);
   double oy = Math_v3dot(ny, spec->unit);

   double cx = q * ox;
   double cy = q * oy;

   int sx = floor(cx);
   int sy = floor(cy);

   double delta = spec->radius * scale;

   int i0 = floor(cx - delta);
   int i1 = ceil(cx + delta);
   int j0 = floor(cy - delta);
   int j1 = ceil(cy + delta);

   mincos = cos(spec->radius);

   uhat[0] = ox;
   uhat[1] = oy;
   uhat[2] = 1;
   Math_v3normalize(uhat);

   for (i = i0; i < i1; i++) {
      for (j = j0; j < j1; j++) {
         int include = 0;

         int tx = i < sx ? i + 1 : i;
         int ty = j < sy ? j + 1 : j;

         if (i == sx && j == sy)
            include = 1;
         else if (i == sx)
            include = Catalog_test(cx, ty);
         else if (j == sy)
            include = Catalog_test(tx, cy);
         else
            include = Catalog_test(tx, ty);

         if (include) {
            List *ref;

            char keyName[40];
            sprintf(keyName, "%d, %d", i, j);
            ref = SourceMap_get(map, keyName);
            if (ref != 0)
               Catalog_include(spec, ref, out);
         }
      }
   }
}


void Catalog_include (Position *spec, List *ref, List *out)
{
   int i;

   if (debug_test(DEBUG_CATALOG))
      report_verbose("testing %d for inclusion\n", List_size(ref));

   for (i = 0; i < List_size(ref); i++) {
      Source *r = (Source *) List_get(ref, i);
      if (Math_u3cosangle(spec->unit, r->unit) >= mincos)
         List_push(out, r);
   }
}


int Catalog_test (double x, double y)
{
   double test[3];
   test[0] = x;
   test[1] = y;
   test[2] = q;
   Math_v3normalize(test);
   return (Math_u3cosangle(uhat, test) >= mincos);
}


void Catalog_index (List *sources)
{

   int i;
   for (i = 0; i < List_size(sources); i++) {
      Source *s = (Source *) List_get(sources, i);
      Catalog_place(s);
   }

   if (debug_test(DEBUG_CATALOG))
      Catalog_describe();
}


void Catalog_place (Source *s)
{
   List *srcs;

   double ox = Math_v3dot(nx, s->unit);
   double oy = Math_v3dot(ny, s->unit);

   int sx = floor(q * ox);
   int sy = floor(q * oy);

   char keyName[20];
   sprintf(keyName, "%d, %d", sx, sy);

   SourceMap_add(map, keyName, s);

   srcs = SourceMap_get(map, keyName);
}


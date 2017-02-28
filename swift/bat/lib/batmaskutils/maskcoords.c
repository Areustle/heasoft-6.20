#include <stdio.h>
#include <string.h>
#include <math.h>
#include "batmask.h"

/*
 * Parse coordinate type and return numerical value
 *
 * char *coords - coordinate type string from parameter file
 * char coord_name[20] - upon return, normalized coordinate name
 * 
 * RETURNS: Numerical coordinate type, one of
 *    0 - error
 *    BCCARTESIAN BCUNIT BCFSWLONLAT BCGRMC BCSKY BCTANGENT
 */
int bat_coordtype(char *coords, char coord_name[20])
{
  int coord_type = 0;
  if ((coords == 0) || (coords[0] == 0)) return 0;

  if (coord_name) coord_name[0] = 0;

  if (strncasecmp(coords, "cartesian", 9) == 0) {
    coord_type = BCCARTESIAN;
    if (coord_name) strcpy(coord_name, "cartesian");
  } else if (strncasecmp(coords, "unit", 9) == 0) {
    coord_type = BCUNIT;
    if (coord_name) strcpy(coord_name, "unit");
  } else if (strncasecmp(coords, "fswlonlat", 9) == 0) {
    coord_type = BCFSWLONLAT;
    if (coord_name) strcpy(coord_name, "fswlonlat");
  } else if (strncasecmp(coords, "grmclonlat", 9) == 0) {
    coord_type = BCGRMC;
    if (coord_name) strcpy(coord_name, "grmclonlat");
  } else if (strncasecmp(coords, "sky", 9) == 0) {
    coord_type = BCSKY;
    if (coord_name) strcpy(coord_name, "sky");
  } else if (strncasecmp(coords, "tanxy", 5) == 0) {
    coord_type = BCTANGENT;
    if (coord_name) strcpy(coord_name, "tanxy");
  }

  return coord_type;
}


/*
 * Verify and sanitize coordinates read from parameter file
 *
 * Upon return, angular-type coordinates are converted to cartesian.
 * For the BCCARTESIAN coordinate type, the distance is calculated
 * (output); for other coordinate types, the distance is used (input)
 * to compute the cartesian position.
 *
 * int coord_type - coordinate type, as returned by bat_coordtype()
 * double srcpos[3] - (input) BAT coordinates of source from parameter file
 *                    (output) verified coordinates
 * double lonlat[3] - BAT longitude and latitude from parameter file
 * double *distance - (input) distance from parameter file
 *                    (output) computed distance if BCCARTESIAN
 * 
 * RETURNS: -1 - invalid coordinate type
 */
int bat_coordver(int coord_type, 
		 double srcpos[3], double lonlat[2], double *distance)
{
  double lon, lat;
  double x, y, x2y2, z = 0;
  double dtor = 3.1415926535897931e0/180e0;
  
  lon = lonlat[0]; lat = lonlat[1];

  /* Save the Z position for later */
  z = srcpos[2];

  switch(coord_type) {
  case BCUNIT: /* Cartesian unit vectors */
    x = srcpos[0];
    y = srcpos[1];

    x2y2 = x*x + y*y;
    if (x2y2 >= 1) {
      fprintf(stderr, 
	      "Using 'unit' coordinates, ux^2 + uy^2 must not\n"
	      "exceed unity.\n");
	return -1;
      }
    
    /* Form unit vector */
    srcpos[2] = sqrt(1.0 - x2y2);

    break;
  case BCFSWLONLAT: /* Flight software longitude latitude */
    srcpos[0] =  sin(lat*dtor)*cos(lon*dtor);
    srcpos[1] = -sin(lat*dtor)*sin(lon*dtor);
    srcpos[2] =  cos(lat*dtor);
    break;
  case BCGRMC: /* GRMC longitude latitude */
    /* XXX Need to put something here */
    return -1;
    /* fprintf(stderr, "Coordinate type 'grmc' not handled yet\n"); */
    break;
  case BCSKY: /* RA / DEC - compute sky unit vector */
    srcpos[0] =  cos(lat*dtor)*cos(lon*dtor);
    srcpos[1] =  cos(lat*dtor)*sin(lon*dtor);
    srcpos[2] =  sin(lat*dtor);
    break;
  case BCTANGENT: /* Tangent plane coordinates */
    x = srcpos[0];
    y = srcpos[1];

    srcpos[0] = x/sqrt(1 + x*x + y*y);
    srcpos[1] = y/sqrt(1 + x*x + y*y);
    srcpos[2] = 1/sqrt(1 + x*x + y*y);
    
    break;
  }

  /* Scale by the Z distance -- but not for sky coordinates, because
     that needs to be scaled after rotating into the BAT frame */
  if ((coord_type != BCSKY) && (srcpos[2] > 0) && (z > 0)) {
    srcpos[0] *= z/srcpos[2];
    srcpos[1] *= z/srcpos[2];
    srcpos[2] *= z/srcpos[2];
  }

  /* Compute the distance if we know the source position... */
  *distance = sqrt(srcpos[0]*srcpos[0] + srcpos[1]*srcpos[1] + 
		   srcpos[2]*srcpos[2]);

  return 0;
}


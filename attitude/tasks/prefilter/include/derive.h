#ifndef PREFILTER_DERIVE_H
#define PREFILTER_DERIVE_H

/*
 * $Source: /headas/headas/attitude/tasks/prefilter/include/derive.h,v $
 * $Revision: 1.14 $
 * $Date: 2016/10/25 20:00:23 $
 *
 *
 * $Log: derive.h,v $
 * Revision 1.14  2016/10/25 20:00:23  rwiegand
 * Add timeadj parameter. Swift is the instigater: timeadj is a mechanism to address its clock drift. Satellite positions in particular are sensitive to propagating the right amount of time.
 *
 * Revision 1.13  2011/06/30 18:18:44  miket
 * Changes from Craig Markwardt to change "McIlwain L" to use SAX algorithm
 *
 * Revision 1.12  2005/09/14 21:41:15  rwiegand
 * Deleted local copy of report.[ch] and updated report calling sequence.
 * Pruned unused mean alignment structure.
 *
 * Revision 1.11  2004/12/30 22:56:26  rwiegand
 * Implemented writing NULLs for parameters which can not be calculated
 * at particular timestamps because of missing dependencies (e.g., pointing
 * vector cannot be found without attitude).  Require an attitude record
 * within attextrap of output record time or set attitude (and attitude
 * dependent parameters) to NULL.
 *
 * Revision 1.10  2004/12/24 18:48:52  rwiegand
 * Added sun/moon/earth RA/Dec to set of available output parameters.
 *
 * Revision 1.9  2004/02/02 15:54:06  rwiegand
 * Now there is a single vector defining the current pointing derived from
 * the alignment file parameter.
 * There used to be fields defined in terms of the primary spacecraft axis
 * and others in terms of the instrument boresight.
 *
 * Revision 1.8  2003/03/31 15:02:16  rwiegand
 * Added missing typedef to PrefilterOrbitMode enumeration.
 *
 * Revision 1.7  2003/01/22 17:32:43  rwiegand
 * Added parameter for writing HISTORY keywords to output.  Fixed conversion
 * of string to AtTime.
 *
 * Revision 1.6  2002/12/06 20:14:21  rwiegand
 * Added Filter object to pass function pointers for iteration, initialization,
 * status checking.  Broke derive.c into separate files for FORTRAN calling
 * routines, iteration, initialization.  Made compare mode less XTE-centric.
 * Added parameters for pointing axis and boresight.  Allow loading two line
 * elements from FITS or text files.
 *
 * Revision 1.5  2002/05/14 14:51:58  rwiegand
 * Reworked parameter interface with Ed Pier's suggestions.
 *
 * Revision 1.4  2002/03/25 13:46:55  rwiegand
 * Maintain boresight vector (instead of quaternion).  For XTE comparison,
 * calculate boresight separation from nominal instead of spacecraft axis.
 *
 * Revision 1.3  2002/03/21 19:40:08  rwiegand
 * Provided access to Arguments from Context instead of copying over data.
 * Added derivation of deviation from nominal pointing.
 *
 * Revision 1.2  2002/03/15 16:18:54  rwiegand
 * Added XTE comparison mode and clobber parameter.  Differentiate SAX and
 * ASCA derivations of cut off rigidity.  Provide several McIlwain L
 * calculations.
 *
 * Revision 1.1  2002/02/20 16:21:40  wiegand
 * Initial revision
 *
 * Revision 1.5  2002/02/01 23:34:58  rwiegand
 * Added modified julian day (of timestamp) to Context.  Added geodetic
 * to Derived.
 *
 * Revision 1.4  2002/02/01 15:09:37  rwiegand
 * Use term mission time instead of mission elapsed time
 *
 * Revision 1.3  2002/01/31 21:59:28  rwiegand
 * Added FRF orbit mode
 *
 * Revision 1.2  2002/01/31 13:59:18  rwiegand
 * Renamed tool prefilter.  Added mission elapsed time parameter.  Added
 * output parameter listing file and attitude extrapolation limit arguments.
 *
 * Revision 1.1  2002/01/28 19:20:14  rwiegand
 * Initial revision
 *
 */


#include "prefilter.h"

#include "fitsio.h"           /* FITS interface */
#include "tle.h"              /* prefilter TLE interface */
#include "quat.h"             /* coord library */
#include "attfile.h"          /* coordfits library */
#include "align.h"            /* coordfits library */
#include "atFunctions.h"      /* atFunctions library */
#include "compare.h"


typedef enum
{
	ORBIT_TLE_FITS,
	ORBIT_TLE_TEXT2,
	ORBIT_TLE_TEXT3,
	ORBIT_atSetElement,
	ORBIT_atSetElement2,
	ORBIT_COMPARE,
	ORBIT_DUMMY
} PrefilterOrbitMode;


typedef struct
{
	int count;
	int allocated;
	Iterator ** functions;
} Iteration;


typedef struct
{
	double pi;

	double earthRadius;        /* kilometers */

	/* FITS nulls */
	double nulle;
	float nullf;
	short nulli;

} Constants;


struct Arguments
{
	int code;

	/* raw arguments */
	const char * outname;   /* name of FITS output file */
	const char * extname;   /* name of FITS output table extension */
	const char * columns;   /* parameters to output
	                             ALL, @<filename>, comma separated list */

	const char * attname;   /* name of FITS attitude file */
	const char * alignfile; /* name of FITS attitude alignment file */
	const char * leapname;  /* name of FITS leap second file */
	const char * rigname;   /* name of atFunctions rigidity file */
	const char * compareColumns;
	                        /* comma delimited names of compare column names */

	int orbitMode;         /* PrefilterOrbitMode */
	const char * orbname;   /* name of orbit file */

	double start;          /* output start in mission time */
	double end;            /* output end in mission time */
	double interval;       /* output interval */

	const char * missepoch; /* mission epoch yyyy-mm-ddThh:mm:ss.fraction */
	const char * timeadj;   /* time adjustment */

	/* TODO: add limits to extrapolation of orbit? */
	double attitudeExtrapolation;
	                       /* attitude extrapolation limit (seconds) */

	const char * origin;    /* value for FITS ORIGIN keyword */
	double nominalRightAscension;
	                       /* nominal right ascension (degrees) */
	double nominalDeclination;
	                       /* nominal declination (degrees) */


	/* cooked arguments */
	int verbose;

	char ** parameters;    /* (null terminated) list of parameters
	                             to derive/output */

	double mjd0_utc;       /* missepoch converted to MJD */

	AtVect pointingAxis;   /* */
	AtVect bodyBoresight;  /* boresight vector in body frame */

	int clobber;
	int history;
	int compareApplyQuaternion;

	ATTFILE * attfile;
	ALIGN * align;

	/* access to related objects */
	Context * context;
	Derived * derived;
	Output * output;
	Constants * constants;
	Iteration * iteration;
	Compare * compare;
};


/*
 * The Context holds the information needed to support derivation
 * at a given timestamp.
 * Changes to this structure are not expected, but are conceivable.  Please
 * consider whether changes are more appropriate to Derived.
 */

struct Context
{
	int code;

	/* iteration data */
	int first;
	double time_offset;    /* seconds to add to TIME[raw] to correct TIME */

	double time_raw;
	double time_adj;       /* time_raw + time_offset */
	double mjd;            /* mjd0_utc + (time_raw + time_offset) / 86400.0 */
	AtTime timestamp;      /* mjd_utc -> AtTime for use by atFunctions */
	char timestampString[64];

	AtVect position;
	AtVect velocity;
	AtQuat quaternion;

	int havePosition;
	int haveVelocity;
	int haveQuaternion;

	/* work data */
	tlesgp_t tle;
	QUAT * coordquat;

	const Arguments * args;
};


/*
 * The Derived structure holds what is calculated at a given timestamp
 */

struct Derived
{
	int code;
	const Context * context;

	double time_raw;
	double time_adj;

	AtVect position;
	AtVect velocity;
	AtQuat quaternion;

	AtVect pointing;

	AtVect geodetic;
	AtPolarVect polarPosition;

	double rightAscension;
	double declination;
	double roll;

	double latitude;
	double longitude;
	double altitude;

	double earthLimbAngle;
	double brightEarthAngle;
	short sunshine;
	short fovFlag;

	double sunAngle;
	double moonAngle;
	double ramAngle;

	double sunRA, sunDec;
	double moonRA, moonDec;
	double earthRA, earthDec;

	double pointingSeparation;

	double cutOffRigiditySAX;
	double cutOffRigidityASCA;
	double mcIlwainLc;
	double mcIlwainLg;
	double xteMcIlwainLc;
        double mcIlwainL_sax;

	short inSAA;
	short lastSAA;
	double timeSinceSAA;
	double timeChangeSAA;

	/* work data */
	QUAT * coordquat;
	QUAT * alignquat;
};



int prefilter_dispatch (Arguments * args);

int add_iterator (Iteration * i, Iterator * f);


Derivation derive_context;
Derivation derive_mission_time;
Derivation derive_pointing_vector;
Derivation derive_polar_position;
Derivation derive_ra_dec_roll;
Derivation derive_boresight;
Derivation derive_earth_angles;
Derivation derive_geodetic;
Derivation derive_sun_angle;
Derivation derive_moon_angle;
Derivation derive_ram_angle;
Derivation derive_nominal_pointing_separation;
Derivation derive_nominal_boresight_separation;
Derivation derive_saa;
Derivation derive_earth_radec;
Derivation derive_sun_radec;
Derivation derive_moon_radec;

/* mission dependent derivations */
Derivation swift_derive_mcilwain_l;
Derivation asca_derive_cut_off_rigidity;
Derivation sax_derive_cut_off_rigidity;
Derivation xte_derive_mcilwain_l;


int update_keywords (const Arguments * args, Filter * filter);

void iterate_error (const Arguments * args, Context * context,
		const char * module, const char * function);
void context_error (Context * context,
		const char * module, const char * function);
void derive_error (Derived * derived,
		const char * module, const char * function);

int get_argument_code (const Arguments * args);
int get_context_code (const Context * context);
int get_derived_code (const Derived * derived);


double met_to_mjd_utc(const Arguments *args, double met);
double met_to_yyyyddd_fraction(const Arguments *args, double met);
int met_to_AtTime(const Arguments *args, double met, AtTime *at);


#endif


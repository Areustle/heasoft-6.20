/*
 * $Source: /headas/headas/attitude/tasks/prefilter/initialize.c,v $
 * $Revision: 1.15 $
 * $Date: 2016/10/27 17:38:14 $
 *
 * $Log: initialize.c,v $
 * Revision 1.15  2016/10/27 17:38:14  rwiegand
 * When resolving timeadj, handle non-FITS attitude or missing TELESCOP keyword by falling back to LEAPS
 *
 * Revision 1.14  2016/10/25 20:00:22  rwiegand
 * Add timeadj parameter. Swift is the instigater: timeadj is a mechanism to address its clock drift. Satellite positions in particular are sensitive to propagating the right amount of time.
 *
 * Revision 1.13  2011/06/30 18:18:44  miket
 * Changes from Craig Markwardt to change "McIlwain L" to use SAX algorithm
 *
 * Revision 1.12  2005/09/14 21:41:12  rwiegand
 * Deleted local copy of report.[ch] and updated report calling sequence.
 * Pruned unused mean alignment structure.
 *
 * Revision 1.11  2005/01/10 21:39:00  rwiegand
 * Removed PNT_ prefix from RA, DEC, ROLL outputs; renamed POINTING to PNTUNIT
 * to avoid conflict with attitude history file.  Derive SUNSHINE flag
 * before earth angles to avoid unnecessary NULLs.
 *
 * Revision 1.10  2004/12/30 22:56:26  rwiegand
 * Implemented writing NULLs for parameters which can not be calculated
 * at particular timestamps because of missing dependencies (e.g., pointing
 * vector cannot be found without attitude).  Require an attitude record
 * within attextrap of output record time or set attitude (and attitude
 * dependent parameters) to NULL.
 *
 * Revision 1.9  2004/12/24 18:48:52  rwiegand
 * Added sun/moon/earth RA/Dec to set of available output parameters.
 *
 * Revision 1.8  2004/05/11 18:45:48  rwiegand
 * Indicate interval between records in DELTAT keyword.  Indicate that
 * POSITION/VELOCITY vectors hold X,Y,Z.
 *
 * Revision 1.7  2004/02/02 15:53:57  rwiegand
 * Now there is a single vector defining the current pointing derived from
 * the alignment file parameter.
 * There used to be fields defined in terms of the primary spacecraft axis
 * and others in terms of the instrument boresight.
 *
 * Revision 1.6  2003/03/31 14:42:13  rwiegand
 * Was failing to capture error code during setup.  Deleted dead code.
 *
 * Revision 1.5  2003/02/03 15:24:06  rwiegand
 * Fixed orbit mode string constants to match parameter file.  Reworked loading
 * of text TLEs so individual invalid records cause warnings instead of errors.
 * Indicate which record(s) in TLE file are invalid.
 *
 * Revision 1.4  2003/01/22 17:32:43  rwiegand
 * Added parameter for writing HISTORY keywords to output.  Fixed conversion
 * of string to AtTime.
 *
 * Revision 1.3  2003/01/09 21:25:40  rwiegand
 * Put errors on stderr instead of stdout.  Fixed conversion of 2 digit to 4
 * digit years (thanks Ed).  More informative error message when satellite
 * is below minimum altitude.
 *
 * Revision 1.2  2002/12/06 20:59:34  rwiegand
 * Implemented loading of TLEs from text files.
 *
 * Revision 1.1  2002/12/06 20:14:21  rwiegand
 * Added Filter object to pass function pointers for iteration, initialization,
 * status checking.  Broke derive.c into separate files for FORTRAN calling
 * routines, iteration, initialization.  Made compare mode less XTE-centric.
 * Added parameters for pointing axis and boresight.  Allow loading two line
 * elements from FITS or text files.
 *
 */


#include <math.h>
#include <string.h>
#include <errno.h>

#include "prefilter.h"
#include "derive.h"
#include "report.h"
#include "fitsio.h"
#include "tle.h"
#include "tleio.h"
#include "iterate.h"
#include "convert.h"
#include "caldbquery.h"
#include "headas.h"
#include "keyutil.h"
#include "misstime.h"


static int filter_initialization (Arguments * args, Filter * filter);
static int filter_finalization (Arguments * args, Filter * filter);

static Iterator initialize_derive_ra_dec_roll;
static Iterator initialize_derive_saa;
static Iterator initialize_asca_derive_cor;
extern Iterator initialize_swift_derive_mcilwain_l;


void initialize_constants (Constants * constants)
{
	constants->pi = 4 * atan(1);

	constants->earthRadius = 6378.14; /* mean equatorial, IAU, 1976 */
	constants->earthRadius = 6371.01; /* mean global +/-0.02, AGU, 1995 */
	constants->earthRadius = 6371.2;  /* match value in *.f */

	constants->nulle = -999;
	constants->nullf = -999;
	constants->nulli = -999;
}


/*
 * entry point called by driver
 * allocates Context, Derived, Output on stack
 */

int prefilter_dispatch (Arguments * args)
{
	Filter filter = { 0 };

	Context context = { 0 };
	Derived derived = { 0 };
	Output output = { 0 };
	Constants constants = { 0 };
	Iteration iteration = { 0 };
	Compare compare = { 0 };

	initialize_constants(&constants);

	filter.arguments = args;
	filter.context = &context;
	filter.derived = &derived;
	filter.output = &output;

	/* function pointers */
	filter.initialization = filter_initialization;
	filter.finalization = filter_finalization;
	filter.iterate = iterate_context;

	filter.argument_code = get_argument_code;
	filter.context_code = get_context_code;
	filter.derived_code = get_derived_code;

	args->context = &context;
	args->derived = &derived;
	args->output = &output;
	args->constants = &constants;
	args->iteration = &iteration;
	args->compare = &compare;

	context.args = args;
	derived.context = &context;

	context.first = 1;

	args->code = prefilter_execute(&filter);

	return args->code;
}


int filter_finalization (Arguments * args, Filter * filter)
{
	if (!args->code && !filter_code(filter))
		update_keywords(args, filter);

	/* release ATTFILE */
	if (args->attfile)
		{
			closeAttFile(args->attfile);
			args->attfile = 0;
		}

	/* release QUAT */
	if (args->context->coordquat)
		{
			destroyQuat(args->context->coordquat);
			args->context->coordquat = 0;
		}

	/* release output parameter (strings) */
	if (args->parameters)
		{
			char ** pp;
			for (pp = args->parameters; *pp; ++pp)
				free(*pp);
			args->parameters = 0;
		}

	return args->code;
}


static Keyword * create_null (const Arguments * args, int type)
{
	const Constants * constants = args->constants;

	const void * pvoid = 0;
	Keyword * keyword = 0;
	switch (type)
		{
			case TDOUBLE:
				pvoid = &constants->nulle;
				break;

			case TFLOAT:
				pvoid = &constants->nullf;
				break;

			case TSHORT:
				pvoid = &constants->nulli;
				break;

			default:
				report_warning("invalid type [%d] for TNULLn\n",
							type);
		}

	if (pvoid)
		keyword = create_keyword(type, "TNULLn", pvoid, 0);

	return keyword;
}


static int register_parameters (Arguments * args, Filter * filter)
{
	Parameter * pointing = 0;
	Parameter * polarPosition = 0;

	if (!filter_code(filter))
		{
			Parameter * p = create_parameter("TIME");

			p->derivation    = derive_mission_time;
			p->derivedType   = PREFILTER_DOUBLE;
			p->derivedOffset = PREFILTER_OFFSET(time_raw);

			p->fitsType      = TDOUBLE;
			p->fitsUnits     = "s";
			p->fitsComment   = "seconds since mission epoch";

			define_parameter(filter, p);
		}

	if (!filter_code(filter))
		{
			Parameter * p = create_parameter("POSITION");

			p->derivation    = derive_context;
			p->derivedType   = PREFILTER_DOUBLE;
			p->derivedOffset = PREFILTER_OFFSET(position);

			p->fitsType      = TFLOAT;
			p->fitsUnits     = "km";
			p->fitsArray     = 3;
			p->fitsComment   = "ECI position of satellite [X,Y,Z]";
			p->fitsNull      = create_null(args, p->fitsType);

			define_parameter(filter, p);
		}

	if (!filter_code(filter))
		{
			Parameter * p = create_parameter("VELOCITY");

			p->derivation    = derive_context;
			p->derivedType   = PREFILTER_DOUBLE;
			p->derivedOffset = PREFILTER_OFFSET(velocity);

			p->fitsType      = TFLOAT;
			p->fitsUnits     = "km/s";
			p->fitsArray     = 3;
			p->fitsComment   = "ECI velocity of satellite [X,Y,Z]";
			p->fitsNull      = create_null(args, p->fitsType);

			define_parameter(filter, p);
		}

	if (!filter_code(filter))
		{
			Parameter * p = create_parameter("QUATERNION");

			p->derivation    = derive_context;
			p->derivedType   = PREFILTER_DOUBLE;
			p->derivedOffset = PREFILTER_OFFSET(quaternion);

			p->fitsType      = TFLOAT;
			p->fitsArray     = 4;
			p->fitsComment   = "attitude quaternion";
			p->fitsNull      = create_null(args, p->fitsType);

			define_parameter(filter, p);
		}

	if (!filter_code(filter))
		{
			Parameter * p = create_parameter("PNTUNIT");

			p->derivation    = derive_pointing_vector;
			p->derivedType   = PREFILTER_DOUBLE;
			p->derivedOffset = PREFILTER_OFFSET(pointing);

			p->fitsType      = TFLOAT;
			p->fitsArray     = 3;
			p->fitsComment   = "pointing unit vector";
			p->fitsNull      = create_null(args, p->fitsType);

			define_parameter(filter, p);

			pointing = p;
		}

	if (!filter_code(filter))
		{
			Parameter * p = create_parameter("POLAR");

			p->derivation    = derive_polar_position;
			p->derivedType   = PREFILTER_DOUBLE;
			p->derivedOffset = PREFILTER_OFFSET(polarPosition);

			p->fitsType      = TFLOAT;
			p->fitsUnits     = "rad, rad, km";
			p->fitsArray     = 3;
			p->fitsComment   = "geodetic";

			define_parameter(filter, p);

			polarPosition = p;
		}

	if (!filter_code(filter))
		{
			Parameter * p = create_parameter("RA");

			p->derivation    = derive_ra_dec_roll;
			p->initialization = initialize_derive_ra_dec_roll;
			p->derivedType   = PREFILTER_DOUBLE;
			p->derivedOffset = PREFILTER_OFFSET(rightAscension);

			p->fitsType      = TFLOAT;
			p->fitsUnits     = "deg";
			p->fitsComment   = "pointing axis right ascension";
			p->fitsNull      = create_null(args, p->fitsType);

			define_parameter(filter, p);
		}

	if (!filter_code(filter))
		{
			Parameter * p = create_parameter("DEC");

			p->derivation    = derive_ra_dec_roll;
			p->initialization = initialize_derive_ra_dec_roll;
			p->derivedType   = PREFILTER_DOUBLE;
			p->derivedOffset = PREFILTER_OFFSET(declination);

			p->fitsType      = TFLOAT;
			p->fitsUnits     = "deg";
			p->fitsComment   = "pointing axis declination";
			p->fitsNull      = create_null(args, p->fitsType);

			define_parameter(filter, p);
		}

	if (!filter_code(filter))
		{
			Parameter * p = create_parameter("ROLL");

			p->derivation    = derive_ra_dec_roll;
			p->initialization = initialize_derive_ra_dec_roll;
			p->derivedType   = PREFILTER_DOUBLE;
			p->derivedOffset = PREFILTER_OFFSET(roll);

			p->fitsType      = TFLOAT;
			p->fitsUnits     = "deg";
			p->fitsComment   = "pointing axis roll";
			p->fitsNull      = create_null(args, p->fitsType);

			define_parameter(filter, p);
		}

	if (!filter_code(filter))
		{
			Parameter * p = create_parameter("SAT_LAT");

			p->derivation    = derive_polar_position;
			p->derivedType   = PREFILTER_DOUBLE;
			p->derivedOffset = PREFILTER_OFFSET(latitude);

			p->fitsType      = TFLOAT;
			p->fitsUnits     = "deg";
			p->fitsComment   = "sub-satellite point latitude";

			define_parameter(filter, p);
		}

	if (!filter_code(filter))
		{
			Parameter * p = create_parameter("SAT_LON");

			p->derivation    = derive_polar_position;
			p->derivedType   = PREFILTER_DOUBLE;
			p->derivedOffset = PREFILTER_OFFSET(longitude);

			p->fitsType      = TFLOAT;
			p->fitsUnits     = "deg";
			p->fitsComment   = "sub-satellite point longitude";

			define_parameter(filter, p);
		}

	if (!filter_code(filter))
		{
			Parameter * p = create_parameter("SAT_ALT");

			p->derivation    = derive_polar_position;
			p->derivedType   = PREFILTER_DOUBLE;
			p->derivedOffset = PREFILTER_OFFSET(altitude);

			p->fitsType      = TFLOAT;
			p->fitsUnits     = "km";
			p->fitsComment   = "distance from earth center";

			define_parameter(filter, p);
		}

	if (!filter_code(filter))
		{
			Parameter * p = create_parameter("ELV");

			Parameter ** list = create_dependency_list(1);
			list[0] = pointing;
			p->dependencies = list;

			p->derivation    = derive_earth_angles;
			p->derivedType   = PREFILTER_DOUBLE;
			p->derivedOffset = PREFILTER_OFFSET(earthLimbAngle);

			p->fitsType      = TFLOAT;
			p->fitsUnits     = "deg";
			p->fitsComment   = "angle between pointing and earth limb";
			p->fitsNull      = create_null(args, p->fitsType);

			define_parameter(filter, p);
		}

	if (!filter_code(filter))
		{
			Parameter * p = create_parameter("BR_EARTH");

			Parameter ** list = create_dependency_list(1);
			list[0] = pointing;
			p->dependencies = list;

			p->derivation    = derive_earth_angles;
			p->derivedType   = PREFILTER_DOUBLE;
			p->derivedOffset = PREFILTER_OFFSET(brightEarthAngle);

			p->fitsType      = TFLOAT;
			p->fitsUnits     = "deg";
			p->fitsComment   = "angle between pointing and bright earth";
			p->fitsNull      = create_null(args, p->fitsType);

			define_parameter(filter, p);
		}

	if (!filter_code(filter))
		{
			Parameter * p = create_parameter("SUNSHINE");

			p->derivation    = derive_earth_angles;
			p->derivedType   = PREFILTER_SHORT;
			p->derivedOffset = PREFILTER_OFFSET(sunshine);

			p->fitsType      = TSHORT;
			p->fitsComment   = "1=in sunshine; 0=not";

			define_parameter(filter, p);
		}

	if (!filter_code(filter))
		{
			Parameter * p = create_parameter("FOV_FLAG");

			Parameter ** list = create_dependency_list(1);
			list[0] = pointing;
			p->dependencies = list;

			p->derivation    = derive_earth_angles;
			p->derivedType   = PREFILTER_SHORT;
			p->derivedOffset = PREFILTER_OFFSET(fovFlag);

			p->fitsType      = TSHORT;
			p->fitsComment   = "0=sky; 1=dark earth; 2=bright earth";
			p->fitsNull      = create_null(args, p->fitsType);

			define_parameter(filter, p);
		}

	if (!filter_code(filter))
		{
			Parameter * p = create_parameter("SUN_ANGLE");

			Parameter ** list = create_dependency_list(1);
			list[0] = pointing;
			p->dependencies = list;

			p->derivation    = derive_sun_angle;
			p->derivedType   = PREFILTER_DOUBLE;
			p->derivedOffset = PREFILTER_OFFSET(sunAngle);

			p->fitsType      = TFLOAT;
			p->fitsUnits     = "deg";
			p->fitsComment   = "angle between pointing and sun vector";
			p->fitsNull      = create_null(args, p->fitsType);

			define_parameter(filter, p);
		}

	if (!filter_code(filter))
		{
			Parameter * p = create_parameter("MOON_ANGLE");

			Parameter ** list = create_dependency_list(1);
			list[0] = pointing;
			p->dependencies = list;

			p->derivation    = derive_moon_angle;
			p->derivedType   = PREFILTER_DOUBLE;
			p->derivedOffset = PREFILTER_OFFSET(moonAngle);

			p->fitsType      = TFLOAT;
			p->fitsUnits     = "deg";
			p->fitsComment   = "angle between pointing and moon vector";
			p->fitsNull      = create_null(args, p->fitsType);

			define_parameter(filter, p);
		}

	if (!filter_code(filter))
		{
			Parameter * p = create_parameter("RAM_ANGLE");

			Parameter ** list = create_dependency_list(1);
			list[0] = pointing;
			p->dependencies = list;

			p->derivation    = derive_ram_angle;
			p->derivedType   = PREFILTER_DOUBLE;
			p->derivedOffset = PREFILTER_OFFSET(ramAngle);

			p->fitsType      = TFLOAT;
			p->fitsUnits     = "deg";
			p->fitsComment   = "angle between pointing and velocity vector";
			p->fitsNull      = create_null(args, p->fitsType);

			define_parameter(filter, p);
		}

	if (!filter_code(filter))
		{
			Parameter * p = create_parameter("ANG_DIST");

			Parameter ** list = create_dependency_list(1);
			list[0] = pointing;
			p->dependencies = list;

			p->derivation    = derive_nominal_pointing_separation;
			p->derivedType   = PREFILTER_DOUBLE;
			p->derivedOffset = PREFILTER_OFFSET(pointingSeparation);

			p->fitsType      = TFLOAT;
			p->fitsUnits     = "deg";
			p->fitsComment   = "angular distance of pointing from nominal";
			p->fitsNull      = create_null(args, p->fitsType);

			define_parameter(filter, p);
		}

	if (!filter_code(filter))
		{
			Parameter * p = create_parameter("SAA");

			Parameter ** list = create_dependency_list(1);
			list[0] = polarPosition;
			p->dependencies = list;

			p->derivation    = derive_saa;
			p->initialization = initialize_derive_saa;
			p->derivedType   = PREFILTER_SHORT;
			p->derivedOffset = PREFILTER_OFFSET(inSAA);

			p->fitsType      = TSHORT;
			p->fitsUnits     = "";
			p->fitsComment   = "1=in SAA; 0=not";

			define_parameter(filter, p);
		}

	if (!filter_code(filter))
		{
			Parameter * p = create_parameter("SAA_TIME");

			Parameter ** list = create_dependency_list(1);
			list[0] = polarPosition;
			p->dependencies = list;

			p->derivation    = derive_saa;
			p->derivedType   = PREFILTER_DOUBLE;
			p->derivedOffset = PREFILTER_OFFSET(timeSinceSAA);

			p->fitsType      = TFLOAT;
			p->fitsUnits     = "s";
			p->fitsComment   = "time since entering/exiting SAA";

			define_parameter(filter, p);
		}

	if (!filter_code(filter))
		{
			Parameter * p = create_parameter("COR_ASCA");

			Parameter ** list = create_dependency_list(1);
			list[0] = polarPosition;
			p->dependencies = list;

			p->initialization = initialize_asca_derive_cor;
			p->derivation    = asca_derive_cut_off_rigidity;
			p->derivedType   = PREFILTER_DOUBLE;
			p->derivedOffset = PREFILTER_OFFSET(cutOffRigidityASCA);

			p->fitsType      = TFLOAT;
			p->fitsUnits     = "GeV/c";
			p->fitsComment   = "magnetic cut off rigidity";

			define_parameter(filter, p);
		}

	if (!filter_code(filter))
		{
			Parameter * p = create_parameter("COR_SAX");

			p->derivation    = sax_derive_cut_off_rigidity;
			p->derivedType   = PREFILTER_DOUBLE;
			p->derivedOffset = PREFILTER_OFFSET(cutOffRigiditySAX);

			p->fitsType      = TFLOAT;
			p->fitsUnits     = "GeV/c";
			p->fitsComment   = "magnetic cut off rigidity";

			define_parameter(filter, p);
		}


	if (!filter_code(filter))
		{
			Parameter * p = create_parameter("MCILWAIN_L");

			p->derivation    = sax_derive_cut_off_rigidity;
			p->derivedType   = PREFILTER_DOUBLE;
			p->derivedOffset = PREFILTER_OFFSET(mcIlwainL_sax);

			p->fitsType      = TFLOAT;
			p->fitsComment   = "McIlwain L parameter (SAX)";

			define_parameter(filter, p);
		}

	if (!filter_code(filter))
		{
			Parameter * p = create_parameter("SUN_RA");

			p->derivation    = derive_sun_radec;
			p->derivedType   = PREFILTER_DOUBLE;
			p->derivedOffset = PREFILTER_OFFSET(sunRA);

			p->fitsType      = TFLOAT;
			p->fitsUnits     = "deg";
			p->fitsComment   = "RA of sun (equatorial)";

			define_parameter(filter, p);
		}

	if (!filter_code(filter))
		{
			Parameter * p = create_parameter("SUN_DEC");

			p->derivation    = derive_sun_radec;
			p->derivedType   = PREFILTER_DOUBLE;
			p->derivedOffset = PREFILTER_OFFSET(sunDec);

			p->fitsType      = TFLOAT;
			p->fitsUnits     = "deg";
			p->fitsComment   = "Dec of sun (equatorial)";

			define_parameter(filter, p);
		}

	if (!filter_code(filter))
		{
			Parameter * p = create_parameter("MOON_RA");

			p->derivation    = derive_moon_radec;
			p->derivedType   = PREFILTER_DOUBLE;
			p->derivedOffset = PREFILTER_OFFSET(moonRA);

			p->fitsType      = TFLOAT;
			p->fitsUnits     = "deg";
			p->fitsComment   = "RA of moon (equatorial)";

			define_parameter(filter, p);
		}

	if (!filter_code(filter))
		{
			Parameter * p = create_parameter("MOON_DEC");

			p->derivation    = derive_moon_radec;
			p->derivedType   = PREFILTER_DOUBLE;
			p->derivedOffset = PREFILTER_OFFSET(moonDec);

			p->fitsType      = TFLOAT;
			p->fitsUnits     = "deg";
			p->fitsComment   = "Dec of moon (equatorial)";

			define_parameter(filter, p);
		}

	if (!filter_code(filter))
		{
			Parameter * p = create_parameter("EARTH_RA");

			p->derivation    = derive_earth_radec;
			p->derivedType   = PREFILTER_DOUBLE;
			p->derivedOffset = PREFILTER_OFFSET(earthRA);

			p->fitsType      = TFLOAT;
			p->fitsUnits     = "deg";
			p->fitsComment   = "RA of earth (equatorial)";

			define_parameter(filter, p);
		}

	if (!filter_code(filter))
		{
			Parameter * p = create_parameter("EARTH_DEC");

			p->derivation    = derive_earth_radec;
			p->derivedType   = PREFILTER_DOUBLE;
			p->derivedOffset = PREFILTER_OFFSET(earthDec);

			p->fitsType      = TFLOAT;
			p->fitsUnits     = "deg";
			p->fitsComment   = "Dec of earth (equatorial)";

			define_parameter(filter, p);
		}

	if (!filter_code(filter))
		{
			Parameter * p = create_parameter("TIME_ADJ");

			p->derivation    = derive_mission_time;
			p->derivedType   = PREFILTER_DOUBLE;
			p->derivedOffset = PREFILTER_OFFSET(time_adj);

			p->fitsType      = TDOUBLE;
			p->fitsUnits     = "s";
			p->fitsComment   = "adjusted seconds since mission epoch";

			define_parameter(filter, p);
		}

	return args->code;
}


static int initialize_atElement (Arguments * args)
{
	char filename[FLEN_FILENAME];
	double mjd;
	int (* atFunction)(char *, double, int) = 0;

	strcpy(filename, args->orbname);

	mjd = met_to_mjd_utc(args, args->start);

	if (args->orbitMode == ORBIT_atSetElement)
		atFunction = atSetElement;
	else if (args->orbitMode == ORBIT_atSetElement2)
		atFunction = atSetElement2;

	if ((*atFunction)(filename, mjd, 1))
		{
			args->code = PREFILTER_SETUP_ERROR;
			report_error("unable to initialize atSetElementX from %s at mjd %f\n",
					args->orbname, mjd);
		}

	if (!args->code && args->verbose)
		{
			report_verbose("initialized atElement from %s at %f\n",
					args->orbname, mjd);
		}

	return args->code;
}


static void set_tle (tlesgp_t * sgp, const tleconv_t * conv)
{
	/* these numbers are in TLE units */
	sgp->raw = 1;
	sgp->deepspace = 0;

	sgp->epoch  = conv->epoch;
	sgp->xndt2o = conv->motion2dt;
	sgp->xndd6o = conv->motion6d2t;
	sgp->bstar  = conv->bstar;
	sgp->xincl  = conv->inclination;
	sgp->xnodeo = conv->raan;
	sgp->eo     = conv->eccentricity;
	sgp->omegao = conv->argperigee;
	sgp->xmo    = conv->meananomaly;
	sgp->xno    = conv->meanmotion;
}


/*
 * Select TLE from those available based on timestamp.
 * Current logic chooses the latest TLE before the derired timestamp.
 * Alternatively, could choose the nearest TLE.
 */

static int initialize_tle_text (Arguments * args)
{
	FILE *fp = 0;
	tleraw_t raw;
	tleconv_t conv;
	const char * filename = args->orbname;
	int status = 0;
	int flags = 0;
	int record = 0;
	double bestTimestamp = 0;
	double latest = met_to_yyyyddd_fraction(args, args->start);

	fp = fopen(filename, "rt");
	if (!fp)
		{
			status = PREFILTER_SETUP_ERROR;
			report_error("unable to open %s [%s]\n",
				filename, strerror(errno));
		}

	/*
	 * set the UNNAMED flag if the two line elements will not have
	 * an accompanying identifier line
	 */
	if (args->orbitMode == ORBIT_TLE_TEXT2)
		flags |= TLE_UNNAMED;

	while (status != TLE_NOT_FOUND)
		{
			status = read_tle_raw(fp, &raw, flags);
			++record;

			if (raw.ok)
				{
					double realEpoch;
					status = tle_convert(&raw, &conv, flags);
					realEpoch = conv.epoch < 57e3
						? conv.epoch + 2000e3
						: conv.epoch + 1900e3;

					if (!status
							&& (realEpoch > bestTimestamp)
							&& (realEpoch <= latest))
						{
							bestTimestamp = realEpoch;
							set_tle(&args->context->tle, &conv);
						}
				}
			else if (status != TLE_NOT_FOUND)
				report_warning("bad TLE record %d\n", record);
		}

	if (fp)
		fclose(fp);

	if (bestTimestamp == 0)
		{
			status = PREFILTER_SETUP_ERROR;
		  if (!args->code)
				args->code = status;
			report_error("no appropriate TLE found in %s\n", filename);
		}
	else if (args->verbose)
		print_tle(stdout, &args->context->tle, "raw elements");

	return args->code;
}


static int initialize_tle_fits (Arguments * args)
{
	int status = 0;
	fitsfile * fptr = 0;
	long rows;						/* number of rows in TLE table */
	long selected = -1;		/* row of selected TLE, -1 for none */

	if (!args->code)
		{
			char buffer[FLEN_FILENAME];

			sprintf(buffer, "%s[%s]", args->orbname, TLE_EXTNAME);

			fits_open_file(&fptr, buffer, READONLY, &status);

			if (status)
				{
					args->code = PREFILTER_SETUP_ERROR;
					report_error("unable to open TLE extension of FITS file [%s]\n",
							buffer);
				}
		}

	if (!args->code)
		{
			int fields;
			long count;

			fits_read_btblhdr(fptr, 1, &rows, &fields, 0, 0, 0, 0, &count, &status);

			if (fields != TLE_COL_DUMMY - 1)
				{
					args->code = PREFILTER_SETUP_ERROR;
					report_error("wrong number of fields [%d] in TLE table [expected %d]\n",
							fields, TLE_COL_DUMMY - 1);
				}
		}

	if (!args->code)
		{
			/* select the row containing the desired TLE */
			long row;
			double bestTimestamp = 0;
			Constants * constants = args->constants;
			double latest = met_to_yyyyddd_fraction(args, args->start);

			for (row = 1; row <= rows; ++row)
				{
					double timestamp, realEpoch;
					int nulls = 0;

					fits_read_col_dbl(fptr, TLE_COL_EPOCH, row, 1, 1,
							constants->nulle, &timestamp, &nulls, &status);

					realEpoch = timestamp < 57e3
						? timestamp + 2000e3
						: timestamp + 1900e3;

					if ((timestamp != constants->nulle) && (realEpoch <= latest))
						{
							if ((selected < 0) || (realEpoch > bestTimestamp))
								{
									selected = row;
									bestTimestamp = realEpoch;
								}
						}
				}

			if (selected < 0)
				{
					args->code = PREFILTER_SETUP_ERROR;
					report_error("no TLE selected\n");
				}
		}

	if (!args->code)
		{
			/* grab the TLE parameters for the selected row */

			int i;
			tleconv_t conv = { 0 };
			const Constants * constants = args->constants;

#undef TLE_COL
#define TLE_COL(name, type, member) \
		{ TLE_COL_ ## name, PREFILTER_ ## type, offsetof(tleconv_t, member) }

			struct TLEColumn
				{
					int index;
					int type;
					size_t offset;
				} columns[] =
					{
						TLE_COL(EPOCH,        DOUBLE,    epoch),
						TLE_COL(MOTION2_DT,   DOUBLE,    motion2dt),
						TLE_COL(MOTION6_D2T,  DOUBLE,    motion6d2t),
						TLE_COL(BSTAR,        DOUBLE,    bstar),
						TLE_COL(INCLINATION,  DOUBLE,    inclination),
						TLE_COL(RAAN,         DOUBLE,    raan),
						TLE_COL(ECCENTRICITY, DOUBLE,    eccentricity),
						TLE_COL(ARG_PERIGEE,  DOUBLE,    argperigee),
						TLE_COL(MEAN_ANOMALY, DOUBLE,    meananomaly),
						TLE_COL(MEAN_MOTION,  DOUBLE,    meanmotion),
					};

			for (i = 0; i < sizeof(columns) / sizeof(columns[0]); ++i)
				{
					struct TLEColumn * p = &columns[i];
					int nulls;

					if (p->type == PREFILTER_DOUBLE)
						{
							double * address = (double *) (((char *) &conv) + p->offset);
							fits_read_col_dbl(fptr, p->index,
									selected, 1, 1, constants->nulle, address, &nulls, &status);
						}
					else
						{
							args->code = PREFILTER_SETUP_ERROR;
							report_error("unexpected type %d for column %d\n",
									p->type, i);
						}
				}

			if (!status)
				set_tle(&args->context->tle, &conv);
			else
				{
					args->code = PREFILTER_SETUP_ERROR;
					report_error("unable to retrieve TLE columns from FITS\n");
				}
		}

	if (fptr)
		{
			fits_close_file(fptr, &status);
		}

	return args->code;
}


static int initialize_columns_file (Arguments * args)
{
	FILE * file = fopen(args->columns + 1, "rt");
	if (file)
		{
#define MAX_PARNAME_LINE 1024
			char line[MAX_PARNAME_LINE];
			int used = 0;
			int allocated = 0;
			char ** parameters = 0;
			int ln = 0;

			while (fgets(line, sizeof(line), file))
				{
					char c = line[0];
					int length = strlen(line);

					++ln;

					if (length == 0 || c == '\n' || c == '\r' || c == '#')
						{
							/* ignore */
						}
					else if (line[length-1] != '\n')
						{
							report_warning("ignoring line [%d]\n", ln);
						}
					else
						{
							char * copy = malloc(length);
							line[length-1] = 0;
							strcpy(copy, line);

							if (used >= allocated - 1)
								{
									size_t bytes;
									char ** x;

									allocated += 32;
									bytes = allocated * sizeof(char *);

									if (parameters)
										x = realloc(parameters, bytes);
									else
										x = malloc(bytes);

									if (x)
										parameters = x;
									else
										{
											args->code = PREFILTER_SETUP_ERROR;
											report_error("memory allocation failed [%u bytes]\n",
													bytes);
										}
								}

							if (!args->code)
								{
									/* maintain null terminated list */
									parameters[used++] = copy;
									parameters[used] = 0;
								}
						}
				}

			fclose(file);

			args->parameters = parameters;
		}
	else
		{
			args->code = PREFILTER_SETUP_ERROR;
			report_error("unable to open parameter file %s [%d]\n",
					args->columns + 1, errno);
		}

	return args->code;
}


static int initialize_columns_string (Arguments * args)
{
	const char * p;
	int count = 1;

	/* determine how many (space delimited) parameters there are */
	for (p = args->columns; *p; ++p)
		if (*p == ' ')
			{
				++count;
				while (p[1] == ' ')
					++p;
			}

	args->parameters = (char **) malloc((count + 1) * sizeof(char *));
	count = 0;

	for (p = args->columns; *p; )
		{
			const char * start, * end;
			int length;
			char * x;

			while (*p && *p == ' ')
				++p;
			start = p;
			while (*p && *p != ' ')
				++p;
			end = p;

			length = end - start;
			x = (char *) malloc(length + 1);
			strncpy(x, start, length);
			x[length] = 0;
			args->parameters[count++] = x;

			if (*p)
				++p;
		}

	/* null terminate list */
	args->parameters[count] = 0;

	return args->code;
}


static int getArgPar(const char *s, const char *prefix, const char **value)
{
	int match = 0;
	int len = strlen(prefix);
	if (strncasecmp(s, prefix, len) == 0)
		{
			if (s[len] == ':')
				{
					*value = s + len + 1;
					match = 1;
				}
		}

	return match;
}


int initialize_time_adj(Arguments *args)
{
	int code = 0;
	int done = 0;
	const char *par = 0;
	int useDefault = 0;
	int useLeaps = 0;
	const char *loadHeader = 0;
	const char *key = 0;
	char *end = 0;

	if (!strcasecmp(args->timeadj, "DEFAULT"))
		{
			useDefault = 1;
			loadHeader = args->attname;
		}
	else if (getArgPar(args->timeadj, "CONST", &par))
		{
			report_debug("const: '%s'\n", par);
			args->context->time_offset = strtod(par, &end);
			if (end && *end)
				{
					code = 1;
					report_error("invalid constant offset '%s'\n", par);
				}
		}
	else if (getArgPar(args->timeadj, "KEY", &par))
		{
			report_debug("key: '%s'\n", par);
			loadHeader = args->attname;
			key = par;
		}
	else if (!strcasecmp(args->timeadj, "LEAPS"))
		{
			report_debug("will use leap seconds for time offset\n");
			useLeaps = 1;
		}
	else
		{
			code = 1;
			report_error("unrecognized time adjustment '%s'\n", args->timeadj);
		}

	if (loadHeader)
		{
			FITSHeader header = { 0 };
			if (fetch_header_records_path(&header, loadHeader))
				{
					useLeaps = 1;
					report_warning("unable to load header from '%s'; will use LEAPS\n", loadHeader);
				}
			else if (useDefault)
				{
					/*
				 	* special case for missions
				 	*/
					char telescop[FLEN_CARD];
					if (get_header_key_string(&header, "TELESCOP", telescop, 0))
						{
							useLeaps = 1;
							report_warning("unable to get TELESCOP keyword; will use LEAPS\n");
						}
					else if (!strcmp(telescop, "SWIFT"))
						{
							key = "UTCFINIT";
							if (!get_header_key_double(&header, key, &args->context->time_offset, 0))
								{
									done = 1;
									report_status("retrieved time offset %f from %s\n", args->context->time_offset, key);
								}
							else
								{
									useLeaps = 1;
									report_status("unable to get Swift time offset from UTCFINIT - will use leaps\n");
									report_status("set UTCFINIT or use timeadj parameter to control time offset\n");
								}
							key = 0;
						}
#if 0
					else if (!strcmp(telescop, "XYZ"))
						{
							/* XYZ specific handling... */
						}
#endif
					else
						{
							/* for backward compatibility, by default, offset is -(LEAPs-SINCE-EPOCH) */
							report_status("no special handling for TELESCOP '%s'\n", telescop);
							useLeaps = 1;
						}
				}

			if (key && !done)
				{
					if (!get_header_key_double(&header, key, &args->context->time_offset, 0))
						{
							done = 1;
							report_status("retrieved time offset %f from %s\n", args->context->time_offset, key);
						}
					else
						{
							code = 1;
							report_error("unable to get time offset from key %s\n", key);
						}
				}

			release_header(&header);
		}

	if (useLeaps)
		{
			double mjd_next = 0;
			args->context->time_offset = -leaps_from_to_next(args->mjd0_utc, args->start, &mjd_next);
			report_status("derived time offset %f from leap seconds\n", args->context->time_offset);
		}

	return code;
}


int initialize_time (Arguments * args)
{
	AtTime epoch = { 0 };

	if (!string_to_AtTime(args->missepoch, &epoch))
        {
            char ctime[32];
            atMJulian(&epoch, &args->mjd0_utc);
            atCTime(&epoch, ctime);
			report_monitor("%s => mjd[utc]=%.12f str=%s\n", args->missepoch, args->mjd0_utc, ctime);
        }
	else
		{
			args->code = PREFILTER_SETUP_ERROR;
			report_error("invalid epoch [%s]\n", args->missepoch);
		}

	if (!args->code)
		{
			char path[QUERY_MAXPATH];
			if (!strncasecmp(args->leapname, "CALDB", 5))
				{
					CALDBQuery query = { 0 };
					strcpy(query.codename, "LEAPSECS");
					strcpy(query.mission, "GEN");
					strcpy(query.instrument, "SC");
					query.infile = (args->orbitMode != ORBIT_COMPARE)
									? args->attname : args->orbname;
					set_caldb_query_qualifiers(&query, args->leapname);
					if (simple_caldb_query(&query, 0, path))
						report_warning("unable to resolve leapname=%s\n", args->leapname);
					else
						HDpar_note("leapname", path);
				}
			else
				strcpy(path, args->leapname);

			if (readLeapTable(args->leapname, args->mjd0_utc))
				{
					args->code = PREFILTER_SETUP_ERROR;
					report_error("unable to load leap second file %s\n",
							args->leapname);
				}
		}

	if (!args->code)
		{
			if (initialize_time_adj(args))
				args->code = PREFILTER_SETUP_ERROR;
		}

	return args->code;
}


int initialize_attitude (Arguments * args)
{
	/* open ATTFILE/attname */
	ATTFILE * attfile = openAttFile((char *) args->attname);
	if (attfile)
		{
			args->attfile = attfile;
			resetAttFileExtrapolationLimits(
					args->attfile, args->attitudeExtrapolation);
			requireTimeNearAttFileRecord(
					args->attfile, args->attitudeExtrapolation);
		}
	else
		{
			args->code = PREFILTER_SETUP_ERROR;
			report_error("unable to open attitude file %s\n",
					args->attname);
		}

	if (!args->code)
		{
			char path[QUERY_MAXPATH];
			if (!strncasecmp(args->alignfile, "CALDB", 5))
				{
					CALDBQuery query = { 0 };
					strcpy(query.codename, "ALIGNMENT");
					strcpy(query.instrument, "SC");
					set_caldb_query_qualifiers(&query, args->alignfile);
					if (simple_caldb_query(&query, attfile->fp, path))
						report_warning("unable to resolve alignfile=%s\n", args->alignfile);
					else
						HDpar_note("alignfile", path);
				}
			else
				strcpy(path, args->alignfile);

			args->align = readAlign(path);
			if (!args->align)
				{
					args->code = PREFILTER_SETUP_ERROR;
					report_error("unable to open alignment file %s\n", path);
				}
		}

	if (!args->code)
		{
			args->context->coordquat = allocateQuat();
		}

	return args->code;
}


int initialize_derive_ra_dec_roll (const Arguments * args, Context * context)
{
	Derived * derived = args->derived;

	derived->coordquat = allocateQuat();
	derived->alignquat = allocateQuat();

	return context->code;
}


int initialize_derive_saa (const Arguments * args, Context * context)
{
	const Constants * constants = context->args->constants;
	Derived * derived = args->derived;

	derived->lastSAA = constants->nulli;
	derived->timeChangeSAA = constants->nulle;

	return context->code;
}


int initialize_asca_derive_cor (const Arguments * args, Context * context)
{
	char tmp[FLEN_FILENAME];
	int code;

	strcpy(tmp, context->args->rigname);
	code = atRigSet(tmp);
	if (code)
		{
			context->code = PREFILTER_SETUP_ERROR;
			report_error("unable to load rigidity set %s [%d]\n",
					tmp, code);
		}

	return context->code;
}


int initialize_iteration (Arguments * args)
{
	Iteration * iter = args->iteration;

	add_iterator(iter, iterate_position_nullify);
	add_iterator(iter, iterate_attitude_nullify);

	if ((args->orbitMode == ORBIT_TLE_FITS)
			|| (args->orbitMode == ORBIT_TLE_TEXT2)
			|| (args->orbitMode == ORBIT_TLE_TEXT3))
		{
			add_iterator(iter, iterate_timestamp_delta);
			add_iterator(iter, iterate_position_tle);
			add_iterator(iter, iterate_attitude_attfile);
		}
	else if ((args->orbitMode == ORBIT_atSetElement)
			|| (args->orbitMode == ORBIT_atSetElement2))
		{
			add_iterator(iter, iterate_timestamp_delta);
			add_iterator(iter, iterate_position_atSatPos);
			add_iterator(iter, iterate_attitude_attfile);
		}
	else if (args->orbitMode == ORBIT_COMPARE)
		{
			add_iterator(iter, iterate_context_compare);
		}
	else
		{
			args->code = PREFILTER_SETUP_ERROR;
			report_error("invalid orbit mode [%d]\n",
					args->orbitMode);
		}

	add_iterator(iter, iterate_position_validate);

	if (args->verbose)
		{
			add_iterator(iter, iterate_timestamp_report);
			add_iterator(iter, iterate_position_report);
			add_iterator(iter, iterate_attitude_report);
		}

	if (iter->count < 0)
		{
			args->code = PREFILTER_SETUP_ERROR;
			report_error("initialization failed\n");
		}

	return args->code;
}


int filter_initialization (Arguments * args, Filter * filter)
{
	if (!args->code)
		register_parameters(args, filter);

	if (!args->code)
		initialize_iteration(args);

	if (!args->code)
		initialize_time(args);

	/* process columns parameter */
	if (!args->code)
		{
			if (!strlen(args->columns) || !strcasecmp(args->columns, "ALL"))
				args->parameters = 0;
			else if (args->columns[0] == '@')
				args->code = initialize_columns_file(args);
			else
				args->code = initialize_columns_string(args);
		}

	/* attitude */
	if (!args->code)
		if (args->orbitMode != ORBIT_COMPARE)
			initialize_attitude(args);

	/* position */
	if (!args->code)
		{
			if (args->orbitMode == ORBIT_TLE_FITS)
				initialize_tle_fits(args);
			else if ((args->orbitMode == ORBIT_TLE_TEXT2)
					|| (args->orbitMode == ORBIT_TLE_TEXT3))
				initialize_tle_text(args);
			else if ((args->orbitMode == ORBIT_atSetElement2)
					|| (args->orbitMode == ORBIT_atSetElement))
				initialize_atElement(args);
			else
				initialize_compare(args);
		}

	/* set output parameters */
	if (!args->code)
		{
			Output * output = args->output;

			output->outname = args->outname;
			output->extension = args->extname;
			output->clobber = args->clobber;
			output->history = args->history;
			output->parameters = (const char **) args->parameters;
		}

	return args->code;
}



/*
 * $Source: /headas/headas/attitude/tasks/prefilter/fortderive.c,v $
 * $Revision: 1.5 $
 * $Date: 2011/06/30 18:18:44 $
 *
 * $Log: fortderive.c,v $
 * Revision 1.5  2011/06/30 18:18:44  miket
 * Changes from Craig Markwardt to change "McIlwain L" to use SAX algorithm
 *
 * Revision 1.4  2005/09/14 21:41:12  rwiegand
 * Deleted local copy of report.[ch] and updated report calling sequence.
 * Pruned unused mean alignment structure.
 *
 * Revision 1.3  2005/01/10 21:39:00  rwiegand
 * Removed PNT_ prefix from RA, DEC, ROLL outputs; renamed POINTING to PNTUNIT
 * to avoid conflict with attitude history file.  Derive SUNSHINE flag
 * before earth angles to avoid unnecessary NULLs.
 *
 * Revision 1.2  2004/02/02 15:53:57  rwiegand
 * Now there is a single vector defining the current pointing derived from
 * the alignment file parameter.
 * There used to be fields defined in terms of the primary spacecraft axis
 * and others in terms of the instrument boresight.
 *
 * Revision 1.1  2002/12/06 20:14:21  rwiegand
 * Added Filter object to pass function pointers for iteration, initialization,
 * status checking.  Broke derive.c into separate files for FORTRAN calling
 * routines, iteration, initialization.  Made compare mode less XTE-centric.
 * Added parameters for pointing axis and boresight.  Allow loading two line
 * elements from FITS or text files.
 *
 * Revision 1.13  2002/05/07 13:57:45  miket
 * modified to use native F77 versions of geocal/geomag/starksubs via cfortran
 *
 * Revision 1.12  2002/03/29 14:36:06  rwiegand
 * Renamed flag variable to avoid conflict with f2c.h
 */

#include "prefilter.h"
#include "derive.h"
#include "report.h"
#include "atFunctions.h"
#include "convert.h"

#define USE_C_FORTRAN_STRING_LIB
#include "cfortran.h"
#define INITMAGMOD() \
  CCALLSFSUB0(INITMAGMOD,initmagmod)
#define XYZMAG(A1,A2,A3,A4,A5,A6,A7) \
  CCALLSFSUB7(XYZMAG,xyzmag,PFLOAT,PFLOAT,PFLOAT,PFLOAT,PFLOAT,PFLOAT,PFLOAT,A1,A2,A3,A4,A5,A6,A7)
#define SHELLC(A1,A2,A3,A4) \
  CCALLSFSUB4(SHELLC,shellc,PFLOAT,PFLOAT,PINT,PFLOAT,A1,A2,A3,A4)
#define SHELLG(A1,A2,A3,A4,A5,A6) \
  CCALLSFSUB6(SHELLG,shellg,PFLOAT,PFLOAT,PFLOAT,PFLOAT,PINT,PFLOAT,A1,A2,A3,A4,A5,A6)



int initialize_swift_derive_mcilwain_l (
		const Arguments * args, Context * context)
{
	INITMAGMOD();

	return context->code;
}


int swift_derive_mcilwain_l (const Context * context, Derived * derived)
{
	const Constants * constants = context->args->constants;
	float mcIlwainL;

	int icode;
	float b0;
	float scpos[3];

	derived->mcIlwainLc = constants->nulle;
	derived->mcIlwainLg = constants->nulle;

	/* use shellc routine */
	mcIlwainL = constants->nulle;

	scpos[0] = derived->geodetic[0] / constants->earthRadius;
	scpos[1] = derived->geodetic[1] / constants->earthRadius;
	scpos[2] = derived->geodetic[2] / constants->earthRadius;

	icode = 0;
	SHELLC(scpos[0], mcIlwainL, icode, b0);

	if (icode != 1 && icode != 3)
			mcIlwainL = constants->nulle;

	derived->mcIlwainLc = mcIlwainL;

	{
		float latitude;
		float longitude = derived->longitude;
		float altitude;
		double dlat, dalt;
		AtPolarVect * polar = &derived->polarPosition;

		if (atEllipsoid(polar, &dlat, &dalt))
			derive_error(derived, __func__, "atEllipsoid");

		latitude = radians_to_degrees(constants, dlat);
		altitude = dalt;

		icode = 0;
		SHELLG(latitude, longitude, altitude, mcIlwainL, icode, b0);

		if (icode != 1)
				mcIlwainL = constants->nulle;

		derived->mcIlwainLg = mcIlwainL;

		if (context->args->verbose)
			report_verbose("refined latitude %.3f, altitude %.3f\n",
						latitude, altitude);
	}

	if (context->args->verbose)
		report_verbose("McIlwain L %.3f (shellc), %.3f (shellg)\n",
					derived->mcIlwainLc, derived->mcIlwainLg);

	return derived->code;
}


#if 0
int xte_derive_mcilwain_l (const Context * context, Derived * derived)
{
	const Constants * constants = context->args->constants;
	real mcIlwainL;

	integer icode;
	real latitude;
	real longitude;
	real altitude;
	real b0;
	real scpos[3];

	mcIlwainL = constants->nulle;

	icode = 0;

	/* use shellc routine */
	mcIlwainL = constants->nulle;

	scpos[0] = context->bodyPosition[0] / constants->earthRadius;
	scpos[1] = context->bodyPosition[1] / constants->earthRadius;
	scpos[2] = context->bodyPosition[2] / constants->earthRadius;

	SHELLC(scpos, mcIlwainL, icode, b0);

	if (icode != 1)
			mcIlwainL = constants->nulle;

	derived->xteMcIlwainLc = mcIlwainL;

	if (context->args->verbose)
		report_verbose("XTE McIlwain L %.3f\n",
					derived->xteMcIlwainLc);

	return derived->code;
}
#endif


int sax_derive_cut_off_rigidity (const Context * context, Derived * derived)
{
	const Constants * constants = context->args->constants;

	float rigidity;   /* cut-off rigidity (GeV/C) */

	float tjd;        /* truncated julian day */
	float xyz[3];     /* inertial coordinates */

	float b0;         /* field strength (gauss) */
	float fl;         /* field-shell L-value (Earth radii) */
	float latitude;   /* invariant geomagnetic latitude (degrees) */
	float world[4];   /* longitude, latitude, altitude, local time */

	derived->cutOffRigiditySAX = constants->nulle;

	tjd = context->mjd - 40000;

	xyz[0] = context->position[0];
	xyz[1] = context->position[1];
	xyz[2] = context->position[2];

	/* no error code returned... */
	XYZMAG(tjd, xyz[0], b0, fl, rigidity, latitude, world[0]);

	derived->cutOffRigiditySAX = rigidity;
	derived->mcIlwainL_sax = fl;

	if (context->args->verbose) {
		report_verbose("SAX cut off rigidity %.3f\n",
					derived->cutOffRigiditySAX);
		report_verbose("McIlwain L %.3f (SAX)\n",
			       derived->mcIlwainL_sax);
	}

	return derived->code;
}


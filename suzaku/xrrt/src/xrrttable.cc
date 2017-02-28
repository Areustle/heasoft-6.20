// xrrttable.cc
//
// Member functions for XrrtTable class
//
// Richard L Fink GSFC/631
// 1997/05/30
// 1998/05/22 Modify XrrtTable::getReflectivity to support nonexistant
//			relectivity table energies by linear interpolation between
//			existing table rows. This was requested by Astro-E Nagoya/ISAS.
//			R Fink
// 2006/06/13 Y.ISHISAKI	version 6.2.8
//    check refProbArray to avoid floating exception on OSF1 in setTableRow()
//
// Use RCS (Revision Control System) by HIDEYUKI MORI
// Revision 1.1  2000/10/19 12:20:52  mori
// Initial revision
//
// 2006/06/19 Y.ISHISAKI	version 6.3.9
//    define xrrt_isnormal_positive() for Solaris & OSF1
//
// 2006/06/28 Y.ISHISAKI	version 6.3.10
//    remove code to check for denormalized number, instead use -mieee on OSF1
//
// 2006/07/07 Y.ISHISAKI	version 6.3.11
//    change number to check denormalized number 1.0e-307 -> 2.226e-308
//
// 2007/04/05 Y.ISHISAKI	version 6.4.5
//	add XrrtTableRow::calcScatProb(), XrrtTableRow::getScatProb()
//	add XrrtTable::getScatProb()
//	XrrtTableRow::getRowEnergy() moved from xrrttable.hh
//
// 2007/04/05 Y.ISHISAKI	version 6.4.5
//	call XrrtTable::setTableIndex()
//	use index in XrrtTable::getReflectivity() and XrrtTable::getScatProb()
//
// 2007/05/07 Y.ISHISAKI	version 6.4.5
//	calculate scatProb at bin center angle in XrrtTableRow::getScatProb()
//
// 2008/03/02 Y.ISHISAKI	version 6.5.0
//	call XrrtScatter::astroeFdiffract() in XrrtTableRow::calcScatProb()

/* fpclassify [c99], fpclass [solaris], fp_class [OSF1] is OS dependent */
/* #define USE_FP_CLASS */

#ifdef USE_FP_CLASS
#include <math.h>
#if defined(fpclassify)
#define xrrt_isnormal_positive(x)	(FP_NORMAL==fpclassify(x) && 0.0<(x))
#else	// defined(fpclassify)
#if defined(FP_NORMAL)
#include <ieeefp.h>		// Solaris
#define xrrt_isnormal_positive(x)	(FP_PNORM==fpclass(x))
#else	// defined(FP_NORMAL)
#include <fp_class.h>	// OSF1
#define xrrt_isnormal_positive(x)	(FP_POS_NORM==fp_class(x))
#endif	// defined(FP_NORMAL)
#endif	// defined(fpclassify)
#endif	// USE_FP_CLASS

#include <cmath>
#include "xrrttable.hh"
#include "xrrtscatter.hh"
#include "xrrtraytrace.hh"

XrrtTableRow::XrrtTableRow(): 
binAngleNum(0), 
binAngleZero(0.0), 
binAngleDelta(0.0), 
refProbArray(NULL),
scatProbArray(NULL),
tableEnergy(0),
binAngle(),
reflectProb(),
binUsageCount(0)
{
	;
}

inline TableEnergy
XrrtTableRow::getRowEnergy(void) const
{
    return tableEnergy;
}

ReflectProb 
XrrtTableRow::getReflectProb(const BinAngle& angle)
{
	int i, last;

	// new reflect file format (FORMAT_VERSION=2)
	if ( NULL != refProbArray ) {
		i = (int)( (angle - binAngleZero) / binAngleDelta );
		if ( i < 0 ) {
			i = 0;
		} else if ( binAngleNum <= i ) {
			i = binAngleNum - 1;
		}
		binUsageCount[i]++;
		return refProbArray[i];
	}

	// old reflect file format
	// Compute the last entry in the vector table
	last = binAngle.size() - 1;

	if ( last < 0 ) {
		// return zero for a non-existent table
		return 0.0;
	}

	// Scan the table and remember that the angle is the START of the bin!
	for (i = 1; i < last; i++) {
		if ( angle < binAngle[i] ) {
			i--;
			break;
		}
	}
	binUsageCount[i]++;
	return reflectProb[i];
}

double
XrrtTableRow::calcScatProb(const BinAngle& angleInRadian)
{
	const double MIN2RAD = M_PI/180.0/60.0;
	const int ndiv = 200;
	const double xmin = -50.0;		// [arcmin]
	const double xmax = +50.0;		// [arcmin]
	const double pitch = (xmax - xmin) / ndiv;	// 0.5 arcmin
	double energyInKeV = getRowEnergy();
	double ri, rs, xi, prob;
	XrrtScatter& defaultScatterInfo = theRaytraceInfo().defaultScatterInfo;

	prob = 0.0;
	ri = getReflectProb(angleInRadian);
	for (int i = 0; i < ndiv; i++) {
		xi = ( xmin + pitch * (i + 0.5) ) * MIN2RAD;
		rs = getReflectProb(angleInRadian + xi);
		prob += defaultScatterInfo.astroeFdiffract(rs/ri, angleInRadian, energyInKeV, xi);
	}
	prob *= pitch;

/*	printf("\
XrrtTableRow::calcScatProb(): energy=%f, angle=%f, prob=%f\n",
		   energyInKeV, angleInRadian, prob);
*/

	return prob;
}

double 
XrrtTableRow::getScatProb(const BinAngle& angle)
{
	int i;

	// new reflect file format (FORMAT_VERSION=2)
	if ( NULL != scatProbArray ) {
		i = (int)( (angle - binAngleZero) / binAngleDelta );
		if ( i < 0 ) {
			i = 0;
		} else if ( binAngleNum <= i ) {
			i = binAngleNum - 1;
		}
		if ( scatProbArray[i] < 0.0 ) {
			BinAngle angleInRadian = binAngleZero + (i+0.5)*binAngleDelta;
			scatProbArray[i] = calcScatProb(angleInRadian);
		}
		return scatProbArray[i];
	}

	// old reflect file format
	return 0.0;
}

XrrtTable::XrrtTable( ):
reflectTable(),
tableName(" "),
lastRowUsed(-1)
{
	index.norm = 0.0;
	index.offs = 0.0;
	index.nbody = 0;
	index.body = NULL;
}

string
XrrtTable::errorMessage(XrrtTableErrorCode errorCode)
{
	string errorMessage;

	switch (errorCode) {
	case noReflectivityTableForEnergy:
		errorMessage = 
		"No table exists for the requested energy";
		break;
	default:
		char charNumber[1024];
		sprintf(charNumber, "%d",errorCode);
		errorMessage =
		"XrrtTable::errorMessage Unknown error code: ";
		errorMessage.append(charNumber);
		break;
	}

	return errorMessage;
}

void
XrrtTable::setTableEntry(TableEnergy energy,
						 BinAngle angle,
						 ReflectProb probability)
{
	int limit;

	// Check whether we are asking for the same energy as the last call
	if ( 0 <= lastRowUsed ) {
		if (reflectTable[lastRowUsed]->tableEnergy == energy) {
			// Found the row for this energy
			reflectTable[lastRowUsed]->setRowBin(angle, probability);
			return;
		}
	}

	limit = reflectTable.end() - reflectTable.begin();
	for (int i=0; i<limit; i++) {
		if (reflectTable[i]->tableEnergy == energy) {
			// Found the row for this energy
			reflectTable[i]->setRowBin(angle, probability);
			lastRowUsed = i;
			return;
		}
	}

	// No row had this energy so create a new one
	XrrtTableRow* row = new XrrtTableRow;
	reflectTable.push_back(row);
	row->tableEnergy = energy;
	row->setRowBin(angle, probability);
	lastRowUsed = reflectTable.size() - 1;
}

void
XrrtTable::setTableRow(TableEnergy energy,
					   int binAngleNum,
					   BinAngle binAngleZero,
					   BinAngle binAngleDelta,
					   ReflectProb* refProbArray,
					   ScatProb* scatProbArray)
{
//    check refProbArray to avoid floating exception on OSF1
#ifdef USE_FP_CLASS
	for (int i = 0; i < binAngleNum; i++) {
		if ( ! xrrt_isnormal_positive(refProbArray[i]) ) {
#if defined(FP_POS_ZERO)	// OSF1
			if ( FP_POS_ZERO != fp_class(refProbArray[i]) )
#else
			if ( 0.0 != refProbArray[i] )
#endif
			{
				fprintf(stderr, "\
XrrtTable::setTableRow(): denormalized or negative number set to 0.0,\n\
	at Energy=%.3f, i=%d, refProb=%.e\n", energy, i, refProbArray[i]);
			}
			refProbArray[i] = 0.0;
		}
	}
#else	// USE_FP_CLASS
	int num_too_small = 0;
	for (int i = 0; i < binAngleNum; i++) {
		double f = refProbArray[i];
		if ( f < 0.0 || ( 0.0 < f && f < 2.226e-308 ) ) {
			refProbArray[i] = 0.0;
			num_too_small++;
		}
	}
	if ( 0 < num_too_small ) {
        fprintf(stderr, "\
WARNING: %d of denormalized or negative elements set to 0.0, at Energy=%.3f\n",
			num_too_small, energy);
	}
#endif	// USE_FP_CLASS

	XrrtTableRow* row = new XrrtTableRow;
	reflectTable.push_back(row);
	row->tableEnergy = energy;
	row->binAngleNum = binAngleNum;
	row->binAngleZero = binAngleZero;
	row->binAngleDelta = binAngleDelta;
	row->refProbArray = refProbArray;
	row->scatProbArray = scatProbArray;
	row->binUsageCount.resize(binAngleNum, 0);
}

void
XrrtTable::setTableIndex(void)
{
	int i, j, k;
	int n = reflectTable.size();

	index.offs = reflectTable[0]->tableEnergy;
	index.norm = reflectTable[n-1]->tableEnergy - index.offs;
	index.nbody = 4 * n;
	index.body = new unsigned short[index.nbody+1];
	if ( NULL == index.body ) {
		throw outOfMemory;
	}
	for (i = j = 0; i < n; i++) {
		k = (int)( index.nbody *
				   (reflectTable[i]->tableEnergy - index.offs) / index.norm );
		while ( j <= k ) {
			index.body[j++] = i;
		}
	}
	while ( j < index.nbody+1 ) {
		index.body[j++] = i;
	}
}

//double 
//XrrtTable::getReflectivity(const double& energyInKev,
//						   const double& incidentAngle)
//{
//	for (vector<XrrtTableRow*>::iterator tableRow = reflectTable.begin();
//		 tableRow < reflectTable.end();
//		 tableRow++)
//		{
//		if ((*tableRow)->getRowEnergy() == energyInKev)
//		   {
//		   return (*tableRow)->getReflectProb(incidentAngle);
//		   }
//		}
//	throw noReflectivityTableForEnergy;
//}
double 
XrrtTable::getReflectivity(const double& energyInKev,
						   const double& incidentAngle)
{
//vector<XrrtTableRow*>::iterator lowEnergyRow = reflectTable.begin();
//vector<XrrtTableRow*>::iterator highEnergyRow = lowEnergyRow;
//
//	// Scan across the rows of the table looking for either an exact
//	// energy match or two rows that bound the desired energy so an
//	// interpolation can be done to zeroth order.
//	for (vector<XrrtTableRow*>::iterator tableRow = reflectTable.begin();
//		 tableRow < reflectTable.end();
//		 tableRow++)
//		{
//		// If there is a table row with this energy, return the exact result for
//		// this energy.
//		if ((*tableRow)->getRowEnergy() == energyInKev)
//		   {
//		   return (*tableRow)->getReflectProb(incidentAngle);
//		   }
//		// If the table row energy is less than the required energy, 
//		// save it for later.
//		if ((*tableRow)->getRowEnergy() < energyInKev)
//		   {
//		   lowEnergyRow = tableRow;
//		   }
//		// If we find a table row with a greater energy than that desired
//		// we have to linear interpolate between the two rows. At least,
//		// that is what Nagoya/ISAS want for the moment.
//		if ((*tableRow)->getRowEnergy() > energyInKev)
//		   {
//		   highEnergyRow = tableRow;
//		   if (lowEnergyRow == highEnergyRow)
//			  {
//			  // This accounts for the case where we are asked for
//			  // an energy less than the minimum energy in the table.
//			  throw noReflectivityTableForEnergy;
//			  }
//		   return (*lowEnergyRow)->getReflectProb(incidentAngle) +
//				  ((*highEnergyRow)->getReflectProb(incidentAngle) - 
//				   (*lowEnergyRow)->getReflectProb(incidentAngle)) *
//				  ((energyInKev - (*lowEnergyRow)->getRowEnergy()) /
//				  ((*highEnergyRow)->getRowEnergy() -
//				  (*lowEnergyRow)->getRowEnergy()));
//		   }
//		}
//	// If we drop out the bottom of the loop, then we have been asked for
//	// an energy greater than the the maximum energy in the table.
//	throw noReflectivityTableForEnergy;
//
// ISAS code to allow interpolation of energies in the reflection tables
// as per their message; I have reformatted it a bit but not changed the logic.

	double ene, e0, e1;
	double ref, r0, r1;
	int i, i0, i1, n;

	if ( 0 < lastRowUsed &&
		 (e0 = reflectTable[lastRowUsed-1]->getRowEnergy()) <= energyInKev &&
		 energyInKev <= (e1 = reflectTable[lastRowUsed]->getRowEnergy()) ) {
		r0 = reflectTable[lastRowUsed-1]->getReflectProb(incidentAngle);
		r1 = reflectTable[lastRowUsed]->getReflectProb(incidentAngle);
		ref  = (r0*(e1-energyInKev) + r1*(energyInKev-e0)) / (e1 - e0);
		return ref;
	}

	if ( energyInKev <= reflectTable[0]->getRowEnergy() ) {
		return reflectTable[0]->getReflectProb(incidentAngle);
	}

	n = reflectTable.size();
	if ( reflectTable[n-1]->getRowEnergy() < energyInKev ) {
		return 0.0;
	}

	if ( 0 < index.nbody ) {
		i = (int)( index.nbody * (energyInKev - index.offs) / index.norm );
		i = i1 = index.body[i];
		i0 = (0 < i) ? i-1 : 0;
		e0 = reflectTable[i0]->getRowEnergy(),
		e1 = -1;
/*		printf("\
i=%d, index.nbody=%d, energy=%f, index.offs=%f, index.norm=%f\n",
			i, index.nbody, energyInKev, index.offs, index.norm);
		printf("\
tableEnergy[i-1]=%f, tableEnergy[i]=%f, tableEnergy[i+1]=%f\n",
			reflectTable[i-1]->getRowEnergy(),
			reflectTable[i]->getRowEnergy(),
			reflectTable[i+1]->getRowEnergy());
*/
	} else {
		i0 = i1 = i = 0;
		e0 = e1 = -1.0;
	}

	while ( i < n ) {
		ene = reflectTable[i]->getRowEnergy();
		if ( ene == energyInKev ) {
			return reflectTable[i]->getReflectProb(incidentAngle);
		}
		if ( energyInKev < ene ) {
			e1 = ene;
			i1 = i;
			lastRowUsed = i;
			break;
		}
		e0 = ene;
		i0 = i;
		i++;
	}

	if ( e0 < 0.0 || e1 < 0.0 ) {
		throw noReflectivityTableForEnergy;
	}

	r0 = reflectTable[i0]->getReflectProb(incidentAngle);
	r1 = reflectTable[i1]->getReflectProb(incidentAngle);

	ref = (r0*(e1-energyInKev) + r1*(energyInKev-e0)) / (e1 - e0);

	return ref;
}

double 
XrrtTable::getScatProb(const double& energyInKev, const double& incidentAngle)
{
	double ene, e0, e1;
	double prob, s0, s1;
	int i, i0, i1, n;

	if ( 0 < lastRowUsed &&
		 (e0 = reflectTable[lastRowUsed-1]->getRowEnergy()) <= energyInKev &&
		 energyInKev <= (e1 = reflectTable[lastRowUsed]->getRowEnergy()) ) {
		s0 = reflectTable[lastRowUsed-1]->getScatProb(incidentAngle);
		s1 = reflectTable[lastRowUsed]->getScatProb(incidentAngle);
		prob = (s0*(e1-energyInKev) + s1*(energyInKev-e0)) / (e1 - e0);
		return prob;
	}

	if ( energyInKev <= reflectTable[0]->getRowEnergy() ) {
		return reflectTable[0]->getScatProb(incidentAngle);
	}

	n = reflectTable.size();
	if ( reflectTable[n-1]->getRowEnergy() < energyInKev ) {
		return 0.0;
	}

	if ( 0 < index.nbody ) {
		i = (int)( index.nbody * (energyInKev - index.offs) / index.norm );
		i = i1 = index.body[i];
		i0 = (0 < i) ? i-1 : 0;
		e0 = reflectTable[i0]->getRowEnergy(),
		e1 = -1;
	} else {
		i0 = i1 = i = 0;
		e0 = e1 = -1.0;
	}

	while ( i < n ) {
		ene = reflectTable[i]->getRowEnergy();
		if ( ene == energyInKev ) {
			return reflectTable[i]->getScatProb(incidentAngle);
		}
		if ( energyInKev < ene ) {
			e1 = ene;
			i1 = i;
			lastRowUsed = i;
			break;
		}
		e0 = ene;
		i0 = i;
		i++;
	}

	if ( e0 < 0.0 || e1 < 0.0 ) {
		throw noReflectivityTableForEnergy;
	}

	s0 = reflectTable[i0]->getScatProb(incidentAngle);
	s1 = reflectTable[i1]->getScatProb(incidentAngle);

	prob  = (s0*(e1-energyInKev) + s1*(energyInKev-e0)) / (e1 - e0);

	return prob;
}

void
XrrtTableRow::setRowEnergy(const TableEnergy& parameter)
{
	tableEnergy = parameter;
}

void 
XrrtTableRow::setRowBin(const BinAngle& angle, 
						const ReflectProb& probability)
{
	binAngle.push_back( angle);
	reflectProb.push_back( probability);
	binUsageCount.push_back(0);
}

bool
XrrtTableRow::has_energy(const TableEnergy& energy)
{
	return (energy == tableEnergy);
}

void
XrrtTable::setTableName(const string& name)
{
	tableName = name;
}

string
XrrtTable::getTableName() const
{
	return tableName;
}

double
XrrtTable::getTableEnergy(int& rowNumber)
{
	double energy=0.0;
	if ( 0 <= rowNumber && (unsigned int)rowNumber < reflectTable.size() ) {
		energy = reflectTable[rowNumber]->getRowEnergy();
	}
	return energy;
}

/*	for Emacs
;;; Local Variables: ***
;;; mode:C++ ***
;;; tab-width:4 ***
;;; c-indent-level:4  ***
;;; End: ***
*/

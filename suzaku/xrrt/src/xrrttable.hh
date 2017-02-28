// xrrttable.hh
//
// Class definition for X-ray mirror reflection table.
// Richard L Fink GSFC/631
// 1997/05/08
// 1997/09/18 Added support for error codes. R. Fink
// 1997/09/23 Upgraded documentation. R. Fink
// 2006/06/28 Y.ISHISAKI	version 6.3.10
//    check if both arguments are 0.0 before atan()
// 2007/05/07 Y.ISHISAKI	version 6.4.5
//    several variables are move to private -> public for addscatprob
//      XrrtTableRow::binAngleNum, binAngleZero, binAngleDelta
//      XrrtTableRow::refProbArray, scatProbArray
//      XrrtTable::reflectTable

#ifndef XRRTTABLE_HH
#define XRRTTABLE_HH

//
// System interfaces used
//
#include <exception>
#include <string>
// Modified by H. Mori (2005/09/14)
// #include <stdio.h>
#include <cstdio>
// #include <stl.h>
#include <vector>
#include <iterator>

// Compile for GCC 3.3.2
// Modified by H. Mori (2005/09/14)
using namespace std;

// 
// XRRT interfaces used
//
#include "xrrt_types.hh"

//
// Local enums
//
enum XrrtTableErrorCode {
	XrrtTableErrors,
	noReflectivityTableForEnergy,  // No reflection table exists for the
                                       // requested energy.
	XrrtTableErrorsEnd
};

//
// XrrtTableRow supports individual reflectivity table rows (reflectivity
// as a function of energy).
//
class  XrrtTableRow {
    friend class XrrtTable;
    public:
          // Default Constructor
          XrrtTableRow();
          // Copy Constructor
          XrrtTableRow( const XrrtTableRow& reflectByEnergy ); 

          // setRowEnergy sets the defined energy for the current reflection
          //       row entry.
          void setRowEnergy(const TableEnergy& parameter);

          // getRowEnergy returns the energy of the current reflection table
          //       entry.
          TableEnergy getRowEnergy() const;

          // setRowBin inserts a reflection table entry bin with the bin angle
          //       and reflection probability.
	  void setRowBin(const BinAngle& angle, const ReflectProb& probability);

          // getReflectProb returns the reflection probability for a given
          //       bin angle.
          ReflectProb getReflectProb(const BinAngle& angle);

          // has_energy returns true is the given energy matchs the energy
          // stored with this table row
          bool has_energy( const TableEnergy& parameter);

          // getScatProb returns the scatter probability for a given bin angle
          double getScatProb(const BinAngle& angle);
          double calcScatProb(const BinAngle& angle);

	  int binAngleNum;
	  BinAngle binAngleZero;
	  BinAngle binAngleDelta;

	  ReflectProb* refProbArray;
	  ScatProb* scatProbArray;

    private:
	  TableEnergy          tableEnergy;
	  //
	  // Incident angle in radians for photon impact on surface
	  vector<BinAngle>     binAngle;
	  //
	  // Probability that a photon will reflect from the surface at this
	  // incident angle.
	  vector<ReflectProb>  reflectProb;
	  //
	  // Incremented by one every time a bin is used by the program.
	  // this has the effect of monitoring which incident angles are
	  // used by ray tracing.
	  vector<int> binUsageCount;
};

//
// XrrtTable Combines the table rows together to form a single table
//
class XrrtTable {

    public: 
          // Default Constructor
          XrrtTable();
          // Copy Constructor
          XrrtTable( const XrrtTable& reflectTable);

          //
          // Accessors to table info
          //
          void setTableName(const string& name);
          string getTableName() const;
          //
          double getTableEnergy(int& rowNumber);

          //
          // Add a bin to the table
          void setTableEntry(TableEnergy, BinAngle, ReflectProb);

          //
          // Add a bin to the table
          void setTableRow(TableEnergy,
			   int binAngleNum, 
			   BinAngle binAngleZero,
			   BinAngle binAngleDelta,
			   ReflectProb* refProbArray,
			   ScatProb* scatProbArray);

          //
          // Set table index to search energy
	  void setTableIndex(void);

          //
          // Return the reflection probability for a given energy and angle
          double getReflectivity(const double& energyInKev, 
                                 const double& incidentAngle);

          // Return the scatter probability for a given energy angle
          double getScatProb(const double& energyInKev,
			     const double& incidentAngle);

          //
          // Convert error codes to error messages
          //
          string errorMessage(XrrtTableErrorCode errorCode);

	  vector<XrrtTableRow*> reflectTable;

    private:
	  string tableName;
	  int lastRowUsed;
	  struct EnergyIndex {
		  double norm, offs;
		  int nbody;		// in fact, sizeof(body)-1
		  unsigned short *body;	// 0-nbody
	  } index;
};

#endif

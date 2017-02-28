#ifndef HKCOLUMNHH
#define HKCOLUMNHH

#include <string>
extern "C" {
#include "fitsio.h"
}

#include "FITSException.hh"

/***************************************************************************
* contains information abotu a column in a FITS file
***************************************************************************/
class HKColumn {
  private:

    string name;
    int  colnum;
    char comment[FLEN_COMMENT];

    char tform[FLEN_VALUE];
    char tunit[FLEN_VALUE];
    int coltype;
    long  tnull;
    double tscal;
    double tzero;

    bool has_tnull;

    string out_name;
    string interpolation;
    string calibration;

  public:
    HKColumn(const string& name, fitsfile *fp,
             const string& calibration, const string& interpolation,
             const char *xcom, const string& out_name) throw(FITSException);

    virtual ~HKColumn() {}

    string& get_name () { return name; }
    const char* get_out_name() { return out_name.c_str(); }
    char* get_comment () { return comment; }
    int  get_colnum () {return colnum;}
    char* get_tform () { return tform;}
    char* get_tunit () { return tunit;}
    long get_tnull () { return tnull;}
    bool has_tnull_keyword() { return has_tnull; }
    double get_tzero() { return tzero;}
    double get_tscale() { return tscal;}
    int get_coltype() { return coltype; }


    const char* get_interpolation() { return interpolation.c_str(); }
    const char* get_calibration() { return calibration.c_str(); }

}; 
#endif

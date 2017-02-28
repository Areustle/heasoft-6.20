/// \file ahfits_colinfo.cxx
/// \brief ahfits: Function to load/retrieve column information
/// \author James Peachey
/// \date $Date: 2015/05/18 14:31:08 $

#define AHLABEL ahfits_ahfits_colinfo
#define AHCVSID "$Id: ahfits_colinfo.cxx,v 1.37 2015/05/18 14:31:08 asargent Exp $"

#include "ahfits/ahfits_colinfo.h"
#include "ahfits/ahfits_header.h"
#include "ahlog/ahlog.h"

#include "fitsio.h"

#include <cstdlib>
#include <sstream>
#include <list> 

namespace ahfits {

// -----------------------------------------------------------------------------

int columnType(FilePtr ahffp, const std::string & name) {
  loadColInfo(ahffp,name);   // existence check inside
  return ahffp->m_colinfo[name2Num(ahffp,name)]->m_typecode;
}

// -----------------------------------------------------------------------------

std::string columnUnits(FilePtr ahffp, const std::string & name) {
  loadColInfo(ahffp,name);   // existence check inside
  return ahffp->m_colinfo[name2Num(ahffp,name)]->m_units;
}

// -----------------------------------------------------------------------------

ahfits::IndexType columnRepeat(FilePtr ahffp, const std::string & name) {
  loadColInfo(ahffp,name);   // existence check inside
  return ahffp->m_colinfo[name2Num(ahffp,name)]->m_repeat;
}

// -----------------------------------------------------------------------------

ahfits::IndexType columnWidth(FilePtr ahffp, const std::string & name) {
  loadColInfo(ahffp,name);   // existence check inside
  return ahffp->m_colinfo[name2Num(ahffp,name)]->m_width;
}

// -----------------------------------------------------------------------------

bool columnNull(FilePtr ahffp, const std::string & name, long long & tnull) {
  loadColInfo(ahffp,name);   // existence check inside
  tnull=ahffp->m_colinfo[name2Num(ahffp,name)]->m_nullval;
  return ahffp->m_colinfo[name2Num(ahffp,name)]->m_has_nullval;
}

// -----------------------------------------------------------------------------

std::string columnDisplay(FilePtr ahffp, const std::string & name) {
  loadColInfo(ahffp,name);   // existence check inside
  return ahffp->m_colinfo[name2Num(ahffp,name)]->m_disp;
}

// -----------------------------------------------------------------------------

bool columnRange(FilePtr ahffp,  const std::string & name, std::string & minval,
                 std::string & maxval) {

  loadColInfo(ahffp,name);   // existence check inside

  minval = ahffp->m_colinfo[name2Num(ahffp,name)]->m_minval; 
  maxval = ahffp->m_colinfo[name2Num(ahffp,name)]->m_maxval;

  if (maxval < minval) return false;
  return true;
}

// -----------------------------------------------------------------------------

bool columnRange(FilePtr ahffp, const std::string & name, long long & minval, 
                 long long & maxval) {


  
  loadColInfo(ahffp,name);   // existence check inside

  //use stringstreams initialized the the value loaded by loadcolumninfo
  std::stringstream minval_ss(ahffp->m_colinfo[name2Num(ahffp,name)]->m_minval);
  std::stringstream maxval_ss(ahffp->m_colinfo[name2Num(ahffp,name)]->m_maxval);

  //give the value of the stringstreams to minval and maxval, if that fails,
  //set them to zero
  if ( !(minval_ss >> minval) ) minval = 0;
  if ( !(maxval_ss >> maxval) ) maxval = 0;

  if (maxval < minval) return false;
  return true;
}

// -----------------------------------------------------------------------------

bool columnRange(FilePtr ahffp,  const std::string & name, double & minval, 
                 double & maxval) {

  loadColInfo(ahffp,name);   // existence check inside

  //use stringstreams initialized the the value loaded by loadcolumninfo
  std::stringstream minval_ss(ahffp->m_colinfo[name2Num(ahffp,name)]->m_minval);
  std::stringstream maxval_ss(ahffp->m_colinfo[name2Num(ahffp,name)]->m_maxval);

  //give the value of the stringstreams to minval and maxval, if that fails,
  //set them to zero
  if ( !(minval_ss >> minval) ) minval = 0;
  if ( !(maxval_ss >> maxval) ) maxval = 0;
  
  if (maxval < minval) return false;
  return true;
}

// -----------------------------------------------------------------------------

ahfits::IndexType columnLength(FilePtr ahffp, const std::string & name) {
  if (columnType(ahffp,name) == TSTRING)
    return columnRepeat(ahffp,name)/columnWidth(ahffp,name);
  return columnRepeat(ahffp,name);
}

// -----------------------------------------------------------------------------

bool haveColumn(FilePtr ahffp, const std::string & name) {
  int colnum=0;
  int status=0;
  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");
  fits_get_colnum(ahffp->m_cfitsfp,CASEINSEN,const_cast<char*>(name.c_str()),&colnum,&status);
  if (0 == status || COL_NOT_UNIQUE == status) {   // if not unique, will just take first one
    loadColInfo(ahffp,name);
    return true;
  } else if (COL_NOT_FOUND == status) {
    return false;
  } else {
    AH_THROW_LOGIC(ahfits::errPrefix(ahffp,true,false)+"failure calling fits_get_colnum for column: "+name+
                   statusMsg(status));
  }
}

// -----------------------------------------------------------------------------

bool isIntegerTypeColumn(FilePtr ahffp, const std::string & name) {
  int abs_typecode=abs(columnType(ahffp,name));
  switch (abs_typecode) {
    case TBYTE:
    case TSHORT:
    case TLONG:
    case TLONGLONG:
    case TINT:
    case TUINT:
    case TUSHORT:
    case TULONG:
      return true;
      break;
  }
  return false;
}
  
// -----------------------------------------------------------------------------

bool isFloatTypeColumn(FilePtr ahffp, const std::string & name) {
  int abs_typecode=abs(columnType(ahffp,name));
  switch (abs_typecode) {
    case TFLOAT:
    case TDOUBLE:
      return true;
      break;
  }
  return false;
}
  
// -----------------------------------------------------------------------------

void loadColInfo(FilePtr ahffp, const std::string & name) {

  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");

  if (!isBintable(ahffp))
    AH_THROW_LOGIC(ahfits::errPrefix(ahffp,true,false)+"columns only present in binary table extensions");

  int status = 0;

  // if name in map, exit
  if (ahffp->m_name2num.count(name) > 0) return;

  // Find the column number associated with the given column name.
  int colnum = 0;
  if (0 != fits_get_colnum(ahffp->m_cfitsfp, CASEINSEN, 
                           const_cast<char *>(name.c_str()),&colnum,&status)) {
    AH_THROW_LOGIC(ahfits::errPrefix(ahffp,true,false)+"column not found: "+name+
                   statusMsg(status));
  }
  status=0;

  // add name/num to map
  ahffp->m_name2num[name]=colnum;
  ahffp->m_num2name[colnum]=name;

  // get TYPECODE, REPEAT, and WIDTH
  int typecode = 0;
  ahfits::IndexType repeat = 0;
  ahfits::IndexType width = 0;
  status=0;
  if (0 != fits_get_coltypell(ahffp->m_cfitsfp,colnum,&typecode,&repeat,&width,
                              &status)) {
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"could not get column types: "+
                     name+statusMsg(status));
  }
  status=0;
  
  // get repeat value for variable width columns 
  if(0 > typecode){
    
  char value[FLEN_VALUE] = "";
    char keyname[FLEN_KEYWORD] = "";
  
  sprintf(keyname,"TFORM%d",colnum);
  if (0 != fits_read_keyword(ahffp->m_cfitsfp, keyname, value, 0, &status)) {
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"could not get keyname: "+keyname+statusMsg(status));
  }
  
  //Get repeat from value
  //if value is of bad form or empty i.e., 1PE, 1PE(, 1PE(A, 0 is returned
  std::istringstream iss(value);
  iss.ignore(FLEN_KEYWORD,'(');
  iss >> value;
  repeat = std::strtol(value,0,10);
  
  }
  status=0;

  // get units (ignore rest of return parameters)
  std::string units="";
  char tunits[FLEN_CARD] = "";
  status=0;
  if (0 != fits_get_bcolparmsll(ahffp->m_cfitsfp,colnum,NULL,tunits,NULL,NULL,
                                NULL,NULL,NULL,NULL,&status)) {
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"could not get column parameters (binary table): "+
                     name+statusMsg(status));
  }
  units=tunits;
  status=0;

  // get min/max values of column: TLMINn/TLMAXn (ints or floats) store as string
  // note: no range is specified if tmax < tmin
  // note: storing as string to support integer, string and double values
  std::string tmin = "0";
  std::string tmax = "-1";
  std::string hmin,hmax;
  ahfits::formColumnAttribute(ahffp,name,"TLMIN",hmin);
  ahfits::formColumnAttribute(ahffp,name,"TLMAX",hmax);
  if (ahfits::keywordExists(ahffp,hmin)) tmin=ahfits::getKeyValStr(ahffp,hmin);
  if (ahfits::keywordExists(ahffp,hmax)) tmax=ahfits::getKeyValStr(ahffp,hmax);


  // get TNULL for integer types
  long long tnull=0;
  bool has_nullval=false;
  std::string hnull;
  ahfits::formColumnAttribute(ahffp,name,"TNULL",hnull);
  if (ahfits::keywordExists(ahffp,hnull)) {
    tnull=ahfits::getKeyValLLong(ahffp,hnull);
    has_nullval=true;
  }

  // get TDISP
  std::string disp;
  std::string hdisp;
  ahfits::formColumnAttribute(ahffp,name,"TDISP",hdisp);
  if (ahfits::keywordExists(ahffp,hdisp)) disp=ahfits::getKeyValStr(ahffp,hdisp);

  // assign to struct
  ahffp->m_colinfo[colnum]=new ColumnInfo(colnum,name,units,disp,typecode,repeat,
                                          width,tmin,tmax,has_nullval,tnull);

  return;
}

// -----------------------------------------------------------------------------

void clearColInfo(FilePtr ahffp, const std::string& name) {
  int colnum=name2Num(ahffp,name);
  ahffp->m_name2num.erase(name);
  ahffp->m_num2name.erase(colnum);
  delete ahffp->m_colinfo[colnum];
  ahffp->m_colinfo.erase(colnum);
}

// -----------------------------------------------------------------------------

void clearAllColInfo(FilePtr ahffp) {

  // need to deallocate ColumnInfo structs
  std::map<int,ColumnInfo*>::iterator it;
  for (it=ahffp->m_colinfo.begin(); it != ahffp->m_colinfo.end(); it++) {
    delete (*it).second;
  }

  ahffp->m_name2num.clear();
  ahffp->m_num2name.clear();
  ahffp->m_colinfo.clear();
}

// -----------------------------------------------------------------------------

void reloadColInfo(FilePtr ahffp, const std::string& name){
  clearColInfo(ahffp,name);
  loadColInfo(ahffp,name);
}

// -----------------------------------------------------------------------------

void reloadAllColInfo(FilePtr ahffp) {

  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");

  if (!isBintable(ahffp))
    AH_THROW_LOGIC(ahfits::errPrefix(ahffp,true,false)+"columns only present in binary table extensions");

  // get list of columns loaded
  std::list<std::string> colnames;
  std::list<std::string>::iterator lit;
  std::map<std::string,int>::iterator mit;
  for (mit=ahffp->m_name2num.begin(); mit != ahffp->m_name2num.end(); mit++)
    colnames.push_back(mit->first);

  // clear column information
  ahfits::clearAllColInfo(ahffp);

  // reload column info
  for (lit=colnames.begin(); lit != colnames.end(); lit++)
    if (ahfits::haveColumn(ahffp,*lit)) ahfits::loadColInfo(ahffp,*lit);
}

// -----------------------------------------------------------------------------

int name2Num(FilePtr ahffp, const std::string & name) {
  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");
  if (ahffp->m_name2num.count(name) == 0)
    AH_THROW_LOGIC(ahfits::errPrefix(ahffp,true,false)+"column, "+name+", not loaded");
  return ahffp->m_name2num[name];
}

// -----------------------------------------------------------------------------

std::string num2Name(FilePtr ahffp, int colnum) {
  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");
  if (ahffp->m_num2name.count(colnum) == 0) {
    std::stringstream msg;
    msg << "column number, " << colnum << ", not loaded";
    AH_THROW_LOGIC(ahfits::errPrefix(ahffp,true,false)+ahfits::errPrefix(ahffp,true,false)+msg.str());
  }
  return ahffp->m_num2name[colnum];
}

// -----------------------------------------------------------------------------

int searchColumn(FilePtr ahffp, const std::string & srch, ColumnList & collist, 
                 bool casesen) {

  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");

  if (!isBintable(ahffp))
    AH_THROW_LOGIC(ahfits::errPrefix(ahffp,true,false)+"columns only present in binary table extensions");

  // clear output list
  collist.clear();

  // get cfitsio case sensitivity constant
  int tcasesen=CASEINSEN;
  if (casesen) tcasesen=CASESEN;

  // run fits_get_colname until no more matches found
  int status=0;
  while (1) {
    char colname[FLEN_CARD] = "";    // *** QUESTION: is 32 characters enough?
    int colnum=0;
    fits_get_colname(ahffp->m_cfitsfp,tcasesen,const_cast<char *>(srch.c_str()),
                     colname,&colnum, &status);
    std::string tcol=colname;
    if (status == 0 || status == COL_NOT_UNIQUE) collist.push_back(tcol);
    if (status == 0 || status == COL_NOT_FOUND) break;
    if (status == COL_NOT_UNIQUE) continue;
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"problem searching for column fragment: "+srch+
                     statusMsg(status));
  }

  return collist.size();
}

// -----------------------------------------------------------------------------

void formColumnAttribute(FilePtr ahffp, const std::string& name, 
                         const std::string& prefix, std::string& out) {
  ahfits::loadColInfo(ahffp,name);
  ahfits::IndexType colnum=ahfits::name2Num(ahffp,name);
  std::stringstream key;
  key << prefix << colnum;
  out=key.str();
}

// -----------------------------------------------------------------------------

void setColumnAttribute(FilePtr ahffp, const std::string &name,
                        const std::string& keyword, const std::string& value,
                        bool chkintcol) {

  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");

  if (chkintcol && !ahfits::isIntegerTypeColumn(ahffp,name)) {
    std::stringstream msg;
    msg << "trying to set " << keyword << " keyword for non-integer type column: " << name;
    AH_THROW_LOGIC(ahfits::errPrefix(ahffp,true,false)+msg.str());
  }

  std::string key;
  ahfits::formColumnAttribute(ahffp,name,keyword,key);
  ahfits::writeKeyValStr(ahffp,key,value,"");
  ahfits::reloadColInfo(ahffp,name);

  // need to flush the file or else cfitsio won't update the new keyword value
  int status=0;
  fits_flush_file(ahffp->m_cfitsfp, &status);
}

// -----------------------------------------------------------------------------

void setColumnAttribute(FilePtr ahffp, const std::string &name,
                        const std::string& keyword, long long value,
                        bool chkintcol) {

  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");

  if (chkintcol && !ahfits::isIntegerTypeColumn(ahffp,name)) {
    std::stringstream msg;
    msg << "trying to set " << keyword << " keyword for non-integer type column: " << name;
    AH_THROW_LOGIC(ahfits::errPrefix(ahffp,true,false)+msg.str());
  }

  std::string key;
  ahfits::formColumnAttribute(ahffp,name,keyword,key);
  ahfits::writeKeyValLLong(ahffp,key,value,"");
  ahfits::reloadColInfo(ahffp,name);

  // need to flush the file or else cfitsio won't update the new keyword value
  int status=0;
  fits_flush_file(ahffp->m_cfitsfp, &status);
}

// -----------------------------------------------------------------------------

void setTNull(FilePtr ahffp, const std::string &name, long long nullval) {
  ahfits::setColumnAttribute(ahffp,name,"TNULL",nullval,true);
}

// -----------------------------------------------------------------------------

void setTUnit(FilePtr ahffp, const std::string &name, const std::string& unit) {
  ahfits::setColumnAttribute(ahffp,name,"TUNIT",unit,false);
}

// -----------------------------------------------------------------------------

void setTDisp(FilePtr ahffp, const std::string &name, const std::string& disp) {
  ahfits::setColumnAttribute(ahffp,name,"TDISP",disp,false);
}

// -----------------------------------------------------------------------------

void setTZero(FilePtr ahffp, const std::string &name, long long zero) {
  ahfits::setColumnAttribute(ahffp,name,"TZERO",zero,true);
}

// -----------------------------------------------------------------------------

void setTScale(FilePtr ahffp, const std::string &name, long long scale) {
  ahfits::setColumnAttribute(ahffp,name,"TSCALE",scale,true);
}

// -----------------------------------------------------------------------------

void setTLmin(FilePtr ahffp, const std::string &name, long long minval) {
  ahfits::setColumnAttribute(ahffp,name,"TLMIN",minval,false);
}

// -----------------------------------------------------------------------------

void setTLmax(FilePtr ahffp, const std::string &name, long long maxval) {
  ahfits::setColumnAttribute(ahffp,name,"TLMAX",maxval,false);
}


// -----------------------------------------------------------------------------
void setColumnDescription(FilePtr ahffp, const std::string &name, const std::string& desc) {

  std::string key;

  // Get keyword TTYPEn to modify its comment
  ahfits::formColumnAttribute(ahffp,name,"TTYPE",key);

  modifyKeyComment(ahffp,key,desc);
}

// -----------------------------------------------------------------------------

} // namespace ahfits

/* Revision Log
   $Log: ahfits_colinfo.cxx,v $
   Revision 1.37  2015/05/18 14:31:08  asargent
   Added modifyKeyComment and setColumnDescription functions

   Revision 1.36  2015/04/02 15:03:21  mwitthoe
   ahfits: add checks for NULL ahfits or cfitsio FITS pointers; see issue 461

   Revision 1.35  2014/11/03 21:38:30  mwitthoe
   ahfits: change names of setTlmin() & setTlmax() to setTLmin() & setTLmax to make them consistent with other functions

   Revision 1.34  2014/11/03 21:30:56  mwitthoe
   ahfits: add functions to set TLMIN/TLMAX for a column; refactored functions setting column attributes to reduce code duplication; related to issue 459

   Revision 1.33  2014/11/03 20:48:21  mwitthoe
   ahfits: add functions, setTZero() & setTScale(), in ahfits_colinfo for setting additional column properties; related to issue 459

   Revision 1.32  2014/10/07 14:38:59  mdutka
   TLMIN and TLMAX column attributed are stored as strings, TDMIN/TDMAX are ignored see redmine issue #418 for more details

   Revision 1.31  2014/04/04 17:05:07  mwitthoe
   ahfits: discovered two functions with similar capability: isIntegerTypeColumn in ahfits_base and isInteger in ahfits_colinfo; I like the former's name, but the latters implementation, so removed the first and renamed the second; changed all appropriate calls to these functions which were restricted within the ahfits library; the ahfits unit test and all tool unit tests still pass

   Revision 1.30  2014/03/24 21:15:36  mwitthoe
   ahfits: rewrite functions depending on try/catch clauses for non-error behavior; this was sending unnecessary messages to the AH_DEBUG stream; affected functions are loadColInfo(), haveColumn(), and HDUExists()

   Revision 1.29  2013/11/20 14:08:35  klrutkow
   corrected typo in a comment

   Revision 1.28  2013/10/16 12:53:54  peachey
   Use C++ #include and std namespace for strtol.

   Revision 1.27  2013/10/16 01:40:50  mwitthoe
   ahfits library: use ahfits::IndexType instead of long long for cfitsio count variables; see issue 270

   Revision 1.26  2013/10/16 01:14:30  mwitthoe
   ahfits library: add isBintable() calls to restrict certain functions to binary tables, mainly column information routines and the router; remove the function, column(), from ahfits_colinfo since it was only being used by the function, columnType()

   Revision 1.25  2013/10/15 22:29:35  mwitthoe
   ahfits: changed std::strtol to strtol (no std::) since it was giving build problems on Ubuntu; also added the include to stdlib.h where this function comes from

   Revision 1.24  2013/10/15 17:49:00  peachey
   (On behalf of Andy Sargent): correct loadColInfo and columnRepeat
   functions so that they accurately report the maximum repeat
   count of variable-width columns.

   Revision 1.23  2013/09/26 13:45:53  klrutkow
   added fits_flush_file() calls to setTNull, setTUnit, setTDisp in order to fix a perceived bug (issue 293)

   Revision 1.22  2013/07/17 20:40:58  mwitthoe
   ahfits refactoring: change setNull() to use column name instead of column number; remove getColumn* functions from colinfo (they were confusing); add functions to clear/reload information from single column; remove unused statusMessage function; mark several functions as internal

   Revision 1.21  2013/07/17 17:26:56  mwitthoe
   ahfits: add support for TDISP; refactoring related to code review (issue 266)

   Revision 1.20  2013/07/12 13:30:33  mwitthoe
   ahfits: write function to provide standard error message prefix; use prefix throughout ahfits library

   Revision 1.19  2013/07/10 14:50:51  mwitthoe
   ahfits: remove requirement that FITS file have at least one row for firstRow() and lastRow(); in setTNull() and setTUnit() make sure that column information is loaded before inserting keyword

   Revision 1.18  2013/07/10 02:11:22  mwitthoe
   ahfits: add function to set the TUNIT value for a column: setTUnit()

   Revision 1.17  2013/07/08 18:25:38  mwitthoe
   ahfits library: add setTNull() which will write the TNULLL keyword for integer-like columns; when making router connection with local NULL flags, require target, integer-like columns to have the TNULL keyword defined

   Revision 1.16  2013/06/18 01:51:25  mwitthoe
   change ahfits::reloadColInfo() to check for existence of column before reading header information

   Revision 1.15  2013/03/09 04:29:20  peachey
   Remove some unnecessary dynamic memory allocations in favor
   of automatic variables.

   Revision 1.14  2013/03/05 23:00:26  rshill
   Correct handling of -Ttype constants.

   Revision 1.13  2013/03/01 23:04:42  rshill
   Small changes extrapolated from code review by JP, RSH.

   Revision 1.12  2013/02/28 21:59:19  rshill
   Added TNULL and NaN capability for flagging undefined values.

   Revision 1.11  2013/01/11 19:40:53  mwitthoe
   move insert column functions from colinfo to file; add function to update column indices in routers after insertion of a column

   Revision 1.10  2013/01/09 22:06:00  mwitthoe
   add insert column functions to ahfits: insertColBefore() and insertColAfter(); also add function, reloadColInfo(), needed to reset loaded column indices after adding a column

   Revision 1.9  2012/12/11 18:52:50  mwitthoe
   add new function, searchColumn(), to ahfits which will search for a column name matching the given search string (containing wildcards)

   Revision 1.8  2012/12/07 23:00:11  mwitthoe
   make changes to ahfits recommended by Dec 7 code review (see http://zuul:8080/redmine/projects/astroh/wiki/2012-12-07_Ahfits_Code_Review)

   Revision 1.7  2012/11/28 15:11:37  mwitthoe
   fixed memory leak in ahfits_colinfo where 'delete' was used instead of 'delete []'

   Revision 1.6  2012/11/07 20:10:17  mwitthoe
   ahfits: add function, num2Name, in ahfits_colinfo which returns the column name given the column index; this function is used in ahfits::writeRow() to provide a more complete error message

   Revision 1.5  2012/11/04 23:25:20  mwitthoe
   ahfits: add cfitsio error messages based on error status; made clone/create functions that return a fits pointer to the newly created FITS file

   Revision 1.4  2012/10/18 14:38:19  mwitthoe
   fixed a bug in ahfits where column information was not being updated when moving to a new extension when routers were set up outside of the HDU loop; added a new function, reloadColInfo(), to ahfits_connect to perform this update; added test to ut_ahfits_row.cxx to check this scenario

   Revision 1.3  2012/10/12 22:50:00  mwitthoe
   ahfits: use pass-by-reference in scalar connections; add columnLength() function

   Revision 1.2  2012/10/11 21:23:51  mwitthoe
   add min/max column values to ahfits_colinfo

   Revision 1.1  2012/09/27 18:13:18  mwitthoe
   new version of ahfits (currently named ahfits2); removed ahFits prefix and redundant information from connection struct; in progress


*/

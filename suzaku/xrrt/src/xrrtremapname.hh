// xrrtremapname.hh
//
// Class definition for XrrtRemapName class
// Richard L Fink GSFC/631
// 1997/05/19
// 1997/09/22 Upgrade documentation. R. Fink

#ifndef XRRTREMAPNAME_HH
#define XRRTREMAPNAME_HH

//
// System interfaces used
//
#include <exception>
#include <string>
// Modified by H. Mori (2005/09/14)
// #include <stl.h>
#include <vector>

//
// XRRT interfaces used
//
#include "xrrt_types.hh"

// Compile for GCC 3.3.2
// Modified by H. Mori (2005/09/14)
using namespace std;

//
// XrrtRemapName handles the remapping of old reflection table names
// to new names.
//
class  XrrtRemapName
{
    public:
          // Constructor
          XrrtRemapName(); 
          // Copy Constructor
          XrrtRemapName( const XrrtRemapName& namePair ); 

          // 
          // Store a name remapping
          //
          void   setRemapName( const string oldName, const string newName);

          //
          // Get the new name for an old table name
          //
          string getRemapName(const string oldName) const;

    private:
           //
           // Old and new names
           //
           vector<string> oldRemapName;
           vector<string> newRemapName;



}; 


#endif

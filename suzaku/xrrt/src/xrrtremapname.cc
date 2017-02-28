// xrrtremapname.cc
//
// Member functions for XrrtRemapName class
//
// Richard L Fink GSFC/631
// 1997/05/30
// 1997/09/25 Upgrade documentation. R. Fink

#include "xrrtremapname.hh"


XrrtRemapName::XrrtRemapName():
    oldRemapName(),
    newRemapName()
{
// Constructor
}

string
XrrtRemapName::getRemapName( const string oldName) const
{
//
// Return a new reflection table name for an input table name.
// If the old name has not been remapped, return it unchanged.
//
Count numberOfEntrys;
Count i;

      numberOfEntrys = oldRemapName.size();
      for (i=0; i < numberOfEntrys; i++)
          {
          if (oldRemapName[i] == oldName)
             {
             return( newRemapName[i]);
             }
          }
       return( oldName);

}


void 
XrrtRemapName::setRemapName( const string oldName, const string newName)
{
    oldRemapName.push_back(oldName);
    newRemapName.push_back(newName);
}




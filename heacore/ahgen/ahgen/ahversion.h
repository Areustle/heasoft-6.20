/// \file ahversion.h
/// \brief Macros to assist in versioning of libraries and tasks
/// \author Mike Witthoeft
/// \date $Date: 2013/01/24 17:44:30 $

/// \addtogroup mod_ahgen
/// \section ahgen_ahversion Version Information Library - ahversion
///
/// This library contains macros to imprint source version numbers in
/// libraries and tools.
///
/// To use, define AHLABEL and AHCVSID in each library or tool
/// source code prior to any include statements.  AHLABEL is a
/// unique label to assist in creating non-conflicting variable
/// names; the value of AHCVSID should be \$Id\$ which will expand
/// to a version string once deposited in the CVS repository 
/// (note: replace \$ with $).
///
/// In the header, include ahversion.h followed by a call to the
/// AHVERSION macro which takes two arguments.  The first is a
/// label, different to what is used for AHLABEL.  The second
/// argument should be \$Id\$ to identify the version of the header
/// file.
///
/// Below are examples for a source and header file for the files
/// ahtest.h and ahtest.cxx in the ahhello library.
///
/// Source (at the top before any #includes):
///
/// \code
/// #define AHLABEL ahhello_ahtest
/// #define AHCVSID "\$Id\$"
/// \endcode
///
/// Header (after #ifndef, but before #includes):
///
/// \code
/// #include "ahgen/ahversion.h"
/// AHVERSION(AHHELLO_AHTEST,"\$Id\$")
///\endcode
///
/// The versioning information can be viewed from tool executables
/// or shared libaries with the ahsysinfo script, e.g.
///
/// \code
/// $ ahsysinfo ahhello.so
/// \endcode
///
/// or with the `strings` command:
///
/// \code
/// $ strings ahhello.so | grep Id
/// \endcode
///

#ifndef ahgen_ahversion_h
#define ahgen_ahversion_h

#ifndef AHLABEL
   #error Need to define AHLABEL
#endif

#ifndef AHCVSID
   #error Need to define AHCVSID
#endif


// the reason for having JOINTWO and JOINTHREE macros is to get the 
// AHLABEL macro to properly expand; I do not know why this layer of 
// obfuscation is needed
#define AHJOINTHREE(x,y,z) x##y##z
#define AHJOINTWOH(x,y) AHJOINTHREE(x,y,_H)
#define AHJOINTWOC(x,y) AHJOINTHREE(x,y,_C)
#define AHVERSION(x,y) const char* AHJOINTWOC(AHLABEL,x) = AHCVSID; \
                       const char* AHJOINTWOH(AHLABEL,x) = y;
#endif

/* Revision Log
 $Log: ahversion.h,v $
 Revision 1.5  2013/01/24 17:44:30  mwitthoe
 update Doxygen for ahgen

 Revision 1.4  2012/10/01 13:28:34  peachey
 Add guards to prevent multiple inclusion.

 Revision 1.3  2012/09/13 18:09:37  mwitthoe
 need to escape CVS keyword in ahversion description (take 2)

 Revision 1.2  2012/09/13 18:09:02  mwitthoe
 need to escape CVS keyword in ahversion description

 Revision 1.1  2012/09/13 17:56:06  mwitthoe
 add ahversion


*/

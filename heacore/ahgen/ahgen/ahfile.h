/// \file ahfile.h
/// \brief General functions acting on file names.
/// \author James Peachey
/// \date $Date: 2014/12/16 18:45:04 $

/// \addtogroup mod_ahgen
/// \section ahgen_ahfile General file functions - ahfile
///
/// This library contains:
///  - functions to check if a path exists or is equivalent to another path
///  - functions to add/remove the bang (!) preceding filenames to indicate the clobber state
///

#ifndef AHGEN_AHFILE_H
#define AHGEN_AHFILE_H 

#include "ahgen/ahversion.h"
AHVERSION(AHGEN_AHFILE,"$Id: ahfile.h,v 1.2 2014/12/16 18:45:04 mwitthoe Exp $")

#include "ahlog/ahlog.h"

/// \ingroup mod_ahgen
namespace ahgen {

/** \addtogroup mod_ahgen
 *  @{
 */

/// \brief Return true if file can be clobbered
/// \param[in] filename name of file
/// \param[in] clobber global clobber state
/// \return true if file can be clobbered
///
/// this function will check the global clobber state and look for a preceding
/// bang (!) in the file name to decide if the given file can be clobbered.  If
/// either/both the global clobber or bang are set, then return true.  
bool isFileClobbered(const std::string & filename, bool clobber);

/// \brief remove bang (!) from start of filename, if present
/// \param[in] filename name of file
std::string stripBang(const std::string & filename);

/// \brief add bang (!) to start of filename
/// \param[in] filename name of file
/// \param[in] clobber (optional) clobber state to decide to add/remove bang (default: true)
std::string addBang(const std::string & filename, bool clobber=true);

bool fileExists(const std::string & file);

/// \brief return true if given file paths are equivalent
/// \param[in] file1 first file
/// \param[in] file2 second file
///
/// This function will only return true if both file1 and file2 are on disk
/// and equivalent (via the stat command).  If either file is a URL on not
/// found, then false will be returned.
bool filePathsEquivalent(const std::string & file1, const std::string & file2);

/// \brief return true if given file path is a symbolic link
/// \param[in] file name of file
/// \param[in] file2exist true to require file2 to exist
bool filePathSymbolic(const std::string & file, bool file2exist);


/// \brief returns a full path concatenated with the filname for files 
/// which are located in the refdata area.  The tool will search for 
/// refdata area using the $LHEA_DATA enviroment varible and the $HEADAS 
/// enviroment varible
/// param[in] infile         string which could be "CALDB", "refdata" or a file 
///                          path which includes the file name
/// param[in] default file   string which will contain the path and filename to
///                          the desired file from the refdata area.  
/// return the full path concatenated with the file
std::string getRefDataPath(const std::string& infile,const std::string& default_file);

} // namespace ahgen

/** @} */

#endif   /* AHGEN_AHFILE_H */

/* Revision Log
 $Log: ahfile.h,v $
 Revision 1.2  2014/12/16 18:45:04  mwitthoe
 ahgen: change the filePathsEquivalent() function so that no errors are thrown if the stat() call fails on either function; in those cases, false is returned (see issue 469; change AH_OUT messages to AH_INFO in ut_BitBuf

 Revision 1.1  2014/09/23 04:00:05  mwitthoe
 ahgen: split some functions from main ahgen file into ahfile and ahrandom files; see issue 437


*/

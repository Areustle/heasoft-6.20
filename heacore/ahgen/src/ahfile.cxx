/// \file ahfile.cxx
/// \brief Implementation of public members of the libahgen:ahfile library.
/// \author James Peachey
/// \date $Date: 2014/12/16 18:45:04 $

#define AHLABEL ahgen_ahfile
#define AHCVSID "$Id: ahfile.cxx,v 1.2 2014/12/16 18:45:04 mwitthoe Exp $"

#include "ahgen/ahfile.h"
#include "ahlog/ahlog.h"

#include "ape/ape_error.h"
#include "ape/ape_trad.h"

#include <string>
#include <stdio.h>          // ::tolower
#include "sys/stat.h"       // lstat, stat
#include <algorithm>        // std::transform


namespace ahgen {

// -----------------------------------------------------------------------------

bool isFileClobbered(const std::string & filename, bool clobber) {
  if (clobber) return true;
  if (filename[0] == '!') return true;
  return false;
}

// -----------------------------------------------------------------------------

std::string stripBang(const std::string & filename) {
  if (filename[0] != '!') return filename;
  return filename.substr(1);
}

// -----------------------------------------------------------------------------

std::string addBang(const std::string & filename, bool clobber) {
  if (!clobber) return stripBang(filename);
  if (filename[0] == '!') return filename;
  return '!'+filename;
}

// -----------------------------------------------------------------------------

bool fileExists(const std::string & file) {
  struct stat fileStat;
  if(0 != lstat(stripBang(file).c_str(),&fileStat)) return false;
  return true;
} 

// -----------------------------------------------------------------------------

bool filePathsEquivalent(const std::string & file1, const std::string & file2) {

  // try to use stat() to get unique file node
  struct stat fileStat1,fileStat2;
  if (0 != stat(stripBang(file1).c_str(),&fileStat1)) return false;
  if (0 != stat(stripBang(file2).c_str(),&fileStat2)) return false;
  if (fileStat1.st_ino == fileStat2.st_ino) return true;
  return false;
}

// -----------------------------------------------------------------------------

bool filePathSymbolic(const std::string & file, bool file2exist) {
  struct stat fileStat;
  if (0 != lstat(stripBang(file).c_str(),&fileStat)) {
    if (!file2exist) return false;
    AH_THROW_RUNTIME("cannot stat file (does it exist?): "+file);
  }
  if (0 != S_ISLNK(fileStat.st_mode)) return true;
  return false;
}

// -----------------------------------------------------------------------------

std::string getRefDataPath(const std::string& infile,const std::string& default_file) {
  
  std::string name;
  std::string pPathHEADAS;
  std::string pPathLHEA_DATA;

  std::string linfile = infile;  

  //convert infile to lowercase
  std::transform(linfile.begin(), linfile.end(), linfile.begin(), ::tolower);

  pPathHEADAS = getenv("HEADAS");
  pPathLHEA_DATA = getenv("LHEA_DATA");
 
  if (linfile == "refdata"){
    name = pPathLHEA_DATA + "/refdata/" + default_file;
    if (ahgen::fileExists(name)) return name;
   
    name = pPathHEADAS + "/refdata/" + default_file;
    if (ahgen::fileExists(name)) return name;
  }
  return infile;  
}

// -----------------------------------------------------------------------------



} // namespace ahgen

/* Revision Log
 $Log: ahfile.cxx,v $
 Revision 1.2  2014/12/16 18:45:04  mwitthoe
 ahgen: change the filePathsEquivalent() function so that no errors are thrown if the stat() call fails on either function; in those cases, false is returned (see issue 469; change AH_OUT messages to AH_INFO in ut_BitBuf

 Revision 1.1  2014/09/23 04:00:05  mwitthoe
 ahgen: split some functions from main ahgen file into ahfile and ahrandom files; see issue 437


*/

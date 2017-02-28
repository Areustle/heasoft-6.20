/// \file ahfits_file_list.cxx
/// \brief ahfits: FITS file list operations and HDU navigation
/// \author James Peachey
/// \date $Date: 2012/11/29 02:57:18 $

#define AHLABEL ahfits_ahfits_file_list
#define AHCVSID "$Id: ahfits_file_list.cxx,v 1.1 2012/11/29 02:57:18 peachey Exp $"

#include "ahfits/ahfits_file_list.h"

#include "ahlog/ahlog.h"

#include <fstream>
#include <sstream>

namespace ahfits {

void expandFileList(const std::string & filename, ListStringType & filelist) {
  filelist.clear();
  if ('@' == filename[0]) {
    std::ifstream ifs(filename.c_str() + 1);
    if (!ifs.is_open())
      AH_THROW_RUNTIME(std::string("expandFileList could not open file ") + (filename.c_str() + 1));
    while (ifs) {
      char listedfile[FLEN_FILENAME] = "";
      ifs.getline(listedfile, FLEN_FILENAME, '\n');
      if (0 != ifs.gcount()) filelist.push_back(listedfile);
    }
    if (filelist.empty()) AH_THROW_RUNTIME("No files found in " + filename);
  } else {
    /// \todo confirm file exists?
    filelist.push_back(filename);
  }
}

} // namespace ahfits

/* Revision Log
   $Log: ahfits_file_list.cxx,v $
   Revision 1.1  2012/11/29 02:57:18  peachey
   Add initial facilities for handling lists of files (@files).

*/

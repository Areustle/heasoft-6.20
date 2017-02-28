/// \file ut_ahfits_file_list.cxx
/// \brief unit test for ahfits_file_list
/// \author James Peachey
/// \date $Date: 2012/11/29 02:57:17 $
 
#define AHLABEL ahfits_ut_ahfits_file_list
#define AHCVSID "$Id: ut_ahfits_file_list.cxx,v 1.1 2012/11/29 02:57:17 peachey Exp $"

#include "ahfits/ahfits.h"
#include "ahgen/ahtest.h"
#include "ahlog/ahlog.h"

#define DIFFLIST(label1, list1, label2, list2) \
  if (list1 != list2) { \
    FAILTEXT(std::string(label1) + " differs from " + label2); \
    if (list1.empty()) { \
      FAILTEXT(std::string(label1) + " is empty"); \
    } else { \
      FAILTEXT(std::string(label1) + " contains:"); \
      for (ahfits::ListStringType::const_iterator itor = list1.begin(); itor != list1.end(); ++itor) { \
        FAILTEXT(std::string("\t") + *itor); \
      } \
    } \
    if (list2.empty()) { \
      FAILTEXT(std::string("but ") + label2 + " is empty"); \
    } else { \
      FAILTEXT(std::string("but ") + label2 + " contains:"); \
      for (ahfits::ListStringType::const_iterator itor = list2.begin(); itor != list2.end(); ++itor) { \
        FAILTEXT(std::string("\t") + *itor); \
      } \
    } \
  }

// ---------------------------------------------------------------------------

void ut_file_list(void) {

  LABEL_TEST("Test expanding lists of files (\"@files\")");

  START_TEST("Test expanding one plain file name (list contains input)") {
    std::string filename("input/event_file_1.fits");
    ahfits::ListStringType expected_list;
    expected_list.push_back(filename); // List simply contains input FITS file.
    ahfits::ListStringType filelist;
    ahfits::expandFileList(filename, filelist);
    DIFFLIST("actual", filelist, "expected", expected_list)
  } END_TEST

  START_TEST_EXCEPTION("Test expanding one non-existent @file") {
    std::string filename("@non-existent.txt");
    ahfits::ListStringType expected_list; // Non-existent file should not expand.
    ahfits::ListStringType filelist;
    ahfits::expandFileList(filename, filelist); // This should throw.
    DIFFLIST("actual", filelist, "expected", expected_list) // Just in case.
  } END_TEST

  START_TEST("Test expanding an existing @file") {
    std::string filename("@input/event_file_list.txt");
    ahfits::ListStringType expected_list;
    expected_list.push_back("input/event_file_1.fits");
    expected_list.push_back("input/event_file_2.fits");
    expected_list.push_back("input/event_file_3.fits");
    ahfits::ListStringType filelist;
    ahfits::expandFileList(filename, filelist);
    DIFFLIST("actual", filelist, "expected", expected_list)
  } END_TEST
}

// -----------------------------------------------------------------------------

/* Revision Log
 $Log: ut_ahfits_file_list.cxx,v $
 Revision 1.1  2012/11/29 02:57:17  peachey
 Add initial facilities for handling lists of files (@files).

*/

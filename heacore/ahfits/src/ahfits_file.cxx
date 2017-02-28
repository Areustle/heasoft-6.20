/// \file ahfits_file.cxx
/// \brief ahfits: FITS file operations and HDU navigation
/// \author James Peachey
/// \date $Date: 2015/10/09 21:09:21 $

#define AHLABEL ahfits_ahfits_file
#define AHCVSID "$Id: ahfits_file.cxx,v 1.62 2015/10/09 21:09:21 mdutka Exp $"

#include "ahfits/ahfits_file.h"
#include "ahfits/ahfits_connect.h"
#include "ahfits/ahfits_colinfo.h"
#include "ahfits/ahfits_header.h"
#include "ahfits/ahfits_router.h"

#include "ahgen/ahgen.h"
#include "ahlog/ahlog.h"

#include "fitsio.h"

#include "sys/stat.h"
#include <string.h>
#include <sstream>

namespace ahfits {

// -----------------------------------------------------------------------------

void open(const std::string & filename, const std::string & extname, 
          FilePtr * ahffp) {

  // Check inputs.
  if (0 == ahffp)  AH_THROW_LOGIC("null pointer passed");
  if ("" == filename) AH_THROW_RUNTIME("empty string given as filename");

  // Initialize output variable to 0.
  *ahffp = 0;

  // Create local variable to hold created AhFitsFile object.
  FilePtr tahffp(new AhFitsFile(ahgen::stripBang(filename)));

  int status = 0;
  fits_open_file(&(tahffp->m_cfitsfp), filename.c_str(), READWRITE, &status);
  if (FILE_NOT_OPENED == status || READONLY_FILE == status) {
    tahffp->m_readonly=true;
    status = 0;
    fits_open_file(&(tahffp->m_cfitsfp), filename.c_str(), READONLY, &status);
  }
  if (0 != status) {
    close(tahffp);
    AH_THROW_RUNTIME("unable to open file for writing or reading "+filename+
                     statusMsg(status));
  }

  // Need to move to the correct extension.  If extname is not an empty string,
  // then it has priority: move to that extension.  If extname is an empty 
  // string, then check if current HDU number is greater than 1 (non-primary).
  // If so, then store current HDU number and update FilePtr, else call
  // ahfits::move()
  if (extname != "") {    // extension argument takes priority
    ahfits::move(tahffp, extname);
  } else {                // primary HDU or extended syntax
    int hduidx=0;
    fits_get_hdu_num(tahffp->m_cfitsfp,&hduidx);
    tahffp->m_hduidx=hduidx;
    ahfits::updateFilePtr(tahffp);
  }

  // Object is constructed; return it to the caller via the ahffp parameter.
  *ahffp = tahffp;

  // disable padding of table by buffer (assuming file will be edited)
  (*ahffp)->m_add_extrarows_when_buffering=false;

}

// -----------------------------------------------------------------------------

void create(const std::string & filename, const std::string & fits_template, 
            FilePtr * ahffp) {

  // Check inputs.
  if (0 == ahffp) AH_THROW_LOGIC("null pointer passed");
  if ("" == filename) AH_THROW_RUNTIME("empty string given as filename");

  // Initialize output variable to 0.
  *ahffp = 0;

  // Create local variable to hold created AhFitsFile object.
  FilePtr tahffp(new AhFitsFile(ahgen::stripBang(filename)));

  // check clobber state and ensure bang is included in filename
  bool clobber=ahgen::isFileClobbered(filename,ahfits::getClobber());
  std::string tfile=ahgen::addBang(filename,clobber);

  int status = 0;
  if ("" != fits_template) {
    tfile += '(' + fits_template + ')';
  }
  const char* cfile=tfile.c_str();
  if (0 != fits_create_file(&(tahffp->m_cfitsfp), cfile, &status)) {
    close(tahffp);
    AH_THROW_RUNTIME("unable to create file "+filename+statusMsg(status));
  }

  // add keywords for Primary (image) extension
  if ("" == fits_template) {
    if (0 != fits_create_img(tahffp->m_cfitsfp,16,0,0,&status)) {
      close(tahffp);
      AH_THROW_RUNTIME("unable to create Primary HDU "+filename+statusMsg(status));
    }
  }

  // If using a FITS file as a template, fits_create_file will retain
  // the value of NAXIS2 (number of rows).  We want to this function to
  // create only empty tables.
  ahfits::firstHDU(tahffp,ahfits::e_BINARY_TBL);
  do {
    if (ahfits::keywordExists(tahffp,"NAXIS2")) {
      long long nrows=ahfits::getKeyValLLong(tahffp,"NAXIS2");
      if (0 != fits_delete_rows(tahffp->m_cfitsfp,1,nrows,&status)) {
        ahfits::close(tahffp);
        AH_THROW_RUNTIME("unable to delete rows in binary table"+statusMsg(status));
      }
    }
  } while (ahfits::nextHDU(tahffp,ahfits::e_BINARY_TBL));

  // move to Primary HDU
  firstHDU(tahffp);

  // Object is constructed: return it to the caller via the ahffp parameter.
  *ahffp = tahffp;

  // enable padding of table by buffer (assuming file will be edited)
  (*ahffp)->m_add_extrarows_when_buffering=true;

}

// -----------------------------------------------------------------------------

void clone(const std::string & srcfile, const std::string & destfile, 
           FilePtr * ahffp, bool overwriteOnClobber) {

  int status = 0;

  // destination pointer must be NULL
  if (0 == ahffp) AH_THROW_LOGIC("null pointer passed");

  // Initialize output variable to 0.
  *ahffp = 0;

  // Check inputs: 1) source filename cannot be empty string
  //               2) destination filename cannot be empty string 
  //               3) destination filename cannot be symbolic link
  /// \internal
  /// \todo Do we need to trim spaces?
  if (srcfile.empty()) 
    AH_THROW_RUNTIME("empty string given for source file");
  if (destfile.empty()) 
    AH_THROW_RUNTIME("empty string given for destination file");
  if (ahgen::filePathSymbolic(destfile,false))
    AH_THROW_RUNTIME("cannot write to symbolic link");

  // strip extended syntax from input file so we can check if source file
  // is equivalent to the destination file
  char trootname[FLEN_FILENAME];
  if (0 != fits_parse_rootname(const_cast<char*>(srcfile.c_str()),trootname,&status)) {
    AH_THROW_RUNTIME("could not get root filename from: "+srcfile+
                     statusMsg(status));
  }
  std::string rootname=trootname;

  // if source and destination files are the same, then either throw error
  // (if clobber=no) or open source file (clobber=yes)
  if (ahgen::filePathsEquivalent(rootname,destfile)) {
    bool clobber=ahgen::isFileClobbered(destfile,ahfits::getClobber());
    if (overwriteOnClobber && clobber) {
      open(ahgen::stripBang(srcfile),"",ahffp);   // open w/ extended syntax
      return;
    } else {
      AH_THROW_RUNTIME("cannot clone since source and destination files are the same without enabling clobber");
    }
  }

  // open source file
  FilePtr ahffp_src=0;
  open(srcfile,"",&ahffp_src);

  // get name of extenstion that open() leaves you at; after the copy
  // operation, we will move to this extension in the output file
  int hduidx=ahffp_src->m_hduidx;

  // Create local variable to hold created AhFitsFile object.
  FilePtr tahffp(new AhFitsFile(ahgen::stripBang(destfile)));
  bool clobber=ahgen::isFileClobbered(destfile,ahfits::getClobber());
  std::string sfile=ahgen::addBang(destfile,clobber);
  if (0 != fits_create_file(&(tahffp->m_cfitsfp), sfile.c_str(), &status)) {
    close(ahffp_src);
    AH_THROW_RUNTIME("unable to create output file: "+destfile+
                     statusMsg(status));
  }

  // copy entire file using fits_copy_file, which opens up to the last HDU
  if (0 != fits_copy_file(ahffp_src->m_cfitsfp,tahffp->m_cfitsfp,true,true,true,
      &status)) {
    close(ahffp_src);
    close(tahffp);
    AH_THROW_RUNTIME("unable to clone file: "+srcfile+" -> "+destfile+
                     statusMsg(status));
  }

  // close input file
  close(ahffp_src);

  // Object is constructed: return it to the caller via the ahffp parameter.
  *ahffp = tahffp;

  // go to same HDU as opened input file; this will leave you in the primary
  // HDU or whatever is given by the FITS extended syntax
  (*ahffp)->m_hduidx=hduidx;
  if (0 != fits_movabs_hdu((*ahffp)->m_cfitsfp,hduidx,NULL,&status)) {
    AH_THROW_RUNTIME(ahfits::errPrefix(*ahffp,false,false)+
            "unable to move to correct HDU after cloning"+statusMsg(status));
  }
  ahfits::updateFilePtr(*ahffp);

  // disable padding of table by buffer (assuming file will be edited)
  (*ahffp)->m_add_extrarows_when_buffering=false;

}

// -----------------------------------------------------------------------------

void cloneSingleHDU(const std::string & srcfile, const std::string & extname,
                    const std::string & destfile, FilePtr & ahffp,
                    bool overwriteOnClobber) {

  int status = 0;

  // destination pointer must be NULL
  if (ahffp != NULL) AH_THROW_LOGIC("destination FITS pointer is not NULL, but must be");

  // Check inputs: 1) source filename cannot be empty string
  //               2) destination filename cannot be empty string 
  //               3) destination filename cannot be symbolic link
  /// \internal
  /// \todo Do we need to trim spaces?
  if (srcfile.empty()) 
    AH_THROW_RUNTIME("empty string given for source file");
  if (destfile.empty()) 
    AH_THROW_RUNTIME("empty string given for destination file");
  if (ahgen::filePathSymbolic(destfile,false))
    AH_THROW_RUNTIME("cannot write to symbolic link");

  // if source and destination files are the same, then either throw error
  // (if clobber=no) or open source file (clobber=yes)
  if (ahgen::filePathsEquivalent(srcfile,destfile)) {
    bool clobber=ahgen::isFileClobbered(destfile,ahfits::getClobber());
    if (overwriteOnClobber && clobber) {
      open(ahgen::stripBang(srcfile),"",&ahffp);
      return;
    } else {
      AH_THROW_RUNTIME("cannot clone since source and destination files are the same without enabling clobber");
    }
  }

  // open source file
  FilePtr ahffp_src=0;
  open(srcfile,extname,&ahffp_src);

  // create new FITS object, and open destination file
  ahffp=new AhFitsFile(ahgen::stripBang(destfile));
  bool clobber=ahgen::isFileClobbered(destfile,ahfits::getClobber());
  std::string sfile=ahgen::addBang(destfile,clobber);
  if (0 != fits_create_file(&(ahffp->m_cfitsfp), sfile.c_str(), &status)) {
    close(ahffp_src);
    AH_THROW_RUNTIME("unable to create output file: "+destfile+
                     statusMsg(status));
  }

  // copy entire file using fits_copy_file
  if (0 != fits_copy_file(ahffp_src->m_cfitsfp,ahffp->m_cfitsfp,false,true,
      false,&status)) {
    close(ahffp_src);
    AH_THROW_RUNTIME("unable to clone file: "+srcfile+" -> "+destfile+
                     statusMsg(status));
  }

  // close input file
  close(ahffp_src);

  // disable padding of table by buffer (assuming file will be edited)
  ahffp->m_add_extrarows_when_buffering=false;
}

// -----------------------------------------------------------------------------

bool HDUExists(FilePtr & ahffp, const std::string & extname) {

  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");
  std::string curhdu="";  // Primary HDU
  if (!ahfits::isPrimary(ahffp)) curhdu=ahfits::getKeyValStr(ahffp,"EXTNAME");
  int status=0;
  bool out=true;
  fits_movnam_hdu(ahffp->m_cfitsfp,ANY_HDU,const_cast<char*>(extname.c_str()),0,&status);
  if (0 == status) {
    out=true;
  } else if (BAD_HDU_NUM == status) {
    out=false;
  } else {
    AH_THROW_LOGIC(ahfits::errPrefix(ahffp,false,false)+"error in fits_movnam_hdu when trying to check existence of HDU: "+extname+statusMsg(status));
  }
  ahfits::move(ahffp,curhdu);    // return to original HDU
  return out;
}

// -----------------------------------------------------------------------------
void addHDU(FilePtr & ahffp_src, const std::string & extsrc, 
            FilePtr & ahffp_dest, const std::string & extdest) {

  int status=0;

  // ahffp_src and ahffp_dest must not be NULL
  if (0 == ahffp_src || 0 == ahffp_src->m_cfitsfp)
    AH_THROW_LOGIC("source FITS file pointer is NULL, but should not be");
  if (0 == ahffp_dest || 0 == ahffp_dest->m_cfitsfp)
    AH_THROW_LOGIC("destination FITS pointer is NULL, but should not be");

  // Check inputs: destination extension cannot be empty string
  if (extdest.empty())
    AH_THROW_RUNTIME("destination extension name cannot be empty string");

  // move source to given extension
  ahfits::move(ahffp_src,extsrc);

  // temporary FITS file pointer used to create new FilePtr instance in case
  // of ahffp_src == ahffp_dest (where fits_copy_header will fail)
  ahfits::FilePtr ahffp_tmp=0;
  bool closetmp=false;
  if (ahffp_src == ahffp_dest) {
    ahfits::open(ahffp_src->m_filename,extsrc,&ahffp_tmp);
    closetmp=true;
  } else {
    ahffp_tmp=ahffp_src;
  }

  // check that hdudest does not already exist in destination by trying
  // to move to that extension
  if(ahfits::HDUExists(ahffp_dest,extdest))
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp_dest,false,false)+"extension, "+
                     extdest+", already exists in destination"+statusMsg(status));

  // copy header info from source
  status=0;
  if (0 != fits_copy_header(ahffp_tmp->m_cfitsfp,ahffp_dest->m_cfitsfp,&status)) {
    if (closetmp) ahfits::close(ahffp_tmp);
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp_dest,false,false)+"failed to copy header"+statusMsg(status));
  }

  // rename extension and set number of rows to zero
  // note: using fits_delete_rows() did not work to empty the new table
  ahfits::writeKeyValStr(ahffp_dest,"EXTNAME",extdest,"");
  ahfits::writeKeyValLLong(ahffp_dest,"naxis2",0LL,"");

  // close temporary FITS pointer, if needed
  if (closetmp) ahfits::close(ahffp_tmp);

}


// -----------------------------------------------------------------------------

void addEmptyTbl(FilePtr & ahffp, const std::string & extname) {

  int status=0;

  // ahffp must not be NULL
  if (0 == ahffp || 0 == ahffp->m_cfitsfp)
    AH_THROW_LOGIC("destination FITS pointer is NULL, but should not be");

  // Check inputs: destination extension cannot be empty string
  if (extname.empty())
    AH_THROW_RUNTIME("destination extension name cannot be empty string");

  // +++ 2013-07-12 MCW I would rather hide this call inside of the Buffer
  // +++ 2013-07-12 MCW class, but I don't know how to trigger it
  ahfits::flushAndClearBuffers(ahffp);

  // convert extdest to char* for cfitsio function
  char* extnamechar=new char[extname.size()+1];
  strcpy(extnamechar,extname.c_str());

  // create the table extension
  char *ttype[1];
  char *tform[1];
  ttype[0] = 0;
  tform[0] = 0;

  status=0;
  if (0 != fits_create_tbl(ahffp->m_cfitsfp, BINARY_TBL, 0, 0, ttype, tform,
                           0, extnamechar, &status)) {
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,false,false)+"failed to create empty BINTABLE"+statusMsg(status));
  }

  // clean-up char*
  delete [] extnamechar;

  // set number of rows to zero
  ahfits::writeKeyValLLong(ahffp,"NAXIS2",0LL,"");

}

// -----------------------------------------------------------------------------

void move(FilePtr ahffp, const std::string & extname) {

  // Check inputs.
  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");

  // flush buffers from current extension
  ahfits::flushAndClearBuffers(ahffp);

  // Need the cast in the following because cfitsio doesn't know about "const".
  int status = 0;
  if ( extname.empty() ) {
    if (0 != fits_movabs_hdu(ahffp->m_cfitsfp,1,NULL,&status)) {
      AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,false,false)+
                       "unable to move to first extension"+statusMsg(status));
    }
  } else {
    if (0 != fits_movnam_hdu(ahffp->m_cfitsfp, ANY_HDU, 
        const_cast<char *>(extname.c_str()),0, &status)) {
      AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,false,false)+
                      "unable to move to extension "+extname+statusMsg(status));
    }
  }

  // store current HDU number
  fits_get_hdu_num(ahffp->m_cfitsfp,&ahffp->m_hduidx);

  ahfits::updateFilePtr(ahffp);
}

// -----------------------------------------------------------------------------

void move(FilePtr ahffp, int hdunum) {

  // Check inputs.
  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");

  // flush buffers from current extension
  ahfits::flushAndClearBuffers(ahffp);

  // Need the cast in the following because cfitsio doesn't know about "const".
  int status = 0;
  if (0 != fits_movabs_hdu(ahffp->m_cfitsfp, hdunum, 0, &status)) {
    std::stringstream msg;
    msg << ahfits::errPrefix(ahffp,false,false) << "unable to move to HDU number"
        << hdunum << statusMsg(status);
    AH_THROW_RUNTIME(msg.str());
  }

  // store current HDU number
  fits_get_hdu_num(ahffp->m_cfitsfp,&ahffp->m_hduidx);

  ahfits::updateFilePtr(ahffp);
}

// -----------------------------------------------------------------------------

bool firstHDU(FilePtr ahffp, HDUTypeEnum hdutype) {

  // Check inputs
  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");

  // flush buffers from current extension
  ahfits::flushAndClearBuffers(ahffp);

  // first, go to the primary HDU, which is an image HDU.  
  move(ahffp, "");
  
  // if we want the first ASCII or Binary table, go there, return result
  if (hdutype == ahfits::e_BINARY_TBL || hdutype == ahfits::e_ASCII_TBL ) {
    return nextHDU(ahffp, hdutype);
  }
  
  // if hdutype was e_PRIMARY_HDU, e_IMAGE_HDU, or e_ANY_HDU then we 
  // went there by going to primary.  If we didn't throw a runtime error above,
  // we can continue this function.
  
  // store current HDU number, update pointer
  fits_get_hdu_num(ahffp->m_cfitsfp,&ahffp->m_hduidx);
  ahfits::updateFilePtr(ahffp);
  
  return true;
}

// -----------------------------------------------------------------------------

bool nextHDU(FilePtr ahffp, HDUTypeEnum hdutype) {

  // Check inputs.
  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");

  // flush buffers from current extension
  ahfits::flushAndClearBuffers(ahffp);

  // cannot move to 'next' Primary since there is only one
  if (hdutype == ahfits::e_PRIMARY_HDU) return false;

  int cf_hdutype_wanted=ahfits::convertAhfitsToCfitsioHDUType(hdutype);

  // attempt to move to next HDU
  while (1) {
    int status=0;
    int cf_hdutype_gotten=0;
    fits_movrel_hdu(ahffp->m_cfitsfp,1,&cf_hdutype_gotten,&status);
    if (status == END_OF_FILE) {
      return false;
    } else if (status != 0) {
      AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+
                       "problem with moving to next HDU; cfits message: "+
                       statusMsg(status));
    } else if (hdutype == ahfits::e_ANY_HDU || 
               cf_hdutype_gotten == cf_hdutype_wanted) break;
  }

  // store current HDU number
  fits_get_hdu_num(ahffp->m_cfitsfp,&ahffp->m_hduidx);

  ahfits::updateFilePtr(ahffp);
  return true;
}

// -----------------------------------------------------------------------------

void close(FilePtr & ahffp) {
  if (0 != ahffp) {
    // the disconnect() call below will ultimately lead to buffers being flushed
    for (AhFitsFile::RouterIteratorType itor = ahffp->m_router.begin(); itor != ahffp->m_router.end(); ++itor) {
      (*itor)->disconnect();
    }
    ahffp->m_router.clear();

    clearAllColInfo(ahffp);
    if (0 != ahffp->m_cfitsfp) {
      int status = 0;
      for (int hdunum = 1; 
           0 == fits_movabs_hdu(ahffp->m_cfitsfp, hdunum, 0, &status); 
           ++hdunum) {
        fits_write_chksum(ahffp->m_cfitsfp, &status);
      }
      status = 0;
      fits_close_file(ahffp->m_cfitsfp, &status);
      ahffp->m_cfitsfp = 0;
    }

    delete ahffp;
    ahffp = 0;
  }
}

// -----------------------------------------------------------------------------

void insertColBefore(FilePtr ahffp, const std::string & colname, 
                     const std::string & format, const std::string & refcol) {

  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");

  if (!isBintable(ahffp))
    AH_THROW_LOGIC(ahfits::errPrefix(ahffp,true,false)+"can only insert columns in binary tables");

  // check if reference column exists
  if (refcol != "" && !haveColumn(ahffp,refcol)) {
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"reference column, "+
                     refcol+", does not exist");
  }

  // get number position of new column
  int colnum=0;
  if (refcol == "")    // insert at beginning
    colnum=1;
  else
    colnum=name2Num(ahffp,refcol);

  ahfits::insertColAt(ahffp,colname,format,colnum);
}

// -----------------------------------------------------------------------------

void insertColAfter(FilePtr ahffp, const std::string & colname, 
                    const std::string & format, const std::string & refcol) {

  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");

  if (!isBintable(ahffp))
    AH_THROW_LOGIC(ahfits::errPrefix(ahffp,true,false)+"can only insert columns in binary tables");

  // check if reference column exists
  if (refcol != "" && !haveColumn(ahffp,refcol)) {
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"reference column, "+
                     refcol+", does not exist");
  }

  // get number position of new column
  int colnum=0;
  if (refcol == "")    // insert at end
    colnum=ahfits::getKeyValLLong(ahffp,"TFIELDS")+1;
  else
    colnum=name2Num(ahffp,refcol)+1;

  ahfits::insertColAt(ahffp,colname,format,colnum);
}

// -----------------------------------------------------------------------------

void insertColAt(FilePtr ahffp, const std::string & colname, 
                 const std::string & format, int colnum) {

  if (0 == ahffp || 0 == ahffp->m_cfitsfp) AH_THROW_LOGIC("null pointer passed");

  if (!isBintable(ahffp))
    AH_THROW_LOGIC(ahfits::errPrefix(ahffp,true,false)+"can only insert columns in binary tables");

  // new column cannot be empty string
  if (colname == "")
    AH_THROW_LOGIC(ahfits::errPrefix(ahffp,true,false)+"cannot insert column with no name");

  // check if column already exists
  if (haveColumn(ahffp,colname)) {
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"column, "+colname+
                     ", already exists");
  }

  // flush buffers from current extension (since table structure is changing)
  // Note: this is really added so that the row count is read correctly in
  // updateFilePtr() (below); without resetting the buffers, temporary rows may
  // exist in the FITS file which will be interpretted as real rows in
  // updateFilePtr
  ahfits::flushAndClearBuffers(ahffp);

  // call cfitsio insert column function
  int status=0;
  if (0 != fits_insert_col(ahffp->m_cfitsfp,colnum,const_cast<char*>(colname.c_str()),
           const_cast<char *>(format.c_str()),&status)) {
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+
                     "failed to insert column"+statusMsg(status));
  }
  ahfits::updateFilePtr(ahffp);
}

// -----------------------------------------------------------------------------

void updateFilePtr(FilePtr ahffp) {
  if (isBintable(ahffp)) {
    ahffp->m_currow=1;
    if (isBintable(ahffp)) ahfits::reloadAllColInfo(ahffp);
    if (isBintable(ahffp)) ahfits::updateNumberOfRows(ahffp);
  }
}

// -----------------------------------------------------------------------------

void flushAndClearBuffers(FilePtr ahffp) {
  if (0 != ahffp) {
    if (isBintable(ahffp)) {
      for (AhFitsFile::RouterIteratorType itor = ahffp->m_router.begin(); itor != ahffp->m_router.end(); ++itor) {
        (*itor)->flushAndClearBuffers();
      }
    }
  }
}

// -----------------------------------------------------------------------------

void updateNumberOfRows(FilePtr ahffp) {
  int status=0;
  if (0 != fits_get_num_rowsll(ahffp->m_cfitsfp, &ahffp->m_numrow, &status)) {
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp,true,false)+"unable to get number of rows"
                     +statusMsg(status));
  }
  ahffp->m_numrow_with_padding=ahffp->m_numrow;
}

// -----------------------------------------------------------------------------

void addExtraRows(FilePtr ahffp, ahfits::IndexType n) {
  int status=0;
  if (isImage(ahffp)) return;    // nothing to do for image HDUs
  if (0 != fits_insert_rows(ahffp->m_cfitsfp,ahffp->m_numrow_with_padding,n,&status)) {
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp)+"failed to insert rows"+statusMsg(status));
  }
  ahffp->m_numrow_with_padding=ahffp->m_numrow+n;
}

// -----------------------------------------------------------------------------

void removeExtraRows(FilePtr ahffp) {
  if (isImage(ahffp)) return;    // nothing to do for image HDUs

  ahfits::IndexType todel=ahffp->m_numrow_with_padding-ahffp->m_numrow;
  if (todel <= 0) return;

  int status=0;
  ahfits::IndexType firstrow=ahffp->m_numrow+1;
  if (0 != fits_delete_rows(ahffp->m_cfitsfp,firstrow,todel,&status)) {
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp)+"failed to remove extra rows"+statusMsg(status));
  }
  ahfits::updateNumberOfRows(ahffp);

}

// -----------------------------------------------------------------------------

void removeAllRows(FilePtr ahffp) {
 
  if (isImage(ahffp)) return;    // nothing to do for image HDUs  

  ahfits::IndexType todel=ahffp->m_numrow;
  if (todel <= 0) return;

  int status=0;
  ahfits::IndexType firstrow=1;
  if (0 != fits_delete_rows(ahffp->m_cfitsfp,firstrow,todel,&status)) {
    AH_THROW_RUNTIME(ahfits::errPrefix(ahffp)+"failed to remove all rows"+statusMsg(status));
  }
  ahfits::updateNumberOfRows(ahffp);
  ahffp->m_numrow = 0;
  ahffp->m_currow = 1;
}

// ----------------------------------------------------------------------------- 

} // namespace ahfits

/* Revision Log
   $Log: ahfits_file.cxx,v $
   Revision 1.62  2015/10/09 21:09:21  mdutka
   adding new function removeAllRows to ahfits file

   Revision 1.61  2015/07/09 16:26:00  mwitthoe
   ahfits: the create() function will now ensure that new binary tables are empty

   Revision 1.60  2015/06/29 14:40:33  mwitthoe
   ahfits: add new function (ahfits::move) to allow moving to a FITS extension by number; the new function is an overload of ahfits::move where the argument is the extension name

   Revision 1.59  2015/04/02 15:03:21  mwitthoe
   ahfits: add checks for NULL ahfits or cfitsio FITS pointers; see issue 461

   Revision 1.58  2015/01/26 18:54:23  mwitthoe
   ahfits: allow ahfits to open gzipped FITS files in read-only mode; see Astro-H issue 456

   Revision 1.57  2014/12/16 18:46:30  mwitthoe
   ahfits: update call to ahgen::filePathsEquivalent(); the 3rd argument has been removed; see issue 469

   Revision 1.56  2014/11/26 15:12:45  mwitthoe
   ahfits: add clobber, buffer, and history states to library; library now accesses these states instead of those in ahgen; see issue 437

   Revision 1.55  2014/09/23 04:01:24  mwitthoe
   ahfits: update ahfits after change in argument list for ahgen::isFileClobbered; see issue 437

   Revision 1.54  2014/08/20 20:40:30  mwitthoe
   ahfits: add extended syntax support for ahfits::open() and ahfits::clone(); see issue 179

   Revision 1.53  2014/05/13 14:52:37  mwitthoe
   ahfits: add documentation and code tweaks based on the ahfits code review; see issue 376

   Revision 1.52  2014/04/01 14:57:06  mwitthoe
   ahfits: add member to ahfits FilePtr indicating whether extra rows are added to the end of the FITS file when buffering; see issue 368

   Revision 1.51  2014/03/31 18:07:49  mwitthoe
   ahfits: store temporary table row size in ahfits FilePtr instead of buffer instance; see issue 369

   Revision 1.50  2014/03/24 21:15:37  mwitthoe
   ahfits: rewrite functions depending on try/catch clauses for non-error behavior; this was sending unnecessary messages to the AH_DEBUG stream; affected functions are loadColInfo(), haveColumn(), and HDUExists()

   Revision 1.49  2014/01/14 16:35:37  mwitthoe
   ahfits: now flush buffers of binary tables before making a new image HDU; skip updating connections and cleaining up buffering if in image extension (this stuff happens automatically when closing a file or moving between HDUs)

   Revision 1.48  2014/01/06 16:29:28  asargent
   Added new function HDUExists to check whether the HDU exists or not

   Revision 1.47  2013/12/16 22:03:39  mwitthoe
   ahfits: fix ahfits::create(); only call fits_create_img if a template has not been provided (else an extra image extension is created); change BITPIX from 8 to 16 so our unit tests still pass

   Revision 1.46  2013/12/10 20:32:46  mwitthoe
   ahfits bug fixes: the create() function needed to add necessary keywords to the Primary HDU (now accomplished with fits_create_img); the old version was okay if immediately creating a new table after ahfits::create(), but created an incomplete FITS file if the newly created file was closed before any other action; the addHDU() function did not work if the active HDU was the primary HDU

   Revision 1.45  2013/10/16 01:40:50  mwitthoe
   ahfits library: use ahfits::IndexType instead of long long for cfitsio count variables; see issue 270

   Revision 1.44  2013/10/16 01:14:30  mwitthoe
   ahfits library: add isBintable() calls to restrict certain functions to binary tables, mainly column information routines and the router; remove the function, column(), from ahfits_colinfo since it was only being used by the function, columnType()

   Revision 1.43  2013/10/07 15:05:02  mwitthoe
   ahfits: fix bug in nextHDU() where the default value of ANY_HDU was not being checked properly causing the function to always return false

   Revision 1.42  2013/09/17 19:08:38  mwitthoe
   ahfits--ahfits_file: had to update addHDU() to new definition of empty string to mean primary HDU

   Revision 1.41  2013/09/16 20:43:40  klrutkow
   changed clone to end up at primary HDU instead of last HDU

   Revision 1.40  2013/09/16 20:12:06  klrutkow
   changed meaning of empty string to go to Primary HDU, in move() and open(), instead of 1st HDU, updated doxygen

   Revision 1.39  2013/09/11 20:22:33  mwitthoe
   ahfits library: ahfits_file.cxx: remove doxygen note which does not seem to apply anymore

   Revision 1.38  2013/09/10 20:56:01  mwitthoe
   ahfits: remove unused variable, cf_hdutype_wanted, from firstHDU() in ahfits_file

   Revision 1.37  2013/09/10 20:41:32  klrutkow
   added firstHDU function, and tests

   Revision 1.36  2013/09/10 14:10:35  mwitthoe
   ahfits library: begin to add support for primary HDU access; add enumerated type to identify type of extension (e.g. e_BINARY_TBL) and a function to convert from this enumeration to the equivalent cfitsio enumeration (e.g. BINARY_TBL); add optional argument to nextHDU() specifying the desired HDU type to go to (default: e_ANY_HDU); add unit tests to check that new argument acts appropriately

   Revision 1.35  2013/08/30 15:19:23  mwitthoe
   ahfits: add standard error prefix to insertColAt() when an empty string is given as the column name

   Revision 1.34  2013/07/17 21:38:02  mwitthoe
   ahfits: remove duplicated code from insertCol* functions by adding a function insertColumnAt; change buffer OVERlAP constant to 0

   Revision 1.33  2013/07/17 20:40:58  mwitthoe
   ahfits refactoring: change setNull() to use column name instead of column number; remove getColumn* functions from colinfo (they were confusing); add functions to clear/reload information from single column; remove unused statusMessage function; mark several functions as internal

   Revision 1.32  2013/07/17 17:26:56  mwitthoe
   ahfits: add support for TDISP; refactoring related to code review (issue 266)

   Revision 1.31  2013/07/16 20:10:06  mwitthoe
   ahfits: fix large inefficiency when writing a new file with varaible-length columns; now, when appending a file, a number of blank rows (equal to the buffer size) are added to the end of the file once the end of file is reached; extra rows are removed when the buffer closes

   Revision 1.30  2013/07/12 22:16:26  mwitthoe
   ahfits buffering: fix bug where buffer was not properly flushed and reset when moving to a new extension but keeping router connections active

   Revision 1.29  2013/07/12 13:30:33  mwitthoe
   ahfits: write function to provide standard error message prefix; use prefix throughout ahfits library

   Revision 1.28  2013/07/11 17:07:20  mwitthoe
   ahfits: add hdu index as a member of FilePtr in order to eliminate access to the disk in the stamp() function; stamp() is potentially called with every writeRow() call and, with buffering, the whole idea is not to access the file all the time.

   Revision 1.27  2013/07/10 18:25:58  mwitthoe
   ahfits: add flag to FilePtr structure indicating if parametes should be written to the FITS header for modified files; the default value is set by the current global history state in ahgen; functions are added to ahfits to change the flag for an individual FITS file

   Revision 1.26  2013/07/09 17:43:33  rshill
   Added functionality to build tables from scratch.

   Revision 1.25  2013/07/01 15:14:45  rshill
   Deleted deprecated routines (which returned FilePtr).

   Revision 1.24  2013/06/17 15:54:34  mwitthoe
   ahfits: add function, clearConnections(), to the router class to allow for changing extension within a file which has different columns than the original extension; needed to have the reloadColInfo() function check to see if a column exists before reloading the information from the header

   Revision 1.23  2013/05/13 19:45:51  mwitthoe
   add buffering to ahfits (no TBIT support yet)

   Revision 1.22  2013/03/09 04:12:14  peachey
   Manage Routers (and indirectly Connections) using the member
   container of Routers instead of the facilities in the
   now-deprecated ahfits_connect.h. In particular use the
   updateConnections function to renumber columns when they are
   inserted. Plug a memory leak in addHDU.

   Revision 1.21  2013/01/20 04:46:24  mwitthoe
   ahfits::addHDU was not using one of its arguments, extsrc, which specifies the extension to use as a template for the new HDU

   Revision 1.20  2013/01/11 19:40:53  mwitthoe
   move insert column functions from colinfo to file; add function to update column indices in routers after insertion of a column

   Revision 1.19  2012/12/18 19:42:34  mwitthoe
   change ahfits::open() call in ahfits_file to version which returns void (other prototype is obsolete)

   Revision 1.18  2012/12/10 19:21:19  mwitthoe
   ahfits: added obsolete warnings to open/create/clone functions returning a FilePtr; made FilePtr argument for clone consistent with open/create; changed ahfits unit tests to use non-obsolete open/create/clone functions

   Revision 1.17  2012/12/10 18:30:44  mwitthoe
   remove incorrect obsolete warning from ahfits::create (returning void)

   Revision 1.16  2012/12/07 23:00:11  mwitthoe
   make changes to ahfits recommended by Dec 7 code review (see http://zuul:8080/redmine/projects/astroh/wiki/2012-12-07_Ahfits_Code_Review)

   Revision 1.15  2012/11/13 18:17:00  mwitthoe
   add open/create/clone functions in ahfits which return a FilePtr instead of passing it by reference; the old functions are still present, but slated for removal

   Revision 1.14  2012/11/07 03:25:05  mwitthoe
   ahfits: move to first extension at the end of clone(); some consistency changes to open() involving strings

   Revision 1.13  2012/11/05 01:35:50  mwitthoe
   add m_readonly member to AhFitsFile struct to mark a FITS file as read-only; all writing routines (row and headers) now check the readonly status before writing, but there is no way currently to set the read-only false to true (except as the backup readonly fits_open_file call in ahfits::open

   Revision 1.12  2012/11/04 23:25:20  mwitthoe
   ahfits: add cfitsio error messages based on error status; made clone/create functions that return a fits pointer to the newly created FITS file

   Revision 1.11  2012/11/02 17:13:05  mwitthoe
   ahfits: cleanp up how clobber is handled in open/create/clone cunctions

   Revision 1.10  2012/11/01 20:49:38  mwitthoe
   ahfits: clobber is not handled internally in create/clone functions; open() ignores bang in filename; open/create now use strings as arguments

   Revision 1.9  2012/10/24 17:02:59  mwitthoe
   changed ahfits::addHDU() to work on file names instead of open FITS pointers

   Revision 1.8  2012/10/24 15:00:07  mwitthoe
   ahfits: move stamp() from ahfits_file to ahfits_header; new function addHDU in ahfits_file which will make a new (empty) extension based on the header of an existing extension from the same or different file

   Revision 1.7  2012/10/18 17:24:08  mwitthoe
   fixed but in ahfits involving iterating over multimap in getConnectedColumns in ahfits_connect

   Revision 1.6  2012/10/18 14:38:19  mwitthoe
   fixed a bug in ahfits where column information was not being updated when moving to a new extension when routers were set up outside of the HDU loop; added a new function, reloadColInfo(), to ahfits_connect to perform this update; added test to ut_ahfits_row.cxx to check this scenario

   Revision 1.5  2012/10/12 22:50:00  mwitthoe
   ahfits: use pass-by-reference in scalar connections; add columnLength() function

   Revision 1.4  2012/10/11 17:56:56  mwitthoe
   include parameter stamping in ahfits

   Revision 1.3  2012/10/10 20:30:56  mwitthoe
   ahfits: complete overloaded connection functions; add test code

   Revision 1.2  2012/10/01 17:21:57  mwitthoe
   new version of ahfits with connections stored outside of FITS file object

   Revision 1.1  2012/09/27 18:13:18  mwitthoe
   new version of ahfits (currently named ahfits2); removed ahFits prefix and redundant information from connection struct; in progress


*/

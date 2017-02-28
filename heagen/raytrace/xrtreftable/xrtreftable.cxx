/// \file xrtreftable.cxx
/// \brief Calculate reflectivity probability for ray tracing
/// \author Kristin Rutkowski, Tahir Yaqoob
/// \date $Date: 2016/03/23 02:06:39 $

/** 

\defgroup tool_xrtreftable Calculate reflectivity tables for raytracing

The xrtreftable tool calculates the reflectivity and transmission probability, 
to be used for ray tracing.

Source files:

  xrtreftable.cxx
  xrtreftable_lib.cxx
  xrtreftable_lib.h

Library dependencies:

  heacore/ape
  heacore/heaapp
  heacore/heautils
  heacore/cfitsio
  heacore/ahlog

Modification history:

  Ver   Date         Author  Description
  1.0   2015-08-10   KLR    Clean-up code



*/


#define AHLABEL tool_xrtreftable
#define AHCVSID "$Id: xrtreftable.cxx,v 1.37 2016/03/23 02:06:39 klrutkow Exp $"
#define TOOLTAG "$Name: heasoft6_20_20170113 $"

#include "xrtreftable_lib.h"

// heaapp access for start-up and shut-down
#include "heaapp/heaapp.h"

// needed by the startUp and shutDown functions
#include "heaapp/heaapp_ape.h"
#include "heaapp/heaapp_ahlog.h"
#include "heaapp/heaapp_heautils.h"

#include "headas_utils.h"   // for get_history, headas_clobpar, etc

#include <cmath>            // for abs() in initialize_surface()


/** \addtogroup tool_xrtreftable
 *  @{
 */

 
// Pre-collimator material.  For AstroH it's aluminum.
// +++ CR or have a func "isCollimator"?
static const std::string s_precollMaterial = "Al";

// extension name for mass absorption coefficients.  It's a char * so 
// we can pass directly to CFITSIO call without .c_str()
static const char * s_massAbsExtName = "MASS_ABSORPTION";

// max epected # elem in files.  This is used to resize atomic data and 
// scattering data vectors, so that we can store entries by Z and not require
// sorted input files
static const int s_maxzofelements = 120;  


/**********************************************
 * ********************************************
 * 		Declare the standard functions
 * ********************************************
**********************************************/


// include startup and shutdown functions from heaapp
int startUp(int argc, char ** argv, const char * tooltag, HeaAppData * appdata);
int shutDown(HeaAppData * appdata);

/// \brief Get parameter values from the xrtreftable.par file and 
///        command input
/// \param[out] param        structure holding all parameter data user entered
void getPar(Param & param);

/// \brief Setup tool operation by, for example: copying files, loaded
///  data sets, allocating memory, etc
/// \param[out] param Param struct that holds all the parameters from the .par
/// \param[out] atom Atom struct that holds all atomic data
/// \param[out] scat Scattering struct that holds all atomic scattering data
/// \param[out] surf Surface struct that holds all the vectors for the surface
/// \param[out] energy Energy struct that holds all energy data
/// \param[out] angle Angle struct that holds all angle data
/// \param[out] edge Edge struct that holds all atomic edge data
void initialize(Param & param, AtomicData & atom, Scattering & scat, 
                Surface & surf, Energy & energy, Angle & angle, Edge & edge);

/// \brief Do the actual work of the tool
/// \param[in] param Param struct that holds all the parameters
/// \param[out] atom Atom struct that holds all atomic data
/// \param[in] scat Scattering struct that holds all atomic scattering data
/// \param[in] surf Surface struct that holds all the vectors for the surface
/// \param[in] energy Energy struct that holds all energy data
/// \param[in] angle Angle struct that holds all angle data
/// \param[in] edge Edge struct that holds all atomic edge data
void doWork(Param & param, AtomicData & atom, Scattering & scat, Surface & surf, Energy & energy, 
            Angle & angle, Edge & edge);

/// \brief Closes the output fits file.  When not in debug mode, this is run
///        even if an earlier function throws an error, in which case it deletes
///        the file.  When in debug mode it closes, but does not delete, the 
///        output file.
void finalize(Param & param);


/**********************************************
 * ********************************************
 * 		Declare initialize() helper functions
 * ********************************************
**********************************************/

/// \param[in] param Param struct that holds all the parameters
/// \param[out] atom Atom struct that holds all atomic data
void initialize_atomic(const Param & param, AtomicData & atom);


/// \param[in] param Param struct that holds all the parameters
/// \param[in] atom Atom struct that holds all atomic data
/// \param[out] surf Surface struct that holds all the vectors for the surface
void initialize_surface(const Param & param, AtomicData & atom, Surface & surf);


/// \param[in] param Param struct that holds all the parameters
/// \param[out] scat Scattering struct that holds all atomic scattering data
void initialize_scattering(Param & param, Scattering & scat);


/// \param[in] param Param struct that holds all the parameters
/// \param[out] energy Energy struct that holds all energy data
void initialize_energy(const Param & param, Energy & energy);


/// \param[in] param Param struct that holds all the parameters
/// \param[out] angle Angle struct that holds all angle data
void initialize_angle(const Param & param, Angle & angle);


/// \param[in] param Param struct that holds all the parameters
/// \param[in] scat Scattering struct that holds all atomic scattering data  
/// \param[out] edge Edge struct that holds all atomic edge data
void initialize_edge(const Param & param, const Scattering & scat, Edge & edge);

/// \brief Does initialization on the output file - opens the file,
///        creates the output extension
/// \param[in] param Param struct that holds all the parameters
/// \param[in] energy Energy struct that holds all energy data
/// \param[in] angle Angle struct that holds all angle data
/// \param[in] surf Surface struct that holds all the vectors for the surface
void initialize_output(Param & param, Energy & energy, Angle & angle, Surface & surf);

/// \brief writes keywords to headers in output fits file
/// \param[in] param Param struct that holds all the parameters from the .par
/// \param[in] surf Surface struct that holds all the vectors for the surface
/// \param[in] energy Energy struct that holds all energy data
/// \internal
/// \note even though the "history" parameter dictates whether to write all
///       the parameters to the header, this function writes the filenames to
///       the header also, to ensure that the input filenames are recorded.
///       So if history=yes, these params will be written twice.
void writeKeywordsToOutput(Param & param, Surface & surf, Energy & energy);

/// \brief writes input parameters to history keywords in output fits file
/// \param[in] param Param struct that holds all the parameters from the .par
void writeHistoryKeywords(Param & param);

/* +++ uncomment this #define in order to print out all the statements that
   ensure that data was read and stored properly.  Each new loop starts with
   a statement "**** test that I've ...".  It might be a good idea to redirect 
   output to a log file ('./xrtreftable mode=h > xrtreftable-output.log') 
   because the output is quite long (for example, it prints all 4000 from the 
   angle grid).  This is only temporary, and once I'm done debugging I will
   change all these to AH_DEBUG statements (but I don't want to see these in
   in the meantime) */
//#define DEBUG



/**********************************************
 * ********************************************
 * 		Define main() function
 * ********************************************
**********************************************/

/// \brief xrtreftable tool main function
int main(int argc, char** argv) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  // variables needed as part of this main() pattern.
  int finalstatus = 0;        // Global status of tool run. Set to non-0 at the 
                              // first error and remains non-0 thereafter.
  int status = 0;             // Status of an individual function call. Reused 
                              // for the next function call.
  HeaAppData appdata = { 0 }; // Application data, used by startUp/shutDown to 
                              // initialize support libraries.
  
  // declare structs to hold all the input data
  Param param;
  AtomicData atom;
  Scattering scat;
  Surface surf;
  Energy energy;
  Angle angle;
  Edge edge;
  
  // -------------------------------------
  
  
  // Global initializations. Set up logging streams, open parameter file,
  // handle universal parameters like "debug" and "chatter", etc.
  status = startUp(argc, argv, TOOLTAG, &appdata);
  if (0 != status) {
    // +++ ahlog_err or AH_ERR ?
    ahlog_err("main", "startUp returned status %d, not 0 as expected.\n", status);
    finalstatus = 1; // Latch error status
  }

  if (0 == status) {
    
    if (ahlog::get_debug()) {
      
      AH_DEBUG << "Entering debug mode." << std::endl;
      
      getPar(param);
      
      // Write all parameters to the log file.  When debug=yes, the params will 
      // be written twice: here, and again at the end of initialize.  That is 
      // to ensure against a possible failure in initialize().
      writeParametersToLog();
      
      initialize(param, atom, scat, surf, energy, angle, edge);
      doWork(param, atom, scat, surf, energy, angle, edge);
      finalize(param);
      
    } else {
      
      try {
        getPar(param);
        initialize(param, atom, scat, surf, energy, angle, edge);
        doWork(param, atom, scat, surf, energy, angle, edge);
      } catch (const std::exception &x) {
        
        // Write all parameters to the log file.  This may be the second time 
        // they are written (the first was at the end of initialize()), just 
        // in case there was a failure in initialize() or doWork().
        writeParametersToLog();
        
        finalstatus = 1;
        AH_ERR << "During main: " << x.what() << std::endl;
      }
      try {
        finalize(param);
      } catch (const std::exception &x) {
        finalstatus = 1;
        AH_ERR << "During finalize: " << x.what() << std::endl;
      }
    
    }
    
  }
  
  // +++ surround with a try-catch block?
  // Perform global clean-up in parallel to startUp function. Close parameter file, close
  // logging streams, etc.
  status = shutDown(&appdata);
  if (0 != status) {
    // Note that after shutDown, the usual error message streams no longer 
    // print anything. Thus, in the unlikely case that shutDown signaled an 
    // error, it must be reported here with appdata's printerr function. This 
    // will still print to the user's terminal even after the streams have been 
    // shut down. Also note this message is not strictly necessary because 
    // shutDown will log its own errors, but this is included just to 
    // demonstrate how heaapp's message functions work.
    appdata.printerr("main", "shutDown returned status %d, not 0 as expected.\n", status);
    finalstatus = 1;
  }

  // Return the final status
  return finalstatus;
  
}   // end main()



// ****************************************************************************


/**********************************************
 * ********************************************
 * 		Define the standard functions
 * ********************************************
**********************************************/

// get the values from the .par file and store them in a Param structure
void getPar(Param & param) {
  
  // input/output filenames and extensions
  param.m_telescop      = getParString("telescop");
  param.m_instrume      = getParString("instrume");
  param.m_atomicfile    = getParString("atomicfile");
  param.m_atmscafile    = getParString("atmscafile");
  param.m_atmsctng    = getParString("atmsctng");
  param.m_energyfile    = getParString("energyfile");
  param.m_anglefile     = getParString("anglefile");
  param.m_mirrorfile    = getParString("mirrorfile");
  param.m_outfile       = getParString("outfile");
  param.m_outext        = getParString("outext");
  
  // which roughness model to use
  param.m_roughmodel    = getParString("roughmodel");
  // values to write to keywords in output file
  param.m_roughkey      = getParString("roughkey");
  
  param.m_validdate     = getParString("validdate");
  param.m_validtime     = getParString("validtime");
  param.m_desc          = getParString("desc");

  // standard params (clobber, chatter, logfile, debug, history, mode) are 
  // retrieved in ahlog::start_up()
  
  // +++ 20150824 remove atmedgefile param
  //param.m_atmedgefile   = getParString("atmedgefile");
  param.m_atmedgefile    = "NONE";

  // ***********************
  // resolve CALDB parameters  
  // ***********************
  
  // resolve them, then record actual path in par file and in history keywords
  // +++ 20150721 KLR use m_validdate and m_validtime for CALDB query?
  
  // atomic data file
  param.m_atomicfile = resolve(param.m_atomicfile, "atomic data", "INS", "-", "ATOMDATA", "-", "-", "GEN");
  ape_trad_set_string("atomicfile",param.m_atomicfile.c_str());
  
  // atomic scattering factors file
  std::string expr = "ATMSCTNG.eq." + param.m_atmsctng;
  param.m_atmscafile = resolve(param.m_atmscafile, "atomic scattering", "GEN", "-", "SFATOM", "-", expr, param.m_telescop);
  ape_trad_set_string("atmscafile",param.m_atmscafile.c_str());
  
  // mirrorfile
  param.m_mirrorfile = resolve(param.m_mirrorfile, "mirror", param.m_instrume, "-", "SURFACE", "-", "-", param.m_telescop);
  ape_trad_set_string("mirrorfile",param.m_mirrorfile.c_str());
  
  // Write all parameters to the log file.  This is placed here so that any 
  // CALDB filenames are resolved.
  writeParametersToLog();
  
  // *******************************************
  // make sure we're going to correct extensions, 
  // else code would just go to the first extension
  // (this is only a concern if user supplied files)
  // *******************************************
  
  if (param.m_mirrorfile.find("[") == std::string::npos) {
    AH_INFO(ahlog::HIGH) << "An extension wasn't provided for the mirror file.  Manually assigning extension 'SURFACE'" << std::endl; 
    param.m_mirrorfile.append("[SURFACE]");
  }
  if (param.m_atomicfile.find("[") == std::string::npos) {
    AH_INFO(ahlog::HIGH) << "An extension wasn't provided for the atomic data file.  Manually assigning the extension 'ATOMDATA'" << std::endl; 
    param.m_atomicfile.append("[ATOMDATA]");
  }
  if (param.m_atmscafile.find("[") == std::string::npos) {
    AH_INFO(ahlog::HIGH) << "An extension wasn't provided for the atomic scattering file.  Manually assigning the extension '"<<param.m_atmsctng<<"', from the 'atmsctng' parameter" << std::endl; 
    param.m_atmscafile.append("["+param.m_atmsctng+"]");
  }
  
  AH_DEBUG << "param.m_atomicfile = " << param.m_atomicfile << std::endl;
  AH_DEBUG << "param.m_atmscafile = " << param.m_atmscafile << std::endl;
  AH_DEBUG << "param.m_mirrorfile = " << param.m_mirrorfile << std::endl; 
  
}   // end getPar()


// ----------------------------------------------------------------------------


void initialize(Param & param, AtomicData & atom, Scattering & scat, 
                Surface & surf, Energy & energy, Angle & angle, Edge & edge) {

  // initialize the data structures default data
  //+++param = Param();
  param.m_errorOccurred = false;
  
  // initialize the output file pointer to NULL
  param.m_out_fp = NULL;
  
  // load atomic data
  initialize_atomic(param, atom);
  
  // load scattering file data
  initialize_scattering(param, scat);  

  // load SURFACE extension of telescope definition file
  initialize_surface(param, atom, surf);
  
  // load energy grid fits file
  initialize_energy(param, energy);
  
  // load angle grid fits file
  initialize_angle(param, angle);
  
  // load the atomic edges file
  initialize_edge(param, scat, edge);
  
  // initialize the output fits file
  initialize_output(param, energy, angle, surf);
  
}   // end initialize()


// ----------------------------------------------------------------------------


void doWork(Param & param, AtomicData & atom, Scattering & scat,
             Surface & surf, Energy & energy, Angle & angle, Edge & edge) {

  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  // for cfitsio calls in using the output file
  int status = 0;                   
  std::string currColName;
  std::stringstream currColNumStrStr;
  std::string currColNumStr;
  int currColNum = 0;
  int currColNumRefl = 0;
  int currColNumTran = 0;
  std::string currColNameRefl;
  std::string currColNameTran;
  
  // f1 and f2 for [num of energies][num of materials]
  std::vector< std::vector<double> > materialf1factor;
  std::vector< std::vector<double> > materialf2factor;
  // mass absorption coefficients for [num of energies][num of materials]
  std::vector< std::vector<double> > massabscoeff;
  // reflectivity as a function of energy and incident angle for one group
  std::vector< std::vector<double> > rprob;
  // transmission as a function of energy and incident angle for one group
  std::vector< std::vector<double> > tprob;
  
  // input for calcsf1sf2materials()
  double currEnergy = 0.;
  double currEnergycm = 0.;
  double currEnergycmsq = 0.;
  
  // output vectors for calcsf1sf2materials() and 
  // input vectors for multilayerRefl()
  std::vector<double> sf1material;
  std::vector<double> sf2material;
  std::vector<double> mabsmaterial;
  
  // output vectors for multilayerRefl()
  std::vector<double> sreflect;
  std::vector<double> strans;
  
  // add 1 because columns are 1-based; add 1 because energy is first column
  const int colOffset = 2;
  
  // -------------------------------------
  
  
  // output to the user a summary of how many calculations will be done
  AH_OUT << "*************************************" << std::endl;
  AH_OUT << "SUMMARY OF INPUT DATA FOR XRTREFTABLE" << std::endl;
  AH_OUT << "*************************************" << std::endl;
  AH_OUT << "Number of energies to loop over: " << energy.m_numenergy << std::endl;
  AH_OUT << "Number of angles to loop over: " << angle.m_numangles << std::endl;
  AH_OUT << "Number of groups of materials on this surface: " << surf.m_numgroups << std::endl;
  AH_OUT << "The following layer counts do not include substrate layers." << std::endl;
  for (int i = 0 ; i < surf.m_numgroups ; i++) {
    AH_OUT << "Group " << i+1 << " has " << surf.m_numlayerplusone[i]-1 << " layers" << std::endl;
  }
  AH_OUT << "Number of unique materials found in thin films: " << surf.m_numThinMaterials << std::endl;
  for (unsigned int i = 0 ; i < surf.m_materialinthinlayer.size() ; i++) {
    if (surf.m_materialinthinlayer[i] > 0) {
      AH_OUT << "     " << surf.m_materialformula[i] << std::endl;
    }
  }
  AH_OUT << "Number of unique materials found in substrates: " << surf.m_numThickMaterials << std::endl;
  for (unsigned int i = 0 ; i < surf.m_materialinthicklayer.size() ; i++) {
    if (surf.m_materialinthicklayer[i] > 0) {
      AH_OUT << "     " << surf.m_materialformula[i] << std::endl;
    }
  }
  AH_OUT << "*************************************" << std::endl;
  
  
  // -------------------------------------
  //      mass absorption extension
  // -------------------------------------

  // Calculate the interpolated scaled f1, scaled f2, and mass-absorption 
  // coefficients, for every energy in energy grid and for every unique 
  // material.
  // for every energy
  for (int iEnergy = 0 ; iEnergy < energy.m_numenergy ; iEnergy++) {
    currEnergy = energy.m_photonenergy[iEnergy];
    currEnergycm = energy.m_photonlamcm[iEnergy];
    currEnergycmsq = energy.m_photonlamcmsq[iEnergy];
    calcsf1sf2materials(currEnergy, currEnergycm, currEnergycmsq, scat, atom, param, edge, surf, sf1material, sf2material, mabsmaterial);
    materialf1factor.push_back(std::vector<double>());
    materialf2factor.push_back(std::vector<double>());
    massabscoeff.push_back(std::vector<double>());
    // for each material
    for (int j = 0 ; j < surf.m_nummaterials ; j++) {
      materialf1factor[iEnergy].push_back(sf1material[j]);
      materialf2factor[iEnergy].push_back(sf2material[j]);
      massabscoeff[iEnergy].push_back(mabsmaterial[j]);
    }
  }
    
  // Write the mass-absorption coefficients for materials that appear in a 
  // thick layer.  Note: The mass-absorption coefficient, mu, is related to 
  // the optical constant (and hence to Beta(lambda)), by 
  // mu=4pi*Beta/(rho*lambda), where rho is the material density. 
  // But materialf2factor() = Beta/rho with rho in cgs, and wavelength in cm
  
  // move to the MASS_ABSORPTION extension
  fits_movnam_hdu(param.m_out_fp, ANY_HDU, const_cast<char *>(s_massAbsExtName), 0, &status);
  checkForFITSError(status, "moving to mass absorption extension in", param.m_outfile);
  
  // loop over each unique material
  for (int iMat = 0 ; iMat < surf.m_nummaterials ; iMat++) {
      
      // write the mass absorption coefficient column for current material
      // +1 because Energy is the first column
      currColNum = iMat + colOffset;
      for (int iEnergy = 0 ; iEnergy < energy.m_numenergy ; iEnergy++) {
        fits_write_col(param.m_out_fp, TDOUBLE, currColNum, iEnergy+1, 1, 1, &(massabscoeff[iEnergy][iMat]), &status);
      }
      checkForFITSError(status, "writing data to", param.m_outfile);
      
  }
  
  // write the DATE keyword for when this HDU was created
  fits_write_date(param.m_out_fp, &status);
  checkForFITSError(status, "writing DATE to", param.m_outfile);
  
  // write the DATASUM and CHECKSUM keyword values into the current header
  fits_write_chksum(param.m_out_fp, &status);
  checkForFITSError(status, "writing CHECKSUM to", param.m_outfile);
  
  // -------------------------------------
  //      reflectivity extension
  // -------------------------------------
  
  // move to the reflectivity extension
  fits_movnam_hdu(param.m_out_fp, ANY_HDU, const_cast<char *>(param.m_outext.c_str()), 0, &status);
  checkForFITSError(status, "moving to reflectivity extension in", param.m_outfile);
  
  // loop over mirror surface foil groups
  for (int iGroup = 1 ; iGroup <= surf.m_numgroups ; iGroup++) {
    
    // clear these vectors for each group
    rprob.clear();
    tprob.clear();
    
    // loop over photon energy
    for (int iEnergy = 0 ; iEnergy < energy.m_numenergy ; iEnergy++) {
      
      // clear these temp vectors so we can reuse them
      sf1material.clear();
      sf2material.clear();
      sreflect.clear();
      strans.clear();
      
      // get the scaled f1 and f2 for this energy
      for (int k = 0 ; k < surf.m_nummaterials ; k++) {
        sf1material.push_back(materialf1factor[iEnergy][k]);
        sf2material.push_back(materialf2factor[iEnergy][k]);
      }
      
      // calculate reflection (sreflect) and transmission (strans) probabilities
      multilayerRefl(energy.m_photonlam[iEnergy], energy.m_lambdasqfac[iEnergy], iGroup, surf, angle, param, sf1material, sf2material, sreflect, strans);
      
      // copy sreflect and strans arrays as new row in rProb and tProb
      rprob.push_back(sreflect);
      tprob.push_back(strans);
      
      // for creating a file with all reflectivities = 1
//      std::vector<double> sreflect1(sreflect.size(), 1.0);
//      std::vector<double> strans0(strans.size(), 0.0);
//      rprob.push_back(sreflect1);
//      tprob.push_back(strans0);
      
    } // end for-loop over energies
    
    // get the current columns to which to write the data, for this group
    currColNumStrStr.str("");
    currColNumStrStr.clear();
    currColNumStrStr << std::setw(2) << std::setfill('0') << iGroup;
    currColNumStr = currColNumStrStr.str();
    currColNameRefl = "RefProb";
    currColNameRefl.append(currColNumStr);
    fits_get_colnum(param.m_out_fp, CASEINSEN, const_cast<char *>(currColNameRefl.c_str()), &currColNumRefl, &status);
    checkForFITSError(status, "getting column numbers in reflectivity extension in", param.m_outfile);
    currColNameTran = "TranProb";
    currColNameTran.append(currColNumStr);
    fits_get_colnum(param.m_out_fp, CASEINSEN, const_cast<char *>(currColNameTran.c_str()), &currColNumTran, &status);
    checkForFITSError(status, "getting column numbers in reflectivity extension in", param.m_outfile);
    
    for (int iEnergy = 0 ; iEnergy < energy.m_numenergy ; iEnergy++) {
      fits_write_col(param.m_out_fp, TDOUBLE, currColNumRefl, iEnergy+1, 1, (LONGLONG)(angle.m_numangles), &(rprob[iEnergy][0]), &status);
      fits_write_col(param.m_out_fp, TDOUBLE, currColNumTran, iEnergy+1, 1, (LONGLONG)(angle.m_numangles), &(tprob[iEnergy][0]), &status);
    }
    checkForFITSError(status, "writing data to", param.m_outfile);
    
  } // end for-loop over groups
  
  // write the DATE keyword for when this HDU was created
  fits_write_date(param.m_out_fp, &status);
  checkForFITSError(status, "writing DATE to", param.m_outfile);
  
  // write the DATASUM and CHECKSUM keyword values into the current header
  fits_write_chksum(param.m_out_fp, &status);
  checkForFITSError(status, "writing CHECKSUM to", param.m_outfile);
  
}   // end doWork()


// ----------------------------------------------------------------------------


void finalize(Param & param) {
  
  int status = 0;                         // for cfitsio function calls

  // delete or close output file
  if(param.m_out_fp != NULL) {
    
    // if we're not in debug mode and an error occurred, delete the file
    if (param.m_errorOccurred && !ahlog::get_debug()) {
      // +++ 20140228 KLR but we don't want to delete if error is from "file already exists"
      fits_delete_file(param.m_out_fp, &status);
    } else {
      // if in debug mode, or if no error, just close the file
      fits_close_file(param.m_out_fp, &status);
    }
    
  }
  checkForFITSError(status, "closing", param.m_outfile);
  
}   // end finalize()


// ----------------------------------------------------------------------------



/**********************************************
 * ********************************************
 * 		initialize() helper functions
 * ********************************************
**********************************************/

// +++ 20140304 KLR the column names for all input files are currently in an 
//                  array, corresponding to an array of column numbers. This  
//                  could be changed to a map, perhaps.  When that happens,
//                  change initialize_edge() and _scat and _surface to get 
//                  Energy col_num from that, instead of hardcoding that it's 
//                  the 4th column num in array, etc.

// +++ 20140304 KLR Make sure I have correct recommended values for TUNIT.
//                  check that density is correct in files, then check in code
//                  for density in atomicData, surface, etc

void initialize_atomic(const Param & param, AtomicData & atom) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  fitsfile * atomdb_fp;                   // atomic data file
  int status = 0;                         // for cfitsio function calls
  int anynul = 0;                         // for the call to fits_read_col
  long num_rows_atom_data = 0;
  const int num_atom_cols = 5;
  const std::string atomic_cols[num_atom_cols] = {"Z", "symbol", "name", "weight", "density"};
  int atomic_col_nums[num_atom_cols] = {0};
  
  // for storing the TSTRING types from the input atomic data fits file 
  char * input_elsymbol;      // 2 character string
  input_elsymbol = (char *) malloc(2*sizeof(char));
  char * input_elname;        // 16 characters are allowed
  input_elname = (char *) malloc(16*sizeof(char));
  
  // for making sure that the input file is in order 
  // (Z is used as index in vectors)
  int currZ = 0;
  
  // -------------------------------------
  
  // open the file, go to the specified extension or the first table extension
  fits_open_table(&atomdb_fp, (param.m_atomicfile).c_str(), READONLY, &status);
  checkForFITSError(status, "opening to specified extension in", param.m_atomicfile);
  
  AH_INFO(ahlog::HIGH) << "Processing file: " << param.m_atomicfile << std::endl;
  
  // store how many rows in this extension
  fits_get_num_rows(atomdb_fp, &num_rows_atom_data, &status);
  checkForFITSError(status, "getting number of rows in", param.m_atomicfile);
  atom.m_nelements = num_rows_atom_data;
  
  // find column numbers for atomic data file
  for (int i = 0 ; i < num_atom_cols ; i++) {
    fits_get_colnum(atomdb_fp, CASEINSEN, const_cast<char *>((atomic_cols[i]).c_str()), &atomic_col_nums[i], &status);
  }
  checkForFITSError(status, "getting column numbers in", param.m_atomicfile);
  
  // resize vectors to m_nelements+1, so that vector[1] is for row 1 Z=1
  // vector[0] will be a garbage row
  atom.m_Z.resize(s_maxzofelements+1);
  atom.m_elsymbol.resize(s_maxzofelements+1);
  atom.m_elname.resize(s_maxzofelements+1);
  atom.m_elweight.resize(s_maxzofelements+1);
  atom.m_eldensity.resize(s_maxzofelements+1);
  
  // get column data from atomic data file, store it in arrays
  // loop through each row, starting at row index = 1 (so index matches Z)
  for (long long i = 1 ; i <= num_rows_atom_data ; i++) {
    // get the data from the fits file
    fits_read_col(atomdb_fp, TINT, atomic_col_nums[0], i, 1, 1, 0, &currZ, &anynul, &status);
    fits_read_col(atomdb_fp, TINT, atomic_col_nums[0], i, 1, 1, 0, &(atom.m_Z[currZ]), &anynul, &status);
    fits_read_col(atomdb_fp, TSTRING, atomic_col_nums[1], i, 1, 1, 0, &input_elsymbol, &anynul, &status);
    fits_read_col(atomdb_fp, TSTRING, atomic_col_nums[2], i, 1, 1, 0, &input_elname, &anynul, &status);
    fits_read_col_dbl(atomdb_fp, atomic_col_nums[3], i, 1, 1, 0, &(atom.m_elweight[currZ]), &anynul, &status);
    fits_read_col_dbl(atomdb_fp, atomic_col_nums[4], i, 1, 1, 0, &(atom.m_eldensity[currZ]), &anynul, &status);
    
    // convert the char * from cfitsio into C++ strings, store in the Param
    atom.m_elsymbol[currZ] = (std::string)input_elsymbol;
    atom.m_elname[currZ] = (std::string)input_elname;
    
  }
  
  // free the char * we used to get the TSTRING data from cfitsio
  free(input_elsymbol); input_elsymbol=0;
  free(input_elname); input_elname=0;
  
  // check fits error after we free the char *
  checkForFITSError(status, "getting data from", param.m_atomicfile);
  
  // close atomic data file
  fits_close_file(atomdb_fp, &status);
  checkForFITSError(status, "closing", param.m_atomicfile);
  
  /* testing that I stored atomic data */
  #ifdef DEBUG
  std::cout << std::endl;
  std::cout << "**** test that I've stored the atomic data ****" << std::endl;
  for (int i = 1 ; i <= num_rows_atom_data ; i++) { 
    std::cout << "atom.m_Z[" << i << "] = " << atom.m_Z[i] << " " << atom.m_elsymbol[i] << " " << atom.m_elname[i];
    std::cout << " wt:  " << atom.m_elweight[i] << " dens: " << atom.m_eldensity[i] << std::endl;
  }
  #endif
  
} // initialize_atomic()



void initialize_scattering(Param & param, Scattering & scat) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  fitsfile * scattering_fp;               // scattering data file
  int status = 0;                         // for calls to cfitsio
  int anynul = 0;                         // for the call to fits_read_col
  long num_rows_scat_data = 0;
  const int num_scat_cols = 5;
  const std::string scat_cols[num_scat_cols] = {"Z", "rowindex", "energy", "f1real", "f2img"};
  int scat_col_nums[num_scat_cols] = {0};

  // these will be used when searching for the indices that search for 
  // valid f1 and f2 values
  int lowindex = 0;
  int highindex = 0;
  int templowindex = 0;        // to keep track of the lowindex within the loop
  int temphighindex = 0;       // to keep track of the highindex within the loop
  int dummy = -9999;           // the dummy f1 and f2 value used in fits file
 
  // for checking that data is in correct units
  std::string tunit = "TUNIT";
  char keywordChar[FLEN_CARD];
  std::string keywordStr;
  
  // for making sure that the input file is sorted
  int currZ = 0;
  int currRowIndex = 0; 
  
  // for error checking later
  std::string errorMsg;
  bool needToAbort = false;
 
  // -------------------------------------
  
  // open the file, go to the specified extension or the first table extension
  fits_open_table(&scattering_fp, (param.m_atmscafile).c_str(), READONLY, &status);
  checkForFITSError(status, "opening to the specified extension in", param.m_atmscafile);
  
  AH_INFO(ahlog::HIGH) << "Processing file: " << param.m_atmscafile << std::endl;
  
  // store extension in param struct, so we can print to output file
  fits_read_key_str(scattering_fp, "EXTNAME", keywordChar, NULL, &status);
  checkForFITSError(status, "reading extension name in", param.m_atmscafile);
  keywordStr = std::string(keywordChar);
  param.m_atmsctng = keywordStr;
  
  // store how many rows in this extension
  fits_get_num_rows(scattering_fp, &num_rows_scat_data, &status);
  checkForFITSError(status, "getting number of rows in", param.m_atmscafile);
  
  // resize data arrays, now that we know how many rows are in the file
  scat.m_Z.resize(num_rows_scat_data);
  scat.m_rowindex.resize(num_rows_scat_data);
  scat.m_energy.resize(num_rows_scat_data);
  scat.m_f1real.resize(num_rows_scat_data);
  scat.m_f2imag.resize(num_rows_scat_data);
  
  // find column numbers for the scattering data file
  for (int i = 0 ; i < num_scat_cols ; i++) {
    fits_get_colnum(scattering_fp, CASEINSEN, const_cast<char *>((scat_cols[i]).c_str()), &scat_col_nums[i], &status);
  }
  checkForFITSError(status, "getting column numbers in", param.m_atmscafile);
  
  // make sure energies are in eV - code calculations depend on this
  tunit.append(intToString(scat_col_nums[2]));
  fits_read_key(scattering_fp, TSTRING, const_cast<char *>(tunit.c_str()), keywordChar, NULL, &status);
  checkForFITSError(status, "reading keyword in", param.m_atmscafile);
  keywordStr = std::string(keywordChar);
  if (!isEqualCaseInsens(keywordStr, "eV")) {
    errorMsg = "Energy needs to be in eV, and ";
    errorMsg.append(tunit);
    errorMsg.append(" needs to say 'eV' in ");
    errorMsg.append(param.m_atmscafile);
    AH_THROW_RUNTIME(errorMsg);
  }
  
  // get column data from the atomic scattering file, store it in arrays
  // starting at row index = 0 (Zs don't need to match)
  for (long long i = 0 ; i < num_rows_scat_data ; i++) {
    
    fits_read_col(scattering_fp, TINT, scat_col_nums[0], i+1, 1, 1, 0, &(scat.m_Z[i]), &anynul, &status);
    fits_read_col(scattering_fp, TINT, scat_col_nums[1], i+1, 1, 1, 0, &(scat.m_rowindex[i]), &anynul, &status);
    fits_read_col_dbl(scattering_fp, scat_col_nums[2], i+1, 1, 1, 0, &(scat.m_energy[i]), &anynul, &status);
    fits_read_col_dbl(scattering_fp, scat_col_nums[3], i+1, 1, 1, 0, &(scat.m_f1real[i]), &anynul, &status);
    fits_read_col_dbl(scattering_fp, scat_col_nums[4], i+1, 1, 1, 0, &(scat.m_f2imag[i]), &anynul, &status);  
    
    // make sure the input file is sorted, by "Z" then "RowIndex"
    if (scat.m_Z[i] > currZ) {
      // if this is the next group, increment currZ and reset currRowIndex
      currZ = scat.m_Z[i];
      currRowIndex = 0;
    } else if (scat.m_Z[i] == currZ) {
      // if we're still at the same Z, check on currRowIndex
      if (scat.m_rowindex[i] >= currRowIndex) {
        currRowIndex = scat.m_rowindex[i];
      } else {
        errorMsg = "Input scattering file (";
        errorMsg.append(param.m_atmscafile);
        errorMsg.append(") must be sorted by Z then RowIndex.");
        // set flag to abort (which is done after we close the fits file)
        needToAbort = true;
        // and we can go ahead and get out of the loop 
        break;
      }
    } else {
      // if they're out of order, abort
      errorMsg = "Input scattering file (";
      errorMsg.append(param.m_atmscafile);
      errorMsg.append(") must be sorted by Z then RowIndex.");
      // set flag to abort (which is done after we close the fits file)
      needToAbort = true;
      // and we can go ahead and get out of the loop 
      break;
    }
    
  }
  checkForFITSError(status, "getting data from", param.m_atmscafile);
  
  // close atomic scattering file
  fits_close_file(scattering_fp, &status);
  checkForFITSError(status, "closing", param.m_atmscafile);
  
  // now that we've closed the fits file, see if we need to abort from an error
  if (needToAbort) {
    AH_THROW_RUNTIME(errorMsg);
  }
  
  /* test that I've stored the scatt data */
  #ifdef DEBUG
  std::cout << std::endl;
  std::cout << "**** test that I've stored the scattering data ****" << std::endl;
  for (int i = 60 ; i < 80 ; i++) {
    std::cout << "scattering i=" << i << ": " << scat.m_Z[i] << " " << scat.m_rowindex[i];
    std::cout << " " << scat.m_energy[i] << " " << scat.m_f1real[i] << " " << scat.m_f2imag[i] << std::endl;
  }
  #endif
  
  // scat.m_zscatindex stores indices by Z, so [0][0] and [0][1] are ignored
  scat.m_zscatindex.resize(s_maxzofelements+1);
  
  // fill in the start indices to the lookup arrays
  int j = 0;      // j will count the new elements we come across
  // loop through each row of the data (starting at 0, to loop through param
  // arrays)
  for (int i = 0 ; i < num_rows_scat_data ; i++) {
    // if we're starting at a new element:
    if (scat.m_rowindex[i] == 1) {
      // add this Z (from the param) to the scat.m_znumbers array
      scat.m_znumbers.push_back(scat.m_Z[i]);
      // record this row as the start rowindex for the new element
      scat.m_elemscatindex.push_back(std::vector<long>());
      scat.m_elemscatindex[j].push_back(i);
      // look up the Z at this row, and use that to store in scat.m_zscatindex
      scat.m_zscatindex[scat.m_Z[i]].push_back(i);
      // increment to the next Z counter for next iteration of rowindex
      j++;
    }
  }
  // we now know how many unique elements were in the file
  scat.m_nscatelements = j;
  
  // fill in end indices for each element
  if (scat.m_nscatelements > 1) {
    for (int i = 0 ; i <= scat.m_nscatelements-2 ; i++) {
      // just get the index prior the start of the next element
      scat.m_elemscatindex[i].push_back(scat.m_elemscatindex[i+1][0] - 1);
      scat.m_zscatindex[ scat.m_znumbers[i] ].push_back( scat.m_zscatindex[scat.m_znumbers[i+1]][0] - 1 );
      // this assigns scat.m_zscatindex[0][1], but that entry is ignored anyway
    }
  }
  // fill in end indices for last elements
  scat.m_elemscatindex[scat.m_nscatelements-1].push_back(num_rows_scat_data - 1);
  scat.m_zscatindex[scat.m_znumbers[scat.m_nscatelements-1]].push_back(num_rows_scat_data - 1);
  
  /* test that I've stored the scattering indices correctly */
  #ifdef DEBUG
  std::cout << std::endl;
  std::cout << "**** test that I've stored the scattering indices correctly, scat.m_elemscatindex ****" << std::endl;
  for (int i = 88 ; i < scat.m_nscatelements ; i++) {
    std::cout << "i=" << i << " ";
    std::cout << scat.m_elemscatindex[i][0] << "  " << scat.m_elemscatindex[i][1];
    std::cout << "  senergy= " << scat.m_energy[scat.m_elemscatindex[i][0]] << " to " << scat.m_energy[scat.m_elemscatindex[i][1]];
    std::cout << std::endl;
  }
  std::cout << "**** test that I've stored the scattering indices correctly, scat.m_zscatindex ****" << std::endl;
  for (int i = 88 ; i <= scat.m_nscatelements ; i++) {
    std::cout << "i=" << i << " ";
    std::cout << scat.m_zscatindex[i][0] << "  " << scat.m_zscatindex[i][1];
    std::cout << "  senergy= " << scat.m_energy[scat.m_zscatindex[i][0]] << " to " << scat.m_energy[scat.m_zscatindex[i][1]];
    std::cout << std::endl;
  }
  #endif
  
  // Per TRF: starting with the full energy range lower and upper indices 
  // (in scat.m_zscatindex(*,2)), move up or down respectively until we reach 
  // first valid data point. Note that this simple implementation implicitly 
  // assumes that there is a single, contiguous energy range of valid data for 
  // f1 and f2 for a given element. If this isn't true, algorithm will fail
  // +++ think about this - is this the best method?
  
  scat.m_f1effindex = scat.m_zscatindex;
  scat.m_f2effindex = scat.m_zscatindex;
  
  // for each of the elements, 
  for (int iElem = 0 ; iElem < scat.m_nscatelements ; iElem++) {
    
    // initially set the low and high indices to be the start and end of 
    // all the energies
    lowindex = scat.m_zscatindex[scat.m_znumbers[iElem]][0];
    highindex = scat.m_zscatindex[scat.m_znumbers[iElem]][1];
    
    // then look though for a valid f1 low index
    templowindex = lowindex;
    while (scat.m_f1real[templowindex] <= dummy) {
      // while the values are dummy, increment the low end of the index
      scat.m_f1effindex[scat.m_znumbers[iElem]][0] = scat.m_f1effindex[scat.m_znumbers[iElem]][0] + 1;
      templowindex++;
      // +++ per TRF, may need checks here to ensure it never reaches high index
    }
    
    // then look though for a valid f2 low index
    templowindex = lowindex;
    while (scat.m_f2imag[templowindex] <= dummy) {
      // while the values are dummy, increment the low end of the index
      scat.m_f2effindex[scat.m_znumbers[iElem]][0] = scat.m_f2effindex[scat.m_znumbers[iElem]][0] + 1;
      templowindex++;
      // +++ per TRF, may need checks here to ensure it never reaches high index
    }
    
    // then look though for a valid f1 high index
    temphighindex = highindex;
    while (scat.m_f1real[temphighindex] <= dummy) {
      // while the values are dummy, decrement the high end of the index
      scat.m_f1effindex[scat.m_znumbers[iElem]][1] = scat.m_f1effindex[scat.m_znumbers[iElem]][1] - 1;
      temphighindex--;
      // +++ per TRF, may need checks here to ensure it never reaches low index
    }
    
    // then look though for a valid f2 high index
    temphighindex = highindex;
    while (scat.m_f2imag[temphighindex] <= dummy) {
      // while the values are dummy, decrement the high end of the index
      scat.m_f2effindex[scat.m_znumbers[iElem]][1] = scat.m_f2effindex[scat.m_znumbers[iElem]][1] - 1;
      temphighindex--;
      // +++ per TRF, may need checks here to ensure it never reaches low index
    }
    
  }

  /* test that I've stored the f1 f2 eff indices correctly */
  #ifdef DEBUG
  std::cout << std::endl;
  std::cout << "**** test that I've stored the f1 f2 eff indices correctly ****" << std::endl;
  for (int i = 1 ; i <= scat.m_nscatelements ; i++) {
    std::cout << "i=z=" << i << " ";
    std::cout << "f1: " << scat.m_f1effindex[i][0] << " (" <<  scat.m_f1real[scat.m_f1effindex[i][0]] << ") ";
    std::cout           << scat.m_f1effindex[i][1] << " (" <<  scat.m_f1real[scat.m_f1effindex[i][1]] << ") ";
    std::cout << "f2: " << scat.m_f2effindex[i][0] << " (" <<  scat.m_f2imag[scat.m_f2effindex[i][0]] << ") ";
    std::cout           << scat.m_f2effindex[i][1] << " (" <<  scat.m_f2imag[scat.m_f2effindex[i][1]] << ") ";
    std::cout << std::endl;
  }
  #endif
  
} // end initialize_scattering()



void initialize_surface(const Param & param, AtomicData & atom, Surface & surf) {
    
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  fitsfile * surface_fp;                  // surface data file
  int status = 0;                         // for calls to cfitsio
  int anynul = 0;                         // for the call to fits_read_col
  
  // for getting string keywords
  char * telescop_c;
  telescop_c    = (char *) malloc(80*sizeof(char));
  char * instrume_c;
  instrume_c    = (char *) malloc(80*sizeof(char));
  char * detnam_c;
  detnam_c      = (char *) malloc(80*sizeof(char));
  
  long nsurfacerows = 0;
  const int num_surf_cols = 8;
  const std::string surf_cols[num_surf_cols] = {"group", "firstshell", 
      "lastshell", "layer", "material", "density", "thickness", "roughness"};
  int surf_col_nums[num_surf_cols] = {0};
  
  // for storing the TSTRING type from the input SURFACE mirror def. fits file 
  char * input_coatformula;
  input_coatformula = (char *) malloc(30*sizeof(char));
  
  // for checking that data is in correct units
  std::string tunit = "TUNIT";
  char keywordChar[FLEN_CARD];
  std::string keywordStr;
  
  // for making sure that the input file is sorted
  int currGroup = 0;
  int currLayer = 0; 
  
  // for error checking later
  std::string errorMsg;
  bool needToAbort = false;
  
  // -------------------------------------
  
  // open the file
  fits_open_file(&surface_fp, (param.m_mirrorfile).c_str(), READONLY, &status);
  checkForFITSError(status, "opening to the specified extension in", param.m_mirrorfile);
  
  AH_INFO(ahlog::HIGH) << "Processing file: " << param.m_mirrorfile << std::endl;
  
  // error checking that TELESCOP and INSTRUME (user input params) match this 
  // file, in case the user supplied the file instead of CALDB
  fits_read_key_str(surface_fp, "TELESCOP", telescop_c, NULL, &status);
  fits_read_key_str(surface_fp, "INSTRUME", instrume_c, NULL, &status);
  if (!isEqualCaseInsens(param.m_telescop, (std::string)telescop_c)) {
    AH_THROW_RUNTIME("TELESCOP keyword in mirror file (" + 
                     (std::string)telescop_c + ", in file " + param.m_mirrorfile + 
                     ") and input parameter 'telescop' (" + param.m_telescop + ") must match");
  }
  if (!isEqualCaseInsens(param.m_instrume, (std::string)instrume_c)) {
    std::string errMsg = "INSTRUME keyword in mirror file (" + 
                     (std::string)instrume_c + ", in file " + param.m_mirrorfile + 
                     ") and input parameter 'instrume' (" + param.m_instrume + ") do not match";
    AH_INFO(ahlog::HIGH) << errMsg << std::endl;
    // +++ 20150909 KLR warning, not error, so that tests with old files will work.
//    AH_THROW_RUNTIME("INSTRUME keyword in mirror file (" + 
//                     (std::string)instrume_c + ", in file " + param.m_mirrorfile + 
//                     ") and input parameter 'instrume' (" + param.m_instrume + ") must match");
  }
  
  // store the DETNAM keyword, to write it to the output file (if it exists)
  fits_read_key_str(surface_fp, "DETNAM", detnam_c, NULL, &status);
  if (status == KEY_NO_EXIST) {
    // TDF doesn't have DETNAM keyword, so don't write one to output
    surf.m_detnam = "VALUE_UNDEFINED";
    status=0;
  } else {
    checkForFITSError(status, "reading DETNAM keyword in", param.m_mirrorfile);
    surf.m_detnam = (std::string)detnam_c;
  }
  
  // free the char * we used to get the string keywords from cfitsio
  free(telescop_c);     telescop_c=0;
  free(instrume_c);     instrume_c=0;
  free(detnam_c);       detnam_c=0;
  
  // store how many rows in this extension
  fits_get_num_rows(surface_fp, &nsurfacerows, &status);
  checkForFITSError(status, "getting number of rows in", param.m_mirrorfile);
  
  // resize data arrays, now that we know how many rows are in the file
  surf.m_groupnumber.resize(nsurfacerows);
  surf.m_firstshell.resize(nsurfacerows);
  surf.m_lastshell.resize(nsurfacerows);
  surf.m_layernumber.resize(nsurfacerows);
  surf.m_coatformula.resize(nsurfacerows);
  surf.m_coatdensity.resize(nsurfacerows);
  surf.m_coatthickness.resize(nsurfacerows);
  surf.m_coatroughness.resize(nsurfacerows);
  
  // find column numbers for surface data file
  for (int i = 0 ; i < num_surf_cols ; i++) {
    // +++ use a map, like I do in xrtraytrace
    fits_get_colnum(surface_fp, CASEINSEN, const_cast<char *>((surf_cols[i]).c_str()), &surf_col_nums[i], &status);
  }
  checkForFITSError(status, "getting column numbers in", param.m_mirrorfile);

  // make sure data is in correct units - code calculations depend on this
  // check that thickness is in Angstrom
    // +++ use a map, like I do in xrtraytrace
  tunit.append(intToString(surf_col_nums[6]));
  fits_read_key(surface_fp, TSTRING, const_cast<char *>(tunit.c_str()), keywordChar, NULL, &status);
  checkForFITSError(status, "reading keyword in", param.m_mirrorfile);
  keywordStr = std::string(keywordChar);
  if (!isEqualCaseInsens(keywordStr, "Angstrom")) {
    errorMsg = "Thickness needs to be in Angstrom, and ";
    errorMsg.append(tunit);
    errorMsg.append(" needs to say 'Angstrom' in ");
    errorMsg.append(param.m_mirrorfile);
    AH_THROW_RUNTIME(errorMsg);
  }
  // check that roughness is in Angstrom
  tunit = "TUNIT";
  tunit.append(intToString(surf_col_nums[7]));
  fits_read_key(surface_fp, TSTRING, const_cast<char *>(tunit.c_str()), keywordChar, NULL, &status);
  checkForFITSError(status, "reading keyword in", param.m_mirrorfile);
  keywordStr = std::string(keywordChar);
  if (!isEqualCaseInsens(keywordStr, "Angstrom")) {
    errorMsg = "Roughness need to be in Angstrom, and ";
    errorMsg.append(tunit);
    errorMsg.append(" needs to say 'Angstrom' in ");
    errorMsg.append(param.m_mirrorfile);
    AH_THROW_RUNTIME(errorMsg);
  }
  
  // get column data from surface data file, store it in arrays
  for (long long i = 0 ; i < nsurfacerows ; i++) {
    
    // get the data from the fits file
    fits_read_col(surface_fp, TINT, surf_col_nums[0], i+1, 1, 1, 0, &(surf.m_groupnumber[i]), &anynul, &status);
    fits_read_col(surface_fp, TINT, surf_col_nums[1], i+1, 1, 1, 0, &(surf.m_firstshell[i]), &anynul, &status);
    fits_read_col(surface_fp, TINT, surf_col_nums[2], i+1, 1, 1, 0, &(surf.m_lastshell[i]), &anynul, &status);
    fits_read_col(surface_fp, TINT, surf_col_nums[3], i+1, 1, 1, 0, &(surf.m_layernumber[i]), &anynul, &status);
    fits_read_col(surface_fp, TSTRING, surf_col_nums[4], i+1, 1, 1, 0, &input_coatformula, &anynul, &status);
    fits_read_col_dbl(surface_fp, surf_col_nums[5], i+1, 1, 1, 0, &(surf.m_coatdensity[i]), &anynul, &status);
    fits_read_col_dbl(surface_fp, surf_col_nums[6], i+1, 1, 1, 0, &(surf.m_coatthickness[i]), &anynul, &status);
    fits_read_col_dbl(surface_fp, surf_col_nums[7], i+1, 1, 1, 0, &(surf.m_coatroughness[i]), &anynul, &status);
    
    // convert the char * from cfitsio into C++ strings, store in the Param
    surf.m_coatformula[i] = (std::string)input_coatformula;
    
    // make sure the input file is sorted, by "Group" then "Layer"
    if (surf.m_groupnumber[i] > currGroup) {
      // if this is the next group, increment currGroup and reset currLayer
      currGroup = surf.m_groupnumber[i];
      currLayer = 0;
    } else if (surf.m_groupnumber[i] == currGroup) {
      // if we're still at the same group, check on currLayer
      if (abs(surf.m_layernumber[i]) >= currLayer) {
        currLayer = abs(surf.m_layernumber[i]);
      } else {
        errorMsg = "Input surface file (";
        errorMsg.append(param.m_mirrorfile);
        errorMsg.append(") must be sorted by Group then Layer.");
        // set flag to abort (which is done after we close the fits file)
        needToAbort = true;
        // and we can go ahead and get out of the loop 
        break;
      }
    } else {
      // if they're out of order, abort
      errorMsg = "Input surface file (";
      errorMsg.append(param.m_mirrorfile);
      errorMsg.append(") must be sorted by Group then Layer.");
      // set flag to abort (which is done after we close the fits file)
      needToAbort = true;
      // and we can go ahead and get out of the loop 
      break;
    }
    
  }
  
  // free the char * we used to get the TSTRING data from cfitsio
  free(input_coatformula); input_coatformula=0;
  
  // check fits error after we free the char *
  checkForFITSError(status, "getting data from", param.m_mirrorfile);
  
  // get the keyword NGROUPS from the fits file
  fits_read_key(surface_fp, TINT, const_cast<char *>("NGROUPS"), &(surf.m_numgroups), NULL, &status);
  checkForFITSError(status, "reading keyword in", param.m_mirrorfile);
  
  // make sure that the NGROUPS keyword is correct (file must be sorted)
  if (surf.m_numgroups != surf.m_groupnumber[nsurfacerows-1]) {
    AH_WARN(ahlog::LOW) << "NGROUPS keyword in surface file is "
                        << surf.m_numgroups 
                        << ", which does not match the number groups found in " 
                        << "the file, " << surf.m_groupnumber[nsurfacerows-1]
                        << ".  Using the number of groups from the file, "
                        << surf.m_groupnumber[nsurfacerows-1] << std::endl;
    surf.m_numgroups = surf.m_groupnumber[nsurfacerows-1];
  }
  
  // close surface data file
  fits_close_file(surface_fp, &status);
  checkForFITSError(status, "closing", param.m_mirrorfile);
  
  // after we close fits file and free the char *, see if need to abort
  if (needToAbort) {
    AH_THROW_RUNTIME(errorMsg);
  }
  
  // the output HDU keyword names can't be over 8 characters long.  (ie: can't
  // be RefProb102).  Make sure we don't get to over 99
  if (surf.m_numgroups > 99) {
    AH_THROW_RUNTIME("Cannot have more than 99 groups for reflectivity.");
  }

  
  /* testing that I got data from SURFACE extension */
  #ifdef DEBUG
  std::cout << std::endl;
  std::cout << "*** testing that I got data from SURFACE extension ***" << std::endl;
  for (int i = 0 ; i < nsurfacerows ; i++) { 
    std::cout << "i=" << i << ": " << surf.m_groupnumber[i] << " " << surf.m_firstshell[i] << " " << surf.m_lastshell[i];
    std::cout << " " << surf.m_layernumber[i] << " ";
    std::cout << surf.m_coatformula[i] << " ";
    std::cout << surf.m_coatdensity[i] << " ";
    std::cout << surf.m_coatthickness[i] << " " << surf.m_coatroughness[i] << std::endl;
  }
  #endif
  
  // arrange coating arrays into 2dim vectors, so can easily look up formula,
  // density, etc for a given group/layer
  // go through all the rows, store group/layer data
  int grp = -1;     // group counter (wait til later to start at 0)
  for (int iRow = 0 ; iRow < nsurfacerows ; iRow++) {
       
    // if starting at a new group (for a multilayer surface) OR
    // if this is a single layer surface, first row will have a neg layernumber
    if (std::abs(surf.m_layernumber[iRow]) == 1) {
      
      grp++;    // go to next group counter (first time through, this is only 0)
      
      // add a new row to the vectors for this group
      surf.m_layerformula.push_back(std::vector<std::string>());
      surf.m_layerdensity.push_back(std::vector<double>());
      surf.m_layerthickness.push_back(std::vector<double>());
      surf.m_layerroughness.push_back(std::vector<double>());
      surf.m_sigmaroughnesssq.push_back(std::vector<double>());
      
      // add dummy data for the vacuum (layer N=0)
      surf.m_layerformula[grp].push_back("");
      surf.m_layerdensity[grp].push_back(0.);
      surf.m_layerthickness[grp].push_back(0.);
      surf.m_layerroughness[grp].push_back(0.);
      surf.m_sigmaroughnesssq[grp].push_back(0.);
      
      // if we're at the start of the next group, can store the previous layer
      // number as the max layer number of the previous group 
      // the last group is done in statement right after this for loop
      if (grp > 0) {
        surf.m_numlayertotal.push_back(std::abs(surf.m_layernumber[iRow-1]));
      }
      
    } // end-if we're at a new group
    
    // if the first layer of group is neg, then we know it's a single layer
    // (it could be mult groups of single layers)
    if (surf.m_layernumber[iRow] == -1) {
      surf.m_numlayerplusone.push_back(1);
    }
    
    // for multilayer surface, record first thick (substrate) layer
    if (iRow > 0) {
      if ( (surf.m_layernumber[iRow] < 0) && (surf.m_layernumber[iRow-1] > 0) ) {
        surf.m_numlayerplusone.push_back(std::abs(surf.m_layernumber[iRow]));
      }
    }
    
    // fill in the data for each layer of this group (the vacuum layer
    // has already by populated with dummy data)
    surf.m_layerformula[grp].push_back(surf.m_coatformula[iRow]);
    surf.m_layerdensity[grp].push_back(surf.m_coatdensity[iRow]);
    surf.m_layerthickness[grp].push_back(surf.m_coatthickness[iRow]);
    surf.m_layerroughness[grp].push_back(surf.m_coatroughness[iRow]);
    surf.m_sigmaroughnesssq[grp].push_back(surf.m_coatroughness[iRow] * surf.m_coatroughness[iRow]);
    
  } // end for-loop through all rows of data
  
  // easily get num layers for last group (as long as fits file is sorted)
  surf.m_numlayertotal.push_back(std::abs(surf.m_layernumber[nsurfacerows-1]));
  
  /* testing that I stored data in vectors correctly from SURFACE extension */
  #ifdef DEBUG
  std::cout << std::endl;
  std::cout << "*** testing that I stored data in vectors correctly from SURFACE extension ***" << std::endl;
  std::cout << "surf.m_numgroups = " << surf.m_numgroups << std::endl;
  for (int groupnum = 0 ; groupnum < surf.m_numgroups ; groupnum++) { 
    std::cout << "group num " << groupnum << ": " << std::endl;
    std::cout << "total num layers in this group = " << surf.m_numlayertotal[groupnum] << std::endl;
    std::cout << "substrate starts at layer num " << surf.m_numlayerplusone[groupnum] << std::endl;
    for (int layernum = 0 ; layernum <= surf.m_numlayertotal[groupnum] ; layernum++) {
      std::cout << "layer num " << layernum << std::endl;
      std::cout << "formula = " << surf.m_layerformula[groupnum][layernum] << std::endl;
      std::cout << "density = " << surf.m_layerdensity[groupnum][layernum] << std::endl;
      std::cout << "thickness = " << surf.m_layerthickness[groupnum][layernum] << std::endl;
      std::cout << "roughness = " << surf.m_layerroughness[groupnum][layernum] << std::endl;
      std::cout << "m_sigmaroughnesssq = " << surf.m_sigmaroughnesssq[groupnum][layernum] << std::endl;
      std::cout << std::endl;
    }
  }
  #endif
  
  // organize mirror coatings into unique materials, and map each group/layer 
  // to that material.  This is to avoid unnecessary repeated calculations
  // for layers that are made of identical materials.  An index for each layer
  // in each group will point to the material it's made of
  surf.m_nummaterials = 0;
  surf.m_numThinMaterials = 0;
  
  // loop through each group
  for (int groupnum = 0 ; groupnum < surf.m_numgroups ; groupnum++) {
    
    // create a new row in the vector for this group
    surf.m_layermaterialindex.push_back(std::vector<long>());
    
    // add dummy data for index layer = 0 for the vacuum
    surf.m_layermaterialindex[groupnum].push_back(0);
    // +++ 20150125 in trf, this is a -1.  change to that and test that it doesn't break
    
    // loop through each layer in this group
    // starting at layer=1, to avoid the vacuum at layer=0
    for (int layernum = 1 ; layernum <= surf.m_numlayertotal[groupnum] ; layernum++) {
      
      // check if this layer's material has already been placed in the 
      // surf.m_materialformula vector.  if so, index it
      bool materialExists = false;
      if (surf.m_nummaterials > 1) {
        for (int m = 0 ; m < surf.m_nummaterials ; m++) {
          if (surf.m_layerformula[groupnum][layernum] == surf.m_materialformula[m]) {
            materialExists = true;
            surf.m_layermaterialindex[groupnum].push_back(m);
          }
        }
      }
      
      // if this layer's material hasn't been seen yet, add it
      if (!materialExists) {
        surf.m_materialformula.push_back(surf.m_layerformula[groupnum][layernum]);
        surf.m_materialdensity.push_back(surf.m_layerdensity[groupnum][layernum]);
        surf.m_layermaterialindex[groupnum].push_back(surf.m_nummaterials);
        surf.m_nummaterials++;
        surf.m_materialinthicklayer.push_back(0);
        surf.m_materialinthinlayer.push_back(0);
        
        // if we haven't seen it yet, and it's in a thin layer, add to that ctr
        if (layernum < surf.m_numlayerplusone[groupnum]) {
          surf.m_numThinMaterials++;
        }
      }
      
      // +++ 20150125 in trf, this is >= surf.m_numlayerplusone[groupnum]  what should it be?
      // if this is a thick layer, flag it
      if (layernum >= surf.m_numlayerplusone[groupnum]) {
        surf.m_materialinthicklayer[surf.m_layermaterialindex[groupnum][layernum]]++;
      }
      
      // if this is a thin layer, flag it
      if (layernum < surf.m_numlayerplusone[groupnum]) {
        surf.m_materialinthinlayer[surf.m_layermaterialindex[groupnum][layernum]]++;
      }
    } // loop through layers in this group
    
  } // loop through groups
  
  // store how many thick substrate materials we found
  for (int iMat = 0 ; iMat < surf.m_nummaterials ; iMat++) {
    if (surf.m_materialinthicklayer[iMat] > 0) {
      surf.m_numThickMaterials++;
    }
  }
  
    
  /* testing that I stored layer material correctly */
  #ifdef DEBUG
  std::cout << std::endl;
  std::cout << "*** testing that I stored layer material correctly ***" << std::endl;
  std::cout << "surf.m_nummaterials = " << surf.m_nummaterials << std::endl;
  std::cout << "surf.m_numThickMaterials = " << surf.m_numThickMaterials << std::endl;
  std::cout << "surf.m_numThinMaterials = " << surf.m_numThinMaterials << std::endl;
  for (int i = 0 ; i < surf.m_nummaterials ; i++) {
    std::cout << "material i=" << i << " " << surf.m_materialformula[i];
    std::cout << "; density=" << surf.m_materialdensity[i];
    std::cout << "; in a thick material " << surf.m_materialinthicklayer[i] << " times";
    std::cout << "; in a thin material " << surf.m_materialinthinlayer[i] << " times" << std::endl;
  }
  std::cout << std::endl;
  for (int groupnum = 0 ; groupnum < surf.m_numgroups ; groupnum++) { 
    std::cout << "group num " << groupnum << ": " << std::endl;
    std::cout << "num layers in this group = " << surf.m_numlayertotal[groupnum] << std::endl;
    std::cout << "substrate starts at layer " << surf.m_numlayerplusone[groupnum] << std::endl;
    for (int layernum = 1 ; layernum <= surf.m_numlayertotal[groupnum] ; layernum++) {
      std::cout << "layer num " << layernum << std::endl;
      std::cout << "surf.m_layermaterialindex = " << surf.m_layermaterialindex[groupnum][layernum];
      std::cout << " is material " << surf.m_materialformula[surf.m_layermaterialindex[groupnum][layernum]];
      std::cout << " thickness " << surf.m_layerthickness[groupnum][layernum] << std::endl;
    }
  }  
  #endif

  // break down each unique coating material into constituent elements
  // store the  atomic elements and total weight
  int nelemcoat = 0;         // number of unique elements in a material
  std::vector<int> zcoat;    // atomic numbers of constituent elements
  std::vector<int> fcoat;    // num atoms w atomic num Z in molecule (w zcoat)
  double wcoat = 0;          // total atomic weight of molecule
  for (int i = 0 ; i < surf.m_nummaterials ; i++) {
    // break down this material into constituent atoms
    decomposeMaterial(surf.m_materialformula[i], atom, nelemcoat, zcoat, fcoat, wcoat);
    surf.m_materialnumelements.push_back(nelemcoat);
    surf.m_materialatomicweight.push_back(wcoat);
    // add rows to the vectors for this material
    surf.m_materialz.push_back(std::vector<long>());
    surf.m_materialnumatoms.push_back(std::vector<long>());
    // for each atom in this molecule, store which atom and how many
    for (int j = 0 ; j < nelemcoat ; j++) {
      surf.m_materialz[i].push_back(zcoat[j]);
      surf.m_materialnumatoms[i].push_back(fcoat[j]);
    }
  }
 
  /* testing that I did decomposeMaterial correctly */
  #ifdef DEBUG
  std::cout << std::endl;
  std::cout << "*** testing that I did decomposeMaterial correctly ***" << std::endl;
  for (int i = 0 ; i < surf.m_nummaterials ; i++) { 
    std::cout << "new coating material " << surf.m_materialformula[i] << std::endl;
    std::cout << " has # elements: " << surf.m_materialnumelements[i] << std::endl;
    std::cout << " has total atomic weight: " << surf.m_materialatomicweight[i] << std::endl;
    for (int j = 0 ; j < surf.m_materialnumelements[i] ; j++) {
      std::cout << j << "th element is " << surf.m_materialz[i][j];
      std::cout << " with " << surf.m_materialnumatoms[i][j] << " atoms" << std::endl;
    }
  }
  #endif

  // Find out if there is more than 1 layer in any group and set the flag 
  // multilayer to true if we are dealing with a multilayer surface.  This 
  // flag will later be used to write the keyword MULTLAYR to the output file.
  int maxlayers = getMaxInt(surf.m_numlayerplusone);
  if (maxlayers > 1) {
    surf.m_isMultilayer = true;
  } else {
    surf.m_isMultilayer = false;
  }
  
  
} // end initialize_surface()



void initialize_energy(const Param & param, Energy & energy) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  int fileExists = 0;                     // see if input is a filename
  fitsfile * energy_fp;                   // energy data file
  int status = 0;                         // for calls to cfitsio
  int anynul = 0;                         // for the call to fits_read_col
  long num_rows_energy_grid = 0;
  const double evtoangstrom  = 12398.5;         // 1eV  = 12398.5A
  const double evtocm = 1.23985e-4;             // 1eV  = 0.000123985cm
  const double eightpisq =78.956839603063;      // 8 * pi^2
  energyUnits_e energyUnit;
  std::vector<double> tempEnergies;       // if param is a string of energies
  
  int columnNum = 0;
  
  // for checking that data is in correct units
//  std::string tunit = "TUNIT";
//  char keywordChar[FLEN_CARD];
//  std::string keywordStr;
  
  // -------------------------------------
  
  
  // see if the energy param was a valid file
  fits_file_exists(param.m_energyfile.c_str(), &fileExists, &status);
  
  // the file does exist
  if (fileExists == 1) {

    // open the file
    fits_open_file(&energy_fp, param.m_energyfile.c_str(), READONLY, &status);

    // move to correct extension
    fits_movnam_hdu(energy_fp, ANY_HDU, const_cast<char *>("ENERGYGRID"), 0, &status);
    checkForFITSError(status, "opening ENERGYGRID extension in", param.m_energyfile);
    
    AH_INFO(ahlog::HIGH) << "Processing file: " << param.m_energyfile << std::endl;

    // see if there were any other errors opening the file
    checkForFITSError(status, "opening", param.m_energyfile);
    
    // store how many rows in this extension
    fits_get_num_rows(energy_fp, &num_rows_energy_grid, &status);
    checkForFITSError(status, "getting number of rows in", param.m_energyfile);
    energy.m_numenergy = num_rows_energy_grid;
    (energy.m_photonenergy).resize(energy.m_numenergy);

    // get the column number for energy
    fits_get_colnum(energy_fp, CASEINSEN, const_cast<char *>("Energy"), &columnNum, &status);

    // get grid data from file, store it in vector
    for (long long i = 0 ; i < energy.m_numenergy ; i++) {
      fits_read_col_dbl(energy_fp, 1, i+1, 1, 1, 0, &(energy.m_photonenergy[i]), &anynul, &status);
    }
    checkForFITSError(status, "getting data from", param.m_energyfile);

    // see what energy this is, to store energies later in eV
    energyUnit = getEnergyUnit(energy_fp, param.m_energyfile, columnNum);

    // close energy grid file
    fits_close_file(energy_fp, &status);
    checkForFITSError(status, "closing", param.m_energyfile);
  
  } else {
    // the file doesn't exist.  This should be a string of energies.
    
    // energy should be a list of numbers in a string; extract those numbers
    listStringsToDoubles(param.m_energyfile, tempEnergies);
    
    energy.m_photonenergy = tempEnergies;
    energy.m_numenergy = tempEnergies.size();
    
    // if it's a list of energies, it must be keV
    energyUnit = e_keV;
  
  } // end-if checking if energy is filename or list of energies
  
  // store energies in eV
  storeEnergyIneV(energyUnit, energy.m_photonenergy);
  
  /* testing that I got energy grid data */
  #ifdef DEBUG
  std::cout << std::endl;
  std::cout << "*** testing that I got energy grid data ***" << std::endl;
  for (int i = 0 ; i < energy.m_numenergy ; i++) {
    std::cout << "energy[" << i << "] = " << energy.m_photonenergy[i] << std::endl;
  }
  #endif
  
  // precalculate wavelength array in angstroms and cm for energy grid
  for (int i = 0 ; i < energy.m_numenergy ; i++) {
    energy.m_photonlam.push_back( evtoangstrom / energy.m_photonenergy[i] );
    energy.m_photonlamsq.push_back( energy.m_photonlam[i] * energy.m_photonlam[i] );
    energy.m_photonlamcm.push_back( evtocm / energy.m_photonenergy[i] );
    energy.m_photonlamcmsq.push_back( energy.m_photonlamcm[i] * energy.m_photonlamcm[i] );
    energy.m_lambdasqfac.push_back( eightpisq * energy.m_photonenergy[i] * energy.m_photonenergy[i]/(evtoangstrom*evtoangstrom) );
  }
  
  /* testing that I converted energy grid data correctly */
  #ifdef DEBUG
  std::cout << std::endl;
  std::cout << "*** testing that I converted energy grid data correctly ***" << std::endl;
  for (int i = 0 ; i < energy.m_numenergy ; i++) { 
    std::cout << "energy[" << i << "] = " << energy.m_photonenergy[i] << " eV ";
    std::cout << energy.m_photonlam[i] << " angstrom ";
    std::cout << energy.m_photonlamsq[i] << " lambda sq ";
    std::cout << energy.m_photonlamcm[i] << " cm ";
    std::cout << energy.m_photonlamcmsq[i] << " cm sq ";
    std::cout << energy.m_lambdasqfac[i] << " energy.m_lambdasqfac ";
    std::cout << std::endl;
  }
  #endif
  
} // end initialize_energy()



void initialize_angle(const Param & param, Angle & angle) {
 
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  fitsfile * angle_fp;                  // angle grid file
  int status = 0;                       // for calls to cfitsio
  int anynul = 0;                       // for the call to fits_read_col
  long num_rows_angle_grid = 0;
  int columnNum = 0;                    // which column number we want
  
  // for checking that data is in correct units
  std::string tunit = "TUNIT";
  char keywordChar[FLEN_CARD];
  std::string keywordStr;
  std::string errorMsg;
  
  // -------------------------------------
  
  // open the file
  fits_open_file(&angle_fp, (param.m_anglefile).c_str(), READONLY, &status);
  checkForFITSError(status, "opening", param.m_anglefile);
  
  // move to correct extension
  fits_movnam_hdu(angle_fp, ANY_HDU, const_cast<char *>("ANGLEGRID"), 0, &status);
  checkForFITSError(status, "opening ANGLEGRID extension in", param.m_anglefile);
  
  AH_INFO(ahlog::HIGH) << "Processing file: " << param.m_anglefile << std::endl;
  
  // get the column number for angles
  fits_get_colnum(angle_fp, CASEINSEN, const_cast<char *>("Angle"), &columnNum, &status);
  
  // make sure angle are in radians - code calculations depend on this
  tunit.append(intToString(columnNum));
  fits_read_key(angle_fp, TSTRING, const_cast<char *>(tunit.c_str()), keywordChar, NULL, &status);
  checkForFITSError(status, "reading keyword in", param.m_anglefile);
  keywordStr = std::string(keywordChar);
  if (!isEqualCaseInsens(keywordStr, "rad")) {
    errorMsg = "Angles need to be in radians, and ";
    errorMsg.append(tunit);
    errorMsg.append(" needs to be 'rad' in ");
    errorMsg.append(param.m_anglefile);
    AH_THROW_RUNTIME(errorMsg);
  }
  
  // store how many rows in this extension
  fits_get_num_rows(angle_fp, &num_rows_angle_grid, &status);
  checkForFITSError(status, "getting number of rows in", param.m_anglefile);
  angle.m_numangles = num_rows_angle_grid;
  (angle.m_incidentangle).resize(num_rows_angle_grid);
  
  // get column data from grid file, store it in vector
  for (long long i = 0 ; i < num_rows_angle_grid ; i++) {
    // get the data from the fits file
    fits_read_col_dbl(angle_fp, 1, i+1, 1, 1, 0, &(angle.m_incidentangle[i]), &anynul, &status);
  }
  checkForFITSError(status, "getting data from", param.m_anglefile);
  
  // close angle grid file
  fits_close_file(angle_fp, &status);
  checkForFITSError(status, "closing", param.m_anglefile);
  
  /* testing that I got angle grid data */
  #ifdef DEBUG
  std::cout << "*** testing that I got angle grid data ***" << std::endl;
  std::cout.precision(15);
  for (int i = 0 ; i < num_rows_angle_grid ; i++) { 
    std::cout << "angle[" << i << "] = " << angle.m_incidentangle[i] << std::endl;
  }
  #endif
  
} // end initialize_angle()



// load the data from the atomic edge file, and create the arrays
void initialize_edge(const Param & param, const Scattering & scat, Edge & edge) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  fitsfile * edge_fp;                     // atomic edges file
  int status = 0;                         // for calls to cfitsio
  int anynul = 0;                         // for the call to fits_read_col
  long num_rows_edge = 0;
  const int num_edge_cols = 5;
  int edge_col_nums[num_edge_cols] = {0};
  std::string edge_cols[num_edge_cols] = {"Z", "EdgeRowIndex", "EdgeName", "Energy", "Wavelength"};
  
  // for storing the TSTRING types from the input file 
  char * input_edgename;
  input_edgename = (char *) malloc(2*sizeof(char));
  
  // for checking that data is in correct units
  std::string tunit = "TUNIT";
  char keywordChar[FLEN_CARD];
  std::string keywordStr;
  std::string errorMsg;
  
  // -------------------------------------
  
  // the edge file is optional
  if ( isEqualCaseInsens(param.m_atmedgefile, "NONE") || ((param.m_atmedgefile).compare("") == 0) ) {
    
    // flag that no edge file was provided; linear interpolation will be used
    // in interpolatef1f2()
    edge.m_nedges = 0;
    
  } else {
    
    // open the file, go to the specified extension or the first table extension
    fits_open_table(&edge_fp, (param.m_atmedgefile).c_str(), READONLY, &status);
    checkForFITSError(status, "opening to the specified extension in", param.m_atmedgefile);
    
    AH_INFO(ahlog::HIGH) << "Processing file: " << param.m_atmedgefile << std::endl;
    
    // store how many rows in this extension
    fits_get_num_rows(edge_fp, &num_rows_edge, &status);
    checkForFITSError(status, "getting number of rows in", param.m_atmedgefile);
    // resize all the vectors that will store the data from the file
    // instead of using .push_back, because this is easier for cfitsio
    edge.m_Z.resize(num_rows_edge);
    edge.m_rowindex.resize(num_rows_edge);
    edge.m_edgename.resize(num_rows_edge);
    edge.m_energy.resize(num_rows_edge);
    edge.m_wavelength.resize(num_rows_edge);
      
    // find column numbers for edge data file
    for (int i = 0 ; i < num_edge_cols ; i++) {
      fits_get_colnum(edge_fp, CASEINSEN, const_cast<char *>((edge_cols[i]).c_str()), &edge_col_nums[i], &status);
    }
    checkForFITSError(status, "getting column numbers in", param.m_atmedgefile);
    
    // make sure data is in correct units - code calculations depend on this
    tunit.append(intToString(edge_col_nums[3]));    // Energy is 4th column in array
    fits_read_key(edge_fp, TSTRING, const_cast<char *>(tunit.c_str()), keywordChar, NULL, &status);
    checkForFITSError(status, "reading keyword in", param.m_atmedgefile);
    keywordStr = std::string(keywordChar);
    if (!isEqualCaseInsens(keywordStr, "eV")) {
      errorMsg = "Angles need to be in eV, and ";
      errorMsg.append(tunit);
      errorMsg.append(" needs to say 'eV' in ");
      errorMsg.append(param.m_atmedgefile);
      AH_THROW_RUNTIME(errorMsg);
    }
  
    for (long long i = 0 ; i < num_rows_edge ; i++) {
      
      // get column data from edge file, store it in vector
      fits_read_col(edge_fp, TINT, edge_col_nums[0], i+1, 1, 1, 0, &(edge.m_Z[i]), &anynul, &status);
      fits_read_col(edge_fp, TINT, edge_col_nums[1], i+1, 1, 1, 0, &(edge.m_rowindex[i]), &anynul, &status);
      fits_read_col(edge_fp, TSTRING, edge_col_nums[2], i+1, 1, 1, 0, &input_edgename, &anynul, &status);
      fits_read_col_dbl(edge_fp, edge_col_nums[3], i+1, 1, 1, 0, &(edge.m_energy[i]), &anynul, &status);
      fits_read_col_dbl(edge_fp, edge_col_nums[4], i+1, 1, 1, 0, &(edge.m_wavelength[i]), &anynul, &status);
      
      // convert the char * from cfitsio into C++ strings, store in the Param
      edge.m_edgename[i] = (std::string)input_edgename;
      
      /* +++ 20140224 KLR remove this part, once I'm sure I'm not going to require this sorting
      // make sure the input file is sorted, by "Z" then "EdgeRowIndex"
      if (edge.m_Z[i] > currZ) {
        // if this is the next group, increment currZ and reset currRowIndex
        currZ = edge.m_Z[i];
        currRowIndex = 0;
      } else if (edge.m_Z[i] == currZ) {
        // if we're still at the same group, check on currRowIndex
        if (edge.m_rowindex[i] >= currRowIndex) {
          currRowIndex = edge.m_rowindex[i];
        } else {
          errorMsg = "Input edge file (";
          errorMsg.append(param.m_atmedgefile);
          errorMsg.append(") must be sorted by Z then EdgeRowIndex.");
          // set flag to abort (which is done after we close the fits file)
          needToAbort = true;
          // and we can go ahead and get out of the loop 
          break;
        }
      } else {
        // if they're out of order, abort
        errorMsg = "Input edge file (";
        errorMsg.append(param.m_atmedgefile);
        errorMsg.append(") must be sorted by Z then EdgeRowIndex.");
        // set flag to abort (which is done after we close the fits file)
        needToAbort = true;
        // and we can go ahead and get out of the loop 
        break;
      }
      */
    
    }
   
    // free the char * we used to get the TSTRING data from cfitsio
    free(input_edgename); input_edgename=0;
    
    // check for fits error after free the char *
    checkForFITSError(status, "getting data from", param.m_atmedgefile);
 
    // close edge file
    fits_close_file(edge_fp, &status);
    checkForFITSError(status, "closing", param.m_atmedgefile);
    
    edge.m_nedges = edge.m_Z.size();
        
  }
  
  /* test that I've stored the edge data correctly */
  #ifdef DEBUG
  std::cout << std::endl;
  std::cout << "**** test that I've stored the edge data ****" << std::endl;
  std::cout << "edge.m_nedges: " << edge.m_nedges << std::endl;
  for (int i = 0 ; i < edge.m_nedges ; i++) {
    std::cout << "edge i=" << i << ": " << "Z=" << edge.m_Z[i] << " ";
    std::cout << "rowindex="  << edge.m_rowindex[i] << " ";
    std::cout << "edgename="  << edge.m_edgename[i] << " ";
    std::cout << "energy="  << edge.m_energy[i] << " ";
    std::cout << "wavelength="  << edge.m_wavelength[i] << std::endl;
  }
  #endif
  
  // loop through each edge
  for (int i = 0 ; i < edge.m_nedges ; i++) {
    
    // check if the this edge energy lies in the energy range of the atomic 
    // scattering factors for this Z
    if ( (edge.m_energy[i] >= scat.m_energy[scat.m_f1effindex[edge.m_Z[i]][0]]) && 
         (edge.m_energy[i] <= scat.m_energy[scat.m_f1effindex[edge.m_Z[i]][1]]) ) {
           
      // loop over each energy in the scattering factor energy array to find 
      // the pair of energies that enclose this edge energy
      for (int j = scat.m_f1effindex[edge.m_Z[i]][0] ; 
                  j < scat.m_f1effindex[edge.m_Z[i]][1] - 1 ; j++) {
       
        // store the lower row index of that pair in edgearray_sbin()
        if ( (edge.m_energy[i] >= scat.m_energy[j]) && 
             (edge.m_energy[i] <  scat.m_energy[j+1]) ) {
          edge.m_edgearray_sbin.push_back(j);
        }
  
        // if ith edge energy is equal to the larger of the pair of scattering
        // factor energies, set edgearray_sbin(i) = row of the larger energy
        if (edge.m_energy[i] == scat.m_energy[scat.m_f1effindex[edge.m_Z[i]][1]]) {
          edge.m_edgearray_sbin.push_back(scat.m_f1effindex[edge.m_Z[i]][1]);
        }
      
      }

    } else {
      // this energy didn't fall inside the effective range. flag it negative
      edge.m_edgearray_sbin.push_back(-1);
    }
    
  }
  
  /* test that I've stored m_edgearray_sbin correctly */
  #ifdef DEBUG
  if (edge.m_nedges > 0) {
    std::cout << std::endl;
    std::cout << "**** test that I've stored m_edgearray_sbin correctly ****" << std::endl;
    std::cout << "edge.m_edgearray_sbin.size(): " << edge.m_edgearray_sbin.size() << std::endl;
    for (unsigned int i = 0 ; i < edge.m_edgearray_sbin.size() ; i++) {
      std::cout << "edge i = " << i << std::endl;
      std::cout << " edge.m_energy[i] = " << edge.m_energy[i] << std::endl;
      std::cout << " edge.m_edgearray_sbin[i] = " << edge.m_edgearray_sbin[i] << std::endl;
      std::cout << " scat.m_energy[edge.m_edgearray_sbin[i]] = " << scat.m_energy[edge.m_edgearray_sbin[i]] << std::endl;
      std::cout << " scat.m_energy[edge.m_edgearray_sbin[i]+1] = " << scat.m_energy[edge.m_edgearray_sbin[i]+1] << std::endl;
      std::cout << std::endl;
    } 
  }
  #endif
  
} // end initialize_edge()



// initialize the output file - open it, create HDU, etc
void initialize_output(Param & param, Energy & energy, Angle & angle, Surface & surf) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  int status = 0;             // for cfitsio calls
  
  // to create the output HDU columns
  int numAngles = angle.m_incidentangle.size();
  std::string numAnglesStr = intToString(numAngles);
  std::string angleTForm = numAnglesStr.append("D");
  int tfields = 0;        // how many columns in the extension
  // the following define the column names, formats, units; are keywords in header
  std::vector<std::string> ttype;       // column name in fits file
  std::vector<std::string> tform;       // data format for column in fits file
  std::vector<std::string> tunit;       // physical unit for column in fits file
  char ** c_ttype;        // convert ttype to c string for fits call
  char ** c_tform;        // convert tform to c string for fits call
  char ** c_tunit;        // convert tunit to c string for fits call
  std::string currColName;
  std::stringstream currColNumStrStr;
  std::string currColNumStr;
  std::string indexComment;
  std::vector<double> energyInkeV;      // energy will be written in keV
  
  // for mass absorption extension HDU header keywords (MATERInn and DENSITnn)
  std::string currKeyName;
  std::vector<std::string> mabsMatKeyNames;
  std::vector<std::string> mabsMatKeyVals;
  std::vector<std::string> mabsDenKeyNames;
  std::vector<double>      mabsDenKeyVals;
  char ** c_currKeyNames;
  char ** c_currKeyVals;
  std::string matComment = "name of this material";
  std::string denComment = "[g / cm**3] density of this material";
  
  // array to store column index numbers of substrate materials (used for 
  // writing SUBSTR01 etc keywords to refl extension)
  std::vector<int> substrateColumnNums;
  // for writing the SUBSTR01 etc keywords to the reflectivity extension
  std::string substrateKeyword;
  std::string substrateComment;
  std::string substrateKeyThick;
  double substrateThickness = 0.0;
  std::string substrateThickComment;

  // for updating the TTYPEn (column name) keyword comments
  int TTYPEnum = 0;
  std::string TTYPE = "TTYPE";
  std::string currTTYPE;
  std::string currComm;
  
  // -------------------------------------
  
  // check if file already exists
  int fileExists = 0;
  fits_file_exists((param.m_outfile).c_str(), &fileExists, &status);
  checkForFITSError(status, "checking for existence of", param.m_outfile);
  
  // if file exists
  if (fileExists == 1) {
    
    // open file
    fits_open_file(&(param.m_out_fp), (param.m_outfile).c_str(), READWRITE, &status);
    checkForFITSError(status, "opening", param.m_outfile);
    
    AH_INFO(ahlog::HIGH) << "Processing file: " << param.m_outfile << std::endl;
    
    // see if it has a refl extension already with desired name
    fits_movnam_hdu(param.m_out_fp, ANY_HDU, const_cast<char *>((param.m_outext).c_str()), 0, &status);
    // if this HDU was found and clobber wasn't set, abort program
    if ( (status != BAD_HDU_NUM) && !headas_clobpar ) {
      AH_THROW_RUNTIME("desired HDU already exists in file");
    } else if ( (status != BAD_HDU_NUM) && headas_clobpar) {
      // if clobber was set, just delete this hdu
      fits_delete_hdu(param.m_out_fp, 0, &status);
    }
    
    // now see if it has a MASS_ABSORPTION extension
    fits_movnam_hdu(param.m_out_fp, ANY_HDU, const_cast<char *>(s_massAbsExtName), 0, &status);
    // if this HDU was found and clobber wasn't set, abort program
    if ( (status != BAD_HDU_NUM) && !headas_clobpar ) {
      AH_THROW_RUNTIME("mass absorption HDU already exists in file");
    } else if ( (status != BAD_HDU_NUM) && headas_clobpar) {
      // if clobber was set, just delete this hdu
      fits_delete_hdu(param.m_out_fp, 0, &status);
    }
    
  } else if (fileExists == -1) {
    AH_THROW_RUNTIME("input file name is not a disk file");
    
  } else {
    // if it doesn't exist, create it
    fits_create_file(&(param.m_out_fp), (param.m_outfile).c_str(), &status);
    checkForFITSError(status, "creating", param.m_outfile);
  }
  
  // convert the energy into keV before writing it
  energyInkeV = energy.m_photonenergy;
  std::transform(energyInkeV.begin(), energyInkeV.end(), 
                     energyInkeV.begin(),
                     std::bind2nd(std::multiplies<double>(), s_eVTokeV));
  
  
  // -------------------------------------
  //    reflectivity extension
  // -------------------------------------
   
  // create the columns for refl extension 
  ttype.push_back("Energy");
  tform.push_back("1D");
  tunit.push_back("keV");
  ttype.push_back("Angle");
  tform.push_back(angleTForm);
  tunit.push_back("rad");
  
  // create the Reflectivity and Transmission Probability columns for each group
  for (int i = 0 ; i < surf.m_numgroups ; i++) {
    
    // get the zero-padded column index for this loop iteration
    currColNumStrStr.str("");
    currColNumStrStr.clear();
    // vector is 0-based, but we want 1-based, so use i+1
    currColNumStrStr << std::setw(2) << std::setfill('0') << i+1;
    currColNumStr = currColNumStrStr.str();
    
    // create the Reflectivity Probability columns
    currColName = "RefProb";
    currColName.append(currColNumStr);
    ttype.push_back(currColName);
    tform.push_back(angleTForm);    // RefProbNN has same dim. as angle col
                                    // and is unitless
    
    // now create the Transmission Probability columns
    currColName = "TranProb";
    currColName.append(currColNumStr);
    ttype.push_back(currColName);
    tform.push_back(angleTForm);    // TrabProbNN has same dim. as angle col
                                    // and is unitless
  }
  
  // see how many columns we now have
  tfields = ttype.size();
  // make the tunit vector the same size
  tunit.resize(tfields);
      
  // copy the C++ strings into C char *s, to pass to cfitsio
  c_ttype = new char * [tfields];
  c_tform = new char * [tfields];
  c_tunit = new char * [tfields];
  for (int i = 0 ; i < tfields ; i++) {
    c_ttype[i] = const_cast<char*>(ttype[i].c_str());
    c_tform[i] = const_cast<char*>(tform[i].c_str());
    c_tunit[i] = const_cast<char*>(tunit[i].c_str());
  }
      
  // create the refl extension
  fits_create_tbl(param.m_out_fp, BINARY_TBL, 0, tfields, c_ttype, c_tform,
                  c_tunit, (param.m_outext).c_str(), &status);
  checkForFITSError(status, "creating extension in", param.m_outfile);
  
  // move to the reflectivity extension
  fits_movnam_hdu(param.m_out_fp, ANY_HDU, const_cast<char *>((param.m_outext).c_str()), 0, &status);
  checkForFITSError(status, "moving to extension in", param.m_outfile);
  
  // -------------------------------------
  // update TTYPEn keywords (column names)
  // -------------------------------------
  
  fits_update_key_str(param.m_out_fp, "TTYPE1", "Energy", "Energy", &status);
  fits_update_key_str(param.m_out_fp, "TTYPE2", "Angle", "Angle of incidence", &status);
  TTYPEnum = 2;
  for (int i = 1 ; i <= surf.m_numgroups ; i++) {
    
    // get the zero-padded column index for this loop iteration
    currColNumStrStr.str("");
    currColNumStrStr.clear();
    // vector is 0-based, but we want 1-based, so use i+1
    currColNumStrStr << std::setw(2) << std::setfill('0') << i;
    currColNumStr = currColNumStrStr.str();
    
    // Reflectivity Probability columns
    ++TTYPEnum;
    currTTYPE = TTYPE + intToString(TTYPEnum);
    currColName = "RefProb";
    currColName.append(currColNumStr);
    currComm = "Probability of reflection for group " + intToString(i);
    fits_update_key_str(param.m_out_fp, currTTYPE.c_str(), currColName.c_str(), currComm.c_str(), &status);
    
    // Transmission Probability columns
    ++TTYPEnum;
    currTTYPE = TTYPE + intToString(TTYPEnum);
    currColName = "TranProb";
    currColName.append(currColNumStr);
    currComm = "Probability of transmission for group " + intToString(i);
    fits_update_key_str(param.m_out_fp, currTTYPE.c_str(), currColName.c_str(), currComm.c_str(), &status);
    
  }
  
  // -------------------------------------
  // write the keywords for the reflectivity extension
  // -------------------------------------
  fits_update_key_longstr(param.m_out_fp, "CDES0001", const_cast<char *>(param.m_desc.c_str()), const_cast<char *>("Description"), &status);
  fits_update_key_str(param.m_out_fp, "CCNM0001", const_cast<char *>("REFLECT-TRANS"), const_cast<char *>("Type of calibration data"), &status);
  writeKeywordsToOutput(param, surf, energy);
  fits_update_key_lng(param.m_out_fp, "NGROUPS", (long)(surf.m_numgroups), const_cast<char *>("Number of mirror groups on this surface"), &status);
  fits_update_key_longstr(param.m_out_fp, param.m_roughkey.c_str(), const_cast<char *>(param.m_roughmodel.c_str()), const_cast<char *>("Roughness model used"), &status);
  checkForFITSError(status, "writing keywords to", param.m_outfile);
  
  // -------------------------------------
  // populate the energy and angle columns
  // -------------------------------------
  fits_write_col(param.m_out_fp, TDOUBLE, 1, 1, 1, (LONGLONG)(energy.m_numenergy), &(energyInkeV[0]), &status);
  checkForFITSError(status, "writing energies to", param.m_outfile);
  // the angle array is written in the angle cell for EVERY energy
  for (int i = 1 ; i <= energy.m_numenergy ; i++) {
    fits_write_col(param.m_out_fp, TDOUBLE, 2, i, 1, (LONGLONG)(angle.m_numangles), &(angle.m_incidentangle[0]), &status);
  }
  checkForFITSError(status, "writing angles to", param.m_outfile);

  // reset the vectors and arrays to use for the next extension columns
  ttype.clear();
  tform.clear();
  tunit.clear();
  delete [] c_ttype;
  delete [] c_tform;
  delete [] c_tunit;
  tfields = 0;
 
  
  // -------------------------------------
  //    mass absorption extension
  // -------------------------------------
  
  // create MABSCOEFNN columns and keywords for mass absorption extension
  ttype.push_back("Energy");
  tform.push_back("1D");
  tunit.push_back("keV");
  
  // add 1 because columns are 1-based; add 1 because energy is first column
  const int colOffset = 2;
  
  for (int iMat = 0 ; iMat < surf.m_nummaterials ; iMat++) {
        
    // if this material is in a thick layer, store it's column number
    if (surf.m_materialinthicklayer[iMat] > 0) {
      // add this column index number to vector keeping track of substrate
      // column indices
      // add 1 because Energy is the first column, and we want the column number
      substrateColumnNums.push_back(iMat + colOffset);
    }
    
    // the HDU keyword names can't be over 8 characters long.  (ie: can't be
    // MATERI102).  Make sure we don't get to over 99
    if (iMat >= 99) {
      AH_THROW_RUNTIME("Cannot have more than 99 materials for mass absorption coefficients.");
    }
    
    // get the zero-padded column index for this loop iteration
    currColNumStrStr.str("");
    currColNumStrStr.clear();
    currColNumStrStr << std::setw(2) << std::setfill('0') << iMat+1;
    currColNumStr = currColNumStrStr.str();
    
    // mass absorption coefficient columns, for each thick material
    currColName = "mabscoef";
    currColName.append(currColNumStr);
    ttype.push_back(currColName);
    tform.push_back("1D");
    tunit.push_back("cm**2 / g");
    
    AH_DEBUG << "currColName = " << currColName << std::endl;
    
    // HDU header keywords (keyword name much be less than 8 characters.)
    currKeyName = "MATERI";
    currKeyName.append(currColNumStr);
    mabsMatKeyNames.push_back(currKeyName);
    mabsMatKeyVals.push_back(surf.m_materialformula[iMat]);
    currKeyName = "DENSIT";
    currKeyName.append(currColNumStr);
    mabsDenKeyNames.push_back(currKeyName);
    mabsDenKeyVals.push_back(surf.m_materialdensity[iMat]);
    
    // keep track of the pre-collimator material column index
    if (surf.m_materialformula[iMat] == s_precollMaterial) {
      surf.m_precolmaterial = iMat+colOffset;
    }
  }
  surf.m_nummassabs = surf.m_nummaterials;
  
  tfields = ttype.size();
  
  // copy the C++ strings into C char *s, to pass to cfitsio
  c_ttype = new char * [tfields];
  c_tform = new char * [tfields];
  c_tunit = new char * [tfields];
  for (int i = 0 ; i < tfields ; i++) {
    c_ttype[i] = const_cast<char*>(ttype[i].c_str());
    c_tform[i] = const_cast<char*>(tform[i].c_str());
    c_tunit[i] = const_cast<char*>(tunit[i].c_str());
  }
  
  // create the mass absorption extension
  fits_create_tbl(param.m_out_fp, BINARY_TBL, 0, tfields, c_ttype, c_tform,
                  c_tunit, s_massAbsExtName, &status);
  checkForFITSError(status, "creating extension in", param.m_outfile);
  
  // move to the mass absorption extension
  fits_movnam_hdu(param.m_out_fp, ANY_HDU, const_cast<char *>(s_massAbsExtName), 0, &status);
  checkForFITSError(status, "moving to extension in", param.m_outfile);
  
  // -------------------------------------
  // update TTYPEn keywords (column names)
  // -------------------------------------
  
  fits_update_key_str(param.m_out_fp, "TTYPE1", "Energy", "Energy", &status);
  TTYPEnum = 1;
  std::string colName = "mabscoef";
  for (int iMat = 1 ; iMat <= surf.m_nummaterials ; iMat++) {
    ++TTYPEnum;    
    currColNumStrStr.str("");
    currColNumStrStr.clear();
    currColNumStrStr << std::setw(2) << std::setfill('0') << iMat;
    currColNumStr = currColNumStrStr.str();
    currTTYPE = TTYPE + intToString(TTYPEnum);
    currColName = colName + currColNumStr;
    AH_DEBUG << "currColName = " << currColName << std::endl;
    currComm = "Mass absorption coefficient for material " + intToString(iMat);
    fits_update_key_str(param.m_out_fp, currTTYPE.c_str(), currColName.c_str(), currComm.c_str(), &status);
  }
  
  // -------------------------------------
  // set the keywords for the mass absorption extension
  // -------------------------------------
  // copy the C++ strings into C char *s, to pass to cfitsio
  c_currKeyNames = new char * [surf.m_nummassabs];
  c_currKeyVals = new char * [surf.m_nummassabs];
  for (int i = 0 ; i < surf.m_nummassabs ; i++) {
    // write the MATERInn keyword
    c_currKeyNames[i] = const_cast<char*>(mabsMatKeyNames[i].c_str());
    c_currKeyVals[i] = const_cast<char*>(mabsMatKeyVals[i].c_str());
    fits_write_key(param.m_out_fp, TSTRING, c_currKeyNames[i], c_currKeyVals[i], const_cast<char *>(matComment.c_str()), &status);
    // write the DENSITnn keyword
    c_currKeyNames[i] = const_cast<char*>(mabsDenKeyNames[i].c_str());
    fits_write_key(param.m_out_fp, TDOUBLE, c_currKeyNames[i], &(mabsDenKeyVals[i]), const_cast<char *>(denComment.c_str()), &status);
  }
  checkForFITSError(status, "writing keywords to", param.m_outfile);
  
  // write the keywords for the mass absorption extension
  fits_update_key_longstr(param.m_out_fp, "CDES0001", const_cast<char *>("Mass absorption coefficients"), const_cast<char *>("Description"), &status);
  fits_update_key_str(param.m_out_fp, "CCNM0001", const_cast<char *>("MASS_ABSORPTION"), const_cast<char *>("Type of calibration data"), &status);
  writeKeywordsToOutput(param, surf, energy);
  fits_update_key_lng(param.m_out_fp, "NMATERIA", surf.m_nummassabs, const_cast<char *>("Number of materials in this extension"), &status);
  checkForFITSError(status, "writing keywords to", param.m_outfile);
  writeHistoryKeywords(param);
  
  // copy the energy grid data to the MASS_ABSORPTION extension of output file
  fits_write_col(param.m_out_fp, TDOUBLE, 1, 1, 1, (LONGLONG)(energy.m_numenergy), &(energyInkeV[0]), &status);
  checkForFITSError(status, "writing mass absorption coefficients to", param.m_outfile);

  // delete the C char* arrays
  delete [] c_ttype;
  delete [] c_tform;
  delete [] c_tunit;
  delete [] c_currKeyNames;
  delete [] c_currKeyVals;
  
  
  // -------------------------------------
  //    back to reflectivity extension 
  //    to write info about mass absorption extension
  // -------------------------------------
  
  // move to the reflectivity extension
  fits_movnam_hdu(param.m_out_fp, ANY_HDU, const_cast<char *>((param.m_outext).c_str()), 0, &status);
  checkForFITSError(status, "moving to extension in", param.m_outfile);
  
  // now write keywords to refl extension about mass absorption, substrate
  fits_update_key_lng(param.m_out_fp, "NMATERIA", surf.m_nummassabs, const_cast<char *>("Number of materials with mass absorption coeff"), &status);
  fits_update_key_lng(param.m_out_fp, "NSUBSTRA", surf.m_numThickMaterials, const_cast<char *>("Number of unique substrate materials in mirror foils"), &status);
  indexComment = "The index in the following keywords refers to the column number in the mass absorption extension. ";
  indexComment.append("Index 2 refers to the second column, which is mabscoef1 for substrate 1, etc.");
  fits_write_comment(param.m_out_fp, const_cast<char *>(indexComment.c_str()), &status);
  fits_update_key_lng(param.m_out_fp, "PCOLMTRL", surf.m_precolmaterial, const_cast<char *>("Precollimator material index number"), &status);
  
  for (int i = 0 ; i < surf.m_numThickMaterials ; i++) {
    
    // add keywords SUBSTR01 etc to identify column number of each substrate material
    substrateKeyword = "SUBSTR";
    substrateKeyword.append(intToString(i+1));
    substrateComment = "Index of material for substrate ";
    substrateComment.append(intToString(i+1));
    fits_update_key_lng(param.m_out_fp, substrateKeyword.c_str(), substrateColumnNums[i], const_cast<char *>(substrateComment.c_str()), &status);
    
    substrateKeyThick = "SUBTHK";
    substrateKeyThick.append(intToString(i+1));
    // this is based off the fact that all substrates in surface, for each 
    // group, are identical.  If there are three substrate materials, starting 
    // at , say, layer -13 for group 1 (we're just grabbing the first group
    // since they should all be identical), then this grabs value for
    // surf.m_coatthickness[13+i] where i is number of substrate materials
    // Also depends on thickness given in Angstrom in mirror surface file
    substrateThickness = surf.m_coatthickness[surf.m_numlayerplusone[0] + i - 1] * s_angstromTomm;
    substrateThickComment = "[mm] Thickness of substrate ";
    substrateThickComment.append(intToString(i+1));
    fits_update_key_dbl(param.m_out_fp, substrateKeyThick.c_str(), substrateThickness, 3, const_cast<char *>(substrateThickComment.c_str()), &status);
    
  }
  
  checkForFITSError(status, "writing keywords to", param.m_outfile);
  
  writeHistoryKeywords(param);
  
} // end initialize_output()



void writeKeywordsToOutput(Param & param, Surface & surf, Energy & energy) {
  
  // -------------------------------------
  // define and initialize all variables
  // -------------------------------------
  
  int status = 0;             // for cfitsio calls
  
  // for writing the energy range to a keyword in the output file
  std::string energyRange = "ENERG(";
  const double minEnergy = energy.m_photonenergy[0];
  const double maxEnergy = energy.m_photonenergy[energy.m_numenergy-1];
  energyRange.append(doubleToString(minEnergy));
  energyRange.append("-");
  energyRange.append(doubleToString(maxEnergy));
  energyRange.append(")eV");
  
  // filenames, without the paths
  std::string atomdbfile    = getFilename(param.m_atomicfile);
  std::string atomscatfile  = getFilename(param.m_atmscafile);
  std::string atomedgefile  = getFilename(param.m_atmedgefile);
  std::string energyfile    = getFilename(param.m_energyfile);   
  std::string anglefile     = getFilename(param.m_anglefile);    
  std::string mirrordeffile = getFilename(param.m_mirrorfile);
  std::string outputfile    = getFilename(param.m_outfile);
  
  // -------------------------------------
  
  // add other keywords
  fits_update_key_str(param.m_out_fp, "CCLS0001", const_cast<char *>("BCF"), const_cast<char *>("Basic calibration file"), &status);
  fits_update_key_str(param.m_out_fp, "CDTP0001", const_cast<char *>("DATA"), const_cast<char *>("Calibration file contains data"), &status);
  fits_update_key_str(param.m_out_fp, "CVSD0001", const_cast<char *>(param.m_validdate.c_str()), const_cast<char *>("UTC date when file should be first used"), &status);       // +++ format date and time correctly
  fits_update_key_str(param.m_out_fp, "CVST0001", const_cast<char *>(param.m_validtime.c_str()), const_cast<char *>("UTC time when file should be first used"), &status);
  fits_update_key_longstr(param.m_out_fp, "CBD10001", const_cast<char *>(energyRange.c_str()), const_cast<char *>("Energy range used"), &status);
  fits_update_key_longstr(param.m_out_fp, "TELESCOP", const_cast<char *>(param.m_telescop.c_str()), const_cast<char *>("Mission name"), &status);
  fits_update_key_longstr(param.m_out_fp, "INSTRUME", const_cast<char *>(param.m_instrume.c_str()), const_cast<char *>("Instrument name"), &status);
  // only write the DETNAM keyword if it was found in the SURFACE extension
  if (surf.m_detnam != "VALUE_UNDEFINED") {
    fits_update_key_longstr(param.m_out_fp, "DETNAM", const_cast<char *>(surf.m_detnam.c_str()), const_cast<char *>("Telescope name"), &status);
  }
  fits_update_key_str(param.m_out_fp, "ORIGIN", const_cast<char *>("NASA/GSFC"), const_cast<char *>("Origin of the file"), &status);
  fits_update_key_str(param.m_out_fp, "CREATOR", const_cast<char *>("xrtreftable"), const_cast<char *>("Software that created this file"), &status);
  fits_write_date(param.m_out_fp, &status);
  fits_update_key_log(param.m_out_fp, "MULTLAYR", (surf.m_isMultilayer ? 1 : 0), "Is reflecting surface a multilayer coating", &status);
  fits_update_key_longstr(param.m_out_fp, "FILENAME", const_cast<char *>(outputfile.c_str()), 0, &status);
  fits_write_comment(param.m_out_fp, const_cast<char *>("Filename"), &status);
  fits_update_key_longstr(param.m_out_fp, "ATOMFILE", const_cast<char *>(atomdbfile.c_str()), 0, &status);
  fits_write_comment(param.m_out_fp, const_cast<char *>("ATOMFILE = Input atomic data file used"), &status);
  fits_update_key_longstr(param.m_out_fp, "EDGEFILE", const_cast<char *>(atomedgefile.c_str()), 0, &status);
  fits_write_comment(param.m_out_fp, const_cast<char *>("EDGEFILE = Input atomic edge data file used"), &status);
  fits_update_key_longstr(param.m_out_fp, "ENRGFILE", const_cast<char *>(energyfile.c_str()), 0, &status);
  fits_write_comment(param.m_out_fp, const_cast<char *>("ENRGFILE = Input energy file used"), &status);
  fits_update_key_longstr(param.m_out_fp, "ANGLFILE", const_cast<char *>(anglefile.c_str()), 0, &status);
  fits_write_comment(param.m_out_fp, const_cast<char *>("ANGLFILE = Input angle file used"), &status);
  fits_update_key_longstr(param.m_out_fp, "SCATFILE", const_cast<char *>(atomscatfile.c_str()), 0, &status);
  fits_write_comment(param.m_out_fp, const_cast<char *>("SCATFILE = Input atomic scattering data file used"), &status);
  fits_update_key_longstr(param.m_out_fp, "ATMSCTNG", const_cast<char *>(param.m_atmsctng.c_str()), 0, &status);
  fits_write_comment(param.m_out_fp, const_cast<char *>("ATMSCTNG = Extension used in atomic scattering data file"), &status);
  fits_update_key_longstr(param.m_out_fp, "MIRRFILE", const_cast<char *>(mirrordeffile.c_str()), 0, &status);
  fits_write_comment(param.m_out_fp, const_cast<char *>("MIRRFILE = Input telescope definition file used"), &status);
  
  checkForFITSError(status, "writing keywords to", param.m_outfile);
  
} // end writeKeywordsToOutput()



void writeHistoryKeywords(Param & param) {
  
  int status = 0;             // for cfitsio calls
  int history = 0;
  
  // right before stamping the history params, stamp the version of the code that produced this output file
  fits_update_key_str(param.m_out_fp, "VERSION", "v1.07", "Version number of xrtreftable code", &status);
  
  // if history parameter was set, write all the input parameters to header
  get_history(&history);
  if (history) {
    // note: 2nd argument = 0 means to stamp the active HDU
    HDpar_stamp(param.m_out_fp, 0, &status);
    checkForFITSError(status, "writing keywords to", param.m_outfile);
  }
  
  
} // end writeKeywordsToOutput()









int startUp(int argc, char ** argv, const char * tooltag, HeaAppData * appdata) {
  int status = 0;

  /* Check arguments. Use stdio functions for reporting problems at this stage, since no other message streams are set up yet. */
  if (0 >= argc) { fprintf(stderr, "startUp: logic (programming) error; argc == %d; must be positive.\n", argc); status = 1; }
  if (0 == argv) { fprintf(stderr, "startUp: logic (programming) error; argv is null.\n"); status = 1; }
  else if (0 == *argv) { fprintf(stderr, "startUp: logic (programming) error; *argv is null.\n"); status = 1; }
  /* if (0 == tooltag) no problem; tooltag is optional. */
  if (0 == appdata) { fprintf(stderr, "startUp: logic (programming) error; appdata pointer is null.\n"); status = 1; }

  if (0 == status) {
    /** Initialize the application data structure. */
    *appdata = heaapp_construct_appdata(argc, argv); /* TODO: add tooltag when heaapp has it. */
  }

  /** From here on, use I/O functions in appdata to report errors. These will function correctly even if
      all else fails below. */

  if (0 == status) {
    /** Connect ape. Note this does not actually initialize ape, but it connects code that will initialize ape.
        The ape initialization code will read standard parameters, including chatter, logfile, history and
        clobber and store them in the application data structure. */
    status = heaapp_ape_connect(appdata);
    if (0 != status) appdata->printerr("startUp", "unable to connect ape.\n");
  }

  if (0 == status) {
    /** Connect ahlog. Note this does not actually initialize ahlog, but it connects code that will initialize ahlog.
        The ahlog initialization code will pull the name of the tool, chatter and logfile parameters, etc., from the
        application data structure when it *does* run. */
    status = heaapp_ahlog_connect(appdata);
    if (0 != status) appdata->printerr("startUp", "unable to connect ahlog.\n");
  }

  if (0 == status) {
    /** Connect heautils. Note this does not actually initialize heautils, but it connects code that will initialize heautils.
        The heautils initialization code will pull history, clobber, etc., from the
        application data structure when it *does* run. */
    status = heaapp_heautils_connect(appdata);
    if (0 != status) appdata->printerr("startUp", "unable to connect heautils.\n");
  }

  if (0 == status) {
    /** Finally, run all the initialization codes that were connected above. */
    status = heaapp_initialize(appdata);
    if (0 != status) appdata->printerr("startUp", "unable to initialize application.\n");
  }

  return status;
}

int shutDown(HeaAppData * appdata) {
  int status = 0;
  /* Check arguments. Use stdio functions (only) for reporting null appdata, since no other option. */
  if (0 == appdata) { fprintf(stderr, "shutDown: logic (programming) error; appdata pointer is null.\n"); status = 1; }

  /* Do finalize operations. */
  if (0 == status) {
    /** This will shut down the libraries in reverse order that were started up in the startUp function. */
    status = heaapp_finalize(appdata);
    /* Report error using appdata IO, which is valid even if there was an error. */
    if (0 != status) { appdata->printerr("shutDown", "heaapp_finalize returned an error.\n"); }
  }
  return status;
}




// end addtogroup
/** @} */


/* Revision Log
  $Log: xrtreftable.cxx,v $
  Revision 1.37  2016/03/23 02:06:39  klrutkow
  write DATE, DATASUM, CHECKSUM keywords to both extensions

  Revision 1.36  2016/02/19 01:22:29  klrutkow
  updated author

  Revision 1.35  2016/02/18 23:54:26  klrutkow
  use input param for telescop when calling resolve function for at scattering file

  Revision 1.34  2015/09/30 14:02:32  klrutkow
  updated VERSION from 1.06 to 1.07

  Revision 1.33  2015/09/15 17:18:07  klrutkow
  added writeParametersToLog

  Revision 1.32  2015/09/10 02:39:14  klrutkow
  used CCNM=SURFACE instead of MIRROR for CALDB TDF search ; add check that telescop and instrume params match TDF SURFACE keywords ; write ATMSCTNG keyword to output ; log statements as each file is opened ; no longer go to SURFACE extension manually if user specified a different ext ; manually assign extensions if not provided ; removed check that atomic data file has CCNM0001=ATOMDATA

  Revision 1.31  2015/08/26 20:40:43  klrutkow
  fix mabscoef column numbering to include leading '0' again, when updating column comment ; only write DETNAM keyword if it's found in TDF, don't through an error if it's not found ; changed order of asking input params

  Revision 1.30  2015/08/25 03:48:43  klrutkow
  changed scattering extension search to use new param atmsctng instead of TDF keyword ; removed edge file param by hardcoding it to NONE for now

  Revision 1.29  2015/08/20 14:01:50  klrutkow
  added search for ATMSCTNG in TDF SURFACE ext, for atomic scattering CALDB search (with understanding it won't work until these files are updated in CALDB)

  Revision 1.28  2015/08/13 03:23:00  klrutkow
  shortened prologue (issue 534) ; change CALDB queries to use new resolve() function ; added check that mirror file has extension ; read DETNAM keyword from SURFACE extension, write it to output file ; edit TTYPEn (column name) keyword comments

  Revision 1.27  2015/06/30 00:40:34  klrutkow
  added correct include for HDgtcalf

  Revision 1.26  2015/06/29 15:29:51  klrutkow
  adding CALDB queries to getPar

  Revision 1.25  2015/03/23 15:23:50  klrutkow
  changed fits_read_col to fits_read_col_dbl

  Revision 1.24  2015/01/29 20:53:52  klrutkow
  updated params for issue 472

  Revision 1.23  2015/01/24 19:00:53  klrutkow
  updated tool with new parameters, per issue 472

  Revision 1.22  2014/11/17 18:07:07  klrutkow
  changed fits_open_file() to fits_open_table() for files which user needs to provide extension; hardcoded SURFACE extension for mirror file, removed ext from example param in .par file (TRF)

  Revision 1.21  2014/11/07 15:43:42  klrutkow
  change comments in output headers to have units in sq brackets right after slash, to  match cfitsio convention (http://heasarc.gsfc.nasa.gov/fitsio/c/c_user/node116.html); updated text for CBD10001 to match CALDB requirements; changed CCNM0001 from MASS ABSORPTION to MASS_ABSORPTION; removed CBD20001, CBD30001 keywords; moved keyword comments to COMMENT field in next line for ATOMFILE, etc so comments aren't cut off; updated TUNIT, DENSIT keyword units to reflect fits standard http://heasarc.gsfc.nasa.gov/docs/heasarc/ofwg/docs/general/ogip_93_001/ogip_93_001.html;  implement heaapp for startup() and shutdown()

  Revision 1.20  2014/10/06 14:10:08  klrutkow
  energy param accepts filename or list of energies; only write substrate (thick) keywords to front extension, not all materials; write all materials to the mass absorption extension; fix the mass absorption calculation in calcsf1sf2materials()

  Revision 1.19  2014/10/03 16:23:21  klrutkow
  param input filenames now include extension

  Revision 1.18  2014/09/04 16:50:07  klrutkow
  updates for v1.01 delivery

  Revision 1.17  2014/08/11 20:46:27  klrutkow
  updated default parameters

  Revision 1.16  2014/03/07 14:32:22  klrutkow
  added error checking for units in fits file

  Revision 1.15  2014/02/28 21:08:40  klrutkow
  updating many misc changes

  Revision 1.14  2014/02/25 18:16:10  klrutkow
  misc updates

  Revision 1.13  2014/02/20 20:51:06  klrutkow
  checking in recent updates for new B05 tag

  Revision 1.12  2014/02/07 21:38:31  klrutkow
  updated, added comments

  Revision 1.10  2014/02/05 18:44:16  klrutkow
  cleaned up, ready for build

  Revision 1.9  2014/02/04 13:44:22  klrutkow
  moved around transmission amplitudes, per updated trf

  Revision 1.8  2014/02/03 14:03:22  klrutkow
  added a comment

  Revision 1.7  2014/02/03 13:18:04  peachey
  Temporarily add internal implementation of startUp and shutDown functions
  pending complete integration of these into headas.

  Revision 1.6  2014/02/03 04:32:55  klrutkow
  added printout to track which energy we're at

  Revision 1.5  2014/01/31 15:56:58  klrutkow
  cleaned up code

  Revision 1.4  2014/01/30 16:29:55  klrutkow
  updated tool, single layer seems to work now
 

*/




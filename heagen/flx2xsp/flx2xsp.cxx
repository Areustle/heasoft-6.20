// A program to take an ASCII file of fluxes with errors on particular
// energy ranges and turn it into FITS spectral and response files for
// XSPEC.

// kaa 3/31/95 - 11/2/98
// kaa 10/11/06   modified to allow multiple input spectra which are
//                written to a typeII file
// kaa 4/2/12     C++ version. Added options for input units
// kaa 1/17/13    Fixed so input file columns can be separated by tabs
// kaa 9/11/13    Better checking of input data file. Allows discontinuous energies.

#include "pha.h"
#include "phaII.h"
#include "rmf.h"

extern "C" {
#include "pil.h"
#include "headas.h"
#include "headas_error.h"
}

// standard ftools magic to do set-up

#define TOOLSUB flx2xsp
#include "headas_main.c"

// Function prototypes

int flx2xsp(void);
int flx2xsp_getpar(string& infile, string& phafile, string& rspfile, int& nspec, string& xunit,
		   string& yunit, bool& clobber);
int flx2xsp_read(string infile, int nspec, vector<Real>& eLow, vector<Real>& eHigh,
		 vector< vector<Real> >& flux, vector< vector<Real> >& error);

// main routine

int flx2xsp(void)
{

  // Register taskname and version

  string taskname = "flx2xsp";
  string version = "2.1";

  set_toolname(taskname.c_str());
  set_toolversion(version.c_str());

  // Get input parameters

  string infile, phafile, rspfile, xunit, yunit;
  int nspec, status;
  bool clobber;

  status = flx2xsp_getpar(infile, phafile, rspfile, nspec, xunit, yunit, clobber); 
  if ( status ) return(status);

  // sanity testing. do this inside {} so that variables are deleted for tidiness.

  {
    if ( nspec <= 0 ) {
      string msg = "nspec parameter must be greater than zero.\n";
      headas_printf(msg.c_str());
      return(-1);
    }

    // Check that the units specified are valid

    if ( !isValidXUnits(xunit) ) {
      string msg = xunit+" is not a supported unit\n";
      headas_printf(msg.c_str());
      return(-2);
    }
    if ( !isValidYUnits(yunit) ) {
      string msg = yunit+" is not a supported unit\n";
      headas_printf(msg.c_str());
      return(-2);
    }

    // test whether the output files exist and if required and necessary delete them

    ifstream ifs(phafile.c_str());
    if (ifs) {
      if (clobber) {
	status = remove(phafile.c_str());
	if (status) {
	  string msg = "Failed to clobber "+phafile+".\n";
	  headas_printf(msg.c_str());
	  return(-3);
	}
      } else {
	string msg = phafile+" already exists. Either set clobber or choose another name.\n";
	headas_printf(msg.c_str());
	return(-3);
      }
      ifs.close();
    }

    ifstream ifs2(rspfile.c_str());
    if (ifs2) {
      if (clobber) {
	status = remove(rspfile.c_str());
	if (status) {
	  string msg = "Failed to clobber "+rspfile+".\n";
	  headas_printf(msg.c_str());
	  return(-4);
	}
      } else {
	string msg = rspfile+" already exists. Either set clobber or choose another name.\n";
	headas_printf(msg.c_str());
	return(-4);
      }
      ifs2.close();
    }
  }

  // Read the ASCII file

  vector<Real> eLow;
  vector<Real> eHigh;
  vector<vector<Real> > flux;
  vector<vector<Real> > error;

  status = flx2xsp_read(infile, nspec, eLow, eHigh, flux, error);
  if ( status ) return(status);

  // construct the pha object(s)

  pha SinglePha;
  phaII MultiplePha;

  for (size_t i=0; i<flux.size(); i++) {

    pha Temp;
    
    Temp.FirstChannel = 0;
    for (size_t j=0; j<flux[i].size(); j++) {
      Temp.Pha.push_back(flux[i][j]);
      Temp.StatError.push_back(error[i][j]);
      Temp.Channel.push_back(j);
    }

    Temp.SysError.push_back(0.0);
    Temp.Quality.push_back(0);
    Temp.Group.push_back(0);
    Temp.AreaScaling.push_back(1.0);
    Temp.BackScaling.push_back(1.0);

    Temp.Exposure = 1.0;
    Temp.CorrectionScaling = 1.0;
    Temp.DetChans = flux[i].size();
    Temp.Poisserr = false;
    Temp.Datatype = "RATE";
    Temp.PHAVersion = "1.2.1";
    Temp.Spectrumtype = "TOTAL";

    Temp.ResponseFile = rspfile;
    Temp.AncillaryFile = "NONE";
    Temp.BackgroundFile = "NONE";
    Temp.CorrectionFile = "NONE";

    Temp.FluxUnits = yunit;

    Temp.ChannelType = "PI";
    Temp.Telescope = "NONE";
    Temp.Instrument = "NONE";
    Temp.Detector = "NONE";
    Temp.Filter = "NONE";
    Temp.Datamode = "NONE";

    if ( flux.size() > 1 ) {
      MultiplePha.phas.push_back(Temp);
    } else {
      SinglePha = Temp;
    }

  }

  // Look for non-contiguous eLow and eHigh and fix up

  vector<Real> teLow, teHigh, rspval;
  teLow.push_back(eLow[0]);
  teHigh.push_back(eHigh[0]);
  rspval.push_back(1.0);
  for (size_t i=1; i<eLow.size(); i++) {
    if ( eHigh[i-1] != eLow[i] ) {
      teLow.push_back(eHigh[i-1]);
      teHigh.push_back(eLow[i]);
      rspval.push_back(0.0);
    }
    teLow.push_back(eLow[i]);
    teHigh.push_back(eHigh[i]);
    rspval.push_back(1.0);
  }

  // construct the rmf object

  rmf Diag;

  Diag.FirstChannel = 0;

  size_t ich = 0;
  for (size_t i=0; i<teLow.size(); i++) {
    Diag.LowEnergy.push_back(teLow[i]);
    Diag.HighEnergy.push_back(teHigh[i]);
    if ( rspval[i] > 0.0 ) {
      Diag.NumberGroups.push_back(1);
      Diag.FirstChannelGroup.push_back(ich);
      Diag.NumberChannelsGroup.push_back(1);
      Diag.Matrix.push_back(rspval[i]);
      ich++;
    } else {
      Diag.NumberGroups.push_back(0);
    }
  }
  for (size_t i=0; i<eLow.size(); i++) {
    Diag.ChannelLowEnergy.push_back(eLow[i]);
    Diag.ChannelHighEnergy.push_back(eHigh[i]);
  }

  Diag.update();

  Diag.AreaScaling = 1.0;
  Diag.ResponseThreshold = 1.0e-3;
  Diag.EnergyUnits = xunit;
  Diag.RMFUnits = " ";

  Diag.ChannelType = "PI";
  Diag.RMFVersion = "1.3.0";
  Diag.EBDVersion = "1.2.0";
  Diag.Telescope = "NONE";
  Diag.Instrument = "NONE";
  Diag.Detector = "NONE";
  Diag.Filter = "NONE";
  Diag.RMFType = "FULL";
  Diag.RMFExtensionName = "MATRIX";
  Diag.EBDExtensionName = "EBOUNDS";

  // convert into standard XSPEC units.

  if ( flux.size() > 1 ) {
    for (size_t i=0; i<flux.size(); i++) {
      status = MultiplePha.phas[i].convertUnits(Diag.ChannelLowEnergy, Diag.ChannelHighEnergy, Diag.EnergyUnits);
    }
  } else {
    status = SinglePha.convertUnits(Diag.ChannelLowEnergy, Diag.ChannelHighEnergy, Diag.EnergyUnits);
  }
  if ( status ) {
    headas_printf(SPgetErrorStack().c_str());
    return(-5);
  }

  status = Diag.convertUnits();
  if ( status ) {
    headas_printf(SPgetErrorStack().c_str());
    return(-5);
  }

  // write the output files

  if ( flux.size() > 1 ) {
    status = MultiplePha.write(phafile);
  } else {
    status = SinglePha.write(phafile);
  }

  status = Diag.write(rspfile);

  if ( status ) {
    headas_printf(SPgetErrorStack().c_str());
    return(-6);
  }

  return(0);

}

//************************************************************************************
// get the parameter values from the .par file

int flx2xsp_getpar(string& infile, string& phafile, string& rspfile, int& nspec, string& xunit,
		   string& yunit, bool& clobber)
{

  char *cinput = new char[PIL_LINESIZE];
  int *iinput = new int[1];
  string msg;

  int status = 0;

  if ((status = PILGetFname("infile", cinput))) {
    msg = "Error reading the 'infile' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    infile = string(cinput);
  }

  if ((status = PILGetFname("phafil", cinput))) {
    msg = "Error reading the 'phafil' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    phafile = string(cinput);
  }

  if ((status = PILGetFname("rspfil", cinput))) {
    msg = "Error reading the 'rspfil' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    rspfile = string(cinput);
  }

  if ((status = PILGetInt("nspec", iinput))) {
    msg = "Error reading the 'nspec' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    nspec = *iinput;
  }

  if ((status = PILGetString("xunit", cinput))) {
    msg = "Error reading the 'xunit' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    xunit = string(cinput);
  }

  if ((status = PILGetString("yunit", cinput))) {
    msg = "Error reading the 'yunit' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    yunit = string(cinput);
  }

  if ((status = PILGetBool("clobber", iinput))) {
    msg = "Error reading the 'clobber' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    clobber = false;
    if ( *iinput ) clobber = true;
  }

  return(status);

}

//************************************************************************************
// read the text file to get the data

int flx2xsp_read(string infile, int nspec, vector<Real>& eLow, vector<Real>& eHigh,
		 vector< vector<Real> >& flux, vector< vector<Real> >& error)
{

  int status = 0;

  // load the input file into a vector of strings

  vector<string> instring = SPreadStrings(infile);
  if ( instring.size() == 0 ) {
    string msg = "Failed to read data from " + infile + ".\n";
    status = -11;
    headas_printf(msg.c_str());
    return(status);
  }

  // do a check to make sure that the correct number of columns are in the file
  // allowed column delimiters are spaces and tabs.

  string delim = " 	";
  vector<string> word = SPtokenize(instring[0], delim);
  if ( word.size()-2 != 2*(size_t)nspec ) {
    stringstream msg;
    msg << "Number of columns in " + infile + " (" << word.size() 
	<< ") is not consistent with that expected from the nspec parameter (" << nspec*2+2 
	<< ").\n" << "Input string = "+instring[0]+".\n";
    headas_printf(msg.str().c_str());
    status = -12;
    return(status);
  }

  flux.resize(nspec);
  error.resize(nspec);

  // process all the lines read in

  for (size_t i=0; i<instring.size(); i++) {

    // split up this line

    word = SPtokenize(instring[i], delim);

    // if there are the wrong number of words then warn and return

    if ( word.size()-2 != 2*(size_t)nspec ) {
      stringstream msg;
      msg << "Line " << i+1 << " in " + infile + " has the wrong number of elements "
	  << "(read " << word.size() << " but expected " << 2*nspec+2 << ").\n";
      headas_printf(msg.str().c_str());
      status = -13;
      return(status);
    }

    // convert the energies into Reals

    vector<Real> rin(2);
    if ( SPstring2Real(word[0], rin[0]) != true ) {
      stringstream msg;
      msg << "The first element of line " << i+1 << " in " + infile + " is not a number.\n";
      headas_printf(msg.str().c_str());
      status = -14;
      return(status);
    }
    if ( SPstring2Real(word[1], rin[1]) != true ) {
      stringstream msg;
      msg << "The second element of line " << i+1 << " in " + infile + " is not a number.\n";
      headas_printf(msg.str().c_str());
      status = -14;
      return(status);
    }
    // make sure that eLow < eHigh
    if ( rin[0] < rin[1] ) {
      eLow.push_back(rin[0]);
      eHigh.push_back(rin[1]);
    } else {
      eLow.push_back(rin[1]);
      eHigh.push_back(rin[0]);
    }

    // now do the fluxes

    for (size_t j=1; j<(size_t)nspec+1; j++) {
      if ( SPstring2Real(word[j*2], rin[0]) != true ) {
	stringstream msg;
	msg << "The " << j*2+1 << " element of line " << i+1 << " in " + infile + " is not a number.\n";
	headas_printf(msg.str().c_str());
	status = -14;
	return(status);
      }
      if ( SPstring2Real(word[j*2+1], rin[1]) != true ) {
	stringstream msg;
	msg << "The " << j*2+2 << " element of line " << i+1 << " in " + infile + " is not a number.\n";
	headas_printf(msg.str().c_str());
	status = -14;
	return(status);
      }
      flux[j-1].push_back(rin[0]);
      error[j-1].push_back(rin[1]);
    }

  }

  // test that the inputs are in increasing order of energy and if necessary
  // reverse the order

  size_t nE = eLow.size();
  if ( eLow[nE-1] < eLow[0] ) {
    vector<Real> teLow(nE);
    vector<Real> teHigh(nE);
    for (size_t i=0; i<nE; i++) {
      teLow[i] = eLow[nE-i-1];
      teHigh[i] = eHigh[nE-i-1];
    }
    for (size_t i=0; i<nE; i++) {
      eLow[i] = teLow[i];
      eHigh[i] = teHigh[i];
    }
    size_t nflux = flux.size();
    for (size_t j=0; j<nflux; j++) {
      vector<Real> tflux(nE);
      vector<Real> terror(nE);
      for (size_t i=0; i<nE; i++) {
	tflux[i] = flux[j][nE-i-1];
	terror[i] = error[j][nE-i-1];
      }
      for (size_t i=0; i<nE; i++) {
	flux[j][i] = tflux[i];
	error[j][i] = terror[i];
      }
    }

  }

  // now test that the input file is not really messed up and if there are
  // still any cases of energies in the wrong order then throw an error

  for (size_t i=0; i<nE-1; i++) {
    if ( eLow[i+1] < eLow[i] ) {
      string msg = "Input energies from " + infile + " are not in a consistent order.\n";
      headas_printf(msg.c_str());
      status = -15;
      return(status);
    }
  }

  return(status);

}

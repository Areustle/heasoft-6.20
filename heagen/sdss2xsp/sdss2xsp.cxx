// A program to read SDSS spectral files from the DR8 spectral search site and turn 
// them into FITS spectral and response files for XSPEC.

// kaa 5/14/12

#include "pha.h"
#include "rmf.h"

extern "C" {
#include "pil.h"
#include "headas.h"
#include "headas_error.h"
}

// standard ftools magic to do set-up

#define TOOLSUB sdss2xsp
#include "headas_main.c"

// Function prototypes

int sdss2xsp(void);
int sdss2xsp_getpar(string& infile, string& phafile, string& rspfile, bool& clobber);
int sdss2xsp_read(string infile, vector<Real>& wLow, vector<Real>& wHigh,
		  vector<Real>& flux, vector<Real>& error, vector<Integer>& quality,
		  string& spectrometer);

// main routine

int sdss2xsp(void)
{

  // Register taskname and version

  string taskname = "sdss2xsp";
  string version = "1.00";

  set_toolname(taskname.c_str());
  set_toolversion(version.c_str());

  // Get input parameters

  string infile, phafile, rspfile;
  int status;
  bool clobber;

  status = sdss2xsp_getpar(infile, phafile, rspfile, clobber); 
  if ( status ) return(status);

  // sanity testing. do this inside {} so that variables are deleted for tidiness.

  {
    // Check that infile exists and we can open it

    ifstream ifs1(infile.c_str());
    if (!ifs1) {
      string msg = "Cannot open "+infile+".\n";
      headas_printf(msg.c_str());
      return(-1);
    } else {
      ifs1.close();
    }

    // test whether the output files exist and if required and necessary delete them
    ifstream ifs(phafile.c_str());
    if (ifs) {
      if (clobber) {
	status = remove(phafile.c_str());
	if (status) {
	  string msg = "Failed to clobber "+phafile+".\n";
	  headas_printf(msg.c_str());
	  return(-2);
	}
      } else {
	string msg = phafile+" already exists. Either set clobber or choose another name.\n";
	headas_printf(msg.c_str());
	return(-2);
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
	  return(-3);
	}
      } else {
	string msg = rspfile+" already exists. Either set clobber or choose another name.\n";
	headas_printf(msg.c_str());
	return(-3);
      }
      ifs2.close();
    }
  }

  // Read the SDSS file

  vector<Real> wLow;
  vector<Real> wHigh;
  vector<Real> flux;
  vector<Real> error;
  vector<Integer> quality;
  string spectrometer;

  status = sdss2xsp_read(infile, wLow, wHigh, flux, error, quality, spectrometer);
  if ( status ) return(status);

  // construct the pha object

  pha SinglePha;

  SinglePha.FirstChannel = 0;
  for (size_t j=0; j<flux.size(); j++) {
    SinglePha.Pha.push_back(flux[j]);
    SinglePha.StatError.push_back(error[j]);
    SinglePha.Channel.push_back(j);
    SinglePha.Quality.push_back(quality[j]);
  }

  SinglePha.SysError.push_back(0.0);
  SinglePha.Group.push_back(0);
  SinglePha.AreaScaling.push_back(1.0);
  SinglePha.BackScaling.push_back(1.0);

  SinglePha.Exposure = 1.0;
  SinglePha.CorrectionScaling = 1.0;
  SinglePha.DetChans = flux.size();
  SinglePha.Poisserr = false;
  SinglePha.Datatype = "RATE";
  SinglePha.PHAVersion = "1.2.1";
  SinglePha.Spectrumtype = "TOTAL";

  SinglePha.ResponseFile = rspfile;
  SinglePha.AncillaryFile = "NONE";
  SinglePha.BackgroundFile = "NONE";
  SinglePha.CorrectionFile = "NONE";

  SinglePha.FluxUnits = "ergs/cm^2/s/A";

  SinglePha.ChannelType = "PI";
  SinglePha.Telescope = "NONE";
  SinglePha.Instrument = "NONE";
  SinglePha.Detector = "NONE";
  SinglePha.Filter = "NONE";
  SinglePha.Datamode = "NONE";

  // construct the rmf object

  rmf Resp;

  // first set all the keywords

  Resp.FirstChannel = 0;
  Resp.AreaScaling = 1.0;
  Resp.ResponseThreshold = 1.0e-6;
  Resp.EnergyUnits = "angstrom";
  Resp.RMFUnits = " ";

  Resp.ChannelType = "PI";
  Resp.RMFVersion = "1.3.0";
  Resp.EBDVersion = "1.2.0";
  Resp.Telescope = "NONE";
  Resp.Instrument = "NONE";
  Resp.Detector = "NONE";
  Resp.Filter = "NONE";
  Resp.RMFType = "FULL";
  Resp.RMFExtensionName = "MATRIX";
  Resp.EBDExtensionName = "EBOUNDS";

  // now set up the response. if the spectrometer is SDSS use a resolution
  // linearly varying from 1850 at 3800A to 2200 at 9200A. if the spectrometer
  // is BOSS use resolution linearly varying from 1560 at 3700A to 2270 at
  // 6000A and from 1850 at 6000A to 2650 at 9000A.
  // loop over wavelengths

  vector<Real> ResponseVector(flux.size());

  for (size_t i=0; i<flux.size(); i++) {

    // sigma for this wavelength
    
    Real wave = (wLow[i]+wHigh[i])/2.0;
    Real resol;
    if ( spectrometer == "SDSS" ) {
      resol = 1850.0 + (wave-3800.0)*350.0/5400.0;
    } else if ( spectrometer == "BOSS" ) {
      if ( wave <= 6000.0 ) {
	resol = 1560.0 + (wave-3700.0)*710.0/2300.0;
      } else {
	resol = 1850.0 + (wave-6000.0)*800.0/3000.0;
      }
    }

    Real sigma = wave/resol;

    // Calculate response vector for this wavelength

    calcGaussResp(sigma, (wLow[i]+wHigh[i])/2.0, Resp.ResponseThreshold,
		  wLow, wHigh, ResponseVector);

    // Incorporate in the response

    Resp.addRow(ResponseVector, wLow[i], wHigh[i]);

  }

  // Set wavelength channel arrays (this is a square response)

  for (size_t i=0; i<flux.size(); i++) {
    Resp.ChannelLowEnergy.push_back(wLow[i]);
    Resp.ChannelHighEnergy.push_back(wHigh[i]);
  }

  // update the FirstGroup and FirstElement arrays

  Resp.update();

  // convert into standard XSPEC units

  status = SinglePha.convertUnits(Resp.ChannelLowEnergy, Resp.ChannelHighEnergy, Resp.EnergyUnits);
  if ( status ) {
    headas_printf(SPgetErrorStack().c_str());
    return(-4);
  }

  status = Resp.convertUnits();
  if ( status ) {
    headas_printf(SPgetErrorStack().c_str());
    return(-4);
  }

  // if necessary reverse the rows in the response matrix

  if ( Resp.LowEnergy[0] > Resp.LowEnergy[Resp.LowEnergy.size()-1] ) {
    Resp.reverseRows();
  }

  // write the output files

  status = SinglePha.write(phafile);
  if ( status ) {
    headas_printf(SPgetErrorStack().c_str());
    return(-5);
  }
  status = Resp.write(rspfile);
  if ( status ) {
    headas_printf(SPgetErrorStack().c_str());
    return(-5);
  }

  return(status);

}

//************************************************************************************
// get the parameter values from the .par file

int sdss2xsp_getpar(string& infile, string& phafile, string& rspfile, bool& clobber)
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

  if ((status = PILGetFname("phafile", cinput))) {
    msg = "Error reading the 'phafile' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    phafile = string(cinput);
  }

  if ((status = PILGetFname("rspfile", cinput))) {
    msg = "Error reading the 'rspfile' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    rspfile = string(cinput);
  }

  if ((status = PILGetBool("clobber", iinput))) {
    msg = "Error reading the 'clobber' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    clobber = false;
    if ( iinput ) clobber = true;
  }

  return(status);

}

//************************************************************************************
// read the SDSS FITS file to get the data

int sdss2xsp_read(string infile, vector<Real>& wLow, vector<Real>& wHigh,
		  vector<Real>& flux, vector<Real>& error, vector<Integer>& quality,
		  string& spectrometer) 
{

  int status = 0;

  // open the file at the first BINTABLE extension

  bool verbosity = FITS::verboseMode();
  FITS::setVerboseMode(true);

  const vector<string> hduKeys;
  const vector<string> primaryKey;

  auto_ptr<FITS> pInfile(0);

  try {
    pInfile.reset(new FITS(infile,Read,1,false,hduKeys,primaryKey));
  } catch(...) {
    return(NoSuchFile);
  }

  ExtHDU& spectrum = pInfile->extension(1);

  // Read the PLATEID keyword to find out whether the spectrometer is SDSS or BOSS
  // Note that the plate number is stored as a string hence need to extract it.

  string plateID, defString=" "; 
  plateID = SPreadKey(spectrum, "PLATEID", defString);
  spectrometer = "SDSS";
  if ( plateID != " " ) {
    Integer plateNum;
    stringstream temp;
    temp << plateID;
    temp >> plateNum;
    if ( plateNum >= 3510 ) spectrometer = "BOSS"; 
  }
  
  // Columns to be read are :
  //  flux                 Flux in units of 1e-17 ergs/cm^2/s/A
  //  wavelength           Wavelength in units of A
  //  or_mask              If non-zero then row is bad
  //  inverse_variance     1/sigma^2

  vector<Real> wave;

  SPreadCol(spectrum, "flux", flux);
  SPreadCol(spectrum, "wavelength", wave);
  SPreadCol(spectrum, "or_mask", quality);
  SPreadCol(spectrum, "inverse_variance", error);

  // multiply flux by 1e-17

  for (size_t i=0; i<flux.size(); i++) flux[i] *= 1.0e-17;

  // turn inverse_variance into standard error number in same
  // units as flux

  for (size_t i=0; i<error.size(); i++) {
    if ( error[i] > 0.0 ) error[i] = 1.0e-17/sqrt(error[i]);
  }

  // set quality array based on or_mask

  for (size_t i=0; i<quality.size(); i++) {
    if ( quality[i] != 0 ) quality[i] = 1;
  }

  // construct wLow and wHigh arrays from wavelength

  size_t N(wave.size());
  wLow.resize(N);
  wHigh.resize(N);

  wLow[0] = wave[0] - 0.5*(wave[1]-wave[0]);
  wHigh[0] = 0.5 * (wave[1]+wave[0]);
  for (size_t i=1; i<N-1; i++) {
    wLow[i] = 0.5 * (wave[i]+wave[i-1]);
    wHigh[i] = 0.5 * (wave[i+1]+wave[i]);
  }
  wLow[N-1] = 0.5 * (wave[N-1]+wave[N-2]);
  wHigh[N-1] = wave[N-1] + 0.5*(wave[N-1]-wave[N-2]);


  FITS::setVerboseMode(verbosity);
    
  return(status);

}

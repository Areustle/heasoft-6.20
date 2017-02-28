// A program to read VOtable spectral files and turn 
// them into FITS spectral and response files for XSPEC.

// kaa 5/30/12   v1.00 VOtable_read function is simplified version designed specifically
//                     for output from JHU VOspec site for SDSS spectra.

#include "pha.h"
#include "rmf.h"

extern "C" {
#include "pil.h"
#include "headas.h"
#include "headas_error.h"
}

// standard ftools magic to do set-up

#define TOOLSUB VOtable2xsp
#include "headas_main.c"

// Function prototypes

int VOtable2xsp(void);
int VOtable2xsp_getpar(string& infile, string& phafile, string& rspfile, bool& clobber, int& status);
int VOtable2xsp_read(string infile, vector<Real>& eLow, vector<Real>& eHigh,
		 vector<Real>& flux, vector<Real>& error, vector<Integer>& quality, int& status);

Real getxmldata(string& line);

// main routine

int VOtable2xsp(void)
{

  // Register taskname and version

  string taskname = "VOtable2xsp";
  string version = "1.00";

  set_toolname(taskname.c_str());
  set_toolversion(version.c_str());

  // Get input parameters

  string infile, phafile, rspfile;
  int status;
  bool clobber;

  VOtable2xsp_getpar(infile, phafile, rspfile, clobber, status); 
  if ( status ) return(status);

  // Read the VOTABLE file

  vector<Real> eLow;
  vector<Real> eHigh;
  vector<Real> flux;
  vector<Real> error;
  vector<Integer> quality;

  VOtable2xsp_read(infile, eLow, eHigh, flux, error, quality, status);
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

  // now set up the response assuming a resolution of 2000.
  // loop over wavelengths

  vector<Real> ResponseVector(flux.size());

  for (size_t i=0; i<flux.size(); i++) {

    // sigma for this wavelength

    Real sigma = (eLow[i]+eHigh[i])/2.0/2000.0;

    // Calculate response vector for this wavelength

    calcGaussResp(sigma, eLow[i], eHigh[i], Resp.ResponseThreshold, eLow, eHigh,
		  ResponseVector);

    // Incorporate in the response

    Resp.addRow(ResponseVector, eLow[i], eHigh[i]);

  }

  // Set wavelength channel arrays (this is a square response)

  for (size_t i=0; i<flux.size(); i++) {
    Resp.ChannelLowEnergy.push_back(eLow[i]);
    Resp.ChannelHighEnergy.push_back(eHigh[i]);
  }

  // update the FirstGroup and FirstElement arrays

  Resp.update();

  // convert into standard XSPEC units

  status = SinglePha.convertUnits(Resp.ChannelLowEnergy, Resp.ChannelHighEnergy, Resp.EnergyUnits);
  if ( status ) {
    cout << "Failed to convert units in pha: EnergyUnits = " << Resp.EnergyUnits << ", FluxUnits = " << SinglePha.FluxUnits << std::endl;
    return(status);
  }

  status = Resp.convertUnits();
  if ( status ) {
    cout << "Failed to convert units in rsp: EnergyUnits = " << Resp.EnergyUnits << ", RMFUnits = " << Resp.RMFUnits << std::endl;
    return(status);
  }

  // if necessary reverse the rows in the response matrix

  if ( Resp.LowEnergy[0] > Resp.LowEnergy[Resp.LowEnergy.size()-1] ) {
    Resp.reverseRows();
  }

  // write the output files

  SinglePha.write(phafile);
  Resp.write(rspfile);

  return(status);

}

//************************************************************************************
// get the parameter values from the .par file

int VOtable2xsp_getpar(string& infile, string& phafile, string& rspfile, bool& clobber, int& status)
{

  char *cinput = new char[PIL_LINESIZE];
  int *iinput = new int[1];
  string msg;

  status = 0;

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

  if ((status = PILGetBool("clobber", iinput))) {
    msg = "Error reading the 'clobber' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    clobber = true;
    if ( iinput ) clobber = false;
  }

  return(status);

}

//************************************************************************************
// read the VOTABLE XML file to get the data

int VOtable2xsp_read(string infile, vector<Real>& eLow, vector<Real>& eHigh,
		  vector<Real>& flux, vector<Real>& error, vector<Integer>& quality, 
                  int& status)
{

  status = 0;

  // simple-minded hardcoding for the moment. Eventually will require a library to read and
  // interpret VOtable XML.

  ifstream fstream(infile.c_str(), ios_base::in);
  if (!fstream) {
    string msg = "Failed to open " + infile;
    status = 1;
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  }

  // loop through lines till we find the string <TABLEDATA>

  string line;
  bool found(false);

  while (!found) {
    getline(fstream, line);
    if (line.find("TABLEDATA") != string::npos) found = true;
  }

  // now loop through data till we find the /TABLEDATA string

  getline(fstream, line);
  while ( line.find("/TABLEDATA") == string::npos ) {

    // current value of line is <TR>. The next 7 lines are the wavelength,
    // low wavelength, high wavelength, flux, flux low, flux high and quality

    getline(fstream, line);

    getline(fstream, line);
    eLow.push_back(getxmldata(line));
    getline(fstream, line);
    eHigh.push_back(getxmldata(line));

    getline(fstream, line);
    flux.push_back(1.0e-17*getxmldata(line));
    getline(fstream, line);
    error.push_back(1.0e-17*sqrt(getxmldata(line)));
    getline(fstream, line);

    getline(fstream, line);
    quality.push_back((Integer)getxmldata(line));
    if ( quality[quality.size()-1] != 0 ) quality[quality.size()-1] = 1;

    getline(fstream,line);
    getline(fstream,line);

  }
    
  return(status);

}

// Return the Real number from a line containing  <TD>nnnnnn</TD>.

Real getxmldata(string& line)
{
  size_t start = line.find(">") + 1;
  size_t length = line.find("<",start);

  return atof((line.substr(start,length)).c_str());
}

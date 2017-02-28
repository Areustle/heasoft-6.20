// A program to read an ascii file giving a filter response and make a 1 channel
// response matrix

// kaa 6/18/12

#include "rmf.h"

extern "C" {
#include "pil.h"
#include "headas.h"
#include "headas_error.h"
}

// standard ftools magic to do set-up

#define TOOLSUB mkfiltrsp
#include "headas_main.c"

// Function prototypes

int mkfiltrsp(void);
int mkfiltrsp_getpar(string& infile, string& rspfile, string& xunit, bool& clobber);
int mkfiltrsp_read(string infile, vector<Real>& eLow, vector<Real>& eHigh,
		   vector<Real>& area);

// main routine

int mkfiltrsp(void)
{

  // Register taskname and version

  string taskname = "mkfiltrsp";
  string version = "1.00";

  set_toolname(taskname.c_str());
  set_toolversion(version.c_str());

  // Get input parameters

  string infile, rspfile;
  string xunit;
  int status;
  bool clobber;

  status = mkfiltrsp_getpar(infile, rspfile, xunit, clobber); 
  if ( status ) return(status);

  // sanity testing. do this inside {} so that variables are deleted for tidiness

  {

    // check for existence of input file

    ifstream ifs1(infile.c_str(), ios_base::in);
    if (!ifs1) {
      string msg = "Cannot open " + infile + "\n";
      headas_printf(msg.c_str());
      return(-1);
    } else {
      ifs1.close();
    }

    // Check that the units specified are valid

    if ( !isValidXUnits(xunit) ) {
      string msg = xunit+" is not a supported unit\n";
      headas_printf(msg.c_str());
      return(-2);
    }
    
    // test whether the output file exists and if required and necessary delete it

    ifstream ifs2(rspfile.c_str());
    if ( ifs2 ) {
      if ( clobber ) {
	status = remove(rspfile.c_str());
	if ( status ) {
	  string msg = "Failed to clobber "+rspfile+".\n";
	  headas_printf(msg.c_str());
	  return(-3);
	}
      } else {
	string msg = rspfile+" alread exists. Either set clobber or choose another name.\n";
	headas_printf(msg.c_str());
	return(-3);
      }
      ifs2.close();
    }

  }

  // Read the filter area file

  vector<Real> eLow;
  vector<Real> eHigh;
  vector<Real> area;

  status = mkfiltrsp_read(infile, eLow, eHigh, area);
  if ( status ) return(status);

  // construct the rmf object

  rmf Resp;

  // first set all the keywords

  Resp.FirstChannel = 0;
  Resp.AreaScaling = 1.0;
  Resp.ResponseThreshold = 1.0e-6;
  Resp.EnergyUnits = xunit;
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

  // now set up the response. this is simple since there is only one channel
  // whose response is given by the area vector

  for (size_t i=0; i<area.size(); i++) {
    Resp.NumberGroups.push_back(1);
    Resp.FirstGroup.push_back(1);
    Resp.FirstChannelGroup.push_back(0);
    Resp.NumberChannelsGroup.push_back(1);
    Resp.FirstElement.push_back(0);
    Resp.LowEnergy.push_back(eLow[i]);
    Resp.HighEnergy.push_back(eHigh[i]);
    Resp.Matrix.push_back(area[i]);
  }

  // Set wavelength channel array - just use entire energy range for the moment

  Resp.ChannelLowEnergy.push_back(eLow[0]);
  Resp.ChannelHighEnergy.push_back(eHigh[eHigh.size()-1]);

  // update the FirstGroup and FirstElement arrays

  Resp.update();

  // convert into standard XSPEC units

  status = Resp.convertUnits();
  if ( status ) {
    string msg = "Failed to convert units in rsp: EnergyUnits = "+Resp.EnergyUnits+", RMFUnits = "+Resp.RMFUnits+"\n";
    headas_printf(msg.c_str());
    return(-4);
  }

  // if necessary reverse the rows in the response matrix

  if ( Resp.LowEnergy[0] > Resp.LowEnergy[Resp.LowEnergy.size()-1] ) {
    Resp.reverseRows();
  }

  // write the output file

  status = Resp.write(rspfile);
  if ( status ) {
    headas_printf(SPgetErrorStack().c_str());
    return(-5);
  }

  return(0);

}

//************************************************************************************
// get the parameter values from the .par file

int mkfiltrsp_getpar(string& infile, string& rspfile, string& xunit, bool& clobber)
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

  if ((status = PILGetFname("rspfile", cinput))) {
    msg = "Error reading the 'rspfile' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    rspfile = string(cinput);
  }

  if ((status = PILGetFname("xunit", cinput))) {
    msg = "Error reading the 'xunit' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    xunit = string(cinput);
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
// read the text file with the filter areas

int mkfiltrsp_read(string infile, vector<Real>& eLow, vector<Real>& eHigh,
		  vector<Real>& area)
{

  // this open should not generate an error because we have already
  // checked for the existence of infile.

  ifstream fstream(infile.c_str(), ios_base::in);
  if (!fstream) {
    string msg = "Failed to open " + infile + "\n";
    headas_printf(msg.c_str());
    return(-1);
  }

  // loop round lines in file processing them

  vector<Real> in(3);
  string instring;
  getline(fstream, instring);

  while (!fstream.eof()) {
    stringstream instream;
    instream << instring;
    instream >> in[0] >> in[1] >> in[2];
    eLow.push_back(in[0]);
    eHigh.push_back(in[1]);
    area.push_back(in[2]);

    getline(fstream, instring);
  }

  fstream.close();
  return(0);

}

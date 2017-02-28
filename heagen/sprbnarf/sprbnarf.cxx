// SPRBNARF rebins an ARF

// kaa 11/25/12   conversion from C to C++

#include "arf.h"

extern "C" {
#include "pil.h"
#include "headas.h"
#include "headas_error.h"
}

// standard ftools magic to do set-up

#define TOOLSUB sprbnarf
#include "headas_main.c"

// Function Prototypes
int sprbnarf (void);
int sprbnarf_getpar (string& infile, string& binfile, int& binfact, string& outfile,
		   int& chatter, bool& clobber, bool& history);

// main routine

int sprbnarf (void)
{

  // Register taskname and version

  string taskname = "sprbnarf";
  string version = "1.10";

  // Register taskname and version.

  set_toolname(taskname.c_str());
  set_toolversion(version.c_str());


  //  get input parameters

  string infile, binfile, outfile;
  int binfact, chatter, status;
  bool clobber, history;

  status = sprbnarf_getpar (infile, binfile, binfact, outfile, chatter, 
			    clobber, history);
  if ( status ) return(status);

  // read the input file into an ARF object

  arf inARF;
  status = inARF.read(infile);
  if ( status ) {
    headas_printf(SPgetErrorStack().c_str());
    return(status);
  }

  // need to know the number of input energies and for binning instruction
  // purposes we count energy bins from 1.

  Integer nBinIn = inARF.NumberEnergyBins();
  Integer fBin = 1;

  // initialize object to hold the binning information

  grouping binInfo;

  if ( binfile.size() > 0 && binfile != "none" && binfile != "NONE" ) {

    status = binInfo.read(binfile, nBinIn, fBin);
    if ( status ) {
      headas_printf(SPgetErrorStack().c_str());
      return(-1);
    }

  } else {

    binInfo.load(binfact, nBinIn);

  }

  // rebin the ARF

  status = inARF.rebin(binInfo);
  if ( status ) {
    headas_printf(SPgetErrorStack().c_str());
    return(-2);
  }

  // test whether the output file exists and if required and necessary delete it

  ifstream ifs(outfile.c_str());
  if (ifs) {
    if (clobber) {
      status = remove(outfile.c_str());
      if (status) {
	string msg = "Failed to clobber "+outfile+".\n";
	headas_printf(msg.c_str());
	return(-3);
      }
    } else {
      string msg = outfile+" already exists. Either set clobber or choose another name.\n";
      headas_printf(msg.c_str());
      return(-3);
    }
    ifs.close();
  }
  
  // and write it out including any extra keywords and extensions in the input
  // file

  status = inARF.write(outfile, infile);
  if ( status ) {
    headas_printf(SPgetErrorStack().c_str());
    return(status);
  }

  return(status);
}

//************************************************************************************
// get the parameter values from the .par file

int sprbnarf_getpar(string& infile, string& binfile, int& binfact, 
		    string& outfile, int& chatter, bool& clobber, bool& history)
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

  if ((status = PILGetFname("binfile", cinput))) {
    msg = "Error reading the 'binfile' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    binfile = string(cinput);
  }

  if ((status = PILGetInt("binfact", iinput))) {
    msg = "Error reading the 'binfact' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    binfact = *iinput;
  }

  if ((status = PILGetFname("outfile", cinput))) {
    msg = "Error reading the 'outfile' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    outfile = string(cinput);
  }

  if ((status = PILGetInt("chatter", iinput))) {
    msg = "Error reading the 'chatter' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    chatter = *iinput;
  }

  if ((status = PILGetBool("clobber", iinput))) {
    msg = "Error reading the 'clobber' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    clobber = false;
    if ( iinput ) clobber = true;
  }

  if ((status = PILGetBool("history", iinput))) {
    msg = "Error reading the 'history' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    clobber = false;
    if ( iinput ) clobber = true;
  }

  return(status);

}


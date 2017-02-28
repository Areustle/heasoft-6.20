// A program to shift an RMF in either channel or energy space.

// kaa 1/15/13 Converted and expanded from ftool

#include "rmf.h"

extern "C" {
#include "pil.h"
#include "headas.h"
#include "headas_error.h"
}

// standard ftools magic to do set-up

#define TOOLSUB gcorrmf
#include "headas_main.c"

// Function prototypes

int gcorrmf(void);
int gcorrmf_getpar(string& infile, string& ebdfile, string& shftfile,
		   string& eshftfile, string& outfile, int& chatter, bool& clobber);
int gcorrmf_readshiftfile(string& filename, vector<int>& start, vector<int>& end, vector<Real>& factor, vector<Real>& shift, bool& useEnergy);


// main routine

int gcorrmf(void)
{

  // Register taskname and version

  string taskname = "gcorrmf";
  string version = "2.00";

  set_toolname(taskname.c_str());
  set_toolversion(version.c_str());

  // Get input parameters

  string infile, ebdfile, shftfile, eshftfile, outfile;
  int chatter, status;
  bool clobber;

  status = gcorrmf_getpar(infile, ebdfile, shftfile, eshftfile, outfile, 
			  chatter, clobber); 
  if ( status ) return(status);

  // sanity testing. do this inside {} so that variables are deleted for tidiness.

  {

    // test whether the output file exists and if required and necessary delete them

    ifstream ifs(outfile.c_str());
    if (ifs) {
      if (clobber) {
	status = remove(outfile.c_str());
	if (status) {
	  string msg = "Failed to clobber "+outfile+".\n";
	  headas_printf(msg.c_str());
	  return(-1);
	}
      } else {
	string msg = outfile+" already exists. Either set clobber or choose another name.\n";
	headas_printf(msg.c_str());
	return(-1);
      }
      ifs.close();
    }

  }

  // Read the shift files

  vector<int> Start, End, eStart, eEnd;
  vector<Real> Factor, eFactor, Shift, eShift;
  bool useEnergy, junk;

  status = gcorrmf_readshiftfile(shftfile, Start, End, Factor, Shift, useEnergy);
  if ( status ) return(status);
  status = gcorrmf_readshiftfile(eshftfile, eStart, eEnd, eFactor, eShift, junk);
  if ( status ) return(status);

  // Load the rmf object

  rmf inRMF;

  status = inRMF.readMatrix(infile);
  if ( status ) {
    headas_printf(SPgetErrorStack().c_str());
    return(-2);
  }

  if ( ebdfile == "%" ) {
    status = inRMF.readChannelBounds(infile);
  } else {
    status = inRMF.readChannelBounds(ebdfile);
  }
  if ( status ) {
    headas_printf(SPgetErrorStack().c_str());
    return(-3);
  }

  // shift the rmf - have to do the channel and energy shifting separately

  status = inRMF.shiftChannels(Start, End, Shift, Factor, useEnergy);
  if ( status ) {
    headas_printf(SPgetErrorStack().c_str());
    return(-4);
  }

  status = inRMF.shiftEnergies(eStart, eEnd, eShift, eFactor);
  if ( status ) {
    headas_printf(SPgetErrorStack().c_str());
    return(-5);
  }

  // write the output file

  status = inRMF.write(outfile, infile);

  if ( status ) {
    headas_printf(SPgetErrorStack().c_str());
    return(-6);
  }

  return(0);

}

//************************************************************************************
// get the parameter values from the .par file

int gcorrmf_getpar(string& infile, string& ebdfile, string& shftfile, string& eshftfile, string& outfile, int& chatter, bool& clobber) 
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

  if ((status = PILGetFname("ebdfile", cinput))) {
    msg = "Error reading the 'ebdfile' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    ebdfile = string(cinput);
  }

  if ((status = PILGetFname("shftfile", cinput))) {
    msg = "Error reading the 'shftfile' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    shftfile = string(cinput);
  }

  if ((status = PILGetFname("eshftfile", cinput))) {
    msg = "Error reading the 'eshftfile' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    eshftfile = string(cinput);
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
    if ( *iinput ) clobber = true;
  }

  return(status);

}

//************************************************************************************
// read the text file to get the shift instructions

int gcorrmf_readshiftfile(string& filename, vector<int>& start, vector<int>& end, vector<Real>& factor, vector<Real>& shift, bool& useEnergy)
{

  int status = 0;
  useEnergy = false;

  if (filename.length() == 0) return(0);
  if (filename.find_first_not_of(" 	") == string::npos) return(0);
  if (filename == "none" || filename == "NONE") return(0);

  // read the file

  vector<string> inputLines = SPreadStrings(filename);
  if ( inputLines.size() == 0 ) {
    string msg = "Failed to read any lines from file " + filename + ".\n";
    headas_printf(msg.c_str());
    return(-11);
  }

  // check the first line for an energy specification

  bool isWave = false;
  Real xFactor = 1.0;
  if ( inputLines[0].substr(0,1) == "!" ) {
    vector<string> tokens = SPtokenize(inputLines[0], " ");
    if ( tokens.size() > 1 ) {
      useEnergy = true;
      status = calcXfactor(tokens[1], isWave, xFactor);
      if ( status != 0 ) {
	string msg = "Failed to understand energy unit "+tokens[1]+
	  " from filename "+filename+".\n";
	headas_printf(msg.c_str());
	return(-11);
      }
    }
    inputLines.erase(inputLines.begin());
  }

  // loop over the lines

  for (size_t i=0; i<inputLines.size(); i++) {

    if ( inputLines[i].size() > 0 ) {

      // split into invidual tokens

      vector<string> tokens = SPtokenize(inputLines[i], " ");

      // first two should be integers

      vector<Integer> in(2);
      if ( !SPstring2Integer(tokens[0], in[0]) || !SPstring2Integer(tokens[1], in[1]) ) {
	string msg = "Failed to read integer from " + tokens[0] + " or " + tokens[1] +
	  " in file " + filename + ".\n";
	headas_printf(msg.c_str());
	return(-11);
      }

      start.push_back(in[0]);
      end.push_back(in[1]);

      // the next token is the shift which should be a real

      Real rin;
      if ( !SPstring2Real(tokens[2], rin) ) {
	string msg = "Failed to read real from " + tokens[2] + " in file " + filename + ".\n";
	headas_printf(msg.c_str());
	return(-11);
      }
      shift.push_back(rin);

      // if there is another token it will be the factor which should be a real

      if ( tokens.size() == 4 ) {
	if ( !SPstring2Real(tokens[3], rin) ) {
	  string msg = "Failed to read real from " + tokens[3] + " in file " + filename + ".\n";
	  headas_printf(msg.c_str());
	  return(-11);
	}
	factor.push_back(rin);
      } else {
	factor.push_back(1.0);
      }

    }
  }

  // if the shifts were in energies and not in units of keV then convert them
  // not doing anything with the isWave information because it does not make
  // sense here
  if ( useEnergy && xFactor != 1.0 ) {
    for (size_t i=0; i<shift.size(); i++) shift[i] *= xFactor;
  }

  return(status);

}

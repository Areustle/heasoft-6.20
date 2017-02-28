// A program to smooth the RGS rmf for extended sources based on an image 
// (probably from Chandra).
//
//  Parameters are
//    rmffil           Input rmf
//    imgfil           File with Image data
//    order            The grating order being smoothed
//    outfil           Output rmf
//    chatter          Chatter level
//    clobber          If true overwrite output file if it already exists

#include "rmf.h"
extern "C" {
#include "pil.h"
#include "headas.h"
#include "headas_error.h"
#include "rgsxsrc.h"
}

// standard ftools magic to do set-up

#define TOOLSUB rgsrmfsmooth
#include "headas_main.c"

// Function prototypes

int rgsrmfsmooth(void);
int rgsrmfsmooth_getpar(string& rmffil, string& imgfil, int& order, string& outfil, int& chatter, bool& clobber);

// main routine

int rgsrmfsmooth(void)
{
  string msg;

  // Register taskname and version

  string taskname = "rgsrmfsmooth";
  string version = "2.00";

  set_toolname(taskname.c_str());
  set_toolversion(version.c_str());

  // Get input parameters

  string rmffil, imgfil, outfil;
  int order, chatter, status=0;
  bool clobber;

  status = rgsrmfsmooth_getpar(rmffil, imgfil, order, outfil, chatter, clobber);
  if ( status ) return(status);

  // Do some sanity checks on the input parameters. Enclose all this in {} so
  // we clean up after the tests

  {
    // Check that imgfil exists and we can open it

    ifstream ifs(imgfil.c_str());
    if (!ifs) {
      msg = "Cannot open "+imgfil+"\n";
      headas_printf(msg.c_str());
      return(-1);
    } else {
      ifs.close();
    }

    // Find out whether the output file exists. If it does and clobber is set
    // then delete it, otherwise throw an error.

    rmf testRMF;
    if (!testRMF.read(outfil)) {
      if (clobber) {
	status = remove(outfil.c_str());
	if (status) {
	  msg = "Failed to clobber "+outfil+"\n";
	  headas_printf(msg.c_str());
	  return(-2);
	}
      } else {
	msg = outfil+" already exists. Either set clobber or choose another name.\n";
	headas_printf(msg.c_str());
	return(-2);
      }
    }

    // end of sanity testing
  }

  // Load the input RMF

  rmf inputRMF;
  status = inputRMF.read(rmffil);
  if ( status ) {
    headas_printf(SPgetErrorStack().c_str());
    return(-3);
  }

  // copy into the output RMF

  rmf outputRMF(inputRMF);

  // zero out the response matrix elements in the output RMF since we are going
  // to rebuild them

  outputRMF.clearMatrix();

  // Calculate the new smoothed response
  // First set up xspec energy array - note that the channels are in order of
  // decreasing energy in the XMM RGS. The rgsxsrc C function will require this
  // as a float*, not a vector. Should really use an auto_ptr here but being lazy.

  size_t Nchan = (size_t)inputRMF.NumberChannels();
  float *ear = new float[Nchan+1];
  for (size_t i=0; i<Nchan; i++) {
    ear[i] = inputRMF.ChannelLowEnergy[Nchan-1-i];
  }
  ear[Nchan] = inputRMF.ChannelHighEnergy[0];

  // will also need a float* to pass in the response row

  float *photar = new float[Nchan];

  // and define param array

  float param[1];
  param[0] = (float)order;

  // set up char* to pass the filename which rgsxsrc requires

  char *parfile = new char[imgfil.size()+1];
  strcpy(parfile, imgfil.c_str());

  // Now loop over response energies

  for (size_t ie=0; ie<(size_t)inputRMF.NumberEnergyBins(); ie++) {

    // Create "spectrum" from the response for this energy

    vector<Real> RespRow(Nchan);
    RespRow = inputRMF.RowValues(ie);

    // call the rgsxsrc model to convolve the spectrum. first need to copy
    // RespRow vector into a float*. As with the energies the order needs to 
    // be inverted.

    for (size_t i=0; i<Nchan; i++) {
      photar[i] = (float)RespRow[Nchan-i-1];
    }

    rgsxsrc(ear, (int)Nchan, param, (int)0, photar, parfile);
    
    // copy photar back into RespRow

    for (size_t i=0; i<Nchan; i++) {
      RespRow[i] = (Real)photar[Nchan-i-1];
    }
    
    // add the convolved array as a row in the output response

    outputRMF.addRow(RespRow, inputRMF.LowEnergy[ie], inputRMF.HighEnergy[ie]);

    // end loop over response energies

  }

  // tidy up the arrays

  delete ear;
  delete photar;

  // update the output response and write out copying any extra keywords or
  // extensions from the input file

  outputRMF.update();
  status = outputRMF.write(outfil, rmffil);
  if (status) {
    headas_printf(SPgetErrorStack().c_str());
    return(-4);
  }

  return(0);

}

//************************************************************************************
// get the parameter values from the .par file

int rgsrmfsmooth_getpar(string& rmffil, string& imgfil, int& order, string& outfil, int& chatter, bool& clobber)
{

  char *cinput = new char[PIL_LINESIZE];
  int *iinput = new int[1];
  string msg;

  int status = 0;

  if ((status = PILGetFname("rmffil", cinput))) {
    msg = "Error reading the 'rmffil' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    rmffil = string(cinput);
  }

  if ((status = PILGetFname("imgfil", cinput))) {
    msg = "Error reading the 'imgfil' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    imgfil = string(cinput);
  }

  if ((status = PILGetInt("order", iinput))) {
    msg = "Error reading the 'order' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    order = *iinput;
  }

  if ((status = PILGetFname("outfil", cinput))) {
    msg = "Error reading the 'outfil' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    outfil = string(cinput);
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
    if ( *iinput == 0 ) {
      clobber = false;
    } else if ( *iinput == 1 ) {
      clobber = true;
    }
  }

  return(status);

}

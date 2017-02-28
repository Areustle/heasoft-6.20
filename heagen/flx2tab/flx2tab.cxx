// A program to take an ASCII file of fluxes with on particular
// energy ranges and turn it into and XSPEC table model file.
// If only one column of fluxes is input then the table model has no
// parameters. If multiple columns of fluxes are present then create a 1-D
// grid of table models and read parameter values from another file.

// kaa 10/11/12  Initial version after an original program by Randall Smith
// kaa  9/11/13  Better checking of input files

#include "SPutils.h"
#include "table.h"

extern "C" {
#include "pil.h"
#include "headas.h"
#include "headas_error.h"
}

// standard ftools magic to do set-up

#define TOOLSUB flx2tab
#include "headas_main.c"

// Function prototypes

int flx2tab(void);
int flx2tab_getpar(string& infile, string& modelname, string& tabfile, int& nspec, 
		   string& parinfofile, bool& additive, bool& redshift, 
		   string& xunit, string& yunit, bool& clobber);
int flx2tab_read(string infile, int nspec, vector<Real>& eLow, vector<Real>& eHigh,
		 vector< vector<Real> >& flux);
int flx2tab_readParInfo(string parinfofile, int nspec, string& parName, int& interp, 
                        vector<Real>& parDesc, vector<Real>& parVals);

// main routine

int flx2tab(void)
{

  // Register taskname and version

  string taskname = "flx2tab";
  string version = "1.01";

  set_toolname(taskname.c_str());
  set_toolversion(version.c_str());

  string msg;

  // Get input parameters

  string infile, modelname, tabfile, parinfofile, xunit, yunit;
  int nspec, status;
  bool additive, redshift, clobber;

  status = flx2tab_getpar(infile, modelname, tabfile, nspec, parinfofile, additive, redshift, 
		 xunit, yunit, clobber);
  if ( status ) return(status);

  // Do some sanity checks on the input parameters. Enclose all this in {} so
  // we clean up after the tests

  {
    // Check that infile exists and we can open it

    ifstream ifs1(infile.c_str());
    if (!ifs1) {
      msg = "Cannot open "+infile+".\n";
      headas_printf(msg.c_str());
      return(-1);
    } else {
      ifs1.close();
    }

    // If nspec > 1 check that parinfofile exists and we can open it

    if ( nspec > 1 ) {
      ifstream ifs2(parinfofile.c_str());
      if (!ifs2) {
	msg = "Cannot open "+parinfofile+" and nspec > 1.\n";
	headas_printf(msg.c_str());
	return(-2);
      } else {
	ifs2.close();
      }
    }

    // Check that the units specified are valid

    if ( !isValidXUnits(xunit) ) {
      msg = xunit+" is not a supported unit\n";
      headas_printf(msg.c_str());
      return(-3);
    }
    if ( !isValidYUnits(yunit) ) {
      msg = yunit+" is not a supported unit\n";
      headas_printf(msg.c_str());
      return(-3);
    }

    // Find out whether the output file exists. If it does and clobber is set
    // then delete it, otherwise throw an error.

    ifstream ifs3(tabfile.c_str());
    if (ifs3) {
      if (clobber) {
	status = remove(tabfile.c_str());
	if (status) {
	  msg = "Failed to clobber "+tabfile+".\n";
	  headas_printf(msg.c_str());
	  return(-4);
	}
      } else {
	msg = tabfile+" already exists. Either set clobber or choose another name.\n";
	headas_printf(msg.c_str());
	return(-4);
      }
      ifs3.close();
    }

    // end of sanity testing
  }


  // Read the text file with the fluxes

  vector<Real> eLow;
  vector<Real> eHigh;
  vector<vector<Real> > flux;

  status = flx2tab_read(infile, nspec, eLow, eHigh, flux);
  if ( status ) return(status);

  // check that input energy bins are contiguous

  bool isContiguous(true);
  for (size_t i=1; i<eLow.size(); i++) {
    if ( eHigh[i-1] != eLow[i] ) isContiguous = false;
  }
  if ( !isContiguous ) {
    msg = "Input energy (or wavelength) bins are not contiguous.\n";
    headas_printf(msg.c_str());
    return(-5);
  }

  // set up the table object

  table outputTable;

  // construct the table parameter object

  tableParameter tabPar;
  if ( nspec > 1 ) {
    vector<Real> parDesc(6);
    status = flx2tab_readParInfo(parinfofile, nspec, tabPar.Name, tabPar.InterpolationMethod,
			parDesc, tabPar.TabulatedValues);
    if ( status ) return(status);
    tabPar.InitialValue = parDesc[0];
    tabPar.Delta = parDesc[1];
    tabPar.Minimum = parDesc[2];
    tabPar.Bottom = parDesc[3];
    tabPar.Top = parDesc[4];
    tabPar.Maximum = parDesc[5];
  } else {
    tabPar.Name = "Dummy";
    tabPar.InterpolationMethod = 0;
    tabPar.InitialValue = 1.0;
    tabPar.Delta = -1.0;
    tabPar.Minimum = 1.0;
    tabPar.Bottom = 1.0;
    tabPar.Top = 1.0;
    tabPar.Maximum = 1.0;
    tabPar.TabulatedValues.push_back(1.0);
  }

  // add to the output table

  outputTable.Parameters.push_back(tabPar);

  // set up the table spectrum object(s) and add to the output table

  for (size_t i=0; i<(size_t)nspec; i++) {
    tableSpectrum tabSpec;
    tabSpec.Flux.resize(flux[i].size());
    for (size_t j=0; j<flux[i].size(); j++) tabSpec.Flux[j] = flux[i][j];
    tabSpec.ParameterValues.push_back(tabPar.TabulatedValues[i]);
    outputTable.Spectra.push_back(tabSpec);
  }

  // set top-level table descriptors

  outputTable.ModelName = modelname;
  if ( additive ) {
    outputTable.ModelUnits = yunit;
  } else {
    outputTable.ModelUnits = " ";
  }
  outputTable.NumIntParams = 1;
  outputTable.NumAddParams = 0;
  outputTable.isError = false;
  outputTable.isRedshift = redshift;
  outputTable.isAdditive = additive;

  outputTable.Energies.resize(eLow.size()+1);
  for (size_t i=0; i<eLow.size(); i++) outputTable.Energies[i] = eLow[i];
  outputTable.Energies[eLow.size()] = eHigh[eHigh.size()-1];

  outputTable.EnergyUnits = xunit;

  // if any of the table units are non-standard then convert

  status = outputTable.convertUnits();
  if ( status != OK ) {
    headas_printf(SPgetErrorStack().c_str());
    return(status);
  } 

  // if necessary reverse the rows so that energy is increasing

  if ( outputTable.Energies[0] > outputTable.Energies[outputTable.Energies.size()-1] ) {
    outputTable.reverseRows();
  }

  // check for internal consistency of the table

  msg = outputTable.check();
  if ( msg.size() > 0 ) {
    headas_printf(msg.c_str());
    return(-6);
  }

  // write the output file

  status = outputTable.write(tabfile);
  if ( status != OK ) headas_printf(SPgetErrorStack().c_str());

  return(status);

}

//************************************************************************************
// get the parameter values from the .par file

int flx2tab_getpar(string& infile, string& modelname, string& tabfile, int& nspec, 
		   string& parinfofile, bool& additive, bool& redshift,
                   string& xunit, string& yunit, bool& clobber)
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

  if ((status = PILGetString("modelname", cinput))) {
    msg = "Error reading the 'modelname' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    modelname = string(cinput);
  }

  if ((status = PILGetFname("tabfile", cinput))) {
    msg = "Error reading the 'tabfile' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    tabfile = string(cinput);
  }

  if ((status = PILGetInt("nspec", iinput))) {
    msg = "Error reading the 'nspec' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    nspec = *iinput;
  }

  if ( nspec > 1 ) {
    if ((status = PILGetFname("parinfofile", cinput))) {
      msg = "Error reading the 'parinfofile' parameter.";
      HD_ERROR_THROW(msg.c_str(), status);
      return(status);
    } else {
      parinfofile = string(cinput);
    }
  }

  if ((status = PILGetBool("additive", iinput))) {
    msg = "Error reading the 'additive' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    additive = false;
    if ( *iinput ) additive = true;
  }

  if ((status = PILGetBool("redshift", iinput))) {
    msg = "Error reading the 'redshift' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    redshift = false;
    if ( *iinput ) redshift = true;
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

int flx2tab_read(string infile, int nspec, vector<Real>& eLow, vector<Real>& eHigh,
		 vector< vector<Real> >& flux)
{

  int status = 0;

  vector<string> instring = SPreadStrings(infile);
  if ( instring.size() == 0 ) {
    string msg = "Failed to read data from " + infile + ".\n";
    status = 1;
    headas_printf(msg.c_str());
    return(status);
  }

  // do a check to make sure that the correct number of columns are in the file
  // allowed column delimiters are spaces and tabs.

  string delim = " 	";
  vector<string> word = SPtokenize(instring[0], delim);
  if ( word.size()-2 != (size_t)nspec ) {
    stringstream msg;
    msg << "Number of columns in " + infile + " (" << word.size() 
	<< ") is not consistent with that expected from the nspec parameter (" << nspec+2 
	<< ").\n" << "Input string = "+instring[0]+".\n";
    headas_printf(msg.str().c_str());
    status = 2;
    return(status);
  }

  flux.resize(nspec);

  // process all the lines read in

  for (size_t i=0; i<instring.size(); i++) {

    // split up this line

    word = SPtokenize(instring[i], delim);

    // if there are the wrong number of words then warn and return

    if ( word.size()-2 != (size_t)nspec ) {
      stringstream msg;
      msg << "Line " << i+1 << " in " + infile + " has the wrong number of elements "
	  << "(read " << word.size() << " but expected " << nspec+2 << ").\n";
      headas_printf(msg.str().c_str());
      status = 3;
      return(status);
    }

    // convert the energies into Reals

    vector<Real> rin(2);
    if ( SPstring2Real(word[0], rin[0]) != true ) {
      stringstream msg;
      msg << "The first element of line " << i+1 << " in " + infile + " is not a number.\n";
      headas_printf(msg.str().c_str());
      status = 4;
      return(status);
    }
    if ( SPstring2Real(word[1], rin[1]) != true ) {
      stringstream msg;
      msg << "The second element of line " << i+1 << " in " + infile + " is not a number.\n";
      headas_printf(msg.str().c_str());
      status = 4;
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

    for (size_t j=2; j<(size_t)nspec+2; j++) {
      if ( SPstring2Real(word[j], rin[0]) != true ) {
	stringstream msg;
	msg << "The " << j+1 << " element of line " << i+1 << " in " + infile + " is not a number.\n";
	headas_printf(msg.str().c_str());
	status = 4;
	return(status);
      }
      flux[j-2].push_back(rin[0]);
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
      for (size_t i=0; i<nE; i++) tflux[i] = flux[j][nE-i-1];
      for (size_t i=0; i<nE; i++) flux[j][i] = tflux[i];
    }
  }

  // now test that the input file is not really messed up and if there are
  // still any cases of energies in the wrong order then throw an error

  for (size_t i=0; i<nE-1; i++) {
    if ( eLow[i+1] < eLow[i] ) {
      string msg = "Input energies from " + infile + " are not in a consistent order.\n";
      headas_printf(msg.c_str());
      status = 5;
      return(status);
    }
  }

  return(status);

}

//************************************************************************************
// read the text file to get the parameter info

int flx2tab_readParInfo(string parinfofile, int nspec, string& parName, int& interp, 
                        vector<Real>& parDesc, vector<Real>& parVals)
{
  int status = 0;

  // load the input file into a vector of strings

  vector<string> instring = SPreadStrings(parinfofile);
  if ( instring.size() == 0 ) {
    string msg = "Failed to read data from " + parinfofile + ".\n";
    status = 1;
    headas_printf(msg.c_str());
    return(status);
  }

  parDesc.resize(6);
  parVals.resize(nspec);

  // first line is just the parameter name

  parName = instring[0];

  // next line comprises the interp switch

  if ( SPstring2Integer(instring[1], interp) != true ) {
    string msg = "2nd line of " + parinfofile + " is not an integer.\n";
    status = 2;
    headas_printf(msg.c_str());
    return(status);
  }

  // next line contains the six parameter descriptor values

  string delim = " 	";
  vector<string> word = SPtokenize(instring[2], delim);

  if ( word.size() != 6 ) {
    string msg = "3rd line of " + parinfofile + " does not contain six values.\n";
    status = 3;
    headas_printf(msg.c_str());
    return(status);
  }

  for (size_t i=0; i<6; i++) {
    if ( SPstring2Real(word[i], parDesc[i]) != true ) {
      string msg = "In 3rd line of " + parinfofile + " cannot convert " + word[i]
	+ " to a real number.\n";
      status = 4;
      headas_printf(msg.c_str());
      return(status);
    }
  }

  // final line contains the nspec tabulated parameter values

  word = SPtokenize(instring[3], delim);

  if ( word.size() != (size_t)nspec ) {
    string msg = "4th line of " + parinfofile + " does not contain expected number of values.\n";
    status = 5;
    headas_printf(msg.c_str());
    return(status);
  }

  for (size_t i=0; i<(size_t)nspec; i++) {
    if ( SPstring2Real(word[i], parVals[i]) != true ) {
      string msg = "In 4th line of " + parinfofile + " cannot convert " + word[i]
	+ " to a real number.\n";
      status = 6;
      headas_printf(msg.c_str());
      return(status);
    }
  }

  return(status);

}

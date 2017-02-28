// Runs the MEKAL code to produce tabulated table model files for use by XSPEC.

// kaa 11/7/12  Based on mekal.f

#include "table.h"

extern "C" {
#include "pil.h"
#include "headas.h"
#include "headas_error.h"
}

// standard ftools magic to do set-up

#define TOOLSUB mekal
#include "headas_main.c"

// Function prototypes

int mekal(void);
int mekal_getpar(string& filename, Real& tstart, Real& tend, int& ntsteps, 
		 string& efilenm, Real& density, bool& clobber, int& status);
int mekal_read(string efilenm, vector<Real>& energies, int& status);

// wrap-up for xspec model function calls

void mekal_xspecWrap(const vector<Real>& energies, 
	       const vector<Real>& params, vector<Real>& flux);

// main routine

int mekal(void)
{

  // Register taskname and version

  string taskname = "mekal";
  string version = "2.00";

  set_toolname(taskname.c_str());
  set_toolversion(version.c_str());

  string msg;

  // Get input parameters

  string filename, efilenm;
  Real tstart, tend, density;
  int ntsteps, status;
  bool clobber;

  mekal_getpar(filename, tstart, tend, ntsteps, efilenm, density, 
	       clobber, status);
  if ( status ) return(status);

  // Do some sanity checks on the input parameters. Enclose all this in {} so
  // we clean up after the tests

  {
    // Check that efilenm exists and we can open it

    ifstream ifs1(efilenm.c_str());
    if (!ifs1) {
      msg = "Cannot open "+efilenm;
      HD_ERROR_THROW(msg.c_str(), -1);
      return(-1);
    } else {
      ifs1.close();
    }

    // end of sanity testing
  }

  // Read the text file defining the energies

  vector<Real> energies;

  mekal_read(efilenm, energies, status);
  if ( status ) return(status);

  cout << std::endl;
  cout << "Setting up " << energies.size()-1 << " energy bins." << std::endl;

  // set up the table object

  table outputTable;

  // construct the table parameter objects
  // first kT

  {
    tableParameter tabPar;

    tabPar.Name = "kT";
    tabPar.InterpolationMethod = 1;
    tabPar.InitialValue = 1.0;
    tabPar.Delta = 0.01;
    tabPar.Minimum = tstart;
    tabPar.Bottom = tstart;
    tabPar.Top = tend;
    tabPar.Maximum = tend;
    Real tstep = (log10(tend)-log10(tstart))/(ntsteps-1);
    for (size_t i=0; i<(size_t)ntsteps; i++) {
      tabPar.TabulatedValues.push_back(pow((Real)10.0,(Real)(log10(tstart)+i*tstep)));
    }

    // add to the output table

    outputTable.pushParameter(tabPar);

  }

  // now the density, which is fixed at the value input

  {
    tableParameter tabPar;

    tabPar.Name = "nH";
    tabPar.InterpolationMethod = 0;
    tabPar.InitialValue = density;
    tabPar.Delta = -0.01;
    tabPar.Minimum = density;
    tabPar.Bottom = density;
    tabPar.Top = density;
    tabPar.Maximum = density;
    tabPar.TabulatedValues.push_back(density);

    outputTable.pushParameter(tabPar);
  }

  // now the element abundances as the additional parameters

  string elements[] = {"He" , "C " , "N " , "O " , "Ne", "Na", "Mg" ,
		       "Al", "Si" , "S " , "Ar", "Ca" , "Fe" , "Ni"};
  size_t Nelements = 14;

  for (size_t i=0; i<Nelements; i++) {

    tableParameter tabPar;

    tabPar.Name = elements[i];
    tabPar.InterpolationMethod = -1;
    tabPar.InitialValue = 1.0;
    tabPar.Delta = -0.01;
    tabPar.Minimum = 0.0;
    tabPar.Bottom = 0.0;
    tabPar.Top = 1000.0;
    tabPar.Maximum = 1000.0;

    outputTable.pushParameter(tabPar);

  }

  // set top-level table descriptors

  outputTable.ModelName = "vmekal_t";
  outputTable.ModelUnits = "ph/cm^2/s";
  outputTable.NumAddParams = Nelements;
  outputTable.NumIntParams = outputTable.Parameters.size() - Nelements;
  outputTable.isError = false;
  outputTable.isRedshift = true;
  outputTable.isAdditive = true;

  outputTable.Energies.resize(energies.size());
  for (size_t i=0; i<energies.size(); i++) outputTable.Energies[i] = energies[i];
  outputTable.EnergyUnits = "keV";

  // set up the table spectrum object(s) and add to the output table

  vector<Real> flux;
  vector<Real> params(outputTable.NumIntParams+outputTable.NumAddParams);

  // set up the parameter values which are not changed in the loop over
  // spectra

  params[1] = density;      // H density
  for (size_t i=0; i<Nelements; i++) params[i+2] = 1.0e-7;

  // loop over the spectra

  for (size_t ispec=0; ispec<(size_t)ntsteps; ispec++) {

    tableSpectrum tabSpec;

    // set parameter values for this spectrum.

    params[0] = outputTable.Parameters[0].TabulatedValues[ispec];
    tabSpec.ParameterValues.push_back(params[0]);

    tabSpec.ParameterValues.push_back(params[1]);

    // calculate the zero-metal continuum for basic spectrum

    mekal_xspecWrap(energies, params, tabSpec.Flux);

    // loop over the elements calculating the additional spectra

    for (size_t ielt=0; ielt<Nelements; ielt++) {

      params[ielt+2] = 1.0;

      vector<Real> addFlux;
      mekal_xspecWrap(energies, params, addFlux);

      for (size_t i=0; i<addFlux.size(); i++) addFlux[i] -= tabSpec.Flux[i];
      tabSpec.addFlux.push_back(addFlux);

      params[ielt+2] = 1.0e-7;

    }

    // add this spectrum object to the table

    outputTable.pushSpectrum(tabSpec);
  }


  // check for internal consistency of the table

  msg = outputTable.check();
  if ( msg.size() > 0 ) {
    HD_ERROR_THROW(msg.c_str(), -1);
    return(-1);
  }

  // Find out whether the output file exists. If it does and clobber is set
  // then delete it, otherwise throw an error. Currently just try to read as
  // a text file because there is no table::read method at present.

  ifstream ifs3(filename.c_str());
  if (ifs3) {
    if (clobber) {
      status = remove(filename.c_str());
      if (status) {
	msg = "Failed to clobber "+filename;
	HD_ERROR_THROW(msg.c_str(), status);
	return(status);
      }
    } else {
      msg = filename+" already exists. Either set clobber or choose another name.";
      HD_ERROR_THROW(msg.c_str(), -2);
      return(-2);
    }
    ifs3.close();
  }

  // write the output file

  status = outputTable.write(filename);

  return(status);

}

//************************************************************************************
// get the parameter values from the .par file

int mekal_getpar(string& filename, Real& tstart, Real& tend, int& ntsteps, 
	         string& efilenm, Real& density, bool& clobber, int& status)
{

  char *cinput = new char[PIL_LINESIZE];
  int *iinput = new int[1];
  double *rinput = new double[1];
  string msg;

  status = 0;

  if ((status = PILGetFname("filename", cinput))) {
    msg = "Error reading the 'filename' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    filename = string(cinput);
  }

  if ((status = PILGetReal("tstart", rinput))) {
    msg = "Error reading the 'tstart' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    tstart = (Real)*rinput;
  }

  if ((status = PILGetReal("tend", rinput))) {
    msg = "Error reading the 'tend' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    tend = (Real)*rinput;
  }

  if ((status = PILGetInt("ntsteps", iinput))) {
    msg = "Error reading the 'ntsteps' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    ntsteps = *iinput;
  }

  if ((status = PILGetFname("efilenm", cinput))) {
    msg = "Error reading the 'efilenm' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    efilenm = string(cinput);
  }

  if ((status = PILGetReal("density", rinput))) {
    msg = "Error reading the 'density' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    density = (Real)*rinput;
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
// read the text file to get the energies
// each line looks like a dummyrsp command in xspec ie
// start_energy end_energy number_energy_bins lin|log

int mekal_read(string efilenm, vector<Real>& energies, int& status)
{

  status = 0;

  ifstream fstream(efilenm.c_str(), ios_base::in);
  if (!fstream) {
    string msg = "Failed to open " + efilenm;
    status = 1;
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  }

  string instring;
  stringstream instream;
  Real eMin, eMax;
  size_t nBins;
  string type;

  // loop round lines in file processing them      

  getline(fstream, instring);
  while ( !fstream.eof() && instring.size() > 0 ) {

    instream << instring;
    instream >> eMin >> eMax >> nBins >> type;
    instream.clear();

    if ( nBins <= 0 ) {
      string msg = efilenm + " has line with zero bins.";
      status = 2;
      HD_ERROR_THROW(msg.c_str(), status);
      return(status);
    }

    // set up energy bins. note that there are actually nBins+1 energy values.

    if ( type == "lin" ) {

      Real step = (eMax - eMin) / nBins;
      for (size_t i=0; i<=nBins; i++) {
	energies.push_back(eMin + i*step);
      }

    } else if ( type == "log" ) {

      Real step = (log(eMax)-log(eMin)) / nBins;
      for (size_t i=0; i<=nBins; i++) {
	energies.push_back(exp(log(eMin) + i*step));
      }

    } else {

      string msg = efilenm + " has line with neither lin nor log.";
      status = 3;
      HD_ERROR_THROW(msg.c_str(), status);
      return(status);

    }

    getline(fstream, instring);

  }

  fstream.close();

  return(status);

}

// definitions for XSPEC. Note cannot just #include xsFortran.h and functionMap.h
// because heasp currently defines Real as float while XSPEC defines it as double.

#include <cfortran.h>

PROTOCCALLSFSUB6(XSVMKL, xsvmkl, FLOATV, INT, FLOATV, INT, FLOATV, FLOATV)
#define XSVMKL(ear, ne, param, ifl, photar, photer) CCALLSFSUB6(XSVMKL, xsvmkl, FLOATV, INT, FLOATV, INT, FLOATV, FLOATV, ear, ne, param, ifl, photar, photer)

extern "C" {
  void FNINIT();
}

void mekal_xspecWrap(const vector<Real>& energies, const vector<Real>& params, 
		     vector<Real>& flux)
{
  static bool first = true;
  static float* ear = new float[energies.size()];
  static float* pars = new float[params.size()+2];
  static float* photar = new float[energies.size()-1];
  static float* photer = new float[energies.size()-1];

  // if first call then initialize model stuff and set ear array

  if ( first ) {
    FNINIT();
    for (size_t i=0; i<energies.size(); i++) ear[i] = (float)energies[i];
    first = false;
  }

  // set up the pars array. add the redshift and switch parameters

  for (size_t i=0; i<params.size(); i++) pars[i] = (float)params[i];
  pars[params.size()] = 0.0;     // adding the redshift parameter
  pars[params.size()+1] = 0.0;     // adding the switch parameter

  int ne = static_cast<const int>(energies.size()-1);

  // Call the model function

  XSVMKL(ear, ne, pars, 1, photar, photer);

  // Write output into the flux array.

  flux.resize(energies.size()-1);
  for (size_t i=0; i<energies.size()-1; i++) flux[i] = (Real)photar[i];

  return;

}




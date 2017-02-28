// Runs the Raymond-Smith code to produce tabulated table model files for use by XSPEC.

// kaa 11/7/12  Based on raysmith.f

#include "table.h"

extern "C" {
#include "pil.h"
#include "headas.h"
#include "headas_error.h"
}

// standard ftools magic to do set-up

#define TOOLSUB raysmith
#include "headas_main.c"

// Function prototypes

int raysmith(void);
int raysmith_getpar(string& filename, Real& tstart, Real& tend, int& ntsteps, 
		 string& efilenm, bool& clobber, int& status);
int raysmith_read(string efilenm, vector<Real>& energies, int& status);

// wrap-up for the raymond-smith code call

void rayspec_wrap(const vector<Real>& energies, 
		   const vector<Real>& params, vector<Real>& flux);

// main routine

int raysmith(void)
{

  // Register taskname and version

  string taskname = "raysmith";
  string version = "2.00";

  set_toolname(taskname.c_str());
  set_toolversion(version.c_str());

  string msg;

  // Get input parameters

  string filename, efilenm;
  Real tstart, tend;
  int ntsteps, status;
  bool clobber;

  raysmith_getpar(filename, tstart, tend, ntsteps, efilenm, clobber, status);
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

  raysmith_read(efilenm, energies, status);
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

  // now the element abundances as the additional parameters

  string elements[] = {"He" , "C " , "N " , "O " , "Ne", "Mg" ,
		       "Si" , "S " , "Ar", "Ca" , "Fe" , "Ni"};
  size_t Nelements = 12;

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

  outputTable.ModelName = "vraymond_t";
  outputTable.ModelUnits = "ph/cm^2/s";
  outputTable.NumAddParams = Nelements;
  outputTable.NumIntParams = 1;
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

  for (size_t i=0; i<Nelements; i++) params[i+1] = 1.0e-7;

  // loop over the spectra

  for (size_t ispec=0; ispec<(size_t)ntsteps; ispec++) {

    tableSpectrum tabSpec;

    // set parameter values for this spectrum.

    params[0] = outputTable.Parameters[0].TabulatedValues[ispec];
    tabSpec.ParameterValues.push_back(params[0]);

    // debug
    cout << "Calculating kT = " << params[0] << std::endl;

    // calculate the zero-metal continuum for basic spectrum

    rayspec_wrap(energies, params, tabSpec.Flux);

    // loop over the elements calculating the additional spectra

    for (size_t ielt=0; ielt<Nelements; ielt++) {

      params[ielt+1] = 1.0;

      vector<Real> addFlux;
      rayspec_wrap(energies, params, addFlux);

      for (size_t i=0; i<addFlux.size(); i++) addFlux[i] -= tabSpec.Flux[i];
      tabSpec.addFlux.push_back(addFlux);

      params[ielt+1] = 1.0e-7;

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

int raysmith_getpar(string& filename, Real& tstart, Real& tend, int& ntsteps, 
	         string& efilenm, bool& clobber, int& status)
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

int raysmith_read(string efilenm, vector<Real>& energies, int& status)
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

#include <cfortran.h>

PROTOCCALLSFSUB9(RAYSPEC_M, rayspec_m, FLOAT, FLOAT, FLOATV, INTV, FLOATV, FLOATV, INT, INT, FLOATV)
#define RAYSPEC_M(t, dene, frac, nsteps, emin, estep, idens, icx, work1) CCALLSFSUB9(RAYSPEC_M, rayspec_m, FLOAT, FLOAT, FLOATV, INTV, FLOATV, FLOATV, INT, INT, FLOATV, t, dene, frac, nsteps, emin, estep, idens, icx, work1)

void rayspec_wrap(const vector<Real>& energies, const vector<Real>& params, 
		   vector<Real>& flux)
{
  static float* emin = new float[1];
  static float* estep = new float[1];
  static float* work1 = new float[1];
  static float* frac = new float[12];

  static int* nsteps = new int[1];
  nsteps[0] = 1;
  static float dene = 0.0;
  static int idens = 1;
  static int icx = 1;
  static float t, enorm;

  // set up the frac array and the temperature variable

  t = log10(params[0] * 11.6e6);
  for (size_t i=0; i<12; i++) frac[i] = (float)params[i+1];
  
  // loop over energy bins

  flux.resize(energies.size()-1);
  for (size_t i=0; i<(energies.size()-1); i++) {

    emin[0] = (float)energies[i]*1000.0;
    estep[0] = (float)(energies[i+1]-energies[i])*1000.0;
    enorm = (float)2.0/(energies[i+1]+energies[i])/1.60207;

    // do the calculation for this energy

    RAYSPEC_M(t, dene, frac, nsteps, emin, estep, idens, icx, work1);

    // Write output into the flux array.

    flux[i] = (Real)work1[0]*enorm;

  }

  return;

}




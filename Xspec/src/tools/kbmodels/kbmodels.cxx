// Runs Kazik Borkowski's NEI/SNR MEKAL code to produce tabulated table model
// files for use by XSPEC.

// kaa 10/25/12  Based on kbmodels.f

#include "table.h"

extern "C" {
#include "pil.h"
#include "headas.h"
#include "headas_error.h"
}

// standard ftools magic to do set-up

#define TOOLSUB kbmodels
#include "headas_main.c"

// Function prototypes

int kbmodels(void);
int kbmodels_getpar(int& modeloption, bool& isv, bool& isvv, string& tabfile, 
		    Real& tstart, Real& tend, int& nt, Real& taustart, Real& tauend,
		    int& ntau, Real& testart, Real& teend, int& nte, 
		    string& efilenm, string& neivers, bool& clobber, int& status);
int kbmodels_read(string efilenm, vector<Real>& energies, int& status);

// wrap-up for xspec model function calls

void xspecWrap(const int modeloption, const string neivers, const bool isv, 
	       const bool isvv, const vector<Real>& energies, 
	       const vector<Real>& params, vector<Real>& flux);

// model options enumerated

enum{PSHOCK, NPSHOCK, SEDOV};

// main routine

int kbmodels(void)
{

  // Register taskname and version

  string taskname = "kbmodels";
  string version = "1.00";

  set_toolname(taskname.c_str());
  set_toolversion(version.c_str());

  string msg;

  // Get input parameters

  string tabfile, efilenm, neivers;
  Real tstart, tend, taustart, tauend, testart, teend;
  int modeloption, nt, ntau, nte, status;
  bool isv, isvv, clobber;

  kbmodels_getpar(modeloption, isv, isvv, tabfile, tstart, tend, nt, taustart, 
		  tauend, ntau, testart, teend, nte, efilenm, neivers, 
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

    // neivers should be one of "1.0", "1.1", "2.0" or "3.0"

    if ( neivers != "1.0" && neivers != "1.1" && neivers != "2.0" 
                && neivers != "3.0") {
      msg = "neivers must be one of 1.0, 1.1, 2.0 or 3.0";
      HD_ERROR_THROW(msg.c_str(), -1);
      return(-3);
    }

    // end of sanity testing
  }

  // Read the text file defining the energies

  vector<Real> energies;

  kbmodels_read(efilenm, energies, status);
  if ( status ) return(status);

  cout << std::endl;
  cout << "Setting up " << energies.size()-1 << " energy bins." << std::endl;

  // set up the table object

  table outputTable;

  // construct the table parameter objects
  // first T_s

  {
    tableParameter tabPar;

    tabPar.Name = "T_s";
    tabPar.InterpolationMethod = 0;
    tabPar.InitialValue = 2.0;
    tabPar.Delta = 0.01;
    tabPar.Minimum = tstart;
    tabPar.Bottom = tstart;
    tabPar.Top = tend;
    tabPar.Maximum = tend;
    Real tstep = (log10(tend)-log10(tstart))/(nt-1);
    for (size_t i=0; i<(size_t)nt; i++) {
      tabPar.TabulatedValues.push_back(pow((Real)10.0,(Real)(log10(tstart)+i*tstep)));
    }

    // add to the output table

    outputTable.pushParameter(tabPar);

  }

  // next T_e if necessary

  if ( nte > 0 ) {

    {
      tableParameter tabPar;

      tabPar.Name = "T_e";
      tabPar.InterpolationMethod = 0;
      tabPar.InitialValue = 2.0;
      tabPar.Delta = 0.01;
      tabPar.Minimum = testart;
      tabPar.Bottom = testart;
      tabPar.Top = teend;
      tabPar.Maximum = teend;
      Real testep = (log10(teend)-log10(testart))/(nte-1);
      for (size_t i=0; i<(size_t)nte; i++) {
	tabPar.TabulatedValues.push_back(pow((Real)10.0,(Real)(log10(testart)+i*testep)));
      }

      outputTable.pushParameter(tabPar);
    }

  }

  // now the H abundance, which is fixed at 1.0

  {
    tableParameter tabPar;

    tabPar.Name = "H";
    tabPar.InterpolationMethod = 0;
    tabPar.InitialValue = 1.0;
    tabPar.Delta = -0.01;
    tabPar.Minimum = 1.0;
    tabPar.Bottom = 1.0;
    tabPar.Top = 1.0;
    tabPar.Maximum = 1.0;
    tabPar.TabulatedValues.push_back(1.0);

    outputTable.pushParameter(tabPar);
  }

  // now the element abundances as the additional parameters

  string velements[] = {"He" , "C " , "N " , "O " , "Ne", "Mg" ,
			"Si" , "S " , "Ar", "Ca" , "Fe" , "Ni"};
  size_t Nvelements = 12;
  string vvelements[] = {"He", "Li", "Be", "B ", "C ", "N ", "O ", "F ", 
			 "Ne", "Na", "Mg", "Al", "Si", "P ", "S ", "Cl",
			 "Ar", "K ", "Ca", "Sc", "Ti", "V ", "Cr", "Mn",
			 "Fe", "Co", "Ni", "Cu", "Zn"};
  size_t Nvvelements = 29;

  size_t Nelements = 1;
  if ( isv ) Nelements = Nvelements;
  if ( isvv ) Nelements = Nvvelements;

  for (size_t i=0; i<Nelements; i++) {

    tableParameter tabPar;

    if ( isv ) {
      tabPar.Name = velements[i];
    } else if ( isvv ) {
      tabPar.Name = vvelements[i];
    } else {
      tabPar.Name = "abundance";
    }
    tabPar.InterpolationMethod = -1;
    tabPar.InitialValue = 1.0;
    tabPar.Delta = -0.01;
    tabPar.Minimum = 0.0;
    tabPar.Bottom = 0.0;
    tabPar.Top = 1000.0;
    tabPar.Maximum = 1000.0;

    outputTable.pushParameter(tabPar);

  }

  // for pshock and npshock include a fixed tau_l

  if ( modeloption == PSHOCK || modeloption == NPSHOCK ) {

    tableParameter tabPar;

    tabPar.Name = "tau_l";
    tabPar.InterpolationMethod = 0;
    tabPar.InitialValue = 0.0;
    tabPar.Delta = 0.0;
    tabPar.Minimum = 0.0;
    tabPar.Bottom = 0.0;
    tabPar.Top = 0.0;
    tabPar.Maximum = 0.0;
    tabPar.TabulatedValues.push_back(0.0);

    outputTable.pushParameter(tabPar);

  }


  // finally tau or tau_h

  tableParameter tabPar;

  if ( modeloption == SEDOV ) {
    tabPar.Name = "tau";
  } else if ( modeloption == PSHOCK || modeloption == NPSHOCK ) {
    tabPar.Name == "tau_h";
  }
  tabPar.InterpolationMethod = 0;
  tabPar.InitialValue = 1.0e11;
  tabPar.Delta = 1.0e8;
  tabPar.Minimum = taustart;
  tabPar.Bottom = taustart;
  tabPar.Top = tauend;
  tabPar.Maximum = tauend;
  Real taustep = (log10(tauend)-log10(taustart))/(ntau-1);
  for (size_t i=0; i<(size_t)ntau; i++) {
    tabPar.TabulatedValues.push_back(pow((Real)10.0,(Real)(log10(taustart)+i*taustep)));
  }

  outputTable.pushParameter(tabPar);


  // set top-level table descriptors

  if ( modeloption == PSHOCK ) {
    if ( isv ) {
      outputTable.ModelName = "vpshock_t";
    } else if ( isvv ) {
      outputTable.ModelName = "vvpshock_t";
    } else {
      outputTable.ModelName = "pshock_t";
    }
  } else if ( modeloption == NPSHOCK ) {
    if ( isv ) {
      outputTable.ModelName = "vnpshock_t";
    } else if ( isvv ) {
      outputTable.ModelName = "vvnpshock_t";
    } else {
      outputTable.ModelName = "npshock_t";
    }
  } else if ( modeloption == SEDOV ) {
    if ( isv ) {
      outputTable.ModelName = "vsedov_t";
    } else if ( isvv ) {
      outputTable.ModelName = "vvsedov_t";
    } else {
      outputTable.ModelName = "sedov_t";
    }
  }
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

  size_t nspec = (size_t)(nt*ntau);
  if ( nte > 0 ) nspec *= (size_t) nte;

  vector<Real> flux;
  vector<Real> params(outputTable.NumIntParams+outputTable.NumAddParams);

  // set up the parameter values which are not changed in the loop over
  // spectra

  size_t ioff(3);
  if ( modeloption == PSHOCK ) ioff = 2;

  params[ioff-1] = 1.0;      // H abundance
  for (size_t i=0; i<Nelements; i++) params[i+ioff] = 1.0e-7;

  if ( modeloption == PSHOCK || modeloption == NPSHOCK ) {
    params[Nelements+ioff] = 0.0;     // Tau_l
  }

  // loop over the spectra

  for (size_t ispec=0; ispec<(size_t)nspec; ispec++) {

    tableSpectrum tabSpec;

    // set the indices for the parameters on this iteration

    size_t ipart(ispec);
    size_t parind[3];
    if ( nte > 0 ) {
      parind[2] = ispec%nte;
      ipart -= parind[2];
      ipart /= nte;
    } else {
      parind[2] = 0;
    }
    parind[1] = ipart%ntau;
    ipart -= parind[1];
    ipart /= ntau;
    parind[0] = ipart%nt;

    // set parameter values for this spectrum. Messy because of the different
    // cases for three different models.
    // T_s is params[0] for all models; T_e is params[1] for sedov and npshock; 
    // tau is params[ioff+Nelements] for sedov; tau_u is params[ioff+Nelements+1] for
    // pshock and npshock. Also include the H abundance (params[ioff-1]), which is 
    // an interpolated parameter with only one value, and tau_l (params[ioff+Nelements])
    // for pshock and npshock), which is also an interpolated parameter with
    // only one value.

    params[0] = outputTable.Parameters[0].TabulatedValues[parind[0]];

    tabSpec.ParameterValues.push_back(params[0]);

    if ( modeloption == SEDOV || modeloption == NPSHOCK ) {
      params[1] = outputTable.Parameters[1].TabulatedValues[parind[2]];
      tabSpec.ParameterValues.push_back(params[1]);
    }

    tabSpec.ParameterValues.push_back(params[ioff-1]);

    if ( modeloption == SEDOV ) {
      params[ioff+Nelements] = outputTable.Parameters[ioff+Nelements].TabulatedValues[parind[1]];
      tabSpec.ParameterValues.push_back(params[ioff+Nelements]);
    } else if ( modeloption == PSHOCK ) {
      tabSpec.ParameterValues.push_back(params[ioff+Nelements]);
      params[ioff+Nelements+1] = outputTable.Parameters[ioff+Nelements+1].TabulatedValues[parind[1]];
      tabSpec.ParameterValues.push_back(params[ioff+Nelements+1]);
    } else if ( modeloption == NPSHOCK ) {
      tabSpec.ParameterValues.push_back(params[ioff+Nelements]);
      params[ioff+Nelements+1] = outputTable.Parameters[ioff+Nelements+1].TabulatedValues[parind[1]];
      tabSpec.ParameterValues.push_back(params[ioff+Nelements+1]);
    }

    // calculate the zero-metal continuum for basic spectrum

    xspecWrap(modeloption, neivers, isv, isvv, energies, params, tabSpec.Flux);

    // loop over the elements calculating the additional spectra

    for (size_t ielt=0; ielt<Nelements; ielt++) {

      params[ielt+ioff] = 1.0;

      vector<Real> addFlux;
      xspecWrap(modeloption, neivers, isv, isvv, energies, params, addFlux);

      for (size_t i=0; i<addFlux.size(); i++) addFlux[i] -= tabSpec.Flux[i];
      tabSpec.addFlux.push_back(addFlux);

      params[ielt+ioff] = 1.0e-7;

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

  ifstream ifs3(tabfile.c_str());
  if (ifs3) {
    if (clobber) {
      status = remove(tabfile.c_str());
      if (status) {
	msg = "Failed to clobber "+tabfile;
	HD_ERROR_THROW(msg.c_str(), status);
	return(status);
      }
    } else {
      msg = tabfile+" already exists. Either set clobber or choose another name.";
      HD_ERROR_THROW(msg.c_str(), -2);
      return(-2);
    }
    ifs3.close();
  }

  // write the output file

  status = outputTable.write(tabfile);

  return(status);

}

//************************************************************************************
// get the parameter values from the .par file

int kbmodels_getpar(int& modeloption, bool& isv, bool& isvv, string& tabfile, 
		    Real& tstart, Real& tend, int& nt, Real& taustart, Real& tauend,
		    int& ntau, Real& testart, Real& teend, int& nte, 
	            string& efilenm, string& neivers, bool& clobber, int& status)
{

  char *cinput = new char[PIL_LINESIZE];
  int *iinput = new int[1];
  double *rinput = new double[1];
  string msg;

  status = 0;

  if ((status = PILGetString("modelname", cinput))) {
    msg = "Error reading the 'modelname' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  }
  isv = false;
  isvv = false;
  string firstchar = (string(cinput)).substr(0,1);
  if ( firstchar == "v" || firstchar == "V" ) {
    isv = true;
    firstchar = (string(cinput)).substr(1,1);
    if ( firstchar == "v" || firstchar == "V" ) {
      isv = false;
      isvv = true;
      firstchar = (string(cinput)).substr(1,1);
    }
  }
  if ( firstchar == "p" || firstchar == "P" ) {
    modeloption = PSHOCK;
  } else if ( firstchar == "n" || firstchar == "N" ) {
    modeloption = NPSHOCK;
  } else if ( firstchar == "s" || firstchar == "S" ) {
    modeloption = SEDOV;
  } else {
    msg = "modelname must be one of pshock, npshock or sedov";
    HD_ERROR_THROW(msg.c_str(), -1);
    return(-1);
  }

  if ((status = PILGetFname("tabfile", cinput))) {
    msg = "Error reading the 'tabfile' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    tabfile = string(cinput);
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

  if ((status = PILGetInt("nt", iinput))) {
    msg = "Error reading the 'nt' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    nt = *iinput;
  }

  if ((status = PILGetReal("taustart", rinput))) {
    msg = "Error reading the 'taustart' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    taustart = (Real)*rinput;
  }

  if ((status = PILGetReal("tauend", rinput))) {
    msg = "Error reading the 'tauend' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    tauend = (Real)*rinput;
  }

  if ((status = PILGetInt("ntau", iinput))) {
    msg = "Error reading the 'ntau' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    ntau = *iinput;
  }

  if ( modeloption == SEDOV || modeloption == NPSHOCK ) {

    if ((status = PILGetReal("testart", rinput))) {
      msg = "Error reading the 'testart' parameter.";
      HD_ERROR_THROW(msg.c_str(), status);
      return(status);
    } else {
      testart = (Real)*rinput;
    }

    if ((status = PILGetReal("teend", rinput))) {
      msg = "Error reading the 'teend' parameter.";
      HD_ERROR_THROW(msg.c_str(), status);
      return(status);
    } else {
      teend = (Real)*rinput;
    }

    if ((status = PILGetInt("nte", iinput))) {
      msg = "Error reading the 'nte' parameter.";
      HD_ERROR_THROW(msg.c_str(), status);
      return(status);
    } else {
      nte = *iinput;
    }

  }

  if ((status = PILGetFname("efilenm", cinput))) {
    msg = "Error reading the 'efilenm' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    efilenm = string(cinput);
  }

  if ((status = PILGetString("neivers", cinput))) {
    msg = "Error reading the 'neivers' parameter.";
    HD_ERROR_THROW(msg.c_str(), status);
    return(status);
  } else {
    neivers = string(cinput);
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

int kbmodels_read(string efilenm, vector<Real>& energies, int& status)
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

extern "C" {
  void sedov(const valarray<double>& xsEnergy, const valarray<double>& xsParams, int spectrumNumber, valarray<double>& flux, valarray<double>& fluxErr, const string initString);
  void vsedov(const valarray<double>& xsEnergy, const valarray<double>& xsParams, int spectrumNumber, valarray<double>& flux, valarray<double>& fluxErr, const string initString);
  void vvsedov(const valarray<double>& xsEnergy, const valarray<double>& xsParams, int spectrumNumber, valarray<double>& flux, valarray<double>& fluxErr, const string initString);
  void pshock(const valarray<double>& xsEnergy, const valarray<double>& xsParams, int spectrumNumber, valarray<double>& flux, valarray<double>& fluxErr, const string initString);
  void vpshock(const valarray<double>& xsEnergy, const valarray<double>& xsParams, int spectrumNumber, valarray<double>& flux, valarray<double>& fluxErr, const string initString);
  void vvpshock(const valarray<double>& xsEnergy, const valarray<double>& xsParams, int spectrumNumber, valarray<double>& flux, valarray<double>& fluxErr, const string initString);
  void npshock(const valarray<double>& xsEnergy, const valarray<double>& xsParams, int spectrumNumber, valarray<double>& flux, valarray<double>& fluxErr, const string initString);
  void vnpshock(const valarray<double>& xsEnergy, const valarray<double>& xsParams, int spectrumNumber, valarray<double>& flux, valarray<double>& fluxErr, const string initString);
  void vvnpshock(const valarray<double>& xsEnergy, const valarray<double>& xsParams, int spectrumNumber, valarray<double>& flux, valarray<double>& fluxErr, const string initString);
  void FNINIT();
  void FPMSTR(const char* key, const char* value);
}

void xspecWrap(const int modeloption, const string neivers, const bool isv,
	       const bool isvv, const vector<Real>& energies, 
	       const vector<Real>& params, vector<Real>& flux)
{
  static bool first = true;

  // if first call then initialize model stuff and set the nei version.

  if ( first ) {
    FNINIT();
    string keystr = "neivers";
    FPMSTR(keystr.c_str(), neivers.c_str());
    first = false;
  }

  valarray<double> xsEnergy(energies.size());
  for (size_t i=0; i<energies.size(); i++) xsEnergy[i] = (double)energies[i];
  valarray<double> xsParams(params.size()+1);
  for (size_t i=0; i<params.size(); i++) xsParams[i] = (double)params[i];
  xsParams[params.size()] = 0.0;     // adding the redshift parameter

  valarray<double> xsFlux;
  valarray<double> fluxErr;

  if ( modeloption == SEDOV ) {
    if ( isv ) {
      vsedov(xsEnergy, xsParams, 1, xsFlux, fluxErr, " ");
    } else if ( isvv ) {
      vvsedov(xsEnergy, xsParams, 1, xsFlux, fluxErr, " ");
    } else {
      sedov(xsEnergy, xsParams, 1, xsFlux, fluxErr, " ");
    }
  } else if ( modeloption == PSHOCK ) {
    if ( isv ) {
      vpshock(xsEnergy, xsParams, 1, xsFlux, fluxErr, " ");
    } else if ( isvv ) {
      vvpshock(xsEnergy, xsParams, 1, xsFlux, fluxErr, " ");
    } else {
      pshock(xsEnergy, xsParams, 1, xsFlux, fluxErr, " ");
    }
  } else if ( modeloption == NPSHOCK ) {
    if ( isv ) {
      vnpshock(xsEnergy, xsParams, 1, xsFlux, fluxErr, " ");
    } else if ( isvv ) {
      vvnpshock(xsEnergy, xsParams, 1, xsFlux, fluxErr, " ");
    } else {
      npshock(xsEnergy, xsParams, 1, xsFlux, fluxErr, " ");
    }
  }

  flux.resize(xsFlux.size());
  for (size_t i=0; i<flux.size(); i++) flux[i] = (Real)xsFlux[i];

  return;

}




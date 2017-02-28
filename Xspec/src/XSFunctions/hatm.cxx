// Hydrogen model atmosphere model by V. Suleimanov
// with Comptonization taken into account using
// Kompaneets equation.
// Implemented by D. Klochkov 2015
// email: klochkov@astro.uni-tuebingen.de
//
// parameter[0] - effective (unredshifted) temperature of the 
//                neutron star surface (in MK)
//                T = 2.0..10.0
// parameter[1] - neutron star gravitational mass (in solar mass)
// parameter[2] - neutron star radius (in km)
//
//////////////////////////////////////////////////

#include <cmath>
#include <iomanip>
#include "xsTypes.h"
#include "FunctionUtility.h"
#include <iostream>
#include <fstream>
#include <string>

extern "C" void hatm(const RealArray& energy, 
               const RealArray& parameter, 
               int spectrum, 
               RealArray& flux, 
               RealArray& fluxError, 
               const string& init)
{
  using namespace std;
  const Real T      =parameter[0];
  const Real NSmass =parameter[1];
  const Real NSrad  =parameter[2];
  const int  LOGGNUM= 9; //..number of tabulated logg (table files)
  const int  TNUM  =191;//..number of tabulated temperatures
  const int  ENUM =1000; //..number of energy bins in tabulated spectra
  Real logg;             //..common logarithm of surface gravity (in cgs)
  Real z;                //..gravitational redshift at NS surface
  Real loggtab[LOGGNUM]; //..grid for surface gravity logarithms
                         // (ing cgs), for which atmosphere models 
                         // are calculated
  Real Ttab[TNUM];       //..tabulated temperatures in MK
  Real Tmin = 0.5;       //..lowest tabulated temperature in MK
  Real Tstep= 0.05;      //..step size of tabulated temperatures in MK
  string filename[LOGGNUM];  //..file names of model tables
  string directory;      //..path to the derectory containing model tables
                         //..directory with the model tables
  string path;           //..full path to the model table
  Real minlogg, maxlogg; //..min/max logg for which the spectra are calculated
  Real lowlogg, highlogg;//..bracketing values of logg, i.e. the closest
                         // values of tabulated logg around the current value
  int  ilogg;            //..index of the lower bracketing logg file
  Real lowT, highT;      //..bracketing values of T
  int  iT;               //..index of the lower bracketing T
  Real fluxlogg1T1[ENUM];//..flux of the bracketing spectrum at lowT
                         //..and lowlogg
  Real fluxlogg1T2[ENUM];//..flux of the bracketing spectrum at highT
                         //..and lowlogg
  Real fluxlogg2T1[ENUM];//..flux of the bracketing spectrum at lowT
                         //..and highlogg
  Real fluxlogg2T2[ENUM];//..flux of the bracketing spectrum at highT
                         //..and highlogg
  Real fluxInterp[ENUM]; //..bilinearly interpolated model flux
  Real Elow[ENUM],Ehigh[ENUM];//..model energy bins
  string strflux;        //..auxulary string variable
  string foo;            //..auxulary string variable
  Real factor=1.0;       //..The final flux is multiplied by this factor, 
                         // which is normally 1.0 but goes up and generates
                         // a warning message if log(g) is
                         // outside the allowed range. This is to avoid the
                         // fit routine to go deep outside the available log(g)
                         // range by "steppar" or "error" commands)
  int i;                 //..loop counter

  size_t N(energy.size());
  flux.resize(N-1);

  //..path to the directory with model tables:
  // if variable HATM is set, use that path. If not, use
  // $HEADAS/../spectral/modelData
  directory = FunctionUtility::getModelString("HATM");
  if(directory.compare("$$NOT$$") == 0) { // HATM variable is not set
    directory = FunctionUtility::modelDataPath();
  }

  //..initialize model filenames and tabulated logg values
  filename[0] = "spH_g137_ca_ph.dat";
  filename[1] = "spH_g1385_ca_ph.dat";
  filename[2] = "spH_g140_ca_ph.dat";
  filename[3] = "spH_g1415_ca_ph.dat";
  filename[4] = "spH_g143_ca_ph.dat";
  filename[5] = "spH_g1445_ca_ph.dat";
  filename[6] = "spH_g146_ca_ph.dat";
  filename[7] = "spH_g1475_ca_ph.dat";
  filename[8] = "spH_g149_ca_ph.dat";

  loggtab[0] = 13.7;
  loggtab[1] = 13.85;
  loggtab[2] = 14.0;
  loggtab[3] = 14.15;
  loggtab[4] = 14.3;
  loggtab[5] = 14.45;
  loggtab[6] = 14.6;
  loggtab[7] = 14.75;
  loggtab[8] = 14.9;

  minlogg = loggtab[0];
  maxlogg = loggtab[LOGGNUM-1];

  //..initialize tabulated temperature values
  for(i=0;i<TNUM;++i) {
    Ttab[i] = Tmin + i*Tstep;
  }

  //..calculate z and redshift and logg
  if(NSmass/(0.337*NSrad) < 0.99) {
    z    = 1./sqrt(1. - NSmass/(0.337*NSrad)) - 1.;
  } else {
    z=1./sqrt(1. - 0.99) - 1.;
    cout<<"!! Warning: the calculated Z is above the limit (9.0)."<<endl;
    cout<<"The results are not trustable for this M and R ("<<NSmass<<", "<<NSrad<<")!!"<<endl;
  }
  logg = 16.125 + log10(NSmass*(1.+z)/(NSrad*NSrad));
  /*
  if(logg < minlogg || logg >= maxlogg) {
    for(size_t k=0; k<N-1; ++k) {
      flux[k] = 100.+1.e10*fabs(logg-maxlogg)/maxlogg;
    }
    cout<<"!! Warning: log(g)="<<logg<<" is outside the limits for which model atmospheres are calculated (13.7...14.9)"<<endl;
    cout<<"The results are not trustable for this M and R ("<<NSmass<<", "<<NSrad<<")!!"<<endl;
    return;
  }
  */
  
  if(logg < minlogg) {
    factor = 1.+1.e4*(minlogg-logg)/logg;
    logg = minlogg;
    cout<<"!! Warning: log(g)="<<logg<<" is outside the limits for which model atmospheres are calculated (13.7...14.9)"<<endl;
    cout<<"The results are not trustable for this M and R ("<<NSmass<<", "<<NSrad<<")!!"<<endl;
  }
  if(logg > maxlogg) {
    factor = 1.+1.e4*(logg-maxlogg)/maxlogg;
    logg = maxlogg;
    cout<<"!! Warning: log(g)="<<logg<<" is outside the limits for which model atmospheres are calculated (13.7...14.9)"<<endl;
    cout<<"The results are not trustable for this M and R ("<<NSmass<<", "<<NSrad<<")!!"<<endl;
  }
  
  //cout<<"logg= "<<logg<<", z= "<<z<<", M= "<<NSmass<<", R= "<<NSrad<<", T= "<<T<<endl;

  //..find bracketing logg and corresponding file index
  for(i=0;i<LOGGNUM-1;++i) {
    if(logg >= loggtab[i] && logg <= loggtab[i+1]) {
      ilogg = i;
      lowlogg = loggtab[i];
      highlogg= loggtab[i+1];
      break;
    }
  }

  //..find bracketing T and corresponding index
  for(i=0;i<TNUM-1;++i) {
    if(T >= Ttab[i] && T <= Ttab[i+1]) {
      iT = i;
      lowT = Ttab[i];
      highT= Ttab[i+1];
      break;
    }
  }
  
  //..read four corresponding bracketing spectra...
  path = directory+filename[ilogg];
  ifstream fin1(path.c_str());
  path = directory+filename[ilogg+1];
  ifstream fin2(path.c_str());
  if(fin1.is_open() && fin2.is_open()) {
    for(i=0;i<iT*ENUM;++i){ //..skip lines before the needed spectrum
      fin1>>foo>>foo>>foo>>foo>>foo>>foo>>foo;
      fin2>>foo>>foo>>foo>>foo>>foo>>foo>>foo;
    }
    for(i=0;i<ENUM;++i){ //..read spectra for lower bracketing T)
      fin1>>Elow[i]>>Ehigh[i]>>strflux>>foo>>foo>>foo>>foo;
      strflux.replace(strflux.find("D"),1,"E");
      fluxlogg1T1[i] = atof(strflux.c_str());
      fin2>>foo>>foo>>strflux>>foo>>foo>>foo>>foo;
      strflux.replace(strflux.find("D"),1,"E");
      fluxlogg2T1[i] = atof(strflux.c_str());
    }
    for(i=0;i<ENUM;++i){ //..read spectra for higher bracketing T)
      fin1>>foo>>foo>>strflux>>foo>>foo>>foo>>foo;
      strflux.replace(strflux.find("D"),1,"E");
      fluxlogg1T2[i] = atof(strflux.c_str());
      fin2>>foo>>foo>>strflux>>foo>>foo>>foo>>foo;
      strflux.replace(strflux.find("D"),1,"E");
      fluxlogg2T2[i] = atof(strflux.c_str());
    }
    fin1.close();
    fin2.close();
  } else {
    cout << "!!! Can't open model file " << path << "!!!" << endl;
  }

  //..and interpolate between them to calculate flux
  Real A = 1./((highlogg-lowlogg)*(highT-lowT));
  Real B = (highlogg-logg)*(highT-T);
  Real C = (logg -lowlogg)*(highT-T);
  Real D = (highlogg-logg)*(T- lowT);
  Real E = (logg -lowlogg)*(T- lowT);
  for(i=0;i<ENUM;++i) {
    fluxInterp[i]=A*( fluxlogg1T1[i]*B + fluxlogg2T1[i]*C+
       fluxlogg1T2[i]*D + fluxlogg2T2[i]*E );
  }
  
  //..correct interpolated model spectrum for redshift
  for(i=0;i<ENUM;++i) {
    //***put this into previous loop to save time!
    Elow[i]  =  Elow[i]/(1.+z);
    Ehigh[i] = Ehigh[i]/(1.+z);
    fluxInterp[i] = fluxInterp[i]/(1.+z);
  }
  
  //..calculate flux in provided energy bins
  i=0;
  for(size_t k=0; k<N-1; ++k) { //..loop over energy bins in observed spectrum
    if(energy[k+1] < Ehigh[ENUM-1]) {
      while(Ehigh[i] < energy[k]) {i++;}
      if(Ehigh[i] < energy[k+1]) {
	flux[k] = fluxInterp[i]*(Ehigh[i]-energy[k])/(Ehigh[i]-Elow[i]);
	i++;
	while(Ehigh[i] < energy[k+1]) {
	  flux[k]+=fluxInterp[i];
	  i++;
	}
	flux[k] += fluxInterp[i]*(energy[k+1]-Elow[i])/(Ehigh[i]-Elow[i]);
      } else {
	flux[k] = fluxInterp[i]*(energy[k+1]-energy[k])/(Ehigh[i]-Elow[i]);
      }
      //..multiply flux by NSrad^2
      flux[k] = flux[k]*NSrad*NSrad*factor;
    } else { flux[k] = 0.; }
  }

  fluxError.resize(0);
  return;
}

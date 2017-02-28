
#ifndef HAVE_TABLEMODEL
#include "TableModel.h"
#endif

//-------------------------------------------------------------------------------
// Class TableModel

// default constructor

TableModel::TableModel()
{
}

// destructor

TableModel::~TableModel()
{
}

// write to a FITS file

void TableModel::write(string filename)
{

  vector<string> ttype;
  vector<string> tform;
  vector<string> tunit;

  // Create a new FITS file instance

  std::auto_ptr<FITS> pFits(0);

  try {                
    pFits.reset( new FITS(filename,Write) );
  } catch (FITS::CantCreate) {
    return;       
  }

  // Write the keywords to the primary header

  pFits->pHDU().addKey("HDUCLASS", "OGIP"," ");
  pFits->pHDU().addKey("HDUCLAS1", "XSPEC TABLE MODEL"," ");
  pFits->pHDU().addKey("HDUVERS", "1.0.0"," ");
  pFits->pHDU().addKey("MODLNAME", ModelName,"Table model name");
  pFits->pHDU().addKey("MODLUNIT", ModelUnits,"Table model units");
  pFits->pHDU().addKey("REDSHIFT", isRedshift,"Add redshift parameter?");
  pFits->pHDU().addKey("ADDMODEL", isAdditive,"Is model additive?");

  // Set up and create the PARAMETERS extension

  ttype.resize(10);
  tform.resize(10);
  tunit.resize(10);

  ttype[0] = "NAME";
  ttype[1] = "METHOD";
  ttype[2] = "INITIAL";
  ttype[3] = "DELTA";
  ttype[4] = "MINIMUM";
  ttype[5] = "BOTTOM";
  ttype[6] = "TOP";
  ttype[7] = "MAXIMUM";
  ttype[8] = "NUMBVALS";
  ttype[9] = "VALUE";

  tform[0] = "12A";
  tform[1] = "J";
  for (size_t i=2; i<8; i++) tform[i] = "E";
  tform[8] = "J";
  tform[9] = "PE";

  for (size_t i=0; i<10; i++) tunit[i] = " ";

  Table* pparam = pFits->addTable("PARAMETERS",Parameters.size(),ttype,tform,tunit);
  Table& param = *pparam;

  param.addKey("HDUCLASS", "OGIP"," ");
  param.addKey("HDUCLAS1", "XSPEC TABLE MODEL"," ");
  param.addKey("HDUCLAS2", "PARAMETERS"," ");
  param.addKey("HDUVERS", "1.0.0"," ");
  param.addKey("NINTPARM", NumIntParams,"Number of interpolation parameters ");
  param.addKey("NADDPARM", NumAddParams,"Number of additional parameters ");

  // write the parameter info

  size_t Nparams(Parameters.size());

  vector<string> names;
  for (size_t ipar=0; ipar<Nparams; ipar++) names.push_back(Parameters[ipar].Name);
  param.column("NAME").write(names,1);

  IntegerArray ivalues(Nparams);
  for (size_t ipar=0; ipar<Nparams; ipar++) ivalues[ipar] = Parameters[ipar].InterpolationMethod;
  param.column("METHOD").write(ivalues,1);

  RealArray rvalues(Nparams);
  for (size_t ipar=0; ipar<Nparams; ipar++) rvalues[ipar] = Parameters[ipar].InitialValue;
  param.column("INITIAL").write(rvalues,1);

  for (size_t ipar=0; ipar<Nparams; ipar++) rvalues[ipar] = Parameters[ipar].Delta;
  param.column("DELTA").write(rvalues,1);

  for (size_t ipar=0; ipar<Nparams; ipar++) rvalues[ipar] = Parameters[ipar].Minimum;
  param.column("MINIMUM").write(rvalues,1);

  for (size_t ipar=0; ipar<Nparams; ipar++) rvalues[ipar] = Parameters[ipar].Bottom;
  param.column("BOTTOM").write(rvalues,1);

  for (size_t ipar=0; ipar<Nparams; ipar++) rvalues[ipar] = Parameters[ipar].Top;
  param.column("TOP").write(rvalues,1);

  for (size_t ipar=0; ipar<Nparams; ipar++) rvalues[ipar] = Parameters[ipar].Maximum;
  param.column("MAXIMUM").write(rvalues,1);

  for (size_t ipar=0; ipar<Nparams; ipar++) ivalues[ipar] = Parameters[ipar].TabulatedValues.size();
  param.column("NUMBVALS").write(ivalues,1);

  vector<RealArray> pvalues;
  for (size_t ipar=0; ipar<Nparams; ipar++) pvalues.push_back(Parameters[ipar].TabulatedValues);
  param.column("VALUE").writeArrays(pvalues,1);

  // Create the ENERGIES extension

  ttype.resize(2);
  tform.resize(2);
  tunit.resize(2);

  ttype[0] = "ENERG_LO";
  tform[0] = "E";
  tunit[0] = " ";

  ttype[1] = "ENERG_HI";
  tform[1] = "E";
  tunit[1] = " ";

  Table* penergies = pFits->addTable("ENERGIES",Energies.size()-1,ttype,tform,tunit);
  Table& energies = *penergies;

  energies.addKey("HDUCLASS", "OGIP"," ");
  energies.addKey("HDUCLAS1", "XSPEC TABLE MODEL"," ");
  energies.addKey("HDUCLAS2", "ENERGIES"," ");
  energies.addKey("HDUVERS", "1.0.0"," ");

  // write the energies

  size_t Nenergies(Energies.size()-1);
  rvalues.resize(Nenergies);

  for (size_t ien=0; ien<Nenergies; ien++) rvalues[ien] = Energies[ien];
  energies.column("ENERG_LO").write(rvalues,1);

  for (size_t ien=0; ien<Nenergies; ien++) rvalues[ien] = Energies[ien+1];
  energies.column("ENERG_HI").write(rvalues,1);

  // Create the SPECTRA extension

  ttype.resize(2+NumAddParams);
  tform.resize(2+NumAddParams);
  tunit.resize(2+NumAddParams);

  stringstream RepeatStream;
  RepeatStream << NumIntParams;
  string Repeat(RepeatStream.str());

  ttype[0] = "PARAMVAL";
  tform[0] = Repeat+"E";
  tunit[0] = " ";

  RepeatStream.str("");
  RepeatStream << Nenergies;
  Repeat = RepeatStream.str();

  ttype[1] = "INTPSPEC";
  tform[1] = Repeat+"E";
  tunit[1] = " ";

  for (size_t iadd=1; iadd<=(size_t)NumAddParams; iadd++) {
    RepeatStream.str("");
    RepeatStream << iadd;
    Repeat = RepeatStream.str();
    if ( iadd < 10 ) {
      ttype[1+iadd] = "ADDSP00"+Repeat;
    } else if ( iadd < 100 ) {
      ttype[1+iadd] = "ADDSP0"+Repeat;
    } else if ( iadd < 1000 ) {
      ttype[1+iadd] = "ADDSP"+Repeat;
    }
    RepeatStream.str("");
    RepeatStream << Nenergies;
    Repeat = RepeatStream.str();
    tform[1+iadd] = Repeat+"E";
    tunit[1+iadd] = " ";
  }

  Table* pspectra = pFits->addTable("SPECTRA",Spectra.size(),ttype,tform,tunit);
  Table& spectra = *pspectra;

  spectra.addKey("HDUCLASS", "OGIP"," ");
  spectra.addKey("HDUCLAS1", "XSPEC TABLE MODEL"," ");
  spectra.addKey("HDUCLAS2", "MODEL SPECTRA"," ");
  spectra.addKey("HDUVERS", "1.0.0"," ");

  // Write the spectra

  size_t Nspectra(Spectra.size());
  vector<RealArray> rarray(Nspectra);

  if ( NumIntParams > 1 ) {
    for (size_t isp=0; isp<Nspectra; isp++) {
      rarray[isp].resize(NumIntParams);
      rarray[isp] = Spectra[isp].ParameterValues;
    }
    spectra.column("PARAMVAL").writeArrays(rarray,1);
  } else {
    rvalues.resize(Nspectra);
    for (size_t isp=0; isp<Nspectra; isp++) rvalues[isp] = Spectra[isp].ParameterValues[0];
    spectra.column("PARAMVAL").write(rvalues,1);
  }

  if ( Nenergies > 1 ) {
    for (size_t isp=0; isp<Nspectra; isp++) {
      rarray[isp].resize(Nenergies);
      rarray[isp] = Spectra[isp].Flux;
    }
    spectra.column("INTPSPEC").writeArrays(rarray,1);

    for (size_t iadd=1; iadd<=(size_t)NumAddParams; iadd++) {
      for (size_t isp=0; isp<Nspectra; isp++) {
	rarray[isp] = Spectra[isp].addFlux[iadd-1];
      }
      spectra.column(ttype[iadd+1]).writeArrays(rarray,1);
    }
  } else {
    rvalues.resize(Nspectra);
    for (size_t isp=0; isp<Nspectra; isp++) rvalues[isp] = Spectra[isp].Flux[0];
    spectra.column("INTPSPEC").write(rvalues,1);

    for (size_t iadd=1; iadd<=(size_t)NumAddParams; iadd++) {
      for (size_t isp=0; isp<Nspectra; isp++) rvalues[isp] = Spectra[isp].addFlux[iadd-1][0];
      spectra.column(ttype[iadd+1]).write(rvalues,1);
    }

  }  


  return;

}

//-------------------------------------------------------------------------------
// Class TableParameter

// default constructor

TableParameter::TableParameter()
{
}

// destructor

TableParameter::~TableParameter()
{
}

//-------------------------------------------------------------------------------
// Class TableSpectrum

// default constructor

TableSpectrum::TableSpectrum()
{
}

// destructor

TableSpectrum::~TableSpectrum()
{
}



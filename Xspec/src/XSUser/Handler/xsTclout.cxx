
#include <UserInterface/xstcl.h>

#include <XSstreams.h>
#include <XSContainer.h>
#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSFit/Fit/Fit.h>
#include <XSFit/Fit/FitMethod.h>
#include <XSFit/Fit/StatManager.h>
#include <XSFit/Fit/StatMethod.h>
#include <XSFit/MCMC/AsciiChain.h>
#include <XSFit/MCMC/FITSChain.h>
#include <XSFit/MCMC/ChainManager.h>
#include <XSFit/MCMC/MarginGrid.h>
#include <XSFit/Randomizer/RandomizerBase.h>
#include <XSUtil/FunctionUtils/FunctionUtility.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/GlobalContainer/ModelTypes.h>
#include <XSModel/GlobalContainer/ResponseContainer.h>
#include <XSModel/Data/SpectralData.h>
#include <XSModel/Data/BackCorr/Background.h>
#include <XSModel/Data/Detector/Response.h>
#include <XSModel/Data/Detector/RealResponse.h>
#include <XSModel/Data/Detector/MultiResponse.h>
#include <XSModel/Model/Model.h>
#include <XSModel/Model/EmissionLines/LineList.h>
#include <XSModel/Model/Component/Component.h>
#include <XSModel/Parameter/ModParam.h>
#include <XSModel/Parameter/ResponseParam.h>
#include <XSPlot/Plot/PlotDirector.h>
#include <XSUtil/Numerics/Numerics.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSUtil/Utils/XSstream.h>
#include <XSUtil/Parse/XSparse.h>

#include <math.h>
#include <memory>
#include <sstream>

int
XSGlobal::xsTclout(ClientData cdata,Tcl_Interp* interp,int objc, Tcl_Obj* CONST objv[] )
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   char XSPEC_TCLOUT[] = "xspec_tclout";
   bool resultsEntered = false;
   string results;
   int status = doTclout(rawArgs, resultsEntered, results);
   if (!status)
   {
      if (resultsEntered)
         Tcl_SetVar(interp, XSPEC_TCLOUT, results.c_str(), 0);                  
      return TCL_OK;
   }
   else
      return TCL_ERROR;
}

int
XSGlobal::doTclout(const StringArray& rawArgs, bool& resultsEntered, string& results)
{
   using namespace std;
   using namespace XSContainer;        

   enum SUBCOMS {HELP, AREASCAL, ARF, BACKGRND, BACKSCAL, CHAIN, CHATTER, 
		 COMPINFO, COSMO, COVARIANCE, DATAGRP, DATASETS, DOF, ENERGIES,
		 EQWIDTH, ERROR, EXPOS, FILENAME, FILEINFO, FLUX, FTEST, GAIN, 
		 GENPOP, GOODNESS, IDLINE, IGNORE, LUMIN, MARGIN, MODEL, MODCOMP, 
		 MODPAR, MODKEYVAL, MODVAL, NCHAN, NOTICED, NULLHYP, PARAM,
		 PEAKRSID, PINFO, PLINK, PLOT, PLOTGRP, QUERY, RATE, RERROR,
		 RESPONSE, SIGMA, SIMPARS, SOLAB, STAT, STATMETHOD, STEPPAR,
		 VARPAR, VERSION, WEIGHT, XFLT};
   static map<string, size_t> option;
   const string cmd("tclout");
   const size_t nArgs = rawArgs.size();
   resultsEntered = false;

   if (!option.size())
   {
      option["?"]          = HELP;
      option["areascal"]   = AREASCAL;
      option["arf"]        = ARF;
      option["backgrnd"]   = BACKGRND;
      option["backscal"]   = BACKSCAL;
      option["chain"]      = CHAIN;
      option["chatter"]    = CHATTER;
      option["compinfo"]   = COMPINFO;
      option["cosmo"]      = COSMO;
      option["covariance"] = COVARIANCE;
      option["datagrp"]    = DATAGRP;
      option["datasets"]   = DATASETS;
      option["dof"]        = DOF;
      option["energies"]   = ENERGIES;
      option["eqwidth"]    = EQWIDTH;
      option["error"]      = ERROR;
      option["expos"]      = EXPOS;
      option["filename"]   = FILENAME;
      option["fileinfo"]   = FILEINFO;
      option["flux"]       = FLUX;
      option["ftest"]      = FTEST;
      option["gain"]       = GAIN;
      option["genpop"]     = GENPOP;
      option["goodness"]   = GOODNESS;
      option["idline"]     = IDLINE;
      option["ignore"]     = IGNORE;
      option["lumin"]      = LUMIN;
      option["margin"]     = MARGIN;     
      option["model"]      = MODEL;
      option["modcomp"]    = MODCOMP;
      option["modkeyval"]  = MODKEYVAL;
      option["modpar"]     = MODPAR;
      option["modval"]     = MODVAL;
      option["nchan"]      = NCHAN;
      option["noticed"]    = NOTICED;
      option["nullhyp"]    = NULLHYP;
      option["param"]      = PARAM;
      option["peakrsid"]   = PEAKRSID;
      option["pinfo"]      = PINFO;
      option["plink"]      = PLINK;
      option["plot"]       = PLOT;
      option["plotgrp"]    = PLOTGRP;
      option["query"]      = QUERY;
      option["rate"]       = RATE;
      option["rerror"]     = RERROR;
      option["response"]   = RESPONSE;
      option["sigma"]      = SIGMA;
      option["simpars"]    = SIMPARS;
      option["solab"]      = SOLAB;
      option["stat"]       = STAT;
      option["statmethod"] = STATMETHOD;
      option["steppar"]    = STEPPAR;
      option["varpar"]     = VARPAR;
      option["version"]    = VERSION;
      option["weight"]     = WEIGHT;
      option["xflt"]       = XFLT; 
   }

   if (nArgs == 1)  
   {
      XSutility::printValidOptions(tcout, cmd, option);
      return 0;
   }

   string inputOpt(rawArgs[1]);
   map<string,size_t>::const_iterator itOpt(option.lower_bound(inputOpt));
   map<string,size_t>::const_iterator itOptEnd(option.end());

   if (itOpt != itOptEnd && itOpt->first.find(inputOpt) == 0)
   {
      const size_t selected = itOpt->second;
      const string& opt = itOpt->first;
      size_t specNum=0;
      string outToTcl;
      std::ostringstream ss;
      ss << std::setprecision(10);

      switch (selected)
      {
         case HELP:  // ?
            XSutility::printValidOptions(tcout, cmd, option);
            break;
         case AREASCAL: // areascal n s|b (default to s)
         case BACKSCAL: // backscal n s|b
         case EXPOS:    // exposure n s|b
            if (!HandlerUtils::getSpecNumParameter(rawArgs, opt, 3, specNum))
            {
               return -1;
            }
            else
            {
               const SpectralData* sd = datasets->lookup(specNum);
               bool isBackground = false;
               const RealArray* pScaleArray = 0;
               Real exposTime = -999.0;
               if (nArgs > 3)
               {
                  string sOrB(rawArgs[3]);
                  if (sOrB[0] == 'b' || sOrB[0] == 'B')
                        isBackground = true;
               }
               if (isBackground)
               {
                  const Background* back = sd->background();
                  if (!back)
                  {
                     tcerr << "No background data exists for spectrum " 
                           << specNum << std::endl;
                     return -1;
                  }
                  const SpectralData* bsd = back->data();
                  if (selected == AREASCAL|| selected == BACKSCAL)
                  {
                     pScaleArray = (selected == AREASCAL) ?
                        &bsd->areaScale() : &bsd->backgroundScale();
                  }
                  else
                  {
                     exposTime = bsd->exposureTime();
                  }
               }
               else
               {
                  if (selected == AREASCAL|| selected == BACKSCAL)
                  {
                     pScaleArray = (selected == AREASCAL) ?
                        &sd->areaScale() : &sd->backgroundScale();
                  }
                  else
                  {
                     exposTime = sd->exposureTime();
                  }
               }

               if (exposTime >= .0)
               {
                  ss << exposTime;
               }
               else
               {
                  size_t sz = pScaleArray->size();
                  for (size_t i=0; i<sz; ++i)
                  {
                     ss << (*pScaleArray)[i] << "   ";
                  }
               }
            }            
            break;   
         case ARF: // arf n
            if (!HandlerUtils::getSpecNumParameter(rawArgs, opt, 3, specNum))
            {
               return -1;               
            }
            else
            {
               SpectralData* sd = datasets->lookup(specNum);
               string arfNames("");
               const std::vector<Response*>& resps = sd->detector();
               for (size_t i=0; i<resps.size(); ++i)
               {
                  const RealResponse* rrsp = dynamic_cast<RealResponse*>(resps[i]);
                  if (rrsp)
                  {
                     arfNames += rrsp->arfName();
                     arfNames += " ";
                  }
                  else
                  {
                     const MultiResponse* mrsp = dynamic_cast<MultiResponse*>(resps[i]);
                     if (mrsp)
                     {
                        const StringArray& arfs = mrsp->arfNames();
                        for (size_t j=0; j<arfs.size(); ++j)
                        {
                           arfNames += arfs[j];
                           arfNames += " ";
                        }
                     }
                  }
               }
               ss << arfNames;
            }
            break;
	 case BACKGRND: // backgrnd n
            if (!HandlerUtils::getSpecNumParameter(rawArgs, opt, 3, specNum))
            {
               return -1;
            }
            else
	    {
	       SpectralData* sd = datasets->lookup(specNum);
	       const string& backFile = sd->backgroundFile();
               ss << backFile;
	    }
	    break;
         case CHAIN: // last|best|proposal|stat
            {
               const string chainLast("last");
               const string chainBest("best");
               const string chainProposal("proposal");
               const string chainStat("stat");
               if (nArgs > 2)
               {
                  string secondOpt = XSutility::lowerCase(rawArgs[2]);
                  if (chainLast.find(secondOpt)==0 || chainBest.find(secondOpt)==0)
                  {
		     RealArray parVals;
		     Real statVal;
		     try {
		       if ( chainLast.find(secondOpt)==0 ) {
			 fit->chainManager()->getLastPoint(parVals);
		       } else {
			 fit->chainManager()->findBestFit(parVals, statVal);
		       }
		       if ( parVals.size() == 0 ) {
			 tcerr << "***Error: No chains are loaded." << std::endl;
			 return -1;
		       }
		     } catch (YellowAlert&) {
		       tcerr << "***Error: Failed to read chain." << std::endl;
		       return -1;
		     }
		     for (size_t i=0; i<parVals.size(); ++i)
		       ss << parVals[i] <<"  ";
                  }
                  else if (chainProposal.find(secondOpt)==0) // distribution|matrix
                  {
                     const string chainDist("distribution");
                     const string chainMatrix("matrix");
                     if (nArgs > 3)
                     {
                        string infoSpec = XSutility::lowerCase(rawArgs[3]);
                        if (chainDist.find(infoSpec)==0)
                        {
                           ss << fit->chainManager()->chainProposal()->name();
                        }
                        else if (chainMatrix.find(infoSpec)==0)
                        {
                           const RealArray* pCovarMatrix = 
                              fit->chainManager()->chainProposal()->covariance(fit);
                           if (pCovarMatrix)
                           {
                              const RealArray& covarMatrix = *pCovarMatrix;
                              // For consistency with chain prop covariance input,
                              // only print out the lower half and diagonal.
                              // We're going to ASSUME this is a square matrix.
                              const size_t nPar = static_cast<size_t>
                                      (sqrt(static_cast<Real>(covarMatrix.size())));
                              for (size_t i=0; i<nPar; ++i)
                              {
                                 for (size_t j=0; j<=i; ++j)
                                 {
                                    ss << covarMatrix[i*nPar+j] << " ";
                                 }
                              }
                           }
                        }
                        else
                        {
                           tcerr << "Valid chain proposal args: distribution|matrix"
                              << std::endl;
                           return -1;
                        } 
                     }
                     else
                     {
                        tcerr << "Valid chain proposal args: distribution|matrix"
                           <<std::endl;
                        return -1;
                     } 
                  } // end if chain proposal 
                  else if (chainStat.find(secondOpt)==0)
                  {
                     ss << fit->chainManager()->lastStatCalc();
                  } // end if chain stat
                  else
                  {
                     tcerr << "Valid chain options: last | proposal | stat" << std::endl;
                     return -1;
                  }
               } // end if nArgs > 2
               else
               {
                  tcerr << "Valid chain options: best | last | proposal | stat" << std::endl;
                  return -1;
               }
            }          
            break;
	 case CHATTER: // chatter
	    {
               ss << tpout.consoleChatterLevel() << " " 
                  << tpout.logChatterLevel();
	    }
	    break;
         case COMPINFO: // compinfo <modelName>:n <dataGroup n>
            {
               string modName;
               size_t idx = string::npos;
               const Component* comp = 0;
               if (HandlerUtils::verifyStringInt<Component>(rawArgs, 3, modName, idx))
               {
                  comp = HandlerUtils::lookupStringIntObj<Component>(modName, idx);
               }
               if (!comp)
               {
                  return -1;
               }
               else
               {
                  size_t dgNum = 1;
                  size_t groupPos = 0;
                  if (nArgs > 3)
                  {
                     size_t tmpNum = XSutility::isInteger(rawArgs[3]);
                     if (tmpNum != string::npos)
                     {
                        dgNum = tmpNum;
                     }                     
                  }
                  const Model* mod = HandlerUtils::getModFromName(modName);
                  // If the earlier call to retrieve the component
                  // succeeded, there should be no way for this to
                  // fail.  Still, just to be safe.....
                  if (!mod)
                  {
                     return -1;
                  }
                  if (!mod->isActive())
                  {
                     if (dgNum > 1)
                     {
                        tcerr << "Invalid data group number specified."
                           <<"\n Model is currently not active" << std::endl;
                        return -1;
                     }
                     groupPos = 1;
                  }
                  else
                  {
                     std::map<size_t,std::map<size_t,size_t> >::const_iterator 
                           itSource = datasets->sourceToDgs().find(mod->sourceNumber());
                     if (itSource != datasets->sourceToDgs().end())
                     {
                        std::map<size_t,size_t>::const_iterator itGroup = 
                                itSource->second.find(dgNum);
                        if (itGroup != itSource->second.end())
                        {
                           groupPos = itGroup->second;
                        }
                     }
                     if (!groupPos)
                     {
                        tcerr << "Invalid data group number specified for model." 
                           << std::endl;
                        return -1;
                     }
                  }
                  size_t nTotPar = mod->numberOfParameters();
                  const std::vector<Parameter*>& compPars = comp->parameterSet();
                  size_t nPar = compPars.size();
                  size_t firstPar = 0;
                  if (nPar)
                  {
                     firstPar = compPars[0]->index();
                     firstPar += (groupPos-1)*nTotPar;
                  }
                  ss << comp->name() << " " << firstPar << " " <<nPar;
               }
            }
            break;
         case COSMO:  // cosmo
            ss << models->cosmo().H0 << "   " << models->cosmo().q0
               << "   " << models->cosmo().lambda0;
            break;
         case COVARIANCE:  // covariance [m, n]
            {
               const RealArray& covar = fit->fitMethod()->covariance();
               size_t sz = covar.size();
               if (sz)
               {
                  // Just assumes covar is NxN or empty.
                  size_t N = static_cast<size_t>(sqrt(static_cast<Real>(sz)));
                  if (nArgs > 2)
                  {
                     StringArray inArgs, outArgs;
                     IntegerArray dummy;
                     for (size_t i=2; i<nArgs; ++i)
                     {
                        inArgs.push_back(rawArgs[i]);
                     }
                     XSparse::collectParams(inArgs, dummy, outArgs);
                     if (outArgs.size() < 2)
                     {
                       tcerr << "2 indices are required to specify covariance element (m, n)"<<endl;
                       return -1;
                     }
                     size_t m = XSutility::isInteger(outArgs[0]);
                     size_t n = XSutility::isInteger(outArgs[1]);
                     if (m == string::npos || n == string::npos)
                     {
                        tcerr << "Improper covariance index specifier." << endl;
                        return -1;
                     }
                     if (m == 0 || n == 0 || m > N || n > N)
                     {
                        tcerr << "Covariance index is out of range."
                          << "  Valid range is 1 - " << N << endl;
                        return -1;
                     }
                     ss << covar[(m-1)*N + n-1];
                  }
                  else
                  {
                     for (size_t i=0; i<N; ++i)
                     {
                        for (size_t j=0; j<=i; ++j)
                        {
                           ss << covar[i*N+j] << " ";
                        }
                     }
                  }
               }
               else
               {
                  tcerr << "No covariance is currently calculated." << endl;
                  return -1;
               }
            }
            break;
	 case DATAGRP: // datagrp [n]
            if (nArgs < 3)
            {
               ss << datasets->numberOfGroups();
            }
            else
            {
               if (!HandlerUtils::getSpecNumParameter(rawArgs, opt, 3, specNum))
               {
                  return -1;
               }
               else
               {
                  size_t dummyRow=0;
                  const DataSet* ds = datasets->dataSetLookup(specNum, dummyRow);
                  ss << ds->dataGroup();
               }
            }
            break;
	 case DATASETS: // datasets	    
            ss << datasets->numberOfSpectra();
            break;
         case DOF: // dof
            {
               std::pair<int,size_t> dofVals = 
                        fit->statManager()->totalDegreesOfFreedom();
               ss << dofVals.first << " " << dofVals.second;
            }
            break;
         case ENERGIES: // energies n
            if (models->autonomousEnergy().size())
            {
               const RealArray& engs = models->autonomousEnergy();
               for (size_t i=0; i<engs.size(); ++i)
               {
                  ss << engs[i] << "  ";
               }
            }
            else
            {
               if ((!datasets->numberOfSpectra() && nArgs == 2) || (nArgs > 2
                           && rawArgs[2][0] == '0' ))
               {
                  // use singleton dummy response
                  const Response* dummyrsp = responses->responseList(DUMMY_RSP,1);
                  if (dummyrsp)
                  {
                     const RealArray& engs = dummyrsp->energies();
                     for (size_t i=0; i<engs.size(); ++i)
                     {
                        ss << engs[i] << "   ";
                     }
                  }               
               }
               else
               {
                  if (!HandlerUtils::getSpecNumParameter(rawArgs, opt, 3, specNum))
                  {
                     return -1;
                  }
                  else
                  {
                     // Hardcoding this to 1 until multiple sources are 
                     // implemented for this option.
                     const size_t sourceNum = 1;
                     // If extended energies are in use, energy arrays must
                     // come from the model (if it exists) since responses 
                     // don't know about extensions.  
                     const SpectralData* sd = datasets->lookup(specNum);
                     const Response* rsp = sd->detector(sourceNum-1);
                     if (rsp)
                     {
                        const RealArray *engs=0;
                        RealArray ext;
                        if (models->isExtended())
                        {
                           const Model* mod = models->lookup(rsp);
                           if (mod)
                           {
                               ArrayContainer::const_iterator itEngs = 
                                        mod->energy().find(specNum);
                               engs = &(itEngs->second);
                           }
                           else
                           {
                              ext.resize(rsp->energies().size());
                              ext = rsp->energies();
                              Model::makeExtendArray(models->extendedEnergy(), ext);
                              engs = &ext;                                        
                           }
                        }
                        else
                        {
                           engs = &rsp->energies();
                        }
                        for (size_t i=0; i<engs->size(); ++i)
                        {
                           ss << (*engs)[i] << "   ";
                        }
                     }
                     else
                     {
                        tcerr << "No response associated with spectrum "
                           << specNum << " source 1." << std::endl;
                        return -1;
                     }
                  }
               }
            }            
            break;   
         case EQWIDTH: // eqwidth n [errsims]
            if (!HandlerUtils::getSpecNumParameter(rawArgs, opt, 3, specNum))
            {
               return -1;               
            }
            else
            {
               if (nArgs > 3)
               {
                  string lastArg = XSutility::lowerCase(rawArgs[3]);
                  if (lastArg == string("errsims"))
                  {
                     const SpectralData* sd = datasets->lookup(specNum);
                     const SpectralData::FluxCalc& eqWidth = sd->lastEqWidthCalc();
                     const size_t nVals = eqWidth.errorTrialVals.size();
                     for (size_t i=0; i<nVals; ++i)
                     {
                        ss << eqWidth.errorTrialVals[i] << "  ";
                     }                     
                  }
                  else
                  {
                     tcerr << "Unrecognized argument for tclout eqwidth: "
                        << rawArgs[3] << "\n   Did you mean to use 'errsims'?"
                        << std::endl;
                     return -1;
                  }
               }
               else
               {               
                  const SpectralData* sd = datasets->lookup(specNum);
                  const SpectralData::FluxCalc& eqWidth = sd->lastEqWidthCalc();
                  ss << eqWidth.value << " " << eqWidth.errLow << " " 
                     << eqWidth.errHigh;
               }
            }
            break;
         case RERROR: // rerror <sourceNum:>n
         case ERROR:  // error <modelName:>n
         {
            const Parameter* param=0;
            if (selected == ERROR)
            {
               param = HandlerUtils::getFromStringInt<Parameter>(rawArgs);
            }
            else
            {
               int dummySource=1;
               int dummyIdx=1;
               if (rawArgs.size() < 3)
               {
                  tcerr << "Must specify a response parameter <n> or <sourceNum>:<n>"
                     << std::endl;
                  return -1;
               }
               try
               {
                  param = HandlerUtils::getFromIntPair<ResponseParam>
                                (rawArgs[2], dummySource, dummyIdx);
                  if (!param)
                     tcerr <<"Cannot locate response parameter " 
                           << rawArgs[2] << std::endl;
               }
               catch (YellowAlert&)
               {
                  return -1;
               }
            }
            if (!param)
            {
               return -1;
            }
            const ModParam* mPar = dynamic_cast<const ModParam*>(param);
            if (!mPar)
            {
               tcerr << " Parameter is not a fit parameter." << std::endl;
               return -1;
            }
            ss << mPar->emn() << " " << mPar->epe() << "  "
                  << mPar->lastErrorStatus();
         }
            break;
         case FILENAME: // filename n
            if (!HandlerUtils::getSpecNumParameter(rawArgs, opt, 3, specNum))
            {
               return -1;
            }
            else
            {
               size_t dum=0;
               const DataSet* ds = datasets->dataSetLookup(specNum, dum);
               string fileName = ds->getFullPathName();
               ss << fileName;
            }
            break;
         case FILEINFO: // fileinfo keyword n
	    if (!datasets->numberOfSpectra()) {
	      tcerr << "No data read in" << std::endl;
	      return -1;
	    } else {
	      if (nArgs < 3) {
		tcerr << "You need to specify a keyword" << std::endl;
		return -1;
	      }
	      if (datasets->numberOfSpectra() > 1 && nArgs < 4) {
		tcerr << "More than one spectrum has been read in."
		      << " You must specify the spectrum number" << std::endl;
		return -1;
	      }
	      if (!HandlerUtils::getSpecNumParameter(rawArgs, opt, 4, specNum)) return -1;

	      string keyName = XSutility::upperCase(rawArgs[2]);
	      size_t dum=0;
	      const DataSet* ds = datasets->dataSetLookup(specNum, dum);
	      string fileName = ds->getFullPathName();
	      // open file and go to the SPECTRUM extension. We know this must work
	      using namespace CCfits;
	      string extName("SPECTRUM");
	      std::auto_ptr<FITS> pInfile(new FITS(fileName,Read,extName));
	      string keyValue("");
	      try {
		pInfile->extension(extName).readKey(keyName, keyValue);
	      } catch(...) {
		tcerr << "The keyword " << keyName 
		      << " cannot be found in the SPECTRUM extension of " 
		      << fileName << std::endl;
		return -1;
	      }
	      ss << keyValue;
	    }
            break;
         case LUMIN:
         case FLUX: // flux|lumin [n] [errsims]
         {
            const bool isFlux = (selected == FLUX);
            bool doErrsims = false;
            if (nArgs > 3)
            {
               string lastArg = XSutility::lowerCase(rawArgs[3]);
               if (lastArg == string("errsims"))
                  doErrsims = true;
               else
               {
                  tcerr << "Unrecognized argument for tclout "<<opt<<": "
                     << rawArgs[3] << "\n   Did you mean to use 'errsims'?"
                     << std::endl;
                  return -1;
               }
            }
	    if(datasets->numberOfSpectra())
	    {
               if (datasets->numberOfSpectra() > 1 && nArgs == 2)
               {
                  tcerr << "***XSPEC Error: More than 1 spectrum is currently loaded."
                    <<"\n   Must specify spectrum number n: \"tclout "<<opt<<" <n>\"" 
                    <<std::endl;
                  return -1;
               }  
	       if (!HandlerUtils::getSpecNumParameter(rawArgs, opt, 3, specNum))
               {
                  return -1;
               }
	       const SpectralData* sd = datasets->lookup(specNum);
	       const size_t nFlux = isFlux ? sd->lastModelFluxCalc().size() :
                                sd->lastModelLuminCalc().size();
	       for (size_t i=0; i<nFlux; ++i)
	       {
		  const SpectralData::FluxCalc& flux = isFlux ?
                        sd->lastModelFluxCalc(i) : sd->lastModelLuminCalc(i);
                  if (doErrsims)
                  {
                     const size_t nVals = flux.errorTrialVals.size();
                     for (size_t iErrs=0; iErrs<nVals; ++iErrs)
                        ss << flux.errorTrialVals[iErrs] << " ";
                  }
                  else
                  {
		     ss << flux.value <<" "<< flux.errLow <<" "<< flux.errHigh <<"  "
                        << flux.photonValue <<" "<< flux.photonLow <<" "
                        << flux.photonHigh <<" ";
                  }
	       }
	       if (!nFlux && !doErrsims)
               {
		   ss << "0.  0.  0.  0.  0.  0.";
               }
	    } // end if spectra loaded
	    else
	    {
               if (doErrsims)
               {
                  tcerr << "No error simulations saved - no spectra currently loaded."
                     << std::endl;
                  return -1;
               }
	       //get vector of Model pointers
	       std::vector<Model*> mods = XSutility::values<
		   std::map<string, Model*>, Model*>(models->modelSet().begin(),
						     models->modelSet().end());

	       if(mods.size())
	       {
		   //sort based on sourcenumber using binary predicate CompareSource()
		   std::sort(mods.begin(), mods.end(), HandlerUtils::CompareSource());

		   std::vector<Model*>::iterator
		       begMods = mods.begin(), endMods = mods.end();

		   while(begMods != endMods)
		   {
		       if(models->activeModelNames((*begMods)->name()))
		       {
			   Model* m = *begMods;

			   const SpectralData::FluxCalc& lumin = isFlux ?
                                  m->lastModelFluxCalc() : m->lastModelLuminCalc();
			   ss << lumin.value <<" "<< lumin.errLow <<" "<< lumin.errHigh<<"  "
                             << lumin.photonValue <<" "<< lumin.photonLow <<" "
                             << lumin.photonHigh <<" ";
		       }
		       ++begMods;
		   }
	       }
	       else
	       {
		   tcerr << "No spectra are loaded: Unable to process  " << opt 
		         << " option" << std::endl;

		   return -1;
	       }
	    } // end if no spectra loaded
         }  // end tclout flux
	     break;
         case FTEST: // ftest
            ss << Fit::lastFtest();
            break;
         case GAIN: // gain [<sourceNum>:]<specNum> slope | offset
            if (!datasets->numberOfSpectra())
            {
               tcerr<<"No Spectra are loaded: Unable to process gain option" <<std::endl;
               return -1;
            }
            if (nArgs > 2)
            {
               StringArray inArgs, outArgs;
               IntegerArray dummy;
               for (size_t i=2; i<nArgs; ++i)
               {
                  inArgs.push_back(rawArgs[i]);
               }
               XSparse::collectParams(inArgs, dummy, outArgs);
               if (outArgs.size() < 2)
               {
                 tcerr << "Option requires 2 arguments: [<sourceNum>:]<specNum> and \"slope\" or \"offset\""<<endl;
                 return -1;
               }
               string indexPair(outArgs[0]);
               const string parName(XSutility::lowerCase(outArgs[1]));
               int sourceNum=1;
               int specNum=0;
               if (!XSparse::integerPair(indexPair,sourceNum,specNum))
               {
                  tcerr<<"First argument must be of form [<sourceNum>:]<specNum>"<<endl;
                  return -1;
               }
               if (specNum == -1)  // only 1 num was entered
               {
                  specNum = sourceNum;
                  sourceNum = 1;
               }
               if (specNum < 1 || 
                        static_cast<size_t>(specNum) > datasets->numberOfSpectra())
               {
                  tcerr << "***XSPEC Error: "<<specNum <<" is not a valid spectrum number"<<endl;
                  return -1;
               }
               if (sourceNum < 1 || 
                        static_cast<size_t>(sourceNum) > datasets->numSourcesForSpectra())
               {
                  tcerr << "***XSPEC Error: "<<sourceNum<<" is not a valid source number"<<endl;
                  return -1;
               }
               Response* resp=datasets->lookup(specNum)->detector(sourceNum-1);
               if (!resp)
               {
                  tcerr<<"Spectrum "<<specNum<<" has no response for source "
                      << sourceNum <<endl;
                  return -1;
               }

               const string SLOPE("slope");
               const string OFFSET("offset");
               bool isSlope = false;
               if (SLOPE.find(parName) == 0)
                  isSlope = true;
               else if (OFFSET.find(parName) != 0)
               {
                  tcerr << "Second argument to tclout gain must be \"slope\" or \"offset\""<<endl;
                  return -1;
               }
               // Input validation completed

               const Parameter* fitPar = isSlope ? resp->getLinearGain() :
                                        resp->getConstGain();
               if (fitPar)
               {
                  const string delim(" ");
                  const string fullString = fitPar->stringVal();
                  string valueString;
                  // 6 values for ModParams/RespParams, 1 for Switch and Scale.
                  const size_t nVals = 6;
                  string::size_type iPos = 0;
                  for (size_t i=0; i<nVals; ++i)
                  {
                     iPos = fullString.find_first_not_of(delim, iPos);
                     iPos = fullString.find_first_of(delim, iPos);
                  }
                  valueString = fullString.substr(0, iPos); 
                  ss << valueString;
               }
               else
               {
                  ss << (isSlope ? resp->gainFactor()[1] : resp->gainFactor()[0]);
               }
            }
            else
            {
               tcerr << "Option requires 2 arguments: [<sourceNum>:]<specNum> and \"slope\" or \"offset\""<<endl;
               return -1;
            }
            break;
         case GOODNESS: //  goodness [sims]
	    if ( nArgs < 3 ) {
	      ss << Fit::lastGoodness();
            } else {
	      RealArray Sims(Fit::goodnessSims());
	      for (size_t i=0; i<Sims.size(); i++) ss << Sims[i] << " ";
	    }
            break;
         case IDLINE: // idline e d
            if (nArgs < 4)
            {
               tcerr << "Option requires 2 arguments: idline energy del(energy)"
                     << std::endl;
               return -1; 
            }
            else
            {
               RealArray input(.0,2);
               for (size_t i=0; i<2; ++i)
               {
                  string arg(rawArgs[i+2]);
                  if (!XSutility::isReal(arg, input[i]))
                  {
                     tcerr << "Argument is not a number" << std::endl;
                     return -1;
                  }
               }
               Real lowE = input[0] - input[1];
               Real highE = input[0] + input[1];
               LineList *lineList = 0;
               try
               {
                  lineList = LineList::get("bearden");
                  if (lineList)
                  {
                     std::ostringstream ssTmp;
                     lineList->initialize(lowE, highE, .0, .0, false);
                     // This can throw:
                     lineList->showList(ssTmp);
                     outToTcl = ssTmp.str();                     
                  }
                  else
                  {
                     tcerr << "***Warning: \"bearden\" is an unrecognized list name."
                            << std::endl;
                  }
                  lineList = LineList::get("mekal");
                  if (lineList)
                  {
                     std::ostringstream ssTmp;
                     lineList->initialize(lowE, highE, .0, .0, false);
                     // This can throw:
                     lineList->showList(ssTmp);
                     outToTcl += ssTmp.str();                     
                  }
                  else
                  {
                     tcerr << "***Warning: \"mekal\" is an unrecognized list name."
                            << std::endl;
                  }
                  ss << outToTcl;
               }
               catch (YellowAlert&)
               {
                  delete lineList;
                  return -1;
               }
               delete lineList;
            }
            break;
         case IGNORE: //ignore [<specNum>]
            if (!HandlerUtils::getSpecNumParameter(rawArgs, opt, 3, specNum))
            {
               return -1;               
            }
            else
            {
               const SpectralData* sd = datasets->lookup(specNum);
               const BoolArray& noticedChans = sd->noticedChannels();
               const size_t offset = sd->startChan() - sd->firstChan();
               const size_t nChans = sd->channels();
               std::vector<size_t> ignoredChans;
               for (size_t i=0; i<nChans; ++i)
               {
                  if (!noticedChans[i+offset])
                     ignoredChans.push_back(i+1);
               }
               ss << XSparse::ArrayToRange(ignoredChans);               
            }
            break;
         case MODEL: //model
            if (!XSContainer::models->modelSet().empty())
            {
               const ModelMap& mods = XSContainer::models->modelSet();
               bool isFirst=true;
               ModelMapConstIter mod = mods.begin();
               ModelMapConstIter modEnd = mods.end();
               while (mod != modEnd)
               {
                  if (!isFirst)
                     ss << " ";
                  if (mod->second->name() != Model::DEFAULT())
                  {
                     ss << mod->second->name() << ": ";
                  }
                  // Deliberately not sending a newline char.  This was
                  // requested in order to do things like:
                  // editmod pha([tcloutr model])
                  ss << mod->second->fullExpression() << std::flush;
                  isFirst=false;
                  ++mod;
               }
            }
            break;
         case MODCOMP: // modcomp name
            {
               const Model* mod = HandlerUtils::getModFromArg(rawArgs);
               if (!mod)
                  ss << 0;
               else
                  ss << mod->numberOfComponents();
            }
            break;
         case MODKEYVAL: // modkeyval [?|keyword]
            {
	      // value of keyword in model function database
	      // if no argument is given or if it is ? then output a list
	      // of the available keywords
	      string keyword = "?";
	      if ( nArgs > 2 ) {
		keyword = XSutility::lowerCase(rawArgs[2]);
	      }
	      if ( keyword == "?" ) {
		string keywords = FunctionUtility::getDbKeywords();
		if ( keywords.length() == 0 ) {
		  ss << "There are no keywords defined yet.";
		} else {
		  ss << keywords;
		}
	      } else {
		double value = FunctionUtility::getDbValue(keyword);
		if ( value != BADVAL ) {
		  ss << value;
		} else {
                  tcerr << "***XSPEC Error: " << keyword << " does not exist in the database." << endl;
                  return -1;
		}
	      }

            }
            break;
         case MODPAR:  // modpar name
            {
               const Model* mod = HandlerUtils::getModFromArg(rawArgs);
               if (!mod)
                  ss << 0;
               else
               {
                  size_t numPar = mod->numberOfParameters();
                  if (mod->isActive())
                  {
                     numPar *= datasets->getNumberOfGroupsForSource(mod->sourceNumber());
                  }
                  ss << numPar;
               }
            }
            break;
         case MODVAL:  // modval [<specNum> [<modName>]]
         {   
            // First just check if model is inactive.  If so, specNum
            // (or lack thereof) is irrelevant.  Its flux is binned
            // by the singleton dummyrsp energies.
            string modName(Model::DEFAULT());
            string errMsg("Default (unnamed) model");
            // Syntax doesn't allow for <modName> without <specNum>.
            if (nArgs >=4)
            {
               modName = rawArgs[3];
               errMsg = string("Model ") + modName;
            }
            else if (nArgs == 3)
            {
               specNum = XSutility::isInteger(rawArgs[2]);
               if (specNum == string::npos)
               {
                  tcerr << "***XSPEC Error: First argument to tclout modval must be a spectrum number."<<endl;
                  return -1;
               }
            }
            const Model* mod = models->lookup(modName);
            if (!mod)
            {
               tcerr << errMsg << "not found." << endl;
               return -1;
            }
            bool useDummyResp = false;
            if (!mod->isActive())
            {
               useDummyResp = true;
               specNum = 0;
            }

            // getSpecNumParameter repeats some of the work we did above,
            // but puts specNum through some additional testing.
            if (!useDummyResp && !HandlerUtils::getSpecNumParameter(rawArgs, opt, 3, specNum))
            {
               return -1;               
            }
            else
            {
               size_t dummyRow=0;
               size_t dgNum = useDummyResp ? 0 :
                  datasets->dataSetLookup(specNum, dummyRow)->dataGroup();
               mod = models->lookup(modName, dgNum);
               if (!mod)
               {
                  // Can't get in here for dummyrsp case, it was already
                  // checked above.
                  tcerr << errMsg << " not found associated with spectrum " 
                        << specNum << endl;
                  return -1;
               }
               try 
               {
                  const RealArray& modFlux = mod->modelFlux(specNum);
                  size_t nE = modFlux.size();
                  for (size_t i=0; i<nE; ++i)
                  {
                     ss << modFlux[i] << "  ";
                  }
               }
               catch (YellowAlert&)
               {
                  tcerr << errMsg << " not found associated with spectrum " 
                        << specNum << endl;
                  return -1;
               }
            }
         }
            break;
         case NCHAN: // nchan <specNum>
            if (!HandlerUtils::getSpecNumParameter(rawArgs, opt, 3, specNum))
            {
               return -1;               
            }
            else
            {
               const SpectralData* sd = datasets->lookup(specNum);
               ss << sd->channels();
            }
            break;
         case NOTICED: // noticed [<specNum>] | energy [<specNum>]
         {
            const string enString("energy");
            string enTest;
            if (nArgs > 2)
               enTest = XSutility::lowerCase(rawArgs[2]);
            if (nArgs > 2 && enString.find(enTest) == 0)
            {
               // output noticed energy ranges
               if (!HandlerUtils::getSpecNumParameter(rawArgs, opt, 4, specNum))
               {
                  return -1;               
               }
               else
               {
                  const SpectralData* sd = datasets->lookup(specNum);
                  const BoolArray& noticedChans = sd->noticedChannels();
                  std::vector<Response*>::const_iterator itDet = 
                        sd->detector().begin();
                  std::vector<Response*>::const_iterator itDetEnd = 
                        sd->detector().end();
                  const Response* firstResp=0;
                  while (!firstResp && itDet != itDetEnd)
                  {
                     if (*itDet)
                        firstResp = *itDet;
                     ++itDet;
                  }
                  if (!firstResp)
                  {
                     tcerr << "\nNo response associated with spectrum " << specNum
                        <<".  Cannot convert channels to energies." << std::endl;
                     return -1;               
                  }
                  // It's still possible that this could be a dummy resp without
                  // channel energies.  In that case eMin and eMax will not have
                  // been sized to nChans.
                  const RealArray& eMin = firstResp->eboundsMin();
                  const RealArray& eMax = firstResp->eboundsMax();
                  const size_t nChans = sd->channels();
                  if (eMin.size() != nChans)
                  {
                     tcerr << "\nNo channel energies are associated with the response assigned to the spectrum."
                          << std::endl;
                     return -1;
                  }
                  const size_t offset = sd->startChan()-sd->firstChan();
                  bool isPrevNoticed = false;
                  bool isWave = (XSContainer::plot->setplot().xOption()
                                        == WAVELENGTH);
                  Real startRange=0.0, endRange=0.0;
                  for (size_t i=0; i<nChans; ++i)
                  {
                     if (noticedChans[i+offset])
                     {
                        if (!isPrevNoticed)
                        {
                           startRange = eMin[i];
                           if (isWave)
                           {
                              startRange = std::max(SMALL,startRange);
                              startRange = Numerics::KEVTOA/startRange;
                           }
                           ss << startRange <<"-";
                        }
                        endRange = eMax[i];
                        isPrevNoticed = true;
                     }
                     else
                     {
                        if (isPrevNoticed)
                        {
                           if (isWave)
                           {
                              endRange = std::max(SMALL,endRange);
                              endRange = Numerics::KEVTOA/endRange;
                           }
                           ss << endRange <<" ";
                        }
                        isPrevNoticed = false;
                     }
                  }
                  if (isPrevNoticed)
                  {
                     if (isWave)
                     {
                        endRange = std::max(SMALL,endRange);
                        endRange = Numerics::KEVTOA/endRange;
                     }
                     ss << endRange;
                  }
               }  
            }
            else
            {
               // output noticed channel numbers
               if (!HandlerUtils::getSpecNumParameter(rawArgs, opt, 3, specNum))
               {
                  return -1;               
               }
               else
               {
                  std::valarray<size_t> indirectNotice;
                  const SpectralData* sd = datasets->lookup(specNum);
                  sd->buildIndirectNotice(indirectNotice);
                  size_t n = indirectNotice.size();
                  // Using a for loop rather than just valarray += 1 due to some
                  // unexplained compiler issue on Solaris.
                  for (size_t i=0; i<n; ++i)
                  {
                     indirectNotice[i] += 1;
                  }
                  ss << XSparse::ArrayToRange(indirectNotice);
               }
            }
         }
            break;
         case NULLHYP:
         {
            const Real stat = fit->statManager()->totalStatistic();
            if (stat <= 0.0)
            {
               tcerr<<"\nCannot calculate a null hypothesis probability with a 0.0 statistic."
                 << std::endl;
               return -1;
            }
            std::pair<int,size_t> dofVals = 
                     fit->statManager()->totalDegreesOfFreedom();
            if (dofVals.first <= 0)
            {
               tcerr<<"\nCannot calculate a null hypothesis probability for 0"
                    <<"\n   degrees of freedom"<<std::endl;
               return -1;
            }
            const StatMethod* singleStat = fit->statManager()->usingSingleStat();
            if (singleStat && singleStat->name() == "chi")
            {
               Numerics::GammaQ GQ;
               Real nullHyp = GQ(dofVals.first/2.0, stat/2.0);
               ss << nullHyp;
            }
            else
            {
               tcerr << "\n***XSPEC Error: All spectra must be assigned to chi-square statistic"
                     << "\n   in order to retrieve a null hypothesis probability."
                     << std::endl;
               return -1;
            }
         }
            break;
         case PARAM: // param modelName:n
            {
               const Parameter* param = HandlerUtils::getFromStringInt<Parameter>(rawArgs);
               if (!param)
               {
                  return -1;
               }
               const string delim(" ");
               const string fullString = param->stringVal();
               string valueString;
               // 6 values for ModParams, 1 for Switch and Scale.
               size_t nVals = 6;
               if (!dynamic_cast<const ModParam*>(param)) nVals = 1;
               string::size_type iPos = 0;
               for (size_t i=0; i<nVals; ++i)
               {
                  iPos = fullString.find_first_not_of(delim, iPos);
                  iPos = fullString.find_first_of(delim, iPos);
               }
               valueString = fullString.substr(0, iPos); 
               ss << valueString;
            }
            break;
         case PEAKRSID: // peakrsid n {lo,hi}
            if (!HandlerUtils::getSpecNumParameter(rawArgs, opt, 3, specNum))
            {
               return -1;
            }
            else
            {
               try   // fit::peakResidual can throw 
               {
                  using namespace XSutility;
                  std::pair<Real,Real> range(0,0);       
                  if ( nArgs >= 5)
                  {
                     string argn(rawArgs[3]);
                     string argn2(rawArgs[4]);
                     if (!isReal(argn,range.first) || !isReal(argn2,range.second))
                     {
                          tcerr << " syntax error: arguments following spectrum number "
                                << " must be real energy range. \n";
                          return -1;
                     }
                  }               
                  std::pair<Real,Real> max(0,0);
                  std::pair<Real,Real> min(0,0);
                  fit->statManager()->peakResidual(max,min,specNum,range);
                  ss << max.second <<"  "<<max.first<<"  "<<min.second<<"  "
                       <<min.first;
               }
               catch (YellowAlert&)
               {
                  return -1;
               }
            }

            break;
         case PINFO: // pinfo modelName:n
            {
               const Parameter* param = HandlerUtils::getFromStringInt<Parameter>(rawArgs);
               if (!param)
               {
                  return -1;
               }
               string parVal = param->stringVal();
               int c(0);
               int ignore(7);
               if ( !dynamic_cast<const ModParam*>(param)) ignore = 1;

               // ignore the first "ignore" arguments, 
               // 7 for ModParams and 1 for Switch and Scale types.
               while ( c++ < ignore) XSparse::returnDelimitedArgument(parVal," ");
               ss << parVal;
            }
            break;
         case PLINK: // plink modelName:n
            {
               const Parameter* par = HandlerUtils::getFromStringInt<Parameter>(rawArgs);
               if (!par)
               {
                  return -1;
               }
               if (par->isLinked() )
               {
                  const string link(par->parameterSetting());
                  ss << "T " << link;     
               }
               else
               {
                  ss << "F";
               }
            }
            break;
         case PLOT: // plot <option> <array> [<plot group n>]
         {
            map<string, PlotSettings::SaveArrayOption> arrayOpt;
            arrayOpt["x"] = PlotSettings::X_AR;
            arrayOpt["xerr"] = PlotSettings::XERR_AR;
            arrayOpt["y"] = PlotSettings::Y_AR;
            arrayOpt["yerr"] = PlotSettings::YERR_AR;
            arrayOpt["model"] = PlotSettings::MODEL_AR;
            static string savPlotCom = "data";
            static PlotSettings::SaveArrayOption savArrayEnum = PlotSettings::X_AR;
            static int savPlotGroup = 1;

            StringArray tclInArgs;
            StringArray params;
            IntegerArray iParams;
            for (size_t i=2; i<nArgs; ++i)
            {
               tclInArgs.push_back(rawArgs[i]);
            }
            XSparse::collectParams(tclInArgs, iParams, params);
            map<string, PlotSettings::SaveArrayOption>::const_iterator itArrayOpt = arrayOpt.begin();
            map<string, PlotSettings::SaveArrayOption>::const_iterator itArrayOptEnd = arrayOpt.end();
            string genErrMsg("Required tclout plot syntax:\n");
            genErrMsg += "    tclout plot <option> <array> [<plot group no.>]\n\n";
            genErrMsg += "where <option> is a valid plot option and <array> is one of:\n    ";
            while (itArrayOpt != itArrayOptEnd)
            {
               genErrMsg += itArrayOpt->first + "  ";
               ++itArrayOpt;
            }
            string plotCom = savPlotCom;
            PlotSettings::SaveArrayOption arrayEnum = savArrayEnum;
            int plotGroup = savPlotGroup;
            bool rerouteFlag = false;
            for (size_t i=0; i<iParams.size(); ++i)
            {
               switch (iParams[i])
               {
                  case 0: // plot command
                  {
                     // Need to check for "model[:<name>]"
                     plotCom = XSparse::returnDelimitedArgument(params[i], string(":"));
                     if (plotCom.length())
                        plotCom = XSutility::lowerCase(plotCom);
                     if (params[i].length())
                        plotCom += " " + params[i];                        
                  }
                     break;
                  case 1: // array option
                  {
                     const string arrayChoice = XSutility::lowerCase(params[i]);
                     map<string, PlotSettings::SaveArrayOption>::const_iterator itChoice = arrayOpt.lower_bound(arrayChoice);
                     if (itChoice == arrayOpt.end() || itChoice->first.find(arrayChoice) != 0)
                     {
                        tcerr << genErrMsg <<endl;
                        return -1;
                     }
                     arrayEnum = itChoice->second;
                     // if user enters "plot model y" they really want what we store
                     // in PlotGroup.model.  Therefore re-route it...
                     const string testForMod("model");
                     // plotCom may have more than 1 word due to named model.
                     string tmpPlotCom(plotCom);
                     const string upToSpace = XSparse::returnDelimitedArgument(tmpPlotCom, string(" "));
                     if (testForMod.find(upToSpace) == 0 && arrayEnum == PlotSettings::Y_AR)
                     {
                        arrayEnum = PlotSettings::MODEL_AR;
                        rerouteFlag = true;
                     }
                  }
                     break;
                  case 2: // plot group number
                  {
                     istringstream iss(params[i]);
                     if (!(iss >> plotGroup) || !iss.eof() || plotGroup < 1)
                     {
                        tcerr << "Improper plot group number specifier\n"
                           << genErrMsg << endl;
                        return -1;
                     }
                  }
                     break;
                  default:
                     break;
               } // end switch/case
            } // end Params loop

            pair<PlotSettings::SaveArrayOption,int> arrayInfo(arrayEnum, plotGroup);
            PlotSettings& settings = XSContainer::plot->setplot();
            settings.saveArrayInfo(arrayInfo);
            string tclPlotCom("plot ");
            tclPlotCom += plotCom;
            // Exceptions thrown during plot should be caught and handled
            // in xsPlot. Only an error code should exist by this point. 
            int plotStatus = Tcl_Eval(interp, tclPlotCom.c_str());
            if (plotStatus == TCL_OK)
            {
               const vector<Real>& outputArray = XSContainer::plot->savedPlotArray();
               // outputArray size will be 0 if plot was unable to find
               // what user requested.
               size_t sz = outputArray.size();
               if (sz)
               {
                  for (size_t i=0; i<sz; ++i)
                  {
                     ss << outputArray[i] << "   ";
                  }
               }
               else ss << .0;
               savPlotCom = plotCom;
               savArrayEnum = arrayEnum;
               if (rerouteFlag)  savArrayEnum = PlotSettings::Y_AR;
               savPlotGroup = plotGroup;
            }
            else
            {
               ss << .0;
            }
         }
            break;
         case PLOTGRP: // plotgrp
            ss << datasets->numberOfPlotGroups();
            break;
         case QUERY: // query
            if (fit->queryMode() == Fit::ON)
            {
               outToTcl = "on";
            }
            else if (fit->queryMode() == Fit::YES)
            {
               outToTcl = "yes";
            }
            else
            {
               outToTcl = "no";
            }
            ss << outToTcl;
            break;
         case RATE: //rate
            // tclout rate has two options: rate or rate n, according
            // to whether the user wants the total count rate or the
            // count rate for a specific data set and model.
            if (nArgs < 3)
            {
                    tcerr << " syntax: tclout rate <n | all >\n";
                    return -1;
            }
            else
            {
               string arg = XSutility::lowerCase(rawArgs[2]);
               if (arg == "all")
               {
                  specNum = 0;
               }
               else
               {
                  if (!HandlerUtils::getSpecNumParameter(rawArgs, opt, 3, specNum))
                  {
                     tcerr << " syntax: tclout rate <n | all >\n";
                     return -1;
                  }
               }

               // If specNum = 0, countRate function sums over all spectra
               Real modelRate = models->countRate(specNum);
               Real dataRate(.0);
               Real variance(.0);
               Real totFlux(.0);
               Real percent(.0);
               if (specNum)
               {
                  const SpectralData* sd = datasets->lookup(specNum);
                  dataRate = sd->netFlux();
                  variance = sd->netVariance();
                  totFlux = sd->totalFlux();
               }
               else  // sum over all spectra
               {
                  const size_t N = datasets->numberOfSpectra();
                  for ( size_t j = 1; j <= N; ++j)   
                  {
                          SpectralData* spectrum = datasets->lookup(j); 
                          dataRate += spectrum->netFlux();
                          totFlux += spectrum->totalFlux();
                          Real curVar = spectrum->netVariance();
                          variance +=  curVar*curVar;
                  }
                  variance = sqrt (variance);                         
               }
               if (totFlux != .0)
                  percent = 100.*dataRate/totFlux;
               ss << dataRate <<" "<< variance <<" "<< modelRate << " " << percent;;
            }
            break;
         case RESPONSE: // response n
            if (!HandlerUtils::getSpecNumParameter(rawArgs, opt, 3, specNum))
            {
               return -1;               
            }
            else
            {
               SpectralData* sd = datasets->lookup(specNum);
               string respName("");
               const std::vector<Response*>& resps = sd->detector();
               for (size_t i=0; i<resps.size(); ++i)
               {
                  const RealResponse* rrsp = dynamic_cast<RealResponse*>(resps[i]);
                  if (rrsp)
                  {
                     respName += rrsp->rmfName();
                     respName += " ";
                  }
                  else
                  {
                     const MultiResponse* mrsp = dynamic_cast<MultiResponse*>(resps[i]);
                     if (mrsp)
                     {
                        const StringArray& rmfNames = mrsp->rmfNames();
                        for (size_t j=0; j<rmfNames.size(); ++j)
                        {
                           respName += rmfNames[j];
                           respName += " ";
                        }
                     }
                  }
               }
               ss << respName;
            }
            break;     
         case SIGMA:  // sigma [modelName:]n
            {
               const Parameter* param = HandlerUtils::getFromStringInt<Parameter>(rawArgs);
               if (!param)
               {
                  return -1;
               }
               const ModParam* modPar = dynamic_cast<const ModParam*>(param);
               if (!modPar)
               {
                  tcerr << "***Error: Specified parameter is not a fit parameter."<<endl;
                  return -1;
               }
               Real sigma = -1;
               if (!modPar->isFrozen() && !modPar->isLinked())
                  sigma = modPar->value('s');
               ss << sigma;
            }
            break;       
         case SIMPARS:  // simpars
         {
            std::vector<ModParam*> parsToModify;
            std::vector<Real> savePars;
            std::map<int,ModParam*>::const_iterator itVarFit = 
                     fit->variableParameters().begin();
            std::map<int,ModParam*>::const_iterator itVarFitEnd = 
                     fit->variableParameters().end();
            const bool wasStillValid = fit->isStillValid();
            if (fit->chainManager()->isSynchedWithFitParams())
            {
               // See comments in Fit::checkChainsForSynch for further 
               // explanation on where to look for parameters. 
               if (fit->activeModels().size())
               {
                  // We have active/on models
                  while (itVarFit != itVarFitEnd)
                  {
                     parsToModify.push_back(itVarFit->second);
                     savePars.push_back(itVarFit->second->value());
                     ++itVarFit;
                  }
               }
               else
               {
                  // Must be 1 or more active/off models with 1 and
                  // only 1 data group.  The order in which we grab
                  // the pars is unimportant, as long as we put them
                  // back in the same order at the end.
                  std::map<string,bool>::const_iterator itActive =
                        models->activeModelNames().begin();
                  std::map<string,bool>::const_iterator itActiveEnd = 
                        models->activeModelNames().end();
                  while (itActive != itActiveEnd)
                  {
                     if (itActive->second)
                     {
                        const Model* mod = models->lookup(itActive->first);
                        std::vector<Parameter*> pars;
                        mod->bundleParameters(pars);
                        for (size_t iPar=0; iPar<pars.size(); ++iPar)
                        {
                           ModParam* modPar = dynamic_cast<ModParam*>(pars[iPar]);
                           if (modPar && !(modPar->isFrozen() || modPar->isLinked()))
                           {
                              parsToModify.push_back(modPar);
                              savePars.push_back(modPar->value());
                           }
                        }
                     }
                     ++itActive;
                  }

               } // end if no active/on models, taking parameters from active/off
            } // end if able to use chains instead of covar
            else
            {
               // Parameters and randomization can only come from fit.
               if (!wasStillValid)
               {
                  tcerr << "An up-to-date fit is first required to randomize model parameters."<<endl;
                  return -1;
               }
               while (itVarFit != itVarFitEnd)
               {
                  parsToModify.push_back(itVarFit->second);
                  savePars.push_back(itVarFit->second->value());
                  ++itVarFit;
               }
            }

            // OK, we now have the parameters we need to simulate.
            bool isErr = false;
            try
            {
               fit->reportErrorSimulationMethod();
               // this may throw
               fit->randomizeModelParameters(true);
               // This should retrieve all models in alphabetical order, 
               // and internally by data group number.
               const std::map<string,bool>& modNames = models->activeModelNames();
               std::map<string,bool>::const_iterator itNames = modNames.begin();
               while (itNames != modNames.end())
               {
                  if (itNames->second)
                  {
                     const string& modName = itNames->first;
                     const std::vector<Model*> mods = models->lookupModelGroup(modName);
                     if (fit->activeModels().size())
                     {
                        // Only interested in active/on models
                        if (!mods[0]->isActive()) 
                        {
                           ++itNames;
                           continue;
                        }
                     }
                     for (size_t iGroup=0; iGroup<mods.size(); ++iGroup)
                     {
                        const Model* mod = mods[iGroup];
                        std::vector<Parameter*> pars;
                        mod->bundleParameters(pars);
                        for (size_t iPar=0; iPar<pars.size(); ++iPar)
                           ss << pars[iPar]->value() << " ";
                     }
                  }
                  ++itNames;
               }
            }
            catch (YellowAlert&)
            {
               isErr = true;
            }
            // Restore original parameters and fit state
            for (size_t i=0; i<parsToModify.size(); ++i)
            {
               parsToModify[i]->setValue(savePars[i],'v');
            }
            fit->statManager()->performStats();
            fit->isStillValid(wasStillValid);
            if (isErr)
               return -1;
         }
             break;
         case SOLAB: // solab
            {
               const string& abund = FunctionUtility::ABUND();
               const vector<float>& abundances = 
                        FunctionUtility::abundanceVectors(abund);
               const size_t& N = FunctionUtility::NELEMS();

               std::ostringstream oss;
               oss.precision(3);
               oss.setf(ios_base::scientific | ios_base::showpoint);
               for ( size_t j = 0; j < N; ++j)
               {
                  oss << abundances[j] << " ";
               }
               ss << oss.str();
            }
            break;
         case STAT: // stat
	    {
	       if ( nArgs > 2 ) {
		 string tOrF(rawArgs[2]);
		 if ( tOrF[0] == 't' || tOrF[0] == 'T' ) {
		   ss << fit->statManager()->totalTestStatistic();
		 } else if ( tOrF[0] == 'f' || tOrF[0] == 'F' ) {
		   ss << fit->statManager()->totalStatistic();
		 }
	       } else {
		 ss << fit->statManager()->totalStatistic();
	       }
	    }
            break;
         case STATMETHOD: 
	   {
	       if ( nArgs > 2 ) {
		 string tOrF(rawArgs[2]);
		 if ( tOrF[0] == 't' || tOrF[0] == 'T' ) {
		   ss << fit->statManager()->defaultTestStat()->fullName();
		 } else if ( tOrF[0] == 'f' || tOrF[0] == 'F' ) {
		   ss << fit->statManager()->defaultStat()->fullName();
		 }
	       } else {
		 ss << fit->statManager()->defaultStat()->fullName();
	       }
	   }
            break;
         case MARGIN:  // margin probability | [<modName>:]<parameterNum>
         case STEPPAR:  // steppar statistic | [<modName>:]<parameterNum>
         {
            const string cmdLabel = (selected == MARGIN) ? string("margin") :
                                string("steppar");
            const string optLabel = (selected == MARGIN) ? string("probability") :
                                string("statistic");
            const string delstat("delstat");
            if (nArgs < 3)
            {
               tcerr << " Proper syntax is: tclout " << cmdLabel <<" "
                        << optLabel << " | [<modName>:]<parameterNum>" << std::endl;
               return -1;
            }

            string testOpt = XSutility::lowerCase(rawArgs[2]);
            // For either command, we only need what's in the
            // Grid base class.  (Except for the delstat option which was
            // added later.)
            const Grid* grid = (selected == MARGIN) ? 
                        static_cast<Grid*>(fit->chainManager()->marginGrid()) :
                        static_cast<Grid*>(fit->stepGrid());
            if (!grid)
            {
               tcerr << " No " << cmdLabel << " results currently exist." << std::endl;
               return -1;
            }
            if (optLabel.find(testOpt) == 0)
            {
               // The easier case: simply return the fit statistic values.
               const RealArray& gridVals = grid->getGridValues();
               size_t nVals = gridVals.size();
               if (!nVals)
               {
                  // Not sure if this case is even possible, but this can't hurt.
                  ss << "  ";
               }
               for (size_t i=0; i<nVals; ++i)
               {
                  ss << gridVals[i] << "  ";
               }
            }
            else if (selected == STEPPAR && delstat.find(testOpt) == 0)
            {
               const RealArray& gridVals = grid->getGridValues();
               size_t nVals = gridVals.size();
               if (!nVals)
                  ss << "  ";
               // We know we're dealing with a step grid here:
               const Real statMin = fit->stepGrid()->bestFit();
               for (size_t i=0; i<nVals; ++i)
               {
                  ss << gridVals[i] - statMin << "  ";
               }

            }
            else
            {
               // iPar is not the par index number (either user entered or
               // internal).  It merely refers to its offset in the grid
               // parameter array (ie. 2nd steppar param has iPar = 1).
               size_t iPar = 0;
               size_t nIntervalOffset = 0;
               const Grid::ParameterSpec* foundPar = 0;
               bool isFreeSteppar=false;
               Grid::SpecContainer::const_iterator itPars = grid->getParameter().begin();
               Grid::SpecContainer::const_iterator itParsEnd = grid->getParameter().end();
               if (selected == STEPPAR)
               {
                  // Is this a free par?  If so, user's presumed input 
                  // [<modName>:]<parNum> string will find a match in freeParVals map.                  
                  const std::map<string,RealArray>& freeParMap = 
                                fit->stepGrid()->freeParVals();
                  std::map<string,RealArray>::const_iterator itFree =
                                freeParMap.find(rawArgs[2]);
                  if (itFree != freeParMap.end())
                  {
                     isFreeSteppar = true;
                     const RealArray& freeVals = itFree->second;
                     const size_t nFreeVals=freeVals.size();
                     for (size_t iFree=0; iFree<nFreeVals; ++iFree)
                     {
                        ss << freeVals[iFree] << "  ";
                     }
                  }
                  else
                  {
                     // Not a free par, is it a steppar parameter?
                     //  If not, an error message will eventually be returned.
                  
                     // For steppar, parameter arrays have 1 more value than
                     // interval settings, so...
                     nIntervalOffset = 1;

                     // Check for [modName:]parNum.  Can't assume this par will still
                     // be in models container, so don't use lookupStringIntObj.
                     string modName;
                     size_t parIdx=0;
                     if (HandlerUtils::verifyStringInt<Parameter>(rawArgs, 3, modName, parIdx))
                     {
                        while (!foundPar && itPars != itParsEnd)
                        {
                          // Looking for param match is somewhat cumbersome at present
                          // since ParameterSpec struct doesn't store the modName 
                          // in an easily retrievable form.
                          const string& storedName = (*itPars)->name;
                          // storedName is of form  [<modName>:]<parName>
                          string storedModName;
                          string::size_type colonPos = storedName.find(':');
                          if (colonPos != string::npos)
                          {
                             storedModName = storedName.substr(0, colonPos);
                          }
                          if (modName == storedModName)
                          {
                             if (static_cast<int>(parIdx) == (*itPars)->parIndex)
                             {
                                foundPar = *itPars;
                             }
                          }
                           ++iPar;
                           ++itPars;
                        }
                     }
                  } // end if steppar (as opposed to free) parameter
               } // end steppar-specific
               else  // margin
               {
                  string modName;
                  size_t parNum = string::npos;
                  if (HandlerUtils::verifyStringInt<Parameter>(rawArgs,
                                3, modName, parNum))
                  {
                     while (!foundPar && itPars != itParsEnd)
                     {
                        if ((*itPars)->name == modName &&
                              static_cast<size_t>((*itPars)->parIndex) == parNum)
                        {
                           foundPar = *itPars;
                        }
                        ++iPar;
                        ++itPars;
                     }
                  }
               }
               
               if (foundPar)
               {
                  // If we're here iPar must be >= 1, so decrement on a 
                  // size_t is OK.
                  --iPar;
                  const std::vector<Real>& parVals = foundPar->parameterValues;
                  const size_t nParVals = parVals.size();
                  const size_t nGrid = grid->getGridSize();
                  std::vector<Real> repetitiveVals(nGrid, .0);
                  size_t innerRepCount = 1;
                  for (size_t i=0; i<iPar; ++i)
                  {
                     innerRepCount *= (grid->getParameter()[i])->intervals 
                                        + nIntervalOffset;
                  }
                  size_t outerRepCount = nGrid/(innerRepCount*nParVals);
                  for (size_t i=0; i<outerRepCount; ++i)
                  {
                     size_t iOffset = i*nParVals*innerRepCount;
                     for (size_t j=0; j<nParVals; ++j)
                     {
                        Real parVal = parVals[j];
                        size_t jOffset = j*innerRepCount;
                        for (size_t k=0; k<innerRepCount; ++k)
                        {
                           repetitiveVals[k + iOffset + jOffset] = parVal;
                        }
                     }
                  }
                  // Success
                  for (size_t i=0; i<nGrid; ++i)
                  {
                     ss << repetitiveVals[i] << "  ";
                  }                                         
               }
               else if (!isFreeSteppar)
               {
                  tcerr << " ***Error: Parameter " << rawArgs[2]
                      << " not found in results of last " << cmdLabel << "." 
                      << std::endl;
                  return -1;
               }
            } // end handling [<modName>:]<parNum>
         }          
            break;        
         case VARPAR: // varpar
            if (fit->isStillValid())
            {
               size_t nVar = fit->variableParameters().size();
               ss << nVar;
            }
            else
            {
               tcerr << " Valid fit does not exist." << std::endl;
               return -1;
            }
            break;
         case VERSION:
            ss << XSutility::xs_version();
            break;
         case WEIGHT:
            ss << DataUtility::statWeight().name();
            break;
         case XFLT: // xflt n
            if (!HandlerUtils::getSpecNumParameter(rawArgs, opt, 3, specNum))
            {
               return -1;               
            }
            else
            {
               const SpectralData* sd = datasets->lookup(specNum);
               const std::map<string, Real>& vals = sd->xflt();
               size_t sz = vals.size();
               ss << sz;

	       // do a backwards compatibility check. if all the keys are of
	       // form "key#" then don't write out the keys

	       std::map<string, Real>::const_iterator
		   i_xfltBeg = vals.begin(),
		   i_xfltEnd = vals.end();

	       bool backCompat = true;
	       while(i_xfltBeg != i_xfltEnd)
	       {
		 if (i_xfltBeg->first.substr(0,3) != "key" ) backCompat = false;
		  ++i_xfltBeg;
	       }

	       i_xfltBeg = vals.begin();
	       while(i_xfltBeg != i_xfltEnd)
	       {
		  if ( backCompat ) {
		    ss <<" "<< i_xfltBeg->second;
		  } else {
		    ss <<" "<< i_xfltBeg->first << ": " << i_xfltBeg->second;
		  }
		  ++i_xfltBeg;
	       }

            }
            break;
         default:
            {
               string msg("TCLOUT option ");
               msg += opt;
               msg += " not yet implemented";
               tcout << msg << std::endl; 
               XSutility::printValidOptions(tcout, cmd, option);
            }
            break;
      } // end switch
      results = ss.str();
      resultsEntered = true;
   }
   else
   {
      string errMsg(inputOpt);
      errMsg += " is not currently a valid TCLOUT option.\n";
      tcerr << errMsg << std::endl;
      XSutility::printValidOptions(tcout, cmd,option);
      return -1;
   }

   return 0;
}

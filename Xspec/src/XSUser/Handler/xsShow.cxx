//
//  XSPEC12  November 2003
//
//

#include <XSFit/Fit/Fit.h>
#include <XSFit/Fit/FitMethod.h>
#include <XSFit/Fit/StatManager.h>
#include <XSFit/Fit/StatMethod.h>
#include <XSUser/Global/Global.h>        
#include <XSUser/Global/XSGlobal.h>        
#include <XSUser/UserInterface/xstcl.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSModel/Data/DataSet.h>
#include <XSModel/Data/DataUtility.h>
#include <XSModel/Data/SpectralData.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/GlobalContainer/ResponseContainer.h>
#include <XSModel/GlobalContainer/Weight.h>
#include <XSModel/Model/Model.h>
#include <XSModel/Model/Component/Component.h>
#include <XSModel/Parameter/Parameter.h>
#include <XSModel/Parameter/ResponseParam.h>
#include <XSPlot/Plot/PlotDirector.h>
#include <XSUtil/Utils/XSstream.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSUtil/FunctionUtils/FunctionUtility.h>
#include <XSContainer.h>
#include <XSsymbol.h>
#include <XSstreams.h>
#include <xsTypes.h>
#include <XSUser/Handler/HandlerUtils.h>

#include <iomanip>
#include <ctime>
#include <cmath>
#include <cstring>

namespace {
  void showAbund();
  void showControl();
  void showFiles();
  void showFit();
  void showNoticed();
  void showParams(bool onlyFree);
  void showPha();
  void showPlot();
  void showRates();
  void showRespPars(bool onlyFree);
  void showResponse();
  void showSpecificParams(const StringArray& parIDs, bool isRespPar=false);
  void showXsect();
  void commonDataReport(const DataSet* ds, const SpectralData* sd);
  void rebinDisplay(int plotGroup, const PlotSettings::RebinInfo& rbInfo);
}

int
XSGlobal::xsShow(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   static string prevOption("all");

   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);

   doShow(rawArgs, prevOption);

   return TCL_OK;
}

// Declared in XSinterface.h
void XSGlobal::doShow(const StringArray& rawArgs, string& prevOption)
{
   using namespace std;
   using namespace XSContainer;

   string option;
   enum OPTIONS {ABUND, ALL, ALLFILE, CONTROL, DATA, FILES, FIT, FREE, MODEL,
                NOTICED, PARAMETER, PHA, PLOT, RATES, RESPONSE, RPARAMETER, 
                VERSION, XSECT};
   static map<string, size_t> allowedOptions;
   allowedOptions["abund"] = ABUND;
   allowedOptions["all"] = ALL;
   allowedOptions["allfile"] = ALLFILE;
   allowedOptions["control"] = CONTROL;
   allowedOptions["data"] = DATA;
   allowedOptions["files"] = FILES;
   allowedOptions["fit"] = FIT;
   allowedOptions["free"] = FREE;
   allowedOptions["model"] = MODEL;
   allowedOptions["noticed"] = NOTICED;
   allowedOptions["parameters"] = PARAMETER;
   allowedOptions["pha"] = PHA;
   allowedOptions["plot"] = PLOT;
   allowedOptions["rates"] = RATES;
   allowedOptions["response"] = RESPONSE;
   allowedOptions["rparameters"] = RPARAMETER;
   allowedOptions["version"] = VERSION;
   allowedOptions["xsect"] = XSECT;

   const size_t nArgs = rawArgs.size();
   if (nArgs > 1) 
   {
           option = rawArgs[1]; 
           option = XSutility::lowerCase(option);
           if (option == "?")
           {
              XSutility::printValidOptions(tcout, string("show"),
                        allowedOptions);
              return;
           }
   }
   else option = prevOption;

   map<string,size_t>::const_iterator itOpt(allowedOptions.lower_bound(option));
   map<string,size_t>::const_iterator itOptEnd(allowedOptions.end());
   size_t selected = 0;
   if (itOpt != itOptEnd && itOpt->first.find(option) == 0) 
                selected = itOpt->second;
   else
   {
      tcout << "Unrecognized show option." << std::endl;
      XSutility::printValidOptions(tcout, string("show"), allowedOptions);
      return;
   }
   try
   {
      switch (selected)
      {
         case ABUND:
            showAbund();
            break;
         case ALL:
            doVersion(rawArgs);
            showControl();
            if (tpout.maxChatter() >= 15)
            {
               showAbund();
            }
            showPlot();
            showResponse();
            datasets->showData(true);
            models->showModel();
            showFit();
            break;
         case ALLFILE:
            showResponse();
            datasets->showData(true);
            break;
         case CONTROL:
            showControl();
            break;
         case DATA:
            datasets->showData(false);
            break;
         case FILES:
            showFiles();
            break;
         case FIT:
            showFit();
            break;
         case FREE:
            Parameter::onlyPrintFree(true);
            showParams(true);
            Parameter::onlyPrintFree(false);
            break;
         case MODEL:
         {
            // This code is mostly pasted from ModelContainer::showModel() and
            // particularly Model::printHeading() as part of a non-interface-changing
            // patch modification.  
            tcout << "\nCurrent model list:";
            if (models->modelSet().empty())
            {
               tcout << " none " << endl;
            }
            else
            {
               tcout << endl;
               map<string,bool>::const_iterator itModNames = 
                        models->activeModelNames().begin();
               map<string,bool>::const_iterator itNamesEnd = 
                        models->activeModelNames().end();
               while (itModNames != itNamesEnd)
               {
                  // mods size must always be >= 1 if name is in activeModelNames map.
                  vector<Model*> mods = models->lookupModelGroup(itModNames->first);
                  for (size_t iMod=0; iMod<mods.size(); ++iMod)
                  {
                     const Model* mod = mods[iMod];
                     if (iMod==0)
                     {
                        const string delim("()*+ \t");
                        const string& storedFullExpr = mod->fullExpression();
                        string reportFullExpr;
                        // Add bracketed component nums to full expression.
                        int compIdx = 0;
                        string::size_type startPos = 0;
                        do 
                        {
                           string::size_type wordPos = 
                                storedFullExpr.find_first_not_of(delim, startPos);
                           string::size_type endPos = string::npos;
                           if (wordPos != string::npos)
                           {
                              endPos = storedFullExpr.find_first_of(delim, wordPos);
                              string::size_type len = (endPos == string::npos) ?
                                        string::npos : endPos - startPos;
                              reportFullExpr += storedFullExpr.substr(startPos, len);
                              ++compIdx;
                              std::ostringstream oss;
                              oss << '<' << compIdx << '>';
                              reportFullExpr += oss.str(); 
                           }
                           else if (startPos != string::npos)
                           {
                              // We've reached the last component.  Pick up any
                              // trailing ')'s.
                              reportFullExpr += storedFullExpr.substr(startPos);
                           }
                           startPos = endPos;
                        }  while (startPos != string::npos); 

                        tcout << "\nModel ";
                        if (mod->name() != Model::DEFAULT() ) tcout << mod->name() << ':';
                        tcout << reportFullExpr;
                        if (datasets->numSourcesForSpectra() >= 1 ) 
                                tcout << " Source No.: " << mod->sourceNumber();
                        bool isAct = itModNames->second;
                        string status = isAct ? string("   Active") : string("   Inactive");
                        tcout << status << "/";
                        status = mod->isActive() ? string("On") : string("Off");
                        tcout << status << std::endl; 
                        tcout <<"      For Data Group(s): "; 
                     } // end if lowest group numbered model

                     tcout << mod->dataGroupNumber() <<" ";
                  }
                  tcout <<std::endl;
                  ++itModNames;
               }
            }
            tcout << std::endl;
            if (models->autonomousEnergy().size())
            {
               tcout << "   Using autonomous energy array.\n" << endl;
            }
            else if (models->isExtended())
            {
               tcout << "   Using extended response energies.\n" << endl;
            }
            else
            {
               tcout << "   Using energies from responses.\n" << endl;
            }
         }
            break;
         case NOTICED:
            showNoticed();
            break;
         case PARAMETER:
            if (nArgs > 2)
            {
               StringArray inArgs;
               for (size_t i=2; i<nArgs; ++i)
                  inArgs.push_back(rawArgs[i]);
               IntegerArray dummy;
               StringArray inPars;
               XSparse::collectParams(inArgs,dummy,inPars);
               showSpecificParams(inPars);
            } 
            else        
               showParams(false);
            break;
         case PHA:
            showPha();
            break;
         case PLOT:
            showPlot();
            break;
         case RATES:
            showRates();
            break;
         case RESPONSE:
            showResponse();
            break;
         case RPARAMETER:
            if (nArgs > 2)
            {
               StringArray inArgs;
               for (size_t i=2; i<nArgs; ++i)
                  inArgs.push_back(rawArgs[i]);
               IntegerArray dummy;
               StringArray inPars;
               XSparse::collectParams(inArgs,dummy,inPars);
               showSpecificParams(inPars, true);
            } 
            else
               showRespPars(false);
            break;
         case VERSION:
            // This is an allowed redundacy.  Just call the 'version'
            //   command handler.
            doVersion(rawArgs);
            break;
         case XSECT:
            showXsect();
            break;
         default:
            tcout << "Show option " << option << " does not exist" << endl;
            XSutility::printValidOptions(tcout, string("show"), allowedOptions);
            break;
      }
   }
   catch (YellowAlert&)
   {
   }
   if (itOpt != itOptEnd)
   {
      prevOption = itOpt->first;
   }
}

namespace {
   using namespace std;
   using namespace XSContainer;

   void showAbund()
   {
      tcout << "\nSolar Abundance Table: \n";
      const string& abund = FunctionUtility::ABUND();
      tcout << abund  << ": "  <<  FunctionUtility::abundDoc(abund) << '\n';

      const size_t& n = FunctionUtility::NELEMS();
      const vector<float>& abundances = FunctionUtility::abundanceVectors(abund);

      ios_base::fmtflags currentSetting(tcout.flags());
      streamsize p(tcout.precision(3));
      tcout.setf(ios_base::scientific | ios_base::showpoint | ios_base::uppercase );
      for ( size_t j = 0; j < n; ++j)
      {
              tcout << left << "  " << setw(4) 
                    <<  FunctionUtility::elements(j) + ":" <<
                    right << setw(10) << abundances[j];
              if ( j % 5 == 4 || j == n - 1) tcout << '\n';    
      }
      tcout << flush;
      tcout.flags(currentSetting);
      tcout.precision(p);
   }

   void showControl()
   {
      time_t calndrTime;
      char timebuf[100];
      time(&calndrTime);
      strcpy(timebuf, ctime(&calndrTime));
      tcout << "\n" << timebuf;

      size_t saveFreq = XSGlobal::globalData->autoSaveFrequency();
      if (saveFreq == XSparse::NOTFOUND())
      {
         tcout << " Auto-saving is disabled." << endl;
      }
      else if (saveFreq == 1)
      {
         tcout << " Auto-saving is done after every command." << endl;
      }
      else
      {
         tcout << " Auto-saving is done after every " << saveFreq
                   << " commands." << endl;
      }

      const StatMethod* singleStat = fit->statManager()->usingSingleStat();
      if (singleStat)
         tcout << " Fit statistic in use: " << 
                   singleStat->fullName() << endl;
      tcout << " Minimization technique: " << 
                   fit->fitMethod()->fullName() << endl;
      tcout << "    Convergence criterion = " << 
                           fit->fitMethod()->deltaCrit() << endl;
      Real parDelta = XSContainer::models->proportionalDelta();
      tcout << "    Parameter fit deltas: ";
      if (parDelta <= 0.0)
         tcout << "fixed values" << endl;
      else
         tcout << parDelta << " * parValue" << endl;
      if (fit->fitMethod()->delayedGratification())
	tcout << "    Using delayed gratification." << endl;

      tcout << " Always calculate parameter derivatives using full (slower) numerical differentiation: ";
      if (fit->useNumericalDifferentiation())
         tcout << "Yes" << std::endl;
      else
         tcout << "No" << std::endl;

      if (fit->queryMode() == Fit::ON)
      {
         tcout << " Querying enabled." << endl;
      }
      else
      {
         if (fit->queryMode() == Fit::YES)
         {
            tcout << " Querying disabled - will continue fitting." << endl;
         }
         else
         {
            tcout << " Querying disabled - will not continue fitting." << endl;
         }
      }
      if (fit->renormType() == Fit::AUTO)
      {
         tcout << " Auto-renorming enabled." << endl;
      }
      else if (fit->renormType() == Fit::PREFIT)
      {
         tcout << " Prefit-renorming enabled." << endl;
      }           
      if (models->modelSystematicError() >= SMALL)
      {
         tcout << " Model systematic error = " <<
                   models->modelSystematicError() << endl;
      }

      tcout << " Solar abundance table: " << FunctionUtility::ABUND() << endl;
      showXsect();
      tcout << " Cosmology in use: H0 = " << models->cosmo().H0 
            << " q0 = " << models->cosmo().q0 << " Lambda0 = "
            << models->cosmo().lambda0 << endl
            << " Model data directory: " << 
                   FunctionUtility::modelDataPath() << endl;
   } //end showControl

   void showFiles()
   {
      // Essentially the same as showData, but outputs spectra in order
      // of DataSet file names rather than spectrum number.

       if (datasets->dataArray().empty())
       {
          tcout << "\n No Spectra defined." << endl;
       }
       else
       {
          string filx =  
                  datasets->dataArray().size() > 1 ? " files " : " file ";
          string 
                  spec =  datasets->numberOfSpectra() > 1 ? " spectra " : " spectrum ";
          tcout << '\n' << datasets->dataArray().size() << filx
                << datasets->numberOfSpectra() << spec << endl;

          DataArrayConstIt itDs = datasets->dataArray().begin();
          DataArrayConstIt itDsEnd = datasets->dataArray().end();
          while (itDs != itDsEnd)
          {
             if (itDs->second->isMultiple())
             {
                SpectralDataMapConstIt itSd = itDs->second->multiSpectralData().begin();
                SpectralDataMapConstIt itSdEnd = itDs->second->multiSpectralData().end();
                while (itSd != itSdEnd)
                {
                   tcout << endl;
                   itDs->second->report(itSd->first);
                   ++itSd;
                }

             }
             else
             {
                tcout << endl;
                itDs->second->report(0);
             }
             ++itDs;
          }
          tcout << flush;
       }
   }

   void showFit()
   {
      fit->statManager()->reportStats();
      tcout << " Weighting method: " << DataUtility::statWeight().name()
          << endl;

      if (fit->useNumericalDifferentiation())
         tcout << " Fit is using the full (slower) numerical differentiation method."
            << endl;
   }

   void showNoticed()
   {
       if (datasets->dataArray().empty())
       {
          tcout << "\n No Spectra defined." << endl;
       }
       else
       {
          const size_t nSpecs = datasets->numberOfSpectra();
          for (size_t i=1; i<=nSpecs; ++i)
          {
             const SpectralData* sd = datasets->lookup(i);
             tcout << endl << "Spectrum No. " << sd->spectrumNumber() << endl;
             commonDataReport(sd->parent(), sd);
             sd->reportNoticed();
          }
       }   
   }

   void showParams(bool onlyFree)
   {
      if (onlyFree)
      {
         tcout << "\nFree parameters defined:";
      }
      else
      {
         tcout << "\nParameters defined:"; 
      }
      const ModelMap& mods = models->modelSet();

      if (mods.empty())
      {
              tcout << " none" << endl;
      }
      else
      {
         map<string,bool>::const_iterator itModNames = models->activeModelNames().begin();
         map<string,bool>::const_iterator itNamesEnd = models->activeModelNames().end();
         while (itModNames != itNamesEnd)
         {
            vector<Model*> mods = models->lookupModelGroup(itModNames->first);
            // mods should always have size >=1 if in activeModelNames map.
            mods[0]->printHeading();
            for (size_t i=0; i<mods.size(); ++i)
            {
               tcout << *mods[i];
            }
            mods[0]->printMixComp();
            tcout << string(72,'_') << std::endl;
            ++itModNames;
         }  

         tcout << endl;     
      }

      if (responses->totalResponseParams())
      {
         showRespPars(onlyFree);
      }
   } // end showParams


   void showRespPars(bool onlyFree)
   {
      if (onlyFree)
      {
         tcout << "\nFree response parameters defined:";
      }
      else
      {
         tcout << "\nResponse parameters defined:";
      }

      if (!responses->totalResponseParams())
      {
         tcout << " none" << endl;
      }
      else
      {
         responses->reportResponseParams();
      }
   } // end showRespPars


   void showPha()
   {
       if (datasets->dataArray().empty())
       {
          tcout << "\n No Spectra defined." << endl;
       }
       else
       {
          const size_t nSpecs = datasets->numberOfSpectra();
          tcout << endl;
          for (size_t i=1; i<=nSpecs; ++i)
          {
             const SpectralData* sd = datasets->lookup(i);
             tcout << " Information for spectrum " << i 
                << "\n  belonging to plot group " << sd->plotGroup()
                << ", data group " << sd->parent()->dataGroup() << endl;
             sd->reportKeywords();
             tcout << endl;
             sd->reportPha();
             tcout << endl;
          }
       }
   }

   void showPlot()
   {
      using XSContainer::plot;
      const PlotSettings& settings = plot->setplot();
      tcout << " Plot settings:" << endl;
      string onOrOff = settings.getShowAddComponent() ? string("ON."):string("OFF.");
      tcout << "   Showing of individual additive components is " << onOrOff << endl;
      onOrOff = settings.getShowBackground() ? string("ON.") : string("OFF.");
      tcout << "   Showing of background spectra is " << onOrOff << endl;
      onOrOff = settings.getDivideByArea() ? string("ON."):string("OFF.");
      tcout << "   Effective area normalization is " << onOrOff << endl;
      tcout << "   Current unit settings:" << endl;
      tcout << "      Energy     = " << settings.getUnitID(ENERGY) << endl;
      tcout << "      Wavelength = " << settings.getUnitID(WAVELENGTH)
            << ", with Y-Axis displayed per ";
      if (settings.isWavePerHz())
         tcout << "Hz" << endl;
      else
         tcout << "length" << endl;
      string xMode;
      if (settings.xOption() == ENERGY)
         xMode = "Energy";
      else if (settings.xOption() == WAVELENGTH)
         xMode = "Wavelength";
      else
         xMode = "Channels";
      tcout << "   X-Axis data display mode: " << xMode << endl;
      tcout << "   Spectra plots will be shifted to source frame by redshift value z: "
                << settings.redshiftToSource() << endl;
      tcout << "   Device: " << plot->getPlottingDeviceName() << endl;
      onOrOff = settings.getShowLineIDs() ? string("ON."):string("OFF.");
      tcout << "   Plotting of line IDs is " << onOrOff << endl;
      if (settings.getShowLineIDs())
      {
         tcout << "     Temperature: " << settings.temperature()  
               << "     Emissivity Limit: " << settings.emisLimit() 
               << "     Redshift: " << settings.redshiftLinesToObs()
               << "     Low Energy: " << settings.IDLowEnergy()
               << "     High Energy: " << settings.IDHighEnergy()
	       << endl;
      }
      onOrOff = settings.splashPage() ? string("ON."):string("OFF.");
      tcout << "   Splashpage is " << onOrOff << endl;
      onOrOff = settings.xLog() ? string("ON."):string("OFF.");
      tcout << "   xlog for data plots is " << onOrOff << endl;
      onOrOff = settings.yLog() ? string("ON."):string("OFF.");
      tcout << "   ylog for data plots is " << onOrOff << endl;

      tcout <<"\n   Default plot rebin settings for all plot groups:"<<endl;      
      map<int,PlotSettings::RebinInfo>::const_iterator itRb = 
                settings.groupsRebinInfo().find(-1);
      // Entry for -1 should ALWAYS be in plot rebin map.
      tcout << "   Min. Signif.   Max. # Bins   Error Type"<< endl; 
      rebinDisplay(-1, itRb->second);
      map<int,PlotSettings::RebinInfo>::const_iterator itRbEnd = 
                settings.groupsRebinInfo().end();
      ++itRb;
      if (itRb != itRbEnd)
      {      
         tcout << "   With overridden rebin settings for the following plot groups:"
                 << endl;
         tcout << "   Groups    Min. Signif.   Max. # Bins    Error Type"<< endl;
         while (itRb != itRbEnd)
         {
            rebinDisplay(itRb->first, itRb->second);
            ++itRb;
         }
      }
   }  // end showPlot

   void showRates()
   {
       if (datasets->dataArray().empty())
       {
          tcout << "\n No Spectra defined." << endl;
       }
       else
       {
          const size_t nSpecs = datasets->numberOfSpectra();
          for (size_t i=1; i<=nSpecs; ++i)
          {
             const SpectralData* sd = datasets->lookup(i);
             tcout << endl;
             commonDataReport(sd->parent(), sd);
             sd->reportRates(); 
             tcout << " Spectral data counts: " << sd->totalFlux()*sd->exposureTime()
                   << endl;
             models->reportModelRate(sd);
          }
       } 
   }

   void showResponse()
   {
      // primitive data printing option
      tcout << "\n Responses read: ";
      if (responses->responseList().size() == 1)
      {
         tcout << " none " << endl;
         if (responses->responseList().begin()->first != DUMMY_RSP)
         {
            throw RedAlert(" Dummy Response Corrupted or deleted ");
         }
      }
      else
      {
         tcout << '\n';
         ResponseMapConstIter resp = responses->responseList().begin();
         while (resp != responses->responseList().end())
         {
            if ( resp->first != DUMMY_RSP)
            {
               Response* current(resp->second);
               string respName(resp->first);
               if (respName == USR_DUMMY_RSP)
                  respName = "Dummy response";
               tcout << "   " << respName << " associated with spectrum "
                     << current->spectrumNumber() << " source " 
                     << current->sourceNumber();
               if (current->dataGroup() > 1)
               {
                       tcout << " data group " <<  current->dataGroup();      
               }
               tcout << "\n      energies: " << current->numEnergies() 
                     << " channels: " << current->numChannels() << endl;
               if (current->isGainApplied())
               {
                  tcout << "   With applied gain:  slope = " 
                     << current->gainFactor()[1] << "  offset = " 
                     << current->gainFactor()[0] << endl;
               }
               if (current->getConstGain() || current->getLinearGain())
               {
                  tcout << "   Gain parameters will be adjusted by fit." << endl;
               }
            }
            ++resp;                       
         }

         RMFMapConstIter rmf = responses->RMFmap().begin();
         tcout << " Distinct RMF files: \n";
         while (rmf != responses->RMFmap().end())
         {
            tcout << "     " << rmf->first ;
            ++rmf;                       
         }

         tcout << endl;
      }
   }  // end showResponse

   void showSpecificParams(const StringArray& parIDs, bool isRespPar)
   {
      string prevModName;
      const size_t linelen(72);
      for (size_t i=0; i<parIDs.size(); ++i)
      {
         string modName;
         IntegerArray range;
         XSparse::stringRangePair(parIDs[i],modName,range);
         // If user enters "show par alpha:1,2", assume "alpha" also
         // applies to "2".
         const bool isNewName = !i || (modName.length() && modName != prevModName);
         if (modName.length())
            prevModName = modName;
         else
            modName = prevModName;
         if (isNewName)
         {
            // Place bottom bar under previous output
            if (i != 0)
               tcout << string(linelen,'_') << std::endl;
            if (isRespPar)
            {
               // modName string must be a source number in this context.
               size_t sourceNum = 1;
               if (modName.length())
                  sourceNum = XSutility::isInteger(modName);
               if (sourceNum == string::npos || sourceNum == 0)
               {
                  string errMsg("Invalid source number specifier: ");
                  errMsg += modName + '\n';
                  throw YellowAlert(errMsg);
               }
               if (sourceNum <= XSContainer::responses->respParContainer().size())
                  tcout << string(linelen,'=') << "\nSource No.: " << sourceNum
                      << "\nRpar Spectrum Rmodel   Rpar_name  Unit   Value\n" 
                      << std::endl;
               else
               {
                  string errMsg("Out of range source number specifier: ");
                  errMsg += modName + '\n';
                  throw YellowAlert(errMsg);
               }
            } // end if resppar
            else
            {
               // Just get any model object from the group given by modName.
               // It doesn't matter which for printing the heading.
               const string searchName = modName.length() ? 
                   modName : Model::DEFAULT();
               const Model* mod = XSContainer::models->lookup(searchName);
               if (mod)
                  mod->printHeading();
               else
               {
                  string err = modName.length() ?  "Cannot find model: " + modName + "\n" 
                          : string("No unnamed models loaded.  Must enter <mod name>:<par num>\n");
                  throw YellowAlert(err);   
               }
            }

         } // end if new name
         for (size_t j=0; j<range.size(); ++j)
         {
            if (range[j] < 1)
            {
               // Can get here if user entered wildcards, which
               // this isn't handling.
               std::ostringstream oss;
               oss << "Invalid parameter index: " << range[j] << "\n";
               throw YellowAlert(oss.str());
            }
            size_t iPar = static_cast<size_t>(range[j]);
            const Parameter* specificPar = isRespPar ?
                    HandlerUtils::lookupStringIntObj<ResponseParam>(modName,iPar)
                    : HandlerUtils::lookupStringIntObj<Parameter>(modName,iPar);
            if (specificPar)
            {
               tcout <<*specificPar;
            }
         }
         if (i == parIDs.size()-1)
            tcout << string(linelen,'_') << '\n' << std::endl;
      } // end args loop
   } // end showSpecificParams

   void showXsect()
   {
      tcout << " Photoionization Cross-Section Table:\n    ";
      tcout << FunctionUtility::XSECT() << ": " 
                 <<  FunctionUtility::crossSections(FunctionUtility::XSECT()) << endl;    
   }

   void commonDataReport(const DataSet* ds, const SpectralData* sd)
   {
      tcout << "Spectral Data File: " << ds->dataName();
      if (sd->rowNumber() > 0 ) tcout << "{"  << sd->rowNumber()  <<"}";
      tcout << "\n Assigned to Data Group " << ds->dataGroup()
           << " and Plot Group " << sd->plotGroup() << endl;
   }   

   void rebinDisplay(int plotGroup, const PlotSettings::RebinInfo& rbInfo)
   {
      string modeStr;
      switch (rbInfo.mode)
      {
         case PlotSettings::ROOTN:
            modeStr = "sqrt";
            break;
         case PlotSettings::GEHRELS1:
            modeStr = "poiss-1";
            break;
         case PlotSettings::GEHRELS2:
            modeStr = "poiss-2";
            break;
         case PlotSettings::GEHRELSM:
            modeStr = "poiss-3";
            break;
         default:
            modeStr = "quad";
            break;                
      }
      ios_base::fmtflags saveFmt(tcout.flags());
      if (plotGroup != -1)
         tcout << setw(9) << plotGroup;
      tcout << showpoint << setw(15) <<rbInfo.sigma<<setw(14)<<rbInfo.maxBins 
                <<setw(13)<< modeStr << endl;  
      tcout.flags(saveFmt); 
   }
}

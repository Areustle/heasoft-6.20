#include <xsTypes.h>
#include <XSstreams.h>
#include <XSsymbol.h>
#include <XSContainer.h>
#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/Model/Model.h>
#include <XSModel/Model/Component/Component.h>
#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h> 
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSUtil/Error/Error.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSUtil/Utils/XSutility.h> 
#include <algorithm>
#include <fstream> 
#include <sstream>
#include <cmath>

struct RangeSpecs
{
   RangeSpecs(Real lowE=.0, Real highE=.0, int nE=0, bool isLog=false) 
        : m_lowEng(lowE), m_highEng(highE), m_nEngs(nE), m_isLog(isLog) {}
   ~RangeSpecs() {}

   Real m_lowEng;
   Real m_highEng;
   int  m_nEngs;
   bool m_isLog;      
};

namespace
{
   void buildEnergyArray(const std::vector<RangeSpecs>& ranges, RealArray& outputArray);
   void readArrayFromFile(std::ifstream& inFile, RealArray& outputArray);
   size_t countTrailingCommas(const StringArray& inArgs);
   size_t determineNumberOfRanges(const IntegerArray& iParams, size_t trailingCommas, 
                size_t nSavedRanges, size_t NSPEC);
   void parseExtend(const StringArray& xsArgs, const IntegerArray& iPars, 
                XSContainer::ExtendRecord& extendPars, bool& isHigh);
}

int
XSGlobal::xsEnergies(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doEnergies(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int XSGlobal::doEnergies(const StringArray& rawArgs)
{
   using namespace XSContainer;
   const char* cmd = "eneriges";
   const size_t nArgs = rawArgs.size();
   const string RESET("reset");
   const string EXTEND("extend");
   static std::vector<RangeSpecs> savedSpecs;
   const size_t NSPEC = 3;
   static ExtendRecord saveExtend(100.0, true, 200);
   static bool isHigh = true;
   if (savedSpecs.empty())
   {
      savedSpecs.push_back(RangeSpecs(.1, 10., 1000, false));
   }
   try
   {
      if (nArgs == 1)
      {
         printDocs(cmd,"?");  
      }
      else
      {
         // Don't want to take lower case here, it could screw up a file name.
         const string secondArg = rawArgs[1];
         if (secondArg == "?")
         {
            printDocs(cmd,"?");
         }
         else
         {
            RealArray energyArray;
            Real dummyReal=.0;
            bool doRecalculate = true;
            // Criteria for assuming we are dealing with 1 or more energy
            // ranges:  Either the 2nd arg begins with a comma, OR
            // the characters up to the 1st comma (if any) can be
            // parsed as a Real.
            if (XSutility::isReal(secondArg.substr(0, secondArg.find(',')), dummyReal) 
              || secondArg[0] == ',')
            {
               // Assume we are dealing with 1 or more energy ranges.
               StringArray inArgs;
               IntegerArray iParams;
               StringArray xsArgs;
               for (size_t i=1; i<nArgs; ++i)
               {
                  inArgs.push_back(rawArgs[i]);
               }
               size_t trailingCommas = countTrailingCommas(inArgs);
               XSparse::collectParams(inArgs, iParams, xsArgs);
               const size_t nValsEntered = iParams.size();
               size_t nSavedRanges = savedSpecs.size();
               const size_t nRanges = determineNumberOfRanges(iParams, 
                                trailingCommas, nSavedRanges, NSPEC);
               if (nSavedRanges > nRanges)
               {
                  // If one of the new ranges causes a throw savedSpecs
                  // beyond nRanges won't be recoverable, but I 
                  // suppose that's OK.
                  savedSpecs.resize(nRanges);
               }

               std::vector<RangeSpecs> rangeSpecs(nRanges);
               size_t iParamIdx = 0;
               if (!nValsEntered)
               {
                  // Dealing with case of all commas.  This may reduce the
                  // number of saved ranges but it certainly can't 
                  // increase it ( a warning message is issued in 
                  // determineNumberOfRanges if user gives too many commas).
                  rangeSpecs = savedSpecs;
               }
               else
               {
                  for (size_t i=0; i<nRanges; ++i)
                  {
                     RangeSpecs& subRange = rangeSpecs[i];
                     // Start subRange off with all default values, replace
                     // when user specified is available.
                     subRange = (i < nSavedRanges) ? savedSpecs[i] :
                                rangeSpecs[nSavedRanges-1];   

                     if (i == 0)
                     {
                        if (iParams[0] == 0)
                        {
                           // low E case, can only get in here for first range.
                           Real testReal = .0;
                           if (!XSutility::isReal(xsArgs[iParamIdx], testReal))
                           {
                              string errMsg("Syntax error for low energy parameter: ");
                              errMsg += xsArgs[iParamIdx] + "\n";
                              throw YellowAlert(errMsg);
                           }
                           if (testReal < .0)
                           {
                              std::ostringstream oss;
                              oss << "Cannot set negative energy: " << testReal <<"\n";
                              throw YellowAlert(oss.str());
                           }
                           subRange.m_lowEng = testReal;
                           ++iParamIdx;
                        }
                     }
                     else
                     {
                        subRange.m_lowEng = rangeSpecs[i-1].m_highEng;
                     }

                     const size_t iEndParamForRange = (i+1)*NSPEC + 1;
                     while (iParamIdx < nValsEntered && (size_t)iParams[iParamIdx] < iEndParamForRange)
                     {
                        size_t iSpec = ((size_t)iParams[iParamIdx] - 1) % NSPEC;
                        Real testReal = .0;
                        string errMsg("Syntax error for ");
                        switch (iSpec)
                        {
                           case 0:  // high E
                              if (!XSutility::isReal(xsArgs[iParamIdx], testReal))
                              {
                                 errMsg += "high energy parameter: "+xsArgs[iParamIdx]+"\n";
                                 throw YellowAlert(errMsg);
                              }
                              if (testReal <= subRange.m_lowEng)
                              {
                                 std::ostringstream oss;
                                 oss << "Cannot set high range " << testReal 
                                     << " <= low range " << subRange.m_lowEng << "\n";
                                 throw YellowAlert(oss.str());
                              }
                              subRange.m_highEng = testReal;
                              break;
                           case 1:  // n Engs
                              {
                                 std::istringstream iss(xsArgs[iParamIdx]);
                                 int testInt = 0;
                                 if (!(iss >> testInt) || !iss.eof())
                                 {
                                    errMsg += "n energies parameter: "+xsArgs[iParamIdx]+"\n";
                                    throw YellowAlert(errMsg);
                                 }
                                 if (testInt <= 0)
                                 {
                                    string msg("N energies parameter must be > 0\n");
                                    throw YellowAlert(msg);
                                 }
                                 subRange.m_nEngs = testInt;
                              }
                              break;
                           case 2:  // log/lin
                              {
                                 string testStr = XSutility::lowerCase(xsArgs[iParamIdx]);
                                 if (testStr.find("log") == 0)
                                 {
                                    subRange.m_isLog = true;
                                 }
                                 else if (testStr.find("lin") == 0)
                                 {
                                    subRange.m_isLog = false;
                                 }
                                 else
                                 {
                                    errMsg += "log/lin parameter: "+xsArgs[iParamIdx]+"\n";
                                    throw YellowAlert(errMsg);
                                 }
                              }
                              break;
                           default:
                              break;
                        } // end switch
                        ++iParamIdx;
                     } // end while loop for subRange
                     if (subRange.m_highEng <= subRange.m_lowEng)
                     {
                        // Can get here if user didn't specifiy high E for 
                        // range group > 1. 
                        std::ostringstream oss;
                        oss << "Must specify high energy value for range group " 
                            << i+1 << "\n";
                        throw YellowAlert(oss.str());
                     }
                     // If we made it this far things are all valid within
                     // subRange. 
                     if (i < nSavedRanges)
                        savedSpecs[i] = subRange;
                     else
                        savedSpecs.push_back(subRange); 
                  } // end for loop for all ranges
               } // end if not dealing with only commas
               tcout << "\nModels will now use energy array created from:" << std::endl;
               for (size_t i=0; i<nRanges; ++i)
               {
                  const RangeSpecs& specs = rangeSpecs[i];
                  string binType = specs.m_isLog ? string(" log ") : string(" linear ");
                  tcout << "   " << specs.m_lowEng << " - " << specs.m_highEng
                     << "   " << specs.m_nEngs << binType << "bins" << std::endl;
               }
               tcout << std::endl;                  
               buildEnergyArray(rangeSpecs, energyArray);
               models->applyAutonomousEnergies(energyArray);
            } // end if parsing range values 
            else if (XSutility::lowerCase(secondArg) == RESET)
            {
               // Simply leave energyArray empty.
               // Sending in an empty energyArray below causes
               // models to delete their autonomous energy objects.
               tcout << "\nAll model energies will be taken from non-extended response energies.\n"
                  << std::endl;
               models->applyAutonomousEnergies(energyArray);
            }
            else if (XSutility::lowerCase(secondArg) == EXTEND)
            {
               if (!datasets->numberOfSpectra())
               {
                  tcout << "No spectra loaded, nothing to extend." << std::endl;
                  doRecalculate = false;
               }
               else
               {
                  ExtendRecord extendPars(saveExtend);
                  bool tmpIsHigh = isHigh;
                  StringArray inArgs;
                  IntegerArray iParams;
                  StringArray xsArgs;
                  for (size_t i=2; i<nArgs; ++i)
                  {
                     inArgs.push_back(rawArgs[i]);
                  }
                  XSparse::collectParams(inArgs, iParams, xsArgs);
                  parseExtend(xsArgs, iParams, extendPars, tmpIsHigh);
                  models->modifyExtendedEnergies(extendPars, tmpIsHigh);
                  saveExtend = extendPars;
                  isHigh = tmpIsHigh;

                  const ExtendRecord& lowSetting = models->extendedEnergy().first;
                  const ExtendRecord& highSetting = models->extendedEnergy().second;
                  tcout << "\nModels will use response energies extended to:"<<std::endl;
                  if (lowSetting.nBins)
                  {
                     string point;
                     if (lowSetting.energy == floor(lowSetting.energy) && 
                        log10(lowSetting.energy) < tcout.precision())
                           point = ".0"; 
                     string binType = lowSetting.isLog ? string(" log "):string(" linear ");
                     tcout << "   Low:  " << lowSetting.energy << point
                        << " in " << lowSetting.nBins << binType << "bins" << std::endl;
                  } 
                  if (highSetting.nBins)
                  {
                     string point;
                     if (highSetting.energy == floor(highSetting.energy) && 
                        log10(highSetting.energy) < tcout.precision())
                           point = ".0"; 
                     string binType = highSetting.isLog ? string(" log "):string(" linear ");
                     tcout << "   High: " << highSetting.energy << point
                        << " in " << highSetting.nBins << binType << "bins" << std::endl;
                  } 
               }
            }
            else
            {
               // Assume secondArg is an ASCII file of energy values.
               std::ifstream inFile(secondArg.c_str());
               if (!inFile)
               {
                  string msg("Unable to open energy array file ");
                  msg += secondArg + "\n";
                  throw YellowAlert(msg);
               }
               readArrayFromFile(inFile, energyArray);
               tcout << "\nModels will now use energy array read from file: "
                  << secondArg << "\n" << std::endl;
               models->applyAutonomousEnergies(energyArray);
            }

            size_t nEngs = energyArray.size();
            if (tpout.maxChatter() >= 30)
            {
               tcout << "New energy array: " << std::endl;
               size_t iEng = 0;
               while (iEng < nEngs)
               {
                  for (size_t i=0; i<5 && iEng<nEngs; ++i)
                  {
                     tcout << energyArray[iEng] << "   ";
                     ++iEng;
                  }
                  tcout << std::endl;
               }
            }
            if (doRecalculate)
            {
              // Active models will be recalculated during Fit::Update.
               ModelMap::const_iterator itMod = models->modelSet().begin();
               ModelMap::const_iterator itEnd = models->modelSet().end();
               while (itMod != itEnd)
               {
                  Model* mod = itMod->second;
                  if (!mod->isActive())
                  {
                     // Table components are troublesome.  If energy array has
                     // changed, they explicitly need an initializeForFit
                     // call BEFORE recalculating.  This sets up the bin
                     // weighting.
                     std::vector<Component*> comps;
                     mod->bundleComponents(comps);
                     for (size_t i=0; i<comps.size(); ++i)
                        comps[i]->initializeForFit();
                     mod->setComputeFlag(true);
                     models->calculate(mod->name());
                  }
                  ++itMod;
               }
            }
            models->Notify();
         } // end if not help request
      } // end if nArgs > 1
   }
   catch (YellowAlert&)
   {
      return -1;
   }
   return 0;
}

namespace
{
   void buildEnergyArray(const std::vector<RangeSpecs>& ranges, RealArray& outputArray)
   {
      // Presumably all errors have been tested for by the time this
      // is called, so simply build the array.

      const size_t nRanges = ranges.size();
      size_t nTotal = 0;
      for (size_t i=0; i<nRanges; ++i)
      {
         nTotal += ranges[i].m_nEngs;
      }
      // nEngs refers to number of BINS, so need to add 1.
      nTotal++;
      outputArray.resize(nTotal, .0);

      size_t iStart=0;
      for (size_t i=0; i<nRanges; ++i)
      {
         const RangeSpecs& currRange = ranges[i];
         if (i==0)
         {
            outputArray[0] = currRange.m_lowEng;
            ++iStart;
         }
         if (currRange.m_isLog)
         {
            Real interval = log(currRange.m_highEng/currRange.m_lowEng)/
                        currRange.m_nEngs;
            for (size_t j=1; j<=(size_t)currRange.m_nEngs; ++j)
            {
               outputArray[iStart] = currRange.m_lowEng*exp(j*interval);
               ++iStart;
            }
         }
         else
         {
            Real interval = (currRange.m_highEng - currRange.m_lowEng)/
                        currRange.m_nEngs;
            Real eng = currRange.m_lowEng;
            for (size_t j=0; j<(size_t)currRange.m_nEngs; ++j)
            {
               eng += interval;
               outputArray[iStart] = eng;
               ++iStart;
            }
         }
      }      
   }

   void readArrayFromFile(std::ifstream& inFile, RealArray& outputArray)
   {
      std::vector<Real> tmpEngs;
      size_t lineCount = 0;
      while (!inFile.eof())
      {
         string line;
         std::getline(inFile, line);
         ++lineCount;
         if (line.length())
         {
            const string WS(" \t");
            const char COMMENT = '#';
            // Ignore if line contains only whitespace or
            // begins with a '#'.
            string::size_type nwsPos = line.find_first_not_of(WS);
            if (nwsPos != string::npos && line[nwsPos] != COMMENT)
            {
               std::istringstream issTest(line);
               Real testReal=.0;
               if (!(issTest >> testReal))
               {
                  std::ostringstream err;
                  err << "Unable to read floating-point value on line "
                    << lineCount << " of input energy file\n";
                  throw YellowAlert(err.str()); 
               }
               else if (!issTest.eof())
               {
                  // Allow case of a real followed by '#'.
                  string next;
                  issTest >> next;
                  if (next.length() && next[0] == '#')
                  {
                     tmpEngs.push_back(testReal);
                  }
                  else
                  {
                     std::ostringstream err;
                     err << "Unable to read floating-point value on line "
                       << lineCount << " of input energy file\n";
                     throw YellowAlert(err.str());                      
                  }
               }
               else
               {
                  tmpEngs.push_back(testReal);
               }
            }
         }
      }
      // Now validate all the Reals in tmpEngs.
      const size_t nVals = tmpEngs.size();
      Real prevVal = -1.0;
      for (size_t i=0; i<nVals; ++i)
      {
         Real testVal = tmpEngs[i];
         if (testVal < 0.0)
         {
            std::ostringstream errMsg;
            errMsg << "Energy array element number " << i+1 
               << " is a negative value.\n";
            throw YellowAlert(errMsg.str());
         }
         if (testVal == prevVal)
         {
            std::ostringstream errMsg;
            errMsg << "Energy array element number " << i+1 
               << " is the same as the previous element.\n";
            throw YellowAlert(errMsg.str());
         }
         if (testVal < prevVal)
         {
            std::ostringstream errMsg;
            errMsg << "Energy array element number " << i+1
               << " is less than the preceding value.\n"
               << "Values must be in sorted ascending order.\n";
            throw YellowAlert(errMsg.str());
         }
         prevVal = testVal;
      }
      // All is well
      outputArray.resize(nVals);
      for (size_t i=0; i<nVals; ++i)
      {
         outputArray[i] = tmpEngs[i];
      }
   }

   size_t countTrailingCommas(const StringArray& inArgs)
   {
      size_t trailingCommas = 0;
      size_t nArgs = inArgs.size();
      bool nonCommaFound = false;
      if (nArgs)
      {
         for (int i=static_cast<int>(nArgs-1); i>=0 && !nonCommaFound; --i)
         {
            const string& arg = inArgs[i];
            string::size_type argPos = arg.rfind(',');
            if (argPos != arg.length()-1)
               nonCommaFound = true;
            else
            {
               ++trailingCommas;
               while (!nonCommaFound && argPos != 0)
               {
                  --argPos;
                  if (arg[argPos] == ',')
                     ++trailingCommas;
                  else
                     nonCommaFound = true;                 
               }
            }
         }
      }
      return trailingCommas;
   }

   size_t determineNumberOfRanges(const IntegerArray& iParams, size_t trailingCommas, 
                size_t nSavedRanges, size_t NSPEC)
   {
      size_t nRanges = 1;
      // <low eng> is only entered for the first range group.
      // For all others, rangeSpecs[n].m_lowEng = rangeSpecs[n-1].m_highEng.
      const size_t nValsEntered = iParams.size();

      // 0-3 = 1 range, 4-6 = 2 ranges, etc...
      // Need to watch out for the case where trailingCommas add 
      // additional ranges AND there are no saved defaults for them.
      // Can't have both nValsEntered == 0 and no trailing commas,
      // otherwise nArgs = 1 and this would be handled at the top.
      if (nValsEntered == 0)
      {
         // Will get in here if only commas are entered.
         nRanges = (trailingCommas - 1)/NSPEC + 1;
         if (nRanges > nSavedRanges)
         {
            tcout <<"***Warning: Default parameters currently only exist for the first "
              << nSavedRanges << " range(s).\n   Commas after that will be ignored."
              << std::endl;
            nRanges = nSavedRanges;
         }
      }
      else
      {
         size_t lastParEntered = static_cast<size_t>(iParams[nValsEntered-1]);
         // First try without adding trailing commas.
         if (lastParEntered > 0)                  
            nRanges = (lastParEntered - 1)/NSPEC + 1; 
         if (trailingCommas)
         {
            size_t nRangesWithCommas = (lastParEntered+trailingCommas-1)/NSPEC + 1;
            if (nRangesWithCommas != nRanges && nRangesWithCommas > nSavedRanges)
            {
               tcout <<"***Warning: Default parameters currently only exist for the first "
                 << nSavedRanges << " range(s).\n   Trailing commas will be ignored."
                 << std::endl;
               nRangesWithCommas = std::max(nSavedRanges, nRanges);
            }
            nRanges = nRangesWithCommas;
         }
      }
      return nRanges;
   }


   void parseExtend(const StringArray& xsArgs, const IntegerArray& iPars, 
                XSContainer::ExtendRecord& extendPars, bool& isHigh)
   {
      size_t nPars = iPars.size();
      bool warned = false;
      for (size_t i=0; i<nPars; ++i)
      {
         std::istringstream iss(xsArgs[i]);
         switch (iPars[i])
         {
            case 0: // high|low
            {
               string lc(XSutility::lowerCase(xsArgs[i]));
               const string HIGH("high");
               const string LOW("low");
               if (LOW.find(lc) == 0)
                  isHigh = false;
               else if (HIGH.find(lc) == 0)
                  isHigh = true;
               else
               {
                  string errMsg("First parameter after \"extend\" option must be low|high\n");
                  throw YellowAlert(errMsg);
               }
            }
               break;
            case 1: // energy
            {
               Real energy = .0;
               if (!(iss >> energy) || !iss.eof() || energy < .0)
               {
                  string errMsg("Improper energy value for the \"extend\" option: ");
                  errMsg += xsArgs[i] + "\n";
                  throw YellowAlert(errMsg);
               }
               extendPars.energy = energy;
            }
               break;
            case 2: // nBins
            {
               int nBins = 0;
               if (!(iss >> nBins) || !iss.eof() || nBins <= 0)
               {
                  string errMsg("Improper nBins value for the \"extend\" option: ");
                  errMsg += xsArgs[i] + "\n";
                  throw YellowAlert(errMsg);
               }
               extendPars.nBins = nBins;
            }
               break;
            case 3: // log|linear
            {
               string lc(XSutility::lowerCase(xsArgs[i]));
               const string LOG("log");
               const string LINEAR("linear");
               if (LOG.find(lc) == 0)
                  extendPars.isLog = true;
               else if (LINEAR.find(lc) == 0)
                  extendPars.isLog = false;
               else
               {
                  string errMsg("Fourth parameter after \"extend\" option must be log|linear\n");
                  throw YellowAlert(errMsg);
               }

            }
               break;
            default:
               if (!warned)
               {
                  tcout <<"***Warning: \"extend\" option takes a maximum of 4 parameters."
                        <<"\n     Additional arguments will be ignored." << std::endl;
                  warned = true;
               }
               break;
         }
      }
   }
}

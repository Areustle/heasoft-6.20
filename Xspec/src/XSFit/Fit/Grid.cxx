//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSsymbol.h>
#include <XSstreams.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSUtil/Utils/XSstream.h>

// Grid
#include <XSFit/Fit/Grid.h>


// Class Grid::ParameterSpec 

Grid::ParameterSpec::ParameterSpec (int paramIndex, int fullIndex, Real low, Real high, int levels, bool logSpacing)
      : log(logSpacing),      
        lowRange(low),
        highRange(high),
        parIndex(paramIndex),
        fullFitIndex(fullIndex),
        intervals(levels),
        name(""),
        units(""),
        value(0.),
        address(0),
        parameterValues()
{
}


// Class Grid::InvalidParameter 

Grid::InvalidParameter::InvalidParameter (const std::string& diag)
  : YellowAlert(string("Invalid Parameter: "))  
{
   tcerr << diag << '\n';
}


// Class Grid::InsufficientParameterArguments 

Grid::InsufficientParameterArguments::InsufficientParameterArguments (const string& diag)
{
}


// Class Grid 
const Real Grid::s_FLAG = SMALL;
const int Grid::INTERVALS = 10;

Grid::Grid()
      : m_minStat(1.0),
        m_parameter(),
        m_grid(),
        m_contourLevels()
{
}


Grid::~Grid()
{
   SpecContainer dummy;
   replaceParameterSettings(dummy);
}


void Grid::replaceParameterSettings (const SpecContainer& newSetting) throw ()
{
   for (size_t i=0; i<m_parameter.size(); ++i)
      delete m_parameter[i];
      
   // Make shallow copies of newSetting elements.  m_parameter 
   //  takes over the ownership of these ParameterSpec objects.
   m_parameter.resize(newSetting.size());
   for (size_t i=0; i<m_parameter.size(); i++) 
      m_parameter[i] = newSetting[i];
     
   
}

bool Grid::retrieveBestSetting (const StringArray& userString, IntegerArray& userIndex, StringArray& parString, IntegerArray& parIndex)
{

           // deal with "best" or "current" arguments.
   StringArray::const_iterator is (userString.begin());
   StringArray::const_iterator isEnd (userString.end());
   IntegerArray::iterator ii(userIndex.begin());
   parIndex.reserve (userIndex.size());
   parString.reserve (userIndex.size());
   const string TEST("cCbB");
   size_t numberOfBCargs(0);
   bool isBest = false;
   while (is != isEnd)
   {
      // loc = -1 if not present, 0 or 1 if "current".
      const string& t (*is);
      int loc( TEST.find(t[0]));
      // need to test that string isn't a modelName:parameter 
      // index designation.
      bool qualifiedParIndex( is->find_first_of(':') != string::npos );
      if ( loc < 0 || qualifiedParIndex )
      {
         parString.push_back(*is);
         parIndex.push_back(*ii);
      }
      else 
      {
         isBest = ( loc > 1 );

         // removing an argument so have to reduce the index.
         IntegerArray::iterator iird(ii + 1);
         while (iird != userIndex.end())
         {
            (*iird) -= 1;
            ++iird;       
         }


         ++numberOfBCargs;
         if (numberOfBCargs == 2)
         {
            tcerr << " Warning: first setting of current/best "
                  << "applies to all parameters:\n should "
                  << "be entered only once\n";       
         }       
      }
      ++is, ++ii;    
   }
   return isBest;
}

Grid::ParameterSpec* Grid::makeParameterSpec (const Grid::ParameterSpec* const parameter, const string& parameterName, int paramIndex, int fullIndex, std::pair<Real,Real>& lim, bool logSetting, int intervals)
{
  ParameterSpec* r(0);
  if ( lim.first == lim.second )
  {
     string msg (" Range for parameter: ");
     msg += parameterName;
     msg += " has no width";
     throw InvalidParameter(msg);       

  }
  if ( logSetting )
  {
     if (  lim.first <= 0 || lim.second <= 0)
     {
        string msg (" Log setting requested but parameter limits include value 0 ");
        throw InvalidParameter(msg);       
     }

  }                                       

  if ( parameter )
  {
     // we established that the previous
     // setting was valid.
     r = new ParameterSpec(*parameter);

     if ( lim.first !=  s_FLAG )
     {
          r->lowRange = lim.first;       
     }
     if ( lim.second != s_FLAG )
     {
          r->highRange = lim.second;       
     } 
     if ( intervals > 0)
     {
          r->intervals = intervals;       
     }
     r->log = logSetting;
     r->name = parameterName;
     r->parIndex = paramIndex;
     r->fullFitIndex = fullIndex;
  }
  else
  {
     // exit if parameter has insufficient settings.
     // this will leave the current set of parameters
     // to be stepped unchanged.
     if ( lim.first == s_FLAG || lim.second == s_FLAG )
     {
        string msg (" Error: insufficient arguments to grid parameter");
        msg += parameterName;
        throw InsufficientParameterArguments(msg);    
     }
     else 
     {
        int setLevel = intervals > 0 ? intervals : INTERVALS;
        r  = new ParameterSpec(paramIndex,fullIndex,lim.first,lim.second,setLevel,logSetting);
        r->name = parameterName;
     }
  }  

  return r;      
}

bool Grid::parseParameterRange (const string& inputArg, int argIndex, int& base, std::pair<Real,Real>& lims, int& lev, bool& ls, bool& isDelta)
{
  // Meant to process 1 delimited input argument at time, returning false
  // until the last argument of a ParameterSpec is detected.  
  // Assumes best|current specifier(s) has already been removed and processed.
  const string LOG("nNlL");
  const string DELTA("dD");
  string::size_type loc = LOG.find(inputArg[0]);
  bool result(false);
  if ( loc == string::npos )
  {
    // numeric arguments are in order
    // low, high, levels. The first two
    // are mandatory if not already set.
    // if the string delta is entered then
    // lims.first is not set and lims.second
    // is the delta value
     int thisArg(argIndex - base);
     std::istringstream aa(inputArg);
     aa.exceptions(std::ios_base::failbit);
     string promptIfBad;
     try 
     {
        switch ( thisArg )
        {
	   case 0:
               // ParamID has already been processed, so do nothing.
	       break;
           case 1:
	       if ( DELTA.find(inputArg[0]) == string::npos ) {
		 promptIfBad = "Low Range";
		 aa >> lims.first;
		 isDelta = false;
	       } else {
		 isDelta = true;
	       }
               break;
           case 2:
               promptIfBad = "High Range";
               aa >> lims.second;
               break;
           case 3:
               promptIfBad = "Intervals";
               aa >> lev;
               result = true;
               break;
           default:
               // Can get here if case 3 is skipped via comma delimiters.
               result = true;
               break;
        }
     }
     catch ( std::exception& )
     {
        tcout << "Input Error: setting for "
          << promptIfBad << " is invalid " << std::endl;

        Real replReal(0.);
        int replInt(0);
        string prompt(promptIfBad + ": ");
        switch (thisArg)
        {
           case 1:
              XSparse::getArg(tcin,replReal,lims.first,prompt);  
                lims.first = replReal;
              break;
           case 2: 
              XSparse::getArg(tcin,replReal,lims.second,prompt);  
              lims.second = replReal;
              break;
           case 3:
              XSparse::getArg(tcin,replInt,lev,prompt); 
              lev = replInt;
              result = true;
              break;
        }      
     }  
  } // end if numeric args
  else
  {
     // We have a log or nolog.  It should not get here if log|nolog
     // is the very last argument (with no trailing commas), in which
     // case the calling function should have trapped it. 
     ls = ( loc > 1);
     ++base;                                                               
  }   

  return result;
}

void Grid::resetContourMap ()
{
   m_grid.resize(0);
}

void Grid::reinitialize (const SpecContainer& newSetting)
{
   replaceParameterSettings(newSetting);

   size_t Noutput (1);

   SpecContainer::iterator p    = m_parameter.begin();
   SpecContainer::iterator pEnd = m_parameter.end();

   while ( p != pEnd )
   {
      ParameterSpec*& spec = *p; 
      // make arrays of the fixed values of the parameters of the grid.
      spec->parameterValues.resize(spec->intervals+1,0.);       
      std::vector<Real>&  gv = spec->parameterValues;
      // size of the output array is product of the total number of parameter
      // values at which the fit will be evaluated.
      Noutput *= (spec->intervals + 1);
      gv[0] = spec->lowRange;
      gv[spec->intervals] = spec->highRange;

      if ( spec->log )
      {
         Real stepSize ( (log(spec->highRange) - log(spec->lowRange))/spec->intervals ); 
         for (size_t j = 1; j < spec->intervals; ++j)
         {
            gv[j] = exp(log(spec->lowRange) + j*stepSize);
         }        
      }
      else
      {
         Real stepSize ( (spec->highRange - spec->lowRange)/spec->intervals ) ;
         for (size_t j = 1; j < spec->intervals; ++j)
         {
            gv[j] = spec->lowRange + j*stepSize;
         }        

      }
      ++p;
   }

   // initialize the output array.
   m_grid.resize(Noutput,0);
}

// Additional Declarations

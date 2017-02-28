//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// XSutility
#include <XSUtil/Utils/XSutility.h>
#include <sys/stat.h>
#include <limits>
#include <climits>
#include <ctype.h>
#include <iostream>
#include <sstream>
#include <locale>
#include <XSUtil/Utils/XSstream.h>
#include <XSUtil/Error/Error.h>
#include <XSUtil/Utils/IosHolder.h>
#include <unistd.h>
#include <algorithm>
#include <cstring>
#include <cctype>

namespace XSutility {

  string addSuffix( const string& oldName, XSutility::fileType type)
  {
          static const string pha = ".pha";
          static const string bck = ".bck";
          static const string cor = ".cor";
          static const string rsp = ".rsp";
          static const string arf = ".arf";

          int dot = oldName.find_last_of('.');
          string newName(oldName);
          if (dot >= 1) return newName;

          switch (type)
          {
                  case PHA:  newName += pha;  break;
                  case BCK:  newName += bck;  break;
                  case RSP:  newName += rsp;  break;
                  case COR:  newName += cor;  break;
                  case ARF:  newName += arf;  break;
                  default:  break;
          } 
          return newName;
  }


   void printValidOptions(std::ostream& s, const string& command, const std::map<string, size_t>& options)
   {
      std::ostringstream sOut;

      s << "Syntax: " << command << " <option> [args] - valid options are:" << std::endl;
      std::map<string,size_t>::const_iterator itOpt = options.begin();
      std::map<string,size_t>::const_iterator itOptEnd = options.end();
      size_t iout = 0;
      while (itOpt != itOptEnd)
      {
         const size_t NCOL = 5;
         const int COLWIDTH = 14;
         if (iout % NCOL == 0)  sOut << "\n" ;
         sOut << std::setw(COLWIDTH) << std::left << itOpt->first;
         ++iout;
         ++itOpt;
      }       
      s << sOut.str() << std::endl;
   }


  int yesToQuestion (const string& prompt, int defaultAns, std::istream& a)
  {
     using namespace std;
     string answer;
     int result (defaultAns);
     const string BREAK("/*");
     try
     {
        a.exceptions(ios_base::badbit|ios_base::eofbit);
        XSstream::setPrompter(a,prompt);
        std::ostream& q = *(a.tie());
        while (true)
        {
           getline(a,answer);
           const size_t sz = answer.size();
           if (sz > 0) 
           {
              if (sz > 1 && answer.substr(0,2) == BREAK)
              {
                 result = -1;
                 break;
              }
              else
              { 
                 switch (answer[0])
                 {
                    case 'y':
                    case 'Y':
                            result = 1;
                            break;
                    case 'n':       
                    case 'N': 
                            result = 0;
                            break; 
                    default: 
                            q << " please enter Y/y or N/n " ;
                            continue;
                 }
              }
           }
           else
           {
              result = defaultAns;
           }
           break;
        }
     }
     catch (...)
     {
        result = defaultAns;       
     }
     a.exceptions(ios_base::goodbit);
     return result;

  }

  string lowerCase(const string& inputString)
  {
        size_t n(inputString.length());
        string outputString(inputString);
        for (size_t l = 0 ; l < n; ++l)
        {
                 outputString[l] = tolower(inputString[l]);          
        }
        // this looks extremely dumb but is a workaround for a g++ compiler
        // bug.
        return outputString.substr(0,n);  
  }

  string upperCase(const string& inputString)
  {
        size_t n(inputString.length());
        string outputString(inputString);
        for (size_t l = 0 ; l < n; ++l)
        {
                 outputString[l] = toupper(inputString[l]);          
        }
        // this looks extremely dumb but is a workaround for a g++ compiler
        // bug.
        return outputString.substr(0,n);  
  }


  size_t isInteger(const string& inputString)
  {
        // returns a positive finite integer if the string consists
        // entirely of digits: otherwise returns FFFF.
	size_t intResult (std::string::npos);
        size_t i = 0;
	size_t n = inputString.length();
        while ( i < n  && std::isdigit(inputString[i],std::locale()) )
	{
	   ++i; 
	}
        if (i == n)
        {
                // can't throw because everything is a digit.
		std::istringstream s(inputString);
		s >> intResult;
	}

        return intResult;
  }

  bool isReal (const string& inputString, Real& realResult)
  {
        using namespace std;
        bool result(false);
        static const string realChars("0123456789+-.Ee");

        if ( inputString.find_first_not_of(realChars) == std::string::npos) 
        {
                istringstream s(inputString);
                // ignore eof exceptions.
                s.exceptions(ios_base::badbit | ios_base::failbit );
                try
                {
                        s >> realResult;
                        // check that there is no gobbledegook after
                        // the Real.                        
                        result = s.eof();       
                }
                catch (ios_base::failure&)
                {
                        result = false;       
                }
        }
        return result;
  }

  string insertEscapeChars(const string& inputString, const string& specialChars, const char ESC)
  {
     string newString;
     const string::size_type nChars = inputString.size();
     for (string::size_type i=0; i<nChars; ++i)
     {
        const char c = inputString[i];
        if (specialChars.find(c) != string::npos)
           newString += ESC;
        newString += c;
     }
     return newString;
  }

  unsigned int openBrackets (const string& ss)
  {
#ifndef STD_COUNT_DEFECT
        unsigned int nBracket(std::count(ss.begin(),ss.end(),'{'));
#else
        unsigned int nBracket(0);
        std::count(ss.begin(),ss.end(),'{',nBracket);
#endif
	return nBracket;
  }

  unsigned int closeBrackets (const string& ss)
  {
#ifndef STD_COUNT_DEFECT
        unsigned int nBracket(std::count(ss.begin(),ss.end(),'}'));
#else
        unsigned int nBracket(0);
        std::count(ss.begin(),ss.end(),'}',nBracket);
#endif
	return nBracket;
  }

   void printStrings(const std::vector<string>& strings, std::ostream& s, int perLine, int fieldWidth)
   {
        // print perLine vectors per line.
        using namespace std;
        int count = 0;     
        int N = strings.size();

        while ( (N  - count) >= perLine)
        {
                for (int k = 0; k < perLine; ++k ) s << setw(fieldWidth) << strings[count + k];
                s << '\n';
                count += perLine;      
        } 

        while (count < N) 
        {
                s <<  setw(fieldWidth) << strings[count];
                ++count;        
        }
        s << '\n';  


   }

   void find (const RealArray& array, const Real& target, int& n )
   {
        size_t N (array.size()); 
        if (N < 2)
        {
           throw RedAlert("Attempting to find brackets in array smaller than size 2.");
        }       
        bool increasing(array[0] < array[N-1]);        
        if ( increasing )
        {
           // clarity: the returned value n contains the  index
           // such that  array[n] <= target < array[n+1]
           // Set n =  -1 if target < array[0],
           //       = N-1 if target >= array[N-1].
           if (target < array[0])
           {
              n = -1;
           }
           else if (target >= array[N-1])
           {
              n = N-1;
           }
           else
           {
              n = 0;
              int upper = N-1;
              bisect(n, upper, array, target, increasing);
           }           
        } 
        else
        {
           // Set n such that array[n] >= target > array[n+1]
           //   n = -1 if target > array[0]
           //   n = N-1 if target <= array[N-1]
           if (target > array[0])
           {
              n = -1;
           }
           else if (target <= array[N-1])
           {
              n = N-1;
           }
           else
           {
              n = 0;
              int upper = N-1;
              bisect(n, upper, array, target, increasing);
           }
        }

   }   

   void bisect( int& lower, int& upper, const RealArray& array, const Real& target, bool increasing)
   {
           while ( upper - lower > 1)
           {
                int mid   = ( lower + upper )/ 2;
                if (increasing)
                {
                        if (target < array[mid] ) upper = mid;
                        else lower = mid;
                }       
                else
                {
                        if ( target > array[mid]) upper = mid;
                        else lower = mid;      
                }
           }
   } 

   std::pair<Real,Real> 
   confidenceRange(Real confidenceLevel, std::vector<Real>& array, bool fullSort)
   {
      if (confidenceLevel > 100.0 || confidenceLevel < .0)
         throw YellowAlert("Out of range confidence level (.0 - 100.0%)\n");
      const size_t N(array.size());
      if (N < 2)
         throw YellowAlert("Sample array size not large enough to get a confidence range.\n");
      std::pair<Real,Real> range(0,0);
      Real offset = N*(1.0 - confidenceLevel/100.0)/2.0;
      // Add .5 for truncation effect.  If it passed the 2 checks above,
      // lower and upper must be within array size.  
      size_t lower = static_cast<size_t>(offset+.5);
      // Since highest bin is N-1, want (N-1)-offset+.5
      size_t upper = static_cast<size_t>(N-offset-.5);
      if (fullSort)
         std::sort(array.begin(), array.end());
      else
         std::nth_element(array.begin(), array.begin()+lower, array.end());     
      range.first = array[lower];
      if (!fullSort)
         std::nth_element(array.begin(), array.begin()+upper, array.end());
      range.second = array[upper];

      return range;

   }

   void starPrint ( const std::string& slogan, int i, std::ostream& s )
   {
        const char elts[4] = {'|','/','-','\\'};
        if ( i == 0 ) s << slogan << "...";
        string output(" \b");
        output[0] = elts[i % 4];
        s << output;               
   }

   const string& xs_version()
   {
      static const string version = "12.9.1";
      return version;
   }

   void XSVersionString(string &title, string &buildDate)
   {
           const string& version = xs_version(); 
           static struct stat statbuf;       
	   title = "XSPEC version: " + version;       
           stat(getenv("XSPEC_CURRENT"),&statbuf);
           buildDate = "Build Date/Time: " + string(ctime(&statbuf.st_mtime));
           // ctime standard library function inserts a /n at the
           // end of its return string, remove it.
           buildDate.erase(buildDate.length()-1);
   }



   void decodeKey(const string& input, string& outputName, size_t& outputIndex)
   {

        std::istringstream codedKey(input);
        size_t maxSize(input.size()+1);
        XSutility::auto_array_ptr<char> __buf(new char[maxSize]);
        codedKey.get(__buf.get(),maxSize,'$');
        codedKey.ignore();
        codedKey >> outputIndex;
        outputName = std::string(__buf.get());
   }

   bool equalVector(const RealArray& left, const RealArray& right, Real tolerance)
   {
        if (left.size() != right.size()) return false;
        size_t N (left.size());
        bool equal(true);
        size_t i (0);
        if (tolerance == 0)
        {
                while ( i < N && equal)
                {
                        if (right[i] != left[i]) equal = false; 
                        ++i;      
                }       
        }
        else
        {
                while ( i < N && equal)
                {
                        if (std::abs(right[i] - left[i]) > tolerance ) equal = false; 
                        ++i;      
                }       

        }

        return equal;   
   }

   RealArray gehrelsWeighting(const RealArray& rate, Real norm)
   {
        size_t n = rate.size();
        RealArray gwVar(0.,n);
        static const Real OFFSET(0.75);

        for (size_t j = 0; j < n; ++j)
        {
                Real __tmp = (1. + sqrt(rate[j]*norm + OFFSET))/norm;
                gwVar[j] = __tmp*__tmp;
        }             
        return gwVar;
   }

   RealArray churazovWeighting(const RealArray& rate, Real norm)
   {
        int n = rate.size();
        RealArray cwVar(0.,n);
        static const Real MAXCOUNTS = 20.;
        for (int l = 0; l < n; ++l)
        {
                Real current = rate[l];
                bool terminate = false;
                int lower = l;
                int upper = l;
                size_t bins  = 1;
                while (!terminate)
                {
                        --lower;       
                        ++upper;
                        if (lower >=0 )
                        {
                                ++bins;
                                current += rate[lower];      
                        }
                        if (upper < n)
                        {
                                ++bins;
                                current += rate[upper];
                        }
                        terminate = ( (lower <= 0 && upper >= n) || current >= MAXCOUNTS);
                }

                cwVar[l] = current / bins / norm;               
        }

        return cwVar;
   }  

    string getRunPath()
    {
	char buffer[PATH_MAX];
	string runPath;

	if (!getcwd(buffer, PATH_MAX))
	{
	    *IosHolder::outHolder() << "\n***Warning: Xspec is unable to store the full path name to the data file\n"
	          << "   This may cause failure in functions requiring file to be re-opened at a later time."
	          << std::endl;
	    runPath = "";
	}
	else
	    runPath = string(buffer);

	return runPath;
    }

    string peek(std::istream& s, size_t bytes)
    {
	string str;

	if(s)
	{
	    char* __buf = new char[bytes + 1];
	    memset(__buf, 0, bytes + 1);

	    size_t bytesRead = s.readsome(__buf, bytes);
	    str = __buf;

	    for(; bytesRead > 0; --bytesRead)
		s.unget();

	    delete [] __buf;
	}

	return str;
    }

    // Parameterized Class XSutility::Match1stKeyString 

    // Parameterized Class XSutility::MatchName 

    // Parameterized Class XSutility::MatchPtrName 

    // Parameterized Class XSutility::Carray 

    // Class XSutility::Nocase 

    bool Nocase::operator () (const string& x, const string& y) const
    {
       // This algorithm is taken from the example in Stroustrup C++,
       // section 17.1.4.1.

       // returns true of x is lexicographically less than y, ignoring case.
       string::const_iterator p = x.begin();
       string::const_iterator q = y.begin();
       while (p != x.end() && q != y.end() && (toupper(*p) == toupper(*q)))
       {
          ++p;
          ++q;
       }
       if (p == x.end())
          return (q != y.end());  // ie. false if x == y
       if (q == y.end())
          return false;
       return toupper(*p) < toupper(*q);
    }

} // namespace XSutility

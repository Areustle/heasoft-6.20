
#include "AstroFunctions.h"
#include <cmath>
#include <sstream>

namespace Numerics {

   namespace AstroFunctions {

      bool sexagesimalToDecimal(const string& input, Real& decDegrees)
      {
         // Function to convert angles entered in sexagesimal string
         // format to decimal degrees.  Input must of format
         // "hours[:minutes[:seconds]]".  There must be no whitespace
         // inserted in the string.  If there is, behavior is undefined.
         // (Whitespace leading and trailing the entire string is OK.).
         // If input string is empty (or all whitespace) program simply
         // returns 0.0 degrees with status = OK.

         // Additional checks and requirements:
         //  1.  Only the last parameter entered is allowed to have
         //      floating-point values.  All others must have integer
         //      values.  (They can still have floating-point type
         //      however. ie "3","3.", or "3.0" but not "3.01".)
         //  2.  Only the hours parameter may be negative.
         //  3.  Allowed ranges: minutes = 0-59 if followed by seconds,
         //                                else 0-59.99999.....
         //                      seconds = 0-59.99999......
         // Function returns: true if all is OK
         //                   false if ANY errors, in which case degrees = 0.

         const string delim(":");
         const string wspace(" \t\n");
         string sub("");
         Real hours=0.0, min=0.0, sec=0.0;
         string::size_type len=0;
         int argsRead = 0;

         decDegrees = 0.0;
         // This should not happen.  Just to be sure....
         if (!input.length())
         {
            return true;
         }          
         // Ignore any leading or trailing whitespace that might
         // have gotten into input.
         string::size_type startPos = input.find_first_not_of(wspace);
         if (startPos == string::npos)
         {
             // input is all whitespace
             return true;
         }
         string::size_type endPos = input.find_last_not_of(wspace);
         const string::size_type last = endPos - startPos;
         string inputNoWS = input.substr(startPos, last+1);          
         // inputNoWS should be of the form:  "hours[:min[:sec]]"

         // In case user is using Fortran-style 'D' notation for
         // exponents:
         const string fortranExp("dD");
         string::size_type i=0;
         while (i != string::npos && i < inputNoWS.length())
         {
            i = inputNoWS.find_first_of(fortranExp, i);
            if (i != string::npos) 
            {
               inputNoWS[i] = 'e';
               ++i;
            }  
         }

         // read degrees
         endPos = inputNoWS.find_first_of(delim);
         if (!endPos)
         {
            // leading off with a delimator, not allowed
            return false;
         }
         len = (endPos == string::npos) ? string::npos : endPos;
         sub = inputNoWS.substr(0, len);
         std::istringstream sDeg(sub);
         sDeg >> hours;
         if (!sDeg.eof())
         {
            // Error, entire entry did not convert to floating point.
            return false;
         }
         ++argsRead;
         if (endPos != string::npos && endPos != last)
         {
            // read minutes
            startPos = endPos+1;
            endPos = inputNoWS.find_first_of(delim, startPos);
            if (startPos == endPos)
            {
               // Error, two consecutive delimators.
               return false;
            }
            len = (endPos == string::npos) ? string::npos : endPos-startPos;
            sub = inputNoWS.substr(startPos, len);
            std::istringstream sMin(sub);
            sMin >> min;
            if (!sMin.eof())
            {
               return false;
            }
            ++argsRead;
            if (endPos != string::npos && endPos != last)
            {
               // read seconds
               startPos = endPos+1;
               endPos = inputNoWS.find_first_of(delim, startPos);
               if (startPos == endPos)
               {
                  return false;
               }
               len = (endPos == string::npos) ? string::npos : endPos-startPos;
               sub = inputNoWS.substr(startPos, len);
               std::istringstream sSec(sub);
               sSec >> sec;
               if (!sSec.eof())
               {
                  return false;
               }
               ++argsRead;
            } // end if read seconds
         } // end if read minutes

         // Check hours, min, and sec values.
         bool isOK = true;
         switch (argsRead)
         {
            case 1:
               break;
            case 2:
               if (!isIntValue(hours) || min < .0 || min >= 60.0)
               {
                  isOK = false;
               }
               break;
            case 3:
               if (!isIntValue(hours) || !isIntValue(min) || min < .0 ||
                        min >= 60.0 || sec < .0 || sec >= 60.0)
               {
                  isOK = false;
               }
               break;
            default:
               isOK = false;
               break;
         }
         if (!isOK)
         {
            decDegrees = 0.0;
            return false;
         } 
         decDegrees =  15.0*(fabs(hours) + min/60.0 + sec/(60.0*60.0));
         if (hours < .0)  decDegrees *= -1.0; 
         return true;
      } // end sexagesimalToDegrees

   } // namespace AstroFunctions
}  // namespace Numerics

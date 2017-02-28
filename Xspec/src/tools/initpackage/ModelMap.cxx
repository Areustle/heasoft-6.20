#include "ModelMap.h"
#include <fstream>
#include <iostream>
#include <sstream>
#include <iomanip>
#include <cstdlib>

const string ModelMap::s_funcMapContainer("XSFunctionMap");
const string ModelMap::s_f77linkPrefix;
const string ModelMap::s_f77linkSuffix("_");
const string ModelMap::s_f77funcType("xsf77Call");
const string ModelMap::s_F77funcType("xsF77Call");
const string ModelMap::s_CXXfuncType("XSCCall");
const string ModelMap::s_ccfuncType("xsccCall");
const string ModelMap::s_ccmixfuncType("xsmixcall");
const string ModelMap::s_CXXmixfuncType("XSMixCCall");


ModelMap::ModelMap(const string& modInitFile, const string& directory, const string& packageName)
                : m_modInitFile(modInitFile),
                  m_directory(directory),
                  m_packageName(packageName)
{
}

ModelMap::~ModelMap()
{
}

void ModelMap::processModelFile()
{
   using namespace std;

   vector<size_t> numbersOfComponents(6,0);
   m_headerFileLines.clear();
   m_codeFileLines.clear();

   vector<string> componentTypes;
   componentTypes.push_back("add");
   componentTypes.push_back("mul");
   componentTypes.push_back("con");
   componentTypes.push_back("mix");
   componentTypes.push_back("acn");        
   componentTypes.push_back("amx");

   // ASSUME model.dat readability has already been checked.
   ifstream initFile(m_modInitFile.c_str());
   cout << "\nGathering information from model initialization file: " 
        << m_modInitFile << "...\n" << endl;

   string line;
   while (getline(initFile,line))
   {
     int jType(-1), count = 0;
     bool found(false);
     string token, delim = " \t\n";
     string::size_type end = 0, start = line.find_first_not_of(delim);

     while(start != string::npos && count < 6)
     {
         if((end = line.find_first_of(delim, start)) == string::npos)
	     end = line.size();
         else if((end - start) == 1 && line[start] == '\"')
	     end = (end = line.find('\"', end)) == string::npos ? line.size() : end + 1;

         token = line.substr(start, end - start);

         start = line.find_first_not_of(delim, end);

         ++count;
     }

     if(count == 6)
     {
         for (size_t j = 0; j < componentTypes.size(); ++j)
         {
	     if(token == componentTypes[j])
	     {
	         found = true;
	         jType = j;
	         break;
	     }
         }
     }               

     if ( found )
     {
         std::ostringstream hf;
         std::ostringstream cf;
         std::ostringstream cfmix;

         string modelName;
         int npar(0);
         double enlow(0);
         double enhigh(0);
         string modType;
         string function; 
         string funcType;
         bool usesMixUtility=false;
         istringstream modLine(line);   
         modLine >> modelName >> npar >> enlow >> enhigh >> function >> modType;
         string linkerFunction;
         if ( modType == "mix" || modType == "amx" )
         {
            if (function.substr(0,2) == "C_")
            {
               linkerFunction = function.substr(2);
               funcType = s_CXXmixfuncType;       
            }
            else if ( function.substr(0,2) == "c_")
            {
               linkerFunction = function.substr(2);
               funcType = s_ccmixfuncType;       
            }
            else if (function.substr(0,2) == "U_")
            {
               // The name of the class becomes the template
               //   parameter 'T', which is what we assign to funcType.
               funcType = function.substr(2);
               usesMixUtility = true;
            }
         }
         else
         {
            if ( function.substr(0,2) == "C_")
            {
               // C++ call
               linkerFunction = function.substr(2);  
               funcType = s_CXXfuncType;
               addWrappers(linkerFunction, funcType, npar);     
            }
            else if ( function.substr(0,2) == "c_")
            {
               linkerFunction = function.substr(2);  
               funcType = s_ccfuncType;     
               addWrappers(linkerFunction, funcType, npar);     
            }
            else if ( function.substr(0,2) == "F_")
            {
               // double precision f77 call
               // all fortran calls need to be in lowercase (at least
               // on Solaris and Linux).
               string lowFn(function.substr(2));
               for (size_t j=0; j<lowFn.size(); ++j)
               {
                  lowFn[j] = std::tolower(lowFn[j]);
               }
               linkerFunction = s_f77linkPrefix + lowFn + s_f77linkSuffix;
               funcType = s_F77funcType;
            }
            else
            {
               // f77call
               string lowFn(function.size(),' ');
               for (size_t j = 0; j < function.size(); ++j) 
               {
                       lowFn[j] = std::tolower(function[j]);
               }
               linkerFunction = s_f77linkPrefix + lowFn + s_f77linkSuffix;
               funcType = s_f77funcType;
            }
         }
         if (usesMixUtility)
         {
            cf << "\t" << s_funcMapContainer << "[\""  << modelName << "\"]" 
                     << setw(8-modelName.size() + 3) << " = " 
                     << "new XSCall<" << funcType 
                     << ">(0);\n" ;
            cfmix <<"#include  \"" << funcType << ".h\"\n";       
         }
         else
         {       
            hf << "\t" << left << setw(10) << funcType << "   "  
                            << linkerFunction << ";\n" ;
            cf << "\t" << s_funcMapContainer << "[\""  << modelName << "\"]" 
                     << setw(8-modelName.size() + 3) << " = " 
                     << "new XSCall<" << funcType 
                     << ">(" << linkerFunction << ");\n" ;
         }

         if (linkerFunction == m_packageName)
         {
            cerr << "Error: package name cannot be identical to " 
                 << " the name of a model function (" << linkerFunction
                 <<  ")... exiting\n";
            abort();    

         }
         else if (function.substr(2).find_first_of("_$") != string::npos)
         {
            cerr << "Error: model function name must not contain "
                 << "\"_\" or \"$\" (" << linkerFunction << ") ... exiting\n";
            abort();
         }


         m_headerFileLines.push_back(hf.str());
         m_codeFileLines.push_back(cf.str());
         if (usesMixUtility)
            m_codeFileMixHeaders.push_back(cfmix.str());

         cout.setf(ios::left);
         cout    << "Model name: " << setw(8) << modelName 
                 << "  No. of params: " << setw(4) << npar 
                 << "  Emin: " 
                 << setiosflags(ios::scientific)
                 << setw(12) << setprecision(2) 
                 << setiosflags(ios::right) << enlow 
                 << "  Emax: " 
                 << setw(12) << setiosflags(ios::right) 
                 << setprecision(2) << enhigh 
                 << "  f(): " << setw(8) << linkerFunction
                 << "  Type: " << modType << endl;

         numbersOfComponents[jType]++;
      }
   }

   cout << "... done\n"
        << setw(4) << numbersOfComponents[0] << "  Additive models\n"
        << setw(4) << numbersOfComponents[1] << "  Multiplicative models\n"
        << setw(4) << numbersOfComponents[2] << "  Convolution models\n" 
        << setw(4) << numbersOfComponents[3] << "  Mixing models\n" 
        << setw(4) << numbersOfComponents[4] << "  Pile-up models\n"
        << setw(4) << numbersOfComponents[5] << "  Mixing pile-up models found altogether." << endl;
}

void ModelMap::writeMapFiles() const
{
   using namespace std;

   const bool isLocal = m_packageName.length();
   const string FUNCTIONMAP(isLocal ? string("FunctionMap") : string("functionMap"));
   const string INCLUDE = "#include";
   string comment1 
        = "//\n// Code auto-generated by ";
   comment1 += isLocal ? string("initpackage ") : string("cxsetup ");     
   comment1 += "(XSPEC12 local \n// model package code generator).  Do not edit.\n";
   const string nl = "\n";
   const string nl2 = "\n\n";
   const string funcBaseClassPtr = "XSModelFunction";
   const string containerType = "ModelFunctionMap";
   const string funcMapContainer = "XSFunctionMap";
   const string CREATE = "create";
   const string INI_EXTERN = "extern";
   const string externC = "extern \"C\"";
   const string closeBrac = "}";
   const string ENDIF = "#endif";
   const string openClear = "void\nclearFunctionMap()\n{\n";
   string fullMapFileStem(m_directory + "/" + m_packageName + FUNCTIONMAP);
   string mapHead(fullMapFileStem + ".h");
   string mapFile(fullMapFileStem + ".cxx");

   ofstream packageHeaderFile(mapHead.c_str());
   if (!packageHeaderFile)
   {
      cerr << "***Unable to create the header file: " << mapHead << endl;
      abort();
   }
   ofstream packageCodeFile(mapFile.c_str());
   if (!packageCodeFile)
   {
      cerr << "***Unable to create the code file: " << mapFile << endl;
      abort();
   }

   // write package header preamble        
   packageHeaderFile << comment1;
   packageHeaderFile << "// Package: " << m_packageName << nl;
   packageHeaderFile << "// Function header: " << m_packageName << FUNCTIONMAP << ".h" <<nl2;
   string includeProtect;
   const char* p (FUNCTIONMAP.c_str());
   while (*p != 0) includeProtect += toupper((char)(*p++)); 
   packageHeaderFile << "#ifndef    " << includeProtect << "_H" << nl
                    << "#define    " << includeProtect << "_H" << nl2
                    << INCLUDE << "    " << "<funcType.h>" << nl2
                    << "class    " << funcBaseClassPtr << ";" << nl2
                    << INI_EXTERN << "    " << containerType << "  " << funcMapContainer 
                    << ";" << nl2
                    << "void  " << CREATE << m_packageName << "FunctionMap();" << nl;
   if (isLocal)
      packageHeaderFile << nl;
   else
      packageHeaderFile << "void  clearFunctionMap();" << nl2;
   packageHeaderFile  << externC << "  {" << nl2 << endl;            


   // write package code preamble
   packageCodeFile << comment1 << "// Package: " 
                  << m_packageName << nl << "// Function body: "
                  << m_packageName << FUNCTIONMAP << ".cxx" << nl2
                  << INCLUDE << "    \"" << m_packageName << FUNCTIONMAP << ".h\"" << nl2
                  << INCLUDE << "    "<< "<" << funcBaseClassPtr << ".h>" <<nl;
   if (m_codeFileMixHeaders.size())
   {
       // Add additional headers for user's custom mix model classes.
       for (size_t j=0; j<m_codeFileMixHeaders.size(); ++j)
       {
          packageCodeFile << m_codeFileMixHeaders[j];
       }
   }               
   packageCodeFile << "\nvoid \n" << CREATE << m_packageName << "FunctionMap()\n{" << nl2
                  << endl;

   size_t Nmodels (m_headerFileLines.size());
   for (size_t  j = 0; j < Nmodels; ++j)
   {
      packageHeaderFile << m_headerFileLines[j];
      packageCodeFile << m_codeFileLines[j];       
   }
   packageHeaderFile << closeBrac << nl << endl;
   packageHeaderFile << nl << ENDIF << endl;
   packageCodeFile << nl << closeBrac << endl;

   if (!isLocal)
   {
      packageCodeFile << nl << openClear 
         << "   ModelFunctionMap::iterator itFm = XSFunctionMap.begin();\n"
         << "   ModelFunctionMap::iterator itFmEnd = XSFunctionMap.end();\n"
         << "   while (itFm != itFmEnd)\n"
         << "   {\n"
         << "      delete itFm->second;\n"
         << "      ++itFm;\n"
         << "   }\n"
         << "   XSFunctionMap.clear();\n" 
         << closeBrac << nl << endl;
   }
}


void ModelMap::addWrappers(const string& funcName, const string& funcType, int nPar)
{
}

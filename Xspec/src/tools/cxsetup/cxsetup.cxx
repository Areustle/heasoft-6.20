
#include "MapAndWrappers.h"
#include <string>
#include <iostream>
#include <fstream>
#include <cstdlib>

using namespace std;

int main ()

{
   const string HEADAS(getenv("HEADAS"));
   const string dir = HEADAS + "/../Xspec/src/XSFunctions"; 
   const string modelDatFile = HEADAS + "/../Xspec/src/manager/model.dat";

   ifstream testDatFile(modelDatFile.c_str());
   if (!testDatFile)
   {
      cerr << "Unable to read model data file at " << modelDatFile << endl;
      return -1;
   }
   testDatFile.close(); 

   cout << "Model definition header file:  "  << dir + "/functionMap.h" << endl;
   cout << "Model definition implementation file:  " << dir + "/functionMap.cxx" << endl;
   cout << "Wrapper header file:  " << dir + "/funcWrappers.h" << endl;
   cout << "Wrapper implementation file:  " << dir + "/funcWrappers.cxx" << endl;

   // This differs from initpackage usage in that no package name is sent.
   MapAndWrappers mapCreator(modelDatFile, dir, string(""));
   mapCreator.createWrapperFiles();

   // Add wrappers will be called repeatedly inside of here
   mapCreator.processModelFile();

   mapCreator.writeMapFiles();
   mapCreator.completeWrapperFiles();

}

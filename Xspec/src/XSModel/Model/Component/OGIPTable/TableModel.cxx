// Routines to allow the user to call the table model code from within their own
// Fortran or C++ models

#include <XSModel/Model/Component/OGIPTable/TableModel.h>
#include <XSstreams.h>
#include <XSModel/Model/UniqueEnergy.h>
#include <XSModel/Model/Component/OGIPTable/OGIPTable.h>
#include <XSModel/Parameter/TableModParam.h>
#include <XSUtil/Utils/IosHolder.h>
#include <XSUtil/Utils/XSutility.h>
#include <algorithm>
#include <cfortran.h>

FCALLSCSUB7(xsatbl,XSATBL,xsatbl,FLOATV,INT,FLOATV,STRING,INT,FLOATV,FLOATV)
FCALLSCSUB7(xsetbl,XSETBL,xsetbl,FLOATV,INT,FLOATV,STRING,INT,FLOATV,FLOATV)
FCALLSCSUB7(xsmtbl,XSMTBL,xsmtbl,FLOATV,INT,FLOATV,STRING,INT,FLOATV,FLOATV)

namespace
{
   template <typename T>
   void addTableCalc(const RealArray& energyArray, int specNum, const T* param, OGIPTable& table,
                        RealArray& fluxArray, RealArray& fluxErrArray, bool isExp)
   {
      UniqueEnergy unEng(energyArray);
      // This is probably not necessary.
      unEng.addClient(specNum);
      // In standard xspec usage, table parameters are added to
      // global parameter lists from which their parameters can
      // be adjusted.  These however are only local, so this must
      // do all parameter setting.
      const std::vector<Parameter*>& tablePars = table.parameters();
      const size_t nPars = tablePars.size();
      for (size_t i=0; i<nPars; ++i)
      {
         // Just set using default 'v'.  Presumably any adjustments
         // due to limits have already been performed on incoming
         // param array.
         ModParam* tablePar = dynamic_cast<ModParam*>(tablePars[i]);
         tablePar->setValue(param[i]);
      }
      table.energyWeights(&unEng);
      table.interpolate(&unEng, fluxArray, fluxErrArray, true, isExp);
      
      // time dilation. Check for a redshift parameter and apply redshifts to
      // the energy and variance array.

      XSutility::MatchPtrName<Parameter> matchName;

      std::vector<Parameter*>::const_iterator itPar =
              find_if(tablePars.begin(),tablePars.end(),std::bind2nd(matchName,"z"));

      Real z (0);      
      if ( itPar != tablePars.end() ) z = (*itPar)->value();

      if ( z > -1 ) 
      {
              Real zf (1/(1+z));
              fluxArray *= zf;
              if (fluxErrArray.size())
                 fluxErrArray *= zf*zf;
      }
   }

   template <typename T>
   void mulTableCalc(const RealArray& energyArray, int specNum, const T* param, OGIPTable& table,
                        RealArray& fluxArray, RealArray& fluxErrArray, bool isExp)
   {
      UniqueEnergy unEng(energyArray);
      // This is probably not necessary.
      unEng.addClient(specNum);
      // In standard xspec usage, table parameters are added to
      // global parameter lists from which their parameters can
      // be adjusted.  These however are only local, so xsmtbl must
      // do all parameter setting.
      const std::vector<Parameter*>& tablePars = table.parameters();
      const size_t nPars = tablePars.size();
      for (size_t i=0; i<nPars; ++i)
      {
         // Just set using default 'v'.  Presumably any adjustments
         // due to limits have already been performed on incoming
         // param array.
         ModParam* tablePar = dynamic_cast<ModParam*>(tablePars[i]);
         tablePar->setValue(param[i]);
      }
      table.energyWeights(&unEng);
      table.interpolate(&unEng, fluxArray, fluxErrArray, false, isExp);
   }

}


void additiveTable(const RealArray& energyArray, 
		   const RealArray& params, 
		   string fileName,
		   int spectrumNumber,
		   RealArray& fluxArray, 
		   RealArray& fluxErrArray,
		   const string& initString)
{
   static OGIPTable table("",0);

   bool isExp = false;
   try 
   {
     if (fileName != table.filename()) {      
       table.filename(fileName);
       table.read(true,0,true);
     }
     fluxArray.resize(energyArray.size()-1);
     
     // Hack to get the address of params[0].  This is needed due to the
     // '98-standard's const valarray subscript accessor:
     //    T operator[] (size_t) const;
     // The 0x draft and g++ changed the return type to a const T&, which is
     // what we want, but can't assume every compiler will do the same.
     // params isn't being modified, so its const declaration still holds true.
     RealArray& tmpParams = const_cast<RealArray&>(params);
     const Real* par0 = &tmpParams[0]; 

     addTableCalc(energyArray, spectrumNumber, par0, table, fluxArray, fluxErrArray, isExp);
      
   } 
   catch (...)
   {
      table.filename(" ");
      *IosHolder::errHolder() <<"***Error in additiveTable function."<<std::endl;
   }

}

void exponentialTable(const RealArray& energyArray, 
			 const RealArray& params, 
			 string fileName,
			 int spectrumNumber,
			 RealArray& fluxArray, 
			 RealArray& fluxErrArray,
			 const string& initString)
{
   static OGIPTable table("",0);
   
   bool isExp = true;
   try 
   {
     if (fileName != table.filename()) {      
       table.filename(fileName);
       table.read(true,0,false);
     }
     fluxArray.resize(energyArray.size()-1);  
        
     // Hack to get the address of params[0].  This is needed due to the
     // '98-standard's const valarray subscript accessor:
     //    T operator[] (size_t) const;
     // The 0x draft and g++ changed the return type to a const T&, which is
     // what we want, but can't assume every compiler will do the same.
     // params isn't being modified, so its const declaration still holds true.
     RealArray& tmpParams = const_cast<RealArray&>(params);
     const Real* par0 = &tmpParams[0]; 
          
     mulTableCalc(energyArray, spectrumNumber, par0, table, fluxArray, fluxErrArray, isExp);
      
   } 
   catch (...) 
   {
      table.filename(" ");
      *IosHolder::errHolder() <<"***Error in exponentialTable function."<<std::endl;
   }

}

void multiplicativeTable(const RealArray& energyArray, 
			 const RealArray& params, 
			 string fileName,
			 int spectrumNumber,
			 RealArray& fluxArray, 
			 RealArray& fluxErrArray,
			 const string& initString)
{
   static OGIPTable table("",0);
   
   bool isExp = false;
   try 
   {
     if (fileName != table.filename()) {      
       table.filename(fileName);
       table.read(true,0,false);
     }
     fluxArray.resize(energyArray.size()-1);  
     
     // See comments above in exponentialTable   
     RealArray& tmpParams = const_cast<RealArray&>(params);
     const Real* par0 = &tmpParams[0]; 
          
     mulTableCalc(energyArray, spectrumNumber, par0, table, fluxArray, fluxErrArray, isExp);
      
   } 
   catch (...) 
   {
      table.filename(" ");
      *IosHolder::errHolder() <<"***Error in multiplicativeTable function."<<std::endl;
   }

}

void xsatbl(float* ear, int ne, float* param, const char* filenm, int ifl, 
                float* photar, float* photer)
{
   static OGIPTable table("",0);
   
   string fileName(filenm);
   bool isExp = false;
   try
   {
      if (fileName != table.filename())
      {      
         table.filename(fileName);
         table.read(true,0,true);
      }
      RealArray energyArray(.0,ne+1);
      RealArray fluxArray(.0,ne);
      RealArray fluxErrArray(.0,ne);
      std::copy(&ear[0], &ear[ne+1], &energyArray[0]);
      
      addTableCalc(energyArray, ifl, param, table, fluxArray, fluxErrArray, isExp);
            
      std::copy(&fluxArray[0], &fluxArray[0]+ne, &photar[0]);
      std::copy(&fluxErrArray[0], &fluxErrArray[0]+ne, &photer[0]);
   }
   catch (...)
   {
      table.filename(" ");
      *IosHolder::errHolder() <<"***Error in xsatbl function."<<std::endl;
   }
}

void xsetbl(float* ear, int ne, float* param, const char* filenm, int ifl, 
                float* photar, float* photer)
{
   static OGIPTable table("",0);
   
   string fileName(filenm);
   bool isExp = true;
   try
   {
      if (fileName != table.filename())
      {      
         table.filename(fileName);
         table.read(true,0,false);
      }
      RealArray energyArray(.0,ne+1);
      RealArray fluxArray(.0,ne);
      RealArray fluxErrArray(.0,ne);
      std::copy(&ear[0], &ear[ne+1], &energyArray[0]);
      
      mulTableCalc(energyArray, ifl, param, table, fluxArray, fluxErrArray, isExp);
      
      std::copy(&fluxArray[0], &fluxArray[0]+ne, &photar[0]);
      std::copy(&fluxErrArray[0], &fluxErrArray[0]+ne, &photer[0]);
   }
   catch (...)
   {
      table.filename(" ");
      *IosHolder::errHolder() <<"***Error in xsetbl function."<<std::endl;
   }
      
} 


void xsmtbl(float* ear, int ne, float* param, const char* filenm, int ifl, 
                float* photar, float* photer)
{
   static OGIPTable table("",0);
   
   string fileName(filenm);
   bool isExp = false;
   try
   {
      if (fileName != table.filename())
      {      
         table.filename(fileName);
         table.read(true,0,false);
      }
      RealArray energyArray(.0,ne+1);
      RealArray fluxArray(.0,ne);
      RealArray fluxErrArray(.0,ne);
      std::copy(&ear[0], &ear[ne+1], &energyArray[0]);
      
      mulTableCalc(energyArray, ifl, param, table, fluxArray, fluxErrArray, isExp);
      
      std::copy(&fluxArray[0], &fluxArray[0]+ne, &photar[0]);
      std::copy(&fluxErrArray[0], &fluxErrArray[0]+ne, &photer[0]);
   }
   catch (...)
   {
      table.filename(" ");
      *IosHolder::errHolder() <<"***Error in xsmtbl function."<<std::endl;
   }
      
} 



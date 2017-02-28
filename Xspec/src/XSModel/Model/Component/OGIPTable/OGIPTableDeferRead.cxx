//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <CCfits/CCfits>
#include <CCfits/FITSBase.h>
#include <XSUtil/Error/Error.h>
#include <XSUtil/Utils/XSutility.h>

// OGIPTableDeferRead
#include <XSModel/Model/Component/OGIPTable/OGIPTableDeferRead.h>


// Class OGIPTableDeferRead 

OGIPTableDeferRead::OGIPTableDeferRead()
   :TableAccess(),
    m_tableVals(),
    m_selectedRows(),
    m_isError(false),
    m_isAddParamError(),
    m_absPath()
{
}

OGIPTableDeferRead::OGIPTableDeferRead(const OGIPTableDeferRead &right)
  :TableAccess(right),
   m_tableVals(right.m_tableVals),
   m_selectedRows(right.m_selectedRows),
   m_isError(right.m_isError),
   m_isAddParamError(right.m_isAddParamError),
   m_absPath(right.m_absPath)
{
}


OGIPTableDeferRead::~OGIPTableDeferRead()
{
}


OGIPTableDeferRead* OGIPTableDeferRead::clone ()
{
   OGIPTableDeferRead* newReader = new OGIPTableDeferRead(*this);
   return newReader;
}

void OGIPTableDeferRead::initialRead (CCfits::ExtHDU& spectraExt, const size_t nEngs, const size_t nAddPars, const bool isSameAsPrev)
{
   // Don't actually read any of the intrp table in here.  Just get
   // the location of the file so it can be opened and read later 
   // in readTableRows.  Also determine which columns contain error values,
   // for which they don't need to be read.
   using namespace CCfits;
   m_isAddParamError.clear();
   const ExtHDU& chdu = const_cast<const ExtHDU&>(spectraExt);
   string fileName(chdu.parent()->name());
   if (fileName.find_first_of("/") != 0)
   {
      m_absPath = XSutility::getRunPath();
      m_absPath += "/";
   }
   m_absPath += fileName;

   // If this column is missing, the calling function will catch and
   // handle the error.
   const string colName("INTPSPEC");
   Column& interpCol = spectraExt.column(colName);
   m_isError = (interpCol.repeat() == 2*nEngs);

   char str1[4];
   string addSPFull;
   string columnErr;
   try
   {
      m_isAddParamError.resize(nAddPars,false);
      if (nAddPars > 999)
         throw YellowAlert("Additional parameter limit of 999 is exceeded.\n");
      for (int addParamIndex = 1; addParamIndex <= 
                static_cast<const int>(nAddPars); ++addParamIndex) 
      {
         sprintf(str1, "%03i", addParamIndex);
         addSPFull = "ADDSP";
         addSPFull += str1;
         columnErr = addSPFull;
         Column & addSpectrum = spectraExt.column(addSPFull);
         m_isAddParamError[addParamIndex-1] = ( addSpectrum.repeat() == 2*nEngs );
      }
   }
   catch(Table::NoSuchColumn&)
   {
      string diag("While attempting to read table model column: ");
      diag += columnErr + "\n";
      throw YellowAlert(diag);
   }
}

void OGIPTableDeferRead::initialAccessRows (const IntegerArray& rowNumbers, const size_t nAddPars)
{
   using namespace CCfits;
   using namespace std;

   m_tableVals.clearAll();   

   const string extName("SPECTRA");
   const string colName("INTPSPEC");
   string colErr(colName);
   try
   {
      FITS tableFile(m_absPath, Read, extName, false);
      Column& interpCol = tableFile.currentExtension().column(colName);
      const size_t nRows = rowNumbers.size();
      const size_t nEngs = m_isError ? interpCol.repeat()/2 : interpCol.repeat();
      m_tableVals.m_interpValues.resize(nEngs, 0);
      m_tableVals.m_nSpecRows = nRows;
      for (size_t i=0; i<nEngs; ++i)
         m_tableVals.m_interpValues[i] = new Real[nRows];

      if (m_isError)
      {
         m_tableVals.m_interpValueError.resize(nEngs, 0);
         for (size_t j=0; j<nEngs; ++j)
            m_tableVals.m_interpValueError[j] = new Real[nRows];
      }

      m_tableVals.m_addSpectra.resize(nAddPars);
      m_tableVals.m_addSpectraError.resize(nAddPars);
      for (size_t iAdd=0; iAdd<nAddPars; ++iAdd)
      {
         vector<Real*>& addIntTable = m_tableVals.m_addSpectra[iAdd];
         addIntTable.resize(nEngs,0);
         for (size_t j=0; j<nEngs; ++j)
            addIntTable[j] = new Real[nRows];
         if (m_isAddParamError[iAdd])
         {
            vector<Real*> addErrorTable = m_tableVals.m_addSpectraError[iAdd];
            addErrorTable.resize(nEngs,0);
            for (size_t j=0; j<nEngs; ++j)
               addErrorTable[j] = new Real[nRows];
         }
      }
      
      // Input row numbers are 0 based, CCfits expects 1 based.
      const size_t expectedSize = m_isError ? 2*nEngs : nEngs;
      for (size_t i=0; i<nRows; ++i)
      {
         colErr = colName;
         RealArray tempIntVals;
         interpCol.read(tempIntVals, rowNumbers[i]+1);
         if (tempIntVals.size() != expectedSize)
         {
            throw RedAlert("Array size mismatch in OGIPTableDeferRead::readTableRows.");
         }
         Real* spRow = &tempIntVals[0];
         vector<Real*>::iterator itCol = m_tableVals.m_interpValues.begin();
         for (size_t j=0; j<nEngs; ++j, ++itCol)
         {
            (*itCol)[i] = spRow[j];
         }
         if (m_isError)
         {
            vector<Real*>::iterator itColErr = 
                        m_tableVals.m_interpValueError.begin();
            for (size_t j=0; j<nEngs; ++j, ++itColErr)
               (*itColErr)[i] = spRow[j+nEngs];
         }

         if (nAddPars)
         {
            char str1[4];
            string addSPFull;   
            for (int iAdd=1; iAdd<=static_cast<int>(nAddPars); ++iAdd)
            {
               // Note that iAdd is 1-based in this loop.
               sprintf(str1, "%03i", iAdd);
               addSPFull = "ADDSP";
               addSPFull += str1;
               colErr = addSPFull;
               Column & addSpectrum = tableFile.currentExtension().column(addSPFull);
               vector<Real*>& addIntpTable = m_tableVals.m_addSpectra[iAdd-1];
               RealArray tempAddVals;
               addSpectrum.read(tempAddVals, rowNumbers[i]+1);
               const size_t expectedAddSize = m_isAddParamError[iAdd-1] ?
                                2*nEngs : nEngs;
               if (tempAddVals.size() != expectedAddSize)
               {
                  throw RedAlert("Add par array size mismatch in OGIPTableDeferRead::readTableRows.");
               }
               spRow = &tempAddVals[0];
               vector<Real*>::iterator itAddCol = addIntpTable.begin();
               for (size_t j=0; j<nEngs; ++j, ++itAddCol)
                  (*itAddCol)[i] = spRow[j];
               if (m_isAddParamError[iAdd-1])
               {
                  vector<Real*>& addErrorTable = 
                        m_tableVals.m_addSpectraError[iAdd-1];
                  vector<Real*>::iterator itAddErr = addErrorTable.begin();
                  for (size_t j=0; j<nEngs; ++j, ++itAddErr)
                     (*itAddErr)[i] = spRow[j+nEngs];
               }
            } // end loop over add pars
         } // end add pars section
      } // end loop over rows

   }
   catch (Table::NoSuchColumn&)
   {
      string diag("While attempting to read table model column: ");
      diag += colErr + "\n";
      throw YellowAlert(diag);
   }
   catch (FitsException&)
   {
      string diag("While attempting to read ");
      diag += extName + " extension of file\n";
      diag += m_absPath + "\n";
      throw YellowAlert(diag);
   }
   m_selectedRows = rowNumbers;
}

void OGIPTableDeferRead::getSpectrumEntries (const size_t iEng, Real* values)
{
  const Real* storedVals = m_tableVals.m_interpValues[iEng];
  const size_t nSelectedRows = m_selectedRows.size();
  for (size_t iRow=0; iRow<nSelectedRows; ++iRow)
  {
     values[iRow] = storedVals[iRow];
  }
}

void OGIPTableDeferRead::getVarianceEntries (const size_t iEng, Real* values)
{
  const Real* storedVals = m_tableVals.m_interpValueError[iEng];
  const size_t nSelectedRows = m_selectedRows.size();
  for (size_t iRow=0; iRow<nSelectedRows; ++iRow)
  {
     Real sig_iRow = storedVals[iRow];
     values[iRow] = sig_iRow*sig_iRow;
  }
}

void OGIPTableDeferRead::getAddSpectra (const size_t iEng, const size_t iAdd, Real* values)
{
  const Real* storedAddSp = m_tableVals.m_addSpectra[iAdd][iEng];
  const size_t nSelectedRows = m_selectedRows.size();
  for (size_t iRow=0; iRow<nSelectedRows; ++iRow)
  {
     values[iRow] = storedAddSp[iRow];
  }
}

void OGIPTableDeferRead::getAddVariance (const size_t iEng, const size_t iAdd, Real* values)
{
  const Real* storedAddErr = m_tableVals.m_addSpectraError[iAdd][iEng];
  const size_t nSelectedRows = m_selectedRows.size();
  for (size_t iRow=0; iRow<nSelectedRows; ++iRow)
  {
     Real sig_iRow = storedAddErr[iRow];
     values[iRow] = sig_iRow*sig_iRow;
  }
}

// Additional Declarations

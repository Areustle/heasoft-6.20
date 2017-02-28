//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <CCfits/CCfits>
#include <XSUtil/Error/Error.h>

// OGIPTableReadAll
#include <XSModel/Model/Component/OGIPTable/OGIPTableReadAll.h>


// Class OGIPTableReadAll 
TableValues OGIPTableReadAll::s_prevTable;

OGIPTableReadAll::OGIPTableReadAll()
   :TableAccess(),
    m_selectedRows(),
    m_isError(false),
    m_isAddParamError(),
    m_tableVals()
{
}

OGIPTableReadAll::OGIPTableReadAll(const OGIPTableReadAll &right)
   :TableAccess(right),
    m_selectedRows(right.m_selectedRows),
    m_isError(right.m_isError),
    m_isAddParamError(right.m_isAddParamError),
    m_tableVals(right.m_tableVals)
{
}


OGIPTableReadAll::~OGIPTableReadAll()
{
}


OGIPTableReadAll* OGIPTableReadAll::clone ()
{
   OGIPTableReadAll* newReader = new OGIPTableReadAll(*this);
   return newReader;
}

void OGIPTableReadAll::initialRead (CCfits::ExtHDU& spectraExt, const size_t nEngs, const size_t nAddPars, const bool isSameAsPrev)
{
   // These should all be empty when this function is called.  But we want
   //  to be absolutely sure since we're going to rely on vector::resize calls
   //  to also initialize elements.
   m_tableVals.clearAll();
   m_isAddParamError.clear();

   using namespace CCfits;
   if (isSameAsPrev)
   {
      m_tableVals = s_prevTable;
      m_isError = static_cast<bool>(m_tableVals.m_interpValueError.size());
      m_isAddParamError.resize(nAddPars, false);
      for (size_t iAdd=0; iAdd<nAddPars; ++iAdd)
      {
         if (m_tableVals.m_addSpectraError[iAdd].size())
            m_isAddParamError[iAdd] = true;
      }
   }
   else
   {
      const string colName("INTPSPEC");
      string columnErr = colName;
      try
      {
          const long nSpecRows = spectraExt.rows();
          
          // rearrange FITS tables for storage at fixed energy to
          // optimize interpolation.

          // tempIntpVals will have nSpecRows vectors of size nEngs.
          // m_interpValues will have nEngs arrays of size nSpecRows,
          //   each array representing the table value at fixed energy.
          std::vector<RealArray> tempIntpVals;
          Column& interpCol = spectraExt.column(colName);
          interpCol.readArrays(tempIntpVals, 1, nSpecRows);
          m_tableVals.m_interpValues.resize(nEngs,0);
          m_tableVals.m_nSpecRows = nSpecRows;
          for (size_t i=0; i<nEngs; ++i)
             m_tableVals.m_interpValues[i] = new Real[nSpecRows];

          // is there error data? If so, the rows are double-width.
          m_isError = (interpCol.repeat() == 2*nEngs);

          if (m_isError) 
          {
             m_tableVals.m_interpValueError.resize(nEngs,0);
             for (size_t i=0; i<nEngs; ++i)
                m_tableVals.m_interpValueError[i] = new Real[nSpecRows];
          }

          int i= 0;
          std::vector<RealArray>::iterator intp = tempIntpVals.begin();
          while (intp != tempIntpVals.end()) 
          {
             // foreach row spRow, assign the jth entry along the row
             // to the rowth energy vector  
             Real* spRow = &(*intp)[0];
             std::vector<Real*>::iterator enVector = m_tableVals.m_interpValues.begin();

             for (size_t j = 0; j < nEngs ; ++j, ++enVector) 
             {
	        Real* col = *enVector;
	        col[i] = spRow[j];
             }

             if (m_isError)
             {
	        std::vector<Real*>::iterator errVector = 
                           m_tableVals.m_interpValueError.begin();                      
                for (size_t j = 0; j < nEngs; ++j, ++errVector)
                {
                   Real* col = *errVector;
                   col[i] = spRow[j + nEngs];
                }
             }
             ++intp;
             ++i;
          }

          char str1[4];
          string addSPFull;

          m_isAddParamError.resize(nAddPars,false);
          if (nAddPars > 999)
             throw YellowAlert("Additional parameter limit of 999 is exceeded.\n");
          m_tableVals.m_addSpectra.resize(nAddPars);
          m_tableVals.m_addSpectraError.resize(nAddPars);
          for (size_t addParamIndex = 1; addParamIndex <= nAddPars; ++addParamIndex) 
          {
	     sprintf(str1, "%03i", (int)addParamIndex);
             addSPFull = "ADDSP";
             addSPFull += str1;
             columnErr = addSPFull;
             Column & addSpectrum = spectraExt.column(addSPFull);
             m_isAddParamError[addParamIndex-1] = ( addSpectrum.repeat() == 2*nEngs );

             addSpectrum.readArrays(tempIntpVals, 1, nSpecRows);
             std::vector<Real*>& addIntpTable =  
                           m_tableVals.m_addSpectra[addParamIndex-1];  
             addIntpTable.resize(nEngs,0);
             for (size_t i=0; i<nEngs; ++i)
                addIntpTable[i] = new Real[nSpecRows];
             size_t i=0;   
             intp = tempIntpVals.begin();  
             while (intp != tempIntpVals.end()) 
             {
                 // foreach row spRow, assign the jth entry along the row
                 // to the rowth  energy value of the spectrum.
                 Real* spRow = &(*intp)[0];
	         std::vector<Real*>::iterator enVector = addIntpTable.begin();
	         for (size_t j = 0; j < nEngs ; ++j, ++enVector) 
                 {
	             Real* col = *enVector;
	             col[i] = spRow[j];
	         }
	         ++intp;
	         ++i;
             }

             if (m_isAddParamError[addParamIndex - 1])
             {
                 std::vector<Real*>& addIntpTableError 
                                 =  m_tableVals.m_addSpectraError[addParamIndex-1];
                 addIntpTableError.resize(nEngs,0);
                 for (size_t j=0; j<nEngs; ++j)
                    addIntpTableError[j] = new Real[nSpecRows];  
                 i = 0;   
                 intp = tempIntpVals.begin();    
                 while (intp != tempIntpVals.end()) 
                 {
                     // foreach row spRow, assign the j + nEngs th entry along 
                     // the row to the rowth energy value of the spectrum error  
                     Real* spRow = &(*intp)[0];
	             std::vector<Real*>::iterator enVector = addIntpTableError.begin();
	             for (size_t j = 0; j < nEngs ; ++j, ++enVector) 
                     {
	                 Real* col = *enVector;
	                 col[i] = spRow[j + nEngs];
	             }
	             ++intp;
	             ++i;
                 }
             }
          } // end AddSpec loop
          s_prevTable = m_tableVals;
      }
      catch (CCfits::Table::NoSuchColumn&)
      {
         string diag("While attempting to read table model column: ");
         diag += columnErr + "\n";
         throw YellowAlert(diag);
      }
   }
}

void OGIPTableReadAll::initialAccessRows (const IntegerArray& rowNumbers, const size_t nAddPars)
{
   m_selectedRows = rowNumbers;
   // No need to do anything else in here since all rows have already been 
   // read during initialRead.
}

void OGIPTableReadAll::getSpectrumEntries (const size_t iEng, Real* values)
{
  const Real* valsForEng = m_tableVals.m_interpValues[iEng];
  const size_t nSelectedRows = m_selectedRows.size();
  for (size_t i=0; i<nSelectedRows; ++i)
  {
     values[i] = valsForEng[m_selectedRows[i]];
  }
}

void OGIPTableReadAll::getVarianceEntries (const size_t iEng, Real* values)
{
  const Real* valsForEng = m_tableVals.m_interpValueError[iEng];
  const size_t nSelectedRows = m_selectedRows.size();
  for (size_t i=0; i<nSelectedRows; ++i)
  {
     values[i] = valsForEng[m_selectedRows[i]];
     values[i] *= values[i];
  }
}

void OGIPTableReadAll::getAddSpectra (const size_t iEng, const size_t iAdd, Real* values)
{
  const Real* addSpForEng = m_tableVals.m_addSpectra[iAdd][iEng];
  const size_t nSelectedRows = m_selectedRows.size();
  for (size_t i=0; i<nSelectedRows; ++i)
  {
     values[i] = addSpForEng[m_selectedRows[i]];
  }
}

void OGIPTableReadAll::getAddVariance (const size_t iEng, const size_t iAdd, Real* values)
{
  const Real* addSpErrForEng = 
                m_tableVals.m_addSpectraError[iAdd][iEng];
  const size_t nSelectedRows = m_selectedRows.size();
  for (size_t i=0; i<nSelectedRows; ++i)
  {
     values[i] = addSpErrForEng[m_selectedRows[i]];
     values[i] *= values[i];
  }
}

// Additional Declarations

//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <CCfits/CCfits>

// TableAccess
#include <XSModel/Model/Component/OGIPTable/TableAccess.h>


// Class TableAccess 

TableAccess::TableAccess()
{
}

TableAccess::TableAccess(const TableAccess &right)
{
}


TableAccess::~TableAccess()
{
}


// Additional Declarations

// Class TableValues 

TableValues::TableValues()
  : m_interpValues(),
    m_interpValueError(),
    m_addSpectra(),
    m_addSpectraError(),
    m_nSpecRows(0)
{
}

TableValues::TableValues(const TableValues &right)
   : m_interpValues(), // All of these require deep copies
     m_interpValueError(),
     m_addSpectra(),
     m_addSpectraError(),
     m_nSpecRows(right.m_nSpecRows)
{
   // Let the deep copying begin...
   // Note that this just ASSUMES ANY C-style arrays that have been
   // allocated in the struct to be copied are of size m_nSpecRows.

   using namespace std;
   const size_t nEngs = right.m_interpValues.size();
   m_interpValues.resize(nEngs);
   for (size_t i=0; i<nEngs; ++i)
   {
      const Real* rEngCol = right.m_interpValues[i];
      Real*& engCol = m_interpValues[i];
      engCol = new Real[m_nSpecRows];
      for (size_t j=0; j<m_nSpecRows; ++j)
         engCol[j] = rEngCol[j];
   }

   const size_t nEngErrs = right.m_interpValueError.size();
   m_interpValueError.resize(nEngErrs, 0);
   for (size_t i=0; i<nEngErrs; ++i)
   {
      const Real* rEngCol = right.m_interpValueError[i];
      Real*& engCol = m_interpValueError[i];
      engCol = new Real[m_nSpecRows];
      for (size_t j=0; j<m_nSpecRows; ++j)
         engCol[j] = rEngCol[j];
   }

   const size_t nAdds = right.m_addSpectra.size();
   m_addSpectra.resize(nAdds);
   for (size_t iVec=0; iVec<nAdds; ++iVec)
   {
      const vector<Real*>& rAdd = right.m_addSpectra[iVec];
      const size_t nAddEngs = rAdd.size();
      m_addSpectra[iVec].resize(nAddEngs,0);
      vector<Real*>& m_addSpectra_iVec = m_addSpectra[iVec]; 
      for (size_t i=0; i<nAddEngs; ++i)
      {
         const Real* rEngCol = rAdd[i];
         Real*& engCol = m_addSpectra_iVec[i];
         engCol = new Real[m_nSpecRows];
         for (size_t j=0; j<m_nSpecRows; ++j)
            engCol[j] = rEngCol[j];
      }
   }

   const size_t nAddsError = right.m_addSpectraError.size();
   m_addSpectraError.resize(nAddsError);
   for (size_t iVec=0; iVec<nAddsError; ++iVec)
   {
      const vector<Real*>& rAdd = right.m_addSpectraError[iVec];
      const size_t nAddErrEngs = rAdd.size();
      m_addSpectraError[iVec].resize(nAddErrEngs,0);
      vector<Real*>& m_addSpectraError_iVec = m_addSpectraError[iVec];
      for (size_t i=0; i<rAdd.size(); ++i)
      {
         const Real* rEngCol = rAdd[i];
         Real*& engCol = m_addSpectraError_iVec[i];
         engCol = new Real[m_nSpecRows];
         for (size_t j=0; j<m_nSpecRows; ++j)
            engCol[j] = rEngCol[j];
      }
   }
}


TableValues::~TableValues()
{
   clearAll();
}


TableValues & TableValues::operator=(const TableValues &right)
{
   if (this != &right) copy(right);
   return *this;
}


void TableValues::copy (const TableValues& right)
{
   TableValues temp(right);
   swap(temp);
}

void TableValues::swap (TableValues& right)
{
   std::swap(m_interpValues, right.m_interpValues);
   std::swap(m_interpValueError, right.m_interpValueError);
   std::swap(m_addSpectra, right.m_addSpectra);
   std::swap(m_addSpectraError, right.m_addSpectraError);
   std::swap(m_nSpecRows, right.m_nSpecRows);
}

void TableValues::clearAll ()
{
   for (size_t i=0; i<m_interpValues.size(); ++i)
      delete [] m_interpValues[i];
   m_interpValues.clear();

   for (size_t i=0; i<m_interpValueError.size(); ++i)
      delete [] m_interpValueError[i];
   m_interpValueError.clear();

   for (size_t j=0; j<m_addSpectra.size(); ++j)
   {
      std::vector<Real*>& doomed = m_addSpectra[j];
      for (size_t i=0; i<doomed.size(); ++i)
         delete [] doomed[i];
   }
   m_addSpectra.clear();

   for (size_t j=0; j<m_addSpectraError.size(); ++j)
   {
      std::vector<Real*>& doomed = m_addSpectraError[j];
      for (size_t i=0; i<doomed.size(); ++i)
         delete [] doomed[i];
  }
   m_addSpectraError.clear();

   m_nSpecRows = 0;
}

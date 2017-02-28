//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <XSUtil/Error/Error.h>

// ModularCounter
#include <XSUtil/Numerics/ModularCounter.h>

namespace Numerics {

    // Class Numerics::ModularCounter 

    ModularCounter::ModularCounter (const IntegerArray& bases, bool isRasterScan)
      : m_bases(bases),
        m_factors(bases.size()+1),
        m_counter(bases.size(), 0),
        m_currentPos(0),
        m_isRasterScan(isRasterScan),
        m_direction(bases.size(), 1)
    {
       size_t sz = bases.size();
       if (!sz)
       {
          throw RedAlert("Cannot create ModularCounter of size 0");
       }
       for (size_t i=0; i<sz; ++i)
       {
          if (!bases[i]) 
                throw RedAlert("Cannot create ModularCounter with 0 base.");
       }
       
       int accum=1;
       m_factors[0]=1;
       for (size_t i=0; i<sz; ++i)
       {
          accum *= m_bases[i];
          m_factors[i+1] = accum;
       }
    }


    ModularCounter::~ModularCounter()
    {
    }

    void ModularCounter::calcDirections()
    {
       if (m_isRasterScan)
       {
          for (size_t iCoord=0; iCoord<m_bases.size(); ++iCoord)
          {
             if (m_currentPos/m_factors[iCoord+1] % 2)
                m_direction[iCoord] = -1;
             else
                m_direction[iCoord] = 1;                
          }
       }
    }
    
    void ModularCounter::calcCoordinates()
    {
       const int nCoords = static_cast<int>(m_bases.size());
       int remaining = m_currentPos;
       for (int iCoord=nCoords-1; iCoord>=0; --iCoord)
       {
          int coord = remaining/m_factors[iCoord];
          remaining -= coord*m_factors[iCoord];
          if (m_direction[iCoord] == -1)
          {
             coord = m_bases[iCoord] - coord - 1;
          }
          m_counter[iCoord] = coord;
       }
    }
    
    void ModularCounter::reset (int startingPos)
    {
       if (startingPos >= m_factors[m_bases.size()])
       {
          throw RedAlert("Attempting to set out-of-bounds position in ModularCounter.");
       }
       m_currentPos = startingPos;
       calcDirections();
       calcCoordinates();
    }

    ModularCounter& ModularCounter::operator ++ ()
    {
       const size_t nCoord = m_bases.size();
       bool isFinished = false;
       m_counter[0] += m_direction[0];
       for (size_t i=0; i<nCoord && !isFinished; ++i)
       {
          if (m_counter[i] < 0)
          {
             // Can only get in here if m_isRaster is true.
             // Should NEVER get in here for i==nCoord-1: the outermost
             //   coordinate should always be increasing.
             m_counter[i] = 0;
             m_direction[i] = 1;
             m_counter[i+1] += m_direction[i+1];
          }
          else if (m_counter[i] >= m_bases[i])
          {
             if (i == nCoord-1)
             {
                // We've gone over the edge.  Reset counter to the
                //   beginning and exit.
                m_currentPos = 0;
                calcDirections();
                calcCoordinates();
                isFinished = true;
             }
             else
             {
                if (m_isRasterScan)
                {
                   m_counter[i] = m_bases[i] - 1;
                   m_direction[i] = -1;
                }
                else
                   m_counter[i] = 0;
                m_counter[i+1] += m_direction[i+1];
             }
          }
          else
          {
             isFinished = true;
          }
       }
             
       return *this;
    }

    const ModularCounter ModularCounter::operator ++ (int i)
    {
       ModularCounter oldValue(*this);
       ++(*this);
       return oldValue;
    }

    int ModularCounter::rasterToStandardPos() const
    {
       int val = 0;
       const size_t nCoords = m_counter.size();
       for (size_t iCoord=0; iCoord<nCoords; ++iCoord)
       {
          val += m_counter[iCoord]*m_factors[iCoord];
       }
       return val;
    }
    
    // Additional Declarations

} // namespace Numerics

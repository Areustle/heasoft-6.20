//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// XSutility
#include <XSUtil/Utils/XSutility.h>
// ParamData
#include <XSModel/Parameter/ParamData.h>


// Class ParamData 

ParamData::ParamData(const ParamData &right)
      : m_value(right.m_value),
        m_delta(right.m_delta),
        m_max(right.m_max),
        m_min(right.m_min),
        m_top(right.m_top),
        m_bot(right.m_bot)
{
}

ParamData::ParamData (Real value, Real delta, Real max, Real min, Real top, Real bot)
      : m_value(value),
        m_delta(delta),
        m_max(max),
        m_min(min),
        m_top(top),
        m_bot(bot)
{
}


ParamData::~ParamData()
{
}


ParamData & ParamData::operator=(const ParamData &right)
{
    ParamData __temp(right);
    swap(__temp);
    return *this;
}


void ParamData::init (Real values, Real delta, Real max, Real min, Real top, Real bottom)
{
   m_value = values;
   m_delta = delta;
   m_max = max;
   m_min = min;
   m_top = top;
   m_bot = bottom;
}

void ParamData::swap (ParamData& right) throw ()
{
      XSutility::swap(m_max,right.m_max);
      XSutility::swap(m_min,right.m_min);
      XSutility::swap(m_value,right.m_value);
      XSutility::swap(m_delta,right.m_delta);
      XSutility::swap(m_top,right.m_top);
      XSutility::swap(m_bot,right.m_bot);
}

// Additional Declarations

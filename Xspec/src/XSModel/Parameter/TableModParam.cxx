//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// ParamData
#include <XSModel/Parameter/ParamData.h>
// TableModParam
#include <XSModel/Parameter/TableModParam.h>
#include <XSModel/Model/Component/Component.h>

// Class TableModParam 

TableModParam::TableModParam(const TableModParam &right)
      : ModParam(right), m_logInterp(right.m_logInterp), 
        m_numVals(right.m_numVals), m_tabValue(right.m_tabValue)
{
}

TableModParam::TableModParam (const string& inputName, Component* p, Real val, Real delta, Real high, Real low, Real top, Real bot, const string& unit)
      : ModParam(inputName,p,val,delta,high,low,top,bot,unit), m_logInterp(false),
        m_numVals(0),
        m_tabValue()
{
}


TableModParam::~TableModParam()
{
}


bool TableModParam::compare (const Parameter& right) const
{
  const TableModParam& that = static_cast<const TableModParam&>(right);
  if ( !ModParam::compare(that) ) return false;
  if ( m_logInterp != that.m_logInterp ) return false;
  if ( m_numVals != that.m_numVals ) return false;
  size_t n = that.m_tabValue.size();
  if ( m_tabValue.size() != n )return false;
  std::valarray<bool> _test(m_tabValue != that.m_tabValue);
  for (size_t j = 0; j < n; ++j) if (_test[j]) return false;

  return true;  
}

TableModParam* TableModParam::clone (Component* p) const
{
  TableModParam* cloned = new TableModParam(*this);
  cloned->parent(p);
  return cloned;  
}

// Additional Declarations

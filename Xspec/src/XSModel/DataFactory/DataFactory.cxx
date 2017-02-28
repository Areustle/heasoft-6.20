//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// Background
#include <XSModel/Data/BackCorr/Background.h>
// DataSet
#include <XSModel/Data/DataSet.h>
// Response
#include <XSModel/Data/Detector/Response.h>
// XSutility
#include <XSUtil/Utils/XSutility.h>
// DataFactory
#include <XSModel/DataFactory/DataFactory.h>



// Class DataFactory 

DataFactory::DataFactory()
{
}


DataFactory::~DataFactory()
{
}


// Additional Declarations

// Class DataPrototype 

DataPrototype::DataPrototype(const DataPrototype &right)
  : m_protoBackground(right.MakeBackground()),
  m_protoCorrection(right.MakeCorrection()),
  m_protoDataSet(right.MakeDataSet()),
  m_protoResponse(right.MakeResponse())
{
}

DataPrototype::DataPrototype (DataSet* pDataSet, Response* pResponse, Background* pBackground, Correction* pCorrection)
  : m_protoBackground(pBackground),
  m_protoCorrection(pCorrection),
  m_protoDataSet(pDataSet),
  m_protoResponse(pResponse)
{
}


DataPrototype::~DataPrototype()
{
  // exception safe. delete doesn't throw.      
  delete m_protoDataSet;
  delete m_protoResponse;
  delete m_protoBackground;
  delete m_protoCorrection;
}


DataPrototype & DataPrototype::operator=(const DataPrototype &right)
{
  DataPrototype* temp = new DataPrototype(right);
  swap(*temp);
  return *this;
}


DataSet* DataPrototype::MakeDataSet () const
{
   return m_protoDataSet->clone();
}

Response* DataPrototype::MakeResponse () const
{
   return m_protoResponse->clone();
}

Background* DataPrototype::MakeBackground () const
{
   return m_protoBackground->clone();
}

Correction* DataPrototype::MakeCorrection () const
{
   if (m_protoCorrection) return m_protoCorrection->clone();
   else return 0;
}

void DataPrototype::swap (DataPrototype& right) throw ()
{
  std::swap(m_protoDataSet,right.m_protoDataSet);
  std::swap(m_protoBackground,right.m_protoBackground);
  std::swap(m_protoCorrection,right.m_protoCorrection);
  std::swap(m_protoResponse,right.m_protoResponse); 
}

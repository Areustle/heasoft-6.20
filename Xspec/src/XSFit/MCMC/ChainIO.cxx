//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// ChainIO
#include <XSFit/MCMC/ChainIO.h>
#include <XSstreams.h>
#include <iostream>


// Class ChainIO::ChainIOError 

ChainIO::ChainIOError::ChainIOError (const string& msg)
   : YellowAlert("Chain IO Error: ")
{
   tcerr << msg << std::endl;
}


// Class ChainIO 
const string ChainIO::s_LENGTHLABEL = string("Length");
const string ChainIO::s_WIDTHLABEL = string("Width");
const int ChainIO::s_PRECISION = 14;
const string ChainIO::s_TEMPERLABEL = string("Tempering");
const int ChainIO::s_TEMPERPREC = 4;
const int ChainIO::s_FWIDTH = s_PRECISION + 7;

void ChainIO::readFileInfo ()
{
}

void ChainIO::createFile ()
{
}

void ChainIO::writePoint ()
{
}

void ChainIO::writePoint (RealArray& paramVals, Real& statVal)
{
}

void ChainIO::writeFileInfo ()
{
}

void ChainIO::adjustLengthInfo ()
{
}

void ChainIO::appendFile (RealArray& startingVals)
{
}

void ChainIO::appendFile (RealArray& startingVals, Real& startingStatVal)
{
}

void ChainIO::adjustTemperInfo ()
{
}

bool ChainIO::checkTemperField (const Real temperature) const
{
   return true;
}

void ChainIO::setParent (Chain* chain)
{
}

bool ChainIO::allowsTemper () const
{
   return true;
}

// Additional Declarations

#include <XSUtil/Utils/IosHolder.h>

#include <XSUtil/Parse/ModelExpression.h>



// Class ModelExpression::ModelExpressionError 

ModelExpressionError::ModelExpressionError (const string& errMsg)
  :YellowAlert("\nModel Expression Error: ")
{
  *IosHolder::errHolder() << errMsg << std::endl;
}


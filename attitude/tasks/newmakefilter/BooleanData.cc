#include "BooleanData.hh"


/********************************************************************************
* check thast all bits are equal. Note that we assume that the padding bits
* are always properly set to zero.
********************************************************************************/
BooleanData::BooleanData(int dimen) : Data(dimen, TLOGICAL) {

    value = new char[dimen];

} // end of constructor

/********************************************************************************
*
********************************************************************************/
bool BooleanData::equals(const Data* data) const {

    const BooleanData* cast = dynamic_cast<const BooleanData*>(data);
    if(cast == NULL) return false;

    for(long i=0; i< dimen; ++i) {
        if(value[i] != cast->value[i]) return false;
    }

    return true;

} // end of equals method




/********************************************************************************
* write out the bits. There's probably a more efficient way to do this,
* but who cares?
********************************************************************************/
void BooleanData::print(ostream& out) const {

    for(long i=0; i< dimen; ++i) {
        if(value[i] == 0) out << '0';
        else              out << value[i];
    }

} // end of print method





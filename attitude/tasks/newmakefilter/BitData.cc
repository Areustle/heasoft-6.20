#include "BitData.hh"


/********************************************************************************
* check thast all bits are equal. Note that we assume that the padding bits
* are always properly set to zero.
********************************************************************************/
BitData::BitData(int nbits) : Data((nbits+7)/8, TBYTE) {

    this->nbits = nbits;
    value = new unsigned char[dimen];
    
    /********************************************************
    * make a mask for the padding bits, so se can ensure
    * they are all zero.
    ********************************************************/
    padding_mask=0;
    int npad = dimen*8 - nbits;
    for(int i=0; i< npad; ++i) {
        padding_mask <<= 1;
        padding_mask |= 1;
    }

} // end of constructor

/********************************************************************************
* returns true if two bit arrays are equal.
********************************************************************************/
bool BitData::equals(const Data* data) const {

    const BitData* cast = dynamic_cast<const BitData*>(data);
    if(cast == NULL) return false;

    if(nbits != cast->nbits) return false;
    
    for(long i=0; i< dimen; ++i) {
        if(value[i] != cast->value[i]) return false;
    }
    
    return true;

} // end of equals method




/********************************************************************************
* write out the bits. There's probably a more efficient way to do this,
* but who cares?
********************************************************************************/
void BitData::print(ostream& out) const {

    for(int i=0; i< nbits; ++i) {
        int byte = i/8;
        int bit = i%8;
        
        out << ((value[byte] >> (7-bit)) & 0x01 );
    }


} // end of print method


/****************************************************************************
* we override the read method to make sure the bit values are left-justified.
****************************************************************************/
void BitData::read(long row, int column, fitsfile* fp) throw(FITSException) {

    /*********************************
    * read the data in the usual way *
    *********************************/
    Data::read(row, column, fp);

    /*****************************************************************
    * The FITS standard says that the bits must be left justified, 
    * this is a bit counter-intuitive and not strictly enforced
    * by CFITSIO, so people often do ths incorrectly.
    * So we check this here just to be sure
    ****************************************************************/

    if(value[dimen-1] & padding_mask ) {

printf("%02x %02x mask=%02x dimen=%ld\n", value[0], value[1], padding_mask, dimen);

        value[dimen-1] &= (!padding_mask);
        


        /*************************
        * assemble error message *
        *************************/
        char number[32];
        string message="Non-zero padding bits. Perhaps the bits are not left-justified?";
        sprintf(number, "%ld", row);
        message += " row ";
        message += number;
        
        sprintf(number, "%d", column);
        message += " column ";
        message += number;

        throw FITSException(message, fp, 0, __FILE__, __LINE__);

    }

    
} // end of read method

/********************************************************************************
* overrides the genric method to set all the bits to zero, since there is no
* standard way to specify "null" for a bit field column.
********************************************************************************/
void BitData::set_to_null() {

    for(long i=0; i<dimen; ++i) value[i] =0;
    
}



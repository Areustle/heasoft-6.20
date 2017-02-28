#include <iostream>
/* If this implementation has <limits>, lie to hoops_numeric_limits.h
   so that it includes its own implementation anyway. */
#ifdef HAVE_LIMITS
#include <limits>
#define REALLY_HAVE_LIMITS
#undef HAVE_LIMITS
//#else
///* This should be removed when HAVE_LIMITS is really being set by the build: */
//#define REALLY_HAVE_LIMITS
#endif
#include "hoops/hoops_numeric_limits.h"
int main() {
  int status = 0;
#ifdef REALLY_HAVE_LIMITS
  if (std::numeric_limits<bool>::is_specialized != hoops::numeric_limits<bool>::is_specialized) {
    status = 1;
    std::cerr << "numeric_limits<bool>::is_specialized == " << std::numeric_limits<bool>::is_specialized << " not " << hoops::numeric_limits<bool>::is_specialized << std::endl;
  }
  if (std::numeric_limits<bool>::min() != hoops::numeric_limits<bool>::min()) {
    status = 1;
    std::cerr << "numeric_limits<bool>::min() == " << std::numeric_limits<bool>::min() << " not " << hoops::numeric_limits<bool>::min() << std::endl;
  }
  if (std::numeric_limits<bool>::max() != hoops::numeric_limits<bool>::max()) {
    status = 1;
    std::cerr << "numeric_limits<bool>::max() == " << std::numeric_limits<bool>::max() << " not " << hoops::numeric_limits<bool>::max() << std::endl;
  }
  if (std::numeric_limits<bool>::digits10 != hoops::numeric_limits<bool>::digits10) {
    status = 1;
    std::cerr << "numeric_limits<bool>::digits10 == " << std::numeric_limits<bool>::digits10 << " not " << hoops::numeric_limits<bool>::digits10 << std::endl;
  }
  if (std::numeric_limits<bool>::is_signed != hoops::numeric_limits<bool>::is_signed) {
    status = 1;
    std::cerr << "numeric_limits<bool>::is_signed == " << std::numeric_limits<bool>::is_signed << " not " << hoops::numeric_limits<bool>::is_signed << std::endl;
  }
  if (std::numeric_limits<bool>::is_integer != hoops::numeric_limits<bool>::is_integer) {
    status = 1;
    std::cerr << "numeric_limits<bool>::is_integer == " << std::numeric_limits<bool>::is_integer << " not " << hoops::numeric_limits<bool>::is_integer << std::endl;
  }
  if (std::numeric_limits<bool>::epsilon() != hoops::numeric_limits<bool>::epsilon()) {
    status = 1;
    std::cerr << "numeric_limits<bool>::epsilon() == " << std::numeric_limits<bool>::epsilon() << " not " << hoops::numeric_limits<bool>::epsilon() << std::endl;
  }
  if (std::numeric_limits<bool>::round_error() != hoops::numeric_limits<bool>::round_error()) {
    status = 1;
    std::cerr << "numeric_limits<bool>::round_error() == " << std::numeric_limits<bool>::round_error() << " not " << hoops::numeric_limits<bool>::round_error() << std::endl;
  }
  if (std::numeric_limits<char>::is_specialized != hoops::numeric_limits<char>::is_specialized) {
    status = 1;
    std::cerr << "numeric_limits<char>::is_specialized == " << (int) std::numeric_limits<char>::is_specialized << " not " << (int) hoops::numeric_limits<char>::is_specialized << std::endl;
  }
  if (std::numeric_limits<char>::min() != hoops::numeric_limits<char>::min()) {
    status = 1;
    std::cerr << "numeric_limits<char>::min() == " << (int) std::numeric_limits<char>::min() << " not " << (int) hoops::numeric_limits<char>::min() << std::endl;
  }
  if (std::numeric_limits<char>::max() != hoops::numeric_limits<char>::max()) {
    status = 1;
    std::cerr << "numeric_limits<char>::max() == " << (int) std::numeric_limits<char>::max() << " not " << (int) hoops::numeric_limits<char>::max() << std::endl;
  }
  if (std::numeric_limits<char>::digits10 != hoops::numeric_limits<char>::digits10) {
    status = 1;
    std::cerr << "numeric_limits<char>::digits10 == " << (int) std::numeric_limits<char>::digits10 << " not " << (int) hoops::numeric_limits<char>::digits10 << std::endl;
  }
  if (std::numeric_limits<char>::is_signed != hoops::numeric_limits<char>::is_signed) {
    status = 1;
    std::cerr << "numeric_limits<char>::is_signed == " << (int) std::numeric_limits<char>::is_signed << " not " << (int) hoops::numeric_limits<char>::is_signed << std::endl;
  }
  if (std::numeric_limits<char>::is_integer != hoops::numeric_limits<char>::is_integer) {
    status = 1;
    std::cerr << "numeric_limits<char>::is_integer == " << (int) std::numeric_limits<char>::is_integer << " not " << (int) hoops::numeric_limits<char>::is_integer << std::endl;
  }
  if (std::numeric_limits<char>::epsilon() != hoops::numeric_limits<char>::epsilon()) {
    status = 1;
    std::cerr << "numeric_limits<char>::epsilon() == " << (int) std::numeric_limits<char>::epsilon() << " not " << (int) hoops::numeric_limits<char>::epsilon() << std::endl;
  }
  if (std::numeric_limits<char>::round_error() != hoops::numeric_limits<char>::round_error()) {
    status = 1;
    std::cerr << "numeric_limits<char>::round_error() == " << (int) std::numeric_limits<char>::round_error() << " not " << (int) hoops::numeric_limits<char>::round_error() << std::endl;
  }
  if (std::numeric_limits<signed char>::is_specialized != hoops::numeric_limits<signed char>::is_specialized) {
    status = 1;
    std::cerr << "numeric_limits<signed char>::is_specialized == " << (int) std::numeric_limits<signed char>::is_specialized << " not " << (int) hoops::numeric_limits<signed char>::is_specialized << std::endl;
  }
  if (std::numeric_limits<signed char>::min() != hoops::numeric_limits<signed char>::min()) {
    status = 1;
    std::cerr << "numeric_limits<signed char>::min() == " << (int) std::numeric_limits<signed char>::min() << " not " << (int) hoops::numeric_limits<signed char>::min() << std::endl;
  }
  if (std::numeric_limits<signed char>::max() != hoops::numeric_limits<signed char>::max()) {
    status = 1;
    std::cerr << "numeric_limits<signed char>::max() == " << (int) std::numeric_limits<signed char>::max() << " not " << (int) hoops::numeric_limits<signed char>::max() << std::endl;
  }
  if (std::numeric_limits<signed char>::digits10 != hoops::numeric_limits<signed char>::digits10) {
    status = 1;
    std::cerr << "numeric_limits<signed char>::digits10 == " << (int) std::numeric_limits<signed char>::digits10 << " not " << (int) hoops::numeric_limits<signed char>::digits10 << std::endl;
  }
  if (std::numeric_limits<signed char>::is_signed != hoops::numeric_limits<signed char>::is_signed) {
    status = 1;
    std::cerr << "numeric_limits<signed char>::is_signed == " << (int) std::numeric_limits<signed char>::is_signed << " not " << (int) hoops::numeric_limits<signed char>::is_signed << std::endl;
  }
  if (std::numeric_limits<signed char>::is_integer != hoops::numeric_limits<signed char>::is_integer) {
    status = 1;
    std::cerr << "numeric_limits<signed char>::is_integer == " << (int) std::numeric_limits<signed char>::is_integer << " not " << (int) hoops::numeric_limits<signed char>::is_integer << std::endl;
  }
  if (std::numeric_limits<signed char>::epsilon() != hoops::numeric_limits<signed char>::epsilon()) {
    status = 1;
    std::cerr << "numeric_limits<signed char>::epsilon() == " << (int) std::numeric_limits<signed char>::epsilon() << " not " << (int) hoops::numeric_limits<signed char>::epsilon() << std::endl;
  }
  if (std::numeric_limits<signed char>::round_error() != hoops::numeric_limits<signed char>::round_error()) {
    status = 1;
    std::cerr << "numeric_limits<signed char>::round_error() == " << (int) std::numeric_limits<signed char>::round_error() << " not " << (int) hoops::numeric_limits<signed char>::round_error() << std::endl;
  }
  if (std::numeric_limits<unsigned char>::is_specialized != hoops::numeric_limits<unsigned char>::is_specialized) {
    status = 1;
    std::cerr << "numeric_limits<unsigned char>::is_specialized == " << (int) std::numeric_limits<unsigned char>::is_specialized << " not " << (int) hoops::numeric_limits<unsigned char>::is_specialized << std::endl;
  }
  if (std::numeric_limits<unsigned char>::min() != hoops::numeric_limits<unsigned char>::min()) {
    status = 1;
    std::cerr << "numeric_limits<unsigned char>::min() == " << (int) std::numeric_limits<unsigned char>::min() << " not " << (int) hoops::numeric_limits<unsigned char>::min() << std::endl;
  }
  if (std::numeric_limits<unsigned char>::max() != hoops::numeric_limits<unsigned char>::max()) {
    status = 1;
    std::cerr << "numeric_limits<unsigned char>::max() == " << (int) std::numeric_limits<unsigned char>::max() << " not " << (int) hoops::numeric_limits<unsigned char>::max() << std::endl;
  }
  if (std::numeric_limits<unsigned char>::digits10 != hoops::numeric_limits<unsigned char>::digits10) {
    status = 1;
    std::cerr << "numeric_limits<unsigned char>::digits10 == " << (int) std::numeric_limits<unsigned char>::digits10 << " not " << (int) hoops::numeric_limits<unsigned char>::digits10 << std::endl;
  }
  if (std::numeric_limits<unsigned char>::is_signed != hoops::numeric_limits<unsigned char>::is_signed) {
    status = 1;
    std::cerr << "numeric_limits<unsigned char>::is_signed == " << (int) std::numeric_limits<unsigned char>::is_signed << " not " << (int) hoops::numeric_limits<unsigned char>::is_signed << std::endl;
  }
  if (std::numeric_limits<unsigned char>::is_integer != hoops::numeric_limits<unsigned char>::is_integer) {
    status = 1;
    std::cerr << "numeric_limits<unsigned char>::is_integer == " << (int) std::numeric_limits<unsigned char>::is_integer << " not " << (int) hoops::numeric_limits<unsigned char>::is_integer << std::endl;
  }
  if (std::numeric_limits<unsigned char>::epsilon() != hoops::numeric_limits<unsigned char>::epsilon()) {
    status = 1;
    std::cerr << "numeric_limits<unsigned char>::epsilon() == " << (int) std::numeric_limits<unsigned char>::epsilon() << " not " << (int) hoops::numeric_limits<unsigned char>::epsilon() << std::endl;
  }
  if (std::numeric_limits<unsigned char>::round_error() != hoops::numeric_limits<unsigned char>::round_error()) {
    status = 1;
    std::cerr << "numeric_limits<unsigned char>::round_error() == " << (int) std::numeric_limits<unsigned char>::round_error() << " not " << (int) hoops::numeric_limits<unsigned char>::round_error() << std::endl;
  }
  if (std::numeric_limits<short>::is_specialized != hoops::numeric_limits<short>::is_specialized) {
    status = 1;
    std::cerr << "numeric_limits<short>::is_specialized == " << std::numeric_limits<short>::is_specialized << " not " << hoops::numeric_limits<short>::is_specialized << std::endl;
  }
  if (std::numeric_limits<short>::min() != hoops::numeric_limits<short>::min()) {
    status = 1;
    std::cerr << "numeric_limits<short>::min() == " << std::numeric_limits<short>::min() << " not " << hoops::numeric_limits<short>::min() << std::endl;
  }
  if (std::numeric_limits<short>::max() != hoops::numeric_limits<short>::max()) {
    status = 1;
    std::cerr << "numeric_limits<short>::max() == " << std::numeric_limits<short>::max() << " not " << hoops::numeric_limits<short>::max() << std::endl;
  }
  if (std::numeric_limits<short>::digits10 != hoops::numeric_limits<short>::digits10) {
    status = 1;
    std::cerr << "numeric_limits<short>::digits10 == " << std::numeric_limits<short>::digits10 << " not " << hoops::numeric_limits<short>::digits10 << std::endl;
  }
  if (std::numeric_limits<short>::is_signed != hoops::numeric_limits<short>::is_signed) {
    status = 1;
    std::cerr << "numeric_limits<short>::is_signed == " << std::numeric_limits<short>::is_signed << " not " << hoops::numeric_limits<short>::is_signed << std::endl;
  }
  if (std::numeric_limits<short>::is_integer != hoops::numeric_limits<short>::is_integer) {
    status = 1;
    std::cerr << "numeric_limits<short>::is_integer == " << std::numeric_limits<short>::is_integer << " not " << hoops::numeric_limits<short>::is_integer << std::endl;
  }
  if (std::numeric_limits<short>::epsilon() != hoops::numeric_limits<short>::epsilon()) {
    status = 1;
    std::cerr << "numeric_limits<short>::epsilon() == " << std::numeric_limits<short>::epsilon() << " not " << hoops::numeric_limits<short>::epsilon() << std::endl;
  }
  if (std::numeric_limits<short>::round_error() != hoops::numeric_limits<short>::round_error()) {
    status = 1;
    std::cerr << "numeric_limits<short>::round_error() == " << std::numeric_limits<short>::round_error() << " not " << hoops::numeric_limits<short>::round_error() << std::endl;
  }
  if (std::numeric_limits<int>::is_specialized != hoops::numeric_limits<int>::is_specialized) {
    status = 1;
    std::cerr << "numeric_limits<int>::is_specialized == " << std::numeric_limits<int>::is_specialized << " not " << hoops::numeric_limits<int>::is_specialized << std::endl;
  }
  if (std::numeric_limits<int>::min() != hoops::numeric_limits<int>::min()) {
    status = 1;
    std::cerr << "numeric_limits<int>::min() == " << std::numeric_limits<int>::min() << " not " << hoops::numeric_limits<int>::min() << std::endl;
  }
  if (std::numeric_limits<int>::max() != hoops::numeric_limits<int>::max()) {
    status = 1;
    std::cerr << "numeric_limits<int>::max() == " << std::numeric_limits<int>::max() << " not " << hoops::numeric_limits<int>::max() << std::endl;
  }
  if (std::numeric_limits<int>::digits10 != hoops::numeric_limits<int>::digits10) {
    status = 1;
    std::cerr << "numeric_limits<int>::digits10 == " << std::numeric_limits<int>::digits10 << " not " << hoops::numeric_limits<int>::digits10 << std::endl;
  }
  if (std::numeric_limits<int>::is_signed != hoops::numeric_limits<int>::is_signed) {
    status = 1;
    std::cerr << "numeric_limits<int>::is_signed == " << std::numeric_limits<int>::is_signed << " not " << hoops::numeric_limits<int>::is_signed << std::endl;
  }
  if (std::numeric_limits<int>::is_integer != hoops::numeric_limits<int>::is_integer) {
    status = 1;
    std::cerr << "numeric_limits<int>::is_integer == " << std::numeric_limits<int>::is_integer << " not " << hoops::numeric_limits<int>::is_integer << std::endl;
  }
  if (std::numeric_limits<int>::epsilon() != hoops::numeric_limits<int>::epsilon()) {
    status = 1;
    std::cerr << "numeric_limits<int>::epsilon() == " << std::numeric_limits<int>::epsilon() << " not " << hoops::numeric_limits<int>::epsilon() << std::endl;
  }
  if (std::numeric_limits<int>::round_error() != hoops::numeric_limits<int>::round_error()) {
    status = 1;
    std::cerr << "numeric_limits<int>::round_error() == " << std::numeric_limits<int>::round_error() << " not " << hoops::numeric_limits<int>::round_error() << std::endl;
  }
  if (std::numeric_limits<long>::is_specialized != hoops::numeric_limits<long>::is_specialized) {
    status = 1;
    std::cerr << "numeric_limits<long>::is_specialized == " << std::numeric_limits<long>::is_specialized << " not " << hoops::numeric_limits<long>::is_specialized << std::endl;
  }
  if (std::numeric_limits<long>::min() != hoops::numeric_limits<long>::min()) {
    status = 1;
    std::cerr << "numeric_limits<long>::min() == " << std::numeric_limits<long>::min() << " not " << hoops::numeric_limits<long>::min() << std::endl;
  }
  if (std::numeric_limits<long>::max() != hoops::numeric_limits<long>::max()) {
    status = 1;
    std::cerr << "numeric_limits<long>::max() == " << std::numeric_limits<long>::max() << " not " << hoops::numeric_limits<long>::max() << std::endl;
  }
  if (std::numeric_limits<long>::digits10 != hoops::numeric_limits<long>::digits10) {
    status = 1;
    std::cerr << "numeric_limits<long>::digits10 == " << std::numeric_limits<long>::digits10 << " not " << hoops::numeric_limits<long>::digits10 << std::endl;
  }
  if (std::numeric_limits<long>::is_signed != hoops::numeric_limits<long>::is_signed) {
    status = 1;
    std::cerr << "numeric_limits<long>::is_signed == " << std::numeric_limits<long>::is_signed << " not " << hoops::numeric_limits<long>::is_signed << std::endl;
  }
  if (std::numeric_limits<long>::is_integer != hoops::numeric_limits<long>::is_integer) {
    status = 1;
    std::cerr << "numeric_limits<long>::is_integer == " << std::numeric_limits<long>::is_integer << " not " << hoops::numeric_limits<long>::is_integer << std::endl;
  }
  if (std::numeric_limits<long>::epsilon() != hoops::numeric_limits<long>::epsilon()) {
    status = 1;
    std::cerr << "numeric_limits<long>::epsilon() == " << std::numeric_limits<long>::epsilon() << " not " << hoops::numeric_limits<long>::epsilon() << std::endl;
  }
  if (std::numeric_limits<long>::round_error() != hoops::numeric_limits<long>::round_error()) {
    status = 1;
    std::cerr << "numeric_limits<long>::round_error() == " << std::numeric_limits<long>::round_error() << " not " << hoops::numeric_limits<long>::round_error() << std::endl;
  }
  if (std::numeric_limits<unsigned short>::is_specialized != hoops::numeric_limits<unsigned short>::is_specialized) {
    status = 1;
    std::cerr << "numeric_limits<unsigned short>::is_specialized == " << std::numeric_limits<unsigned short>::is_specialized << " not " << hoops::numeric_limits<unsigned short>::is_specialized << std::endl;
  }
  if (std::numeric_limits<unsigned short>::min() != hoops::numeric_limits<unsigned short>::min()) {
    status = 1;
    std::cerr << "numeric_limits<unsigned short>::min() == " << std::numeric_limits<unsigned short>::min() << " not " << hoops::numeric_limits<unsigned short>::min() << std::endl;
  }
  if (std::numeric_limits<unsigned short>::max() != hoops::numeric_limits<unsigned short>::max()) {
    status = 1;
    std::cerr << "numeric_limits<unsigned short>::max() == " << std::numeric_limits<unsigned short>::max() << " not " << hoops::numeric_limits<unsigned short>::max() << std::endl;
  }
  if (std::numeric_limits<unsigned short>::digits10 != hoops::numeric_limits<unsigned short>::digits10) {
    status = 1;
    std::cerr << "numeric_limits<unsigned short>::digits10 == " << std::numeric_limits<unsigned short>::digits10 << " not " << hoops::numeric_limits<unsigned short>::digits10 << std::endl;
  }
  if (std::numeric_limits<unsigned short>::is_signed != hoops::numeric_limits<unsigned short>::is_signed) {
    status = 1;
    std::cerr << "numeric_limits<unsigned short>::is_signed == " << std::numeric_limits<unsigned short>::is_signed << " not " << hoops::numeric_limits<unsigned short>::is_signed << std::endl;
  }
  if (std::numeric_limits<unsigned short>::is_integer != hoops::numeric_limits<unsigned short>::is_integer) {
    status = 1;
    std::cerr << "numeric_limits<unsigned short>::is_integer == " << std::numeric_limits<unsigned short>::is_integer << " not " << hoops::numeric_limits<unsigned short>::is_integer << std::endl;
  }
  if (std::numeric_limits<unsigned short>::epsilon() != hoops::numeric_limits<unsigned short>::epsilon()) {
    status = 1;
    std::cerr << "numeric_limits<unsigned short>::epsilon() == " << std::numeric_limits<unsigned short>::epsilon() << " not " << hoops::numeric_limits<unsigned short>::epsilon() << std::endl;
  }
  if (std::numeric_limits<unsigned short>::round_error() != hoops::numeric_limits<unsigned short>::round_error()) {
    status = 1;
    std::cerr << "numeric_limits<unsigned short>::round_error() == " << std::numeric_limits<unsigned short>::round_error() << " not " << hoops::numeric_limits<unsigned short>::round_error() << std::endl;
  }
  if (std::numeric_limits<unsigned int>::is_specialized != hoops::numeric_limits<unsigned int>::is_specialized) {
    status = 1;
    std::cerr << "numeric_limits<unsigned int>::is_specialized == " << std::numeric_limits<unsigned int>::is_specialized << " not " << hoops::numeric_limits<unsigned int>::is_specialized << std::endl;
  }
  if (std::numeric_limits<unsigned int>::min() != hoops::numeric_limits<unsigned int>::min()) {
    status = 1;
    std::cerr << "numeric_limits<unsigned int>::min() == " << std::numeric_limits<unsigned int>::min() << " not " << hoops::numeric_limits<unsigned int>::min() << std::endl;
  }
  if (std::numeric_limits<unsigned int>::max() != hoops::numeric_limits<unsigned int>::max()) {
    status = 1;
    std::cerr << "numeric_limits<unsigned int>::max() == " << std::numeric_limits<unsigned int>::max() << " not " << hoops::numeric_limits<unsigned int>::max() << std::endl;
  }
  if (std::numeric_limits<unsigned int>::digits10 != hoops::numeric_limits<unsigned int>::digits10) {
    status = 1;
    std::cerr << "numeric_limits<unsigned int>::digits10 == " << std::numeric_limits<unsigned int>::digits10 << " not " << hoops::numeric_limits<unsigned int>::digits10 << std::endl;
  }
  if (std::numeric_limits<unsigned int>::is_signed != hoops::numeric_limits<unsigned int>::is_signed) {
    status = 1;
    std::cerr << "numeric_limits<unsigned int>::is_signed == " << std::numeric_limits<unsigned int>::is_signed << " not " << hoops::numeric_limits<unsigned int>::is_signed << std::endl;
  }
  if (std::numeric_limits<unsigned int>::is_integer != hoops::numeric_limits<unsigned int>::is_integer) {
    status = 1;
    std::cerr << "numeric_limits<unsigned int>::is_integer == " << std::numeric_limits<unsigned int>::is_integer << " not " << hoops::numeric_limits<unsigned int>::is_integer << std::endl;
  }
  if (std::numeric_limits<unsigned int>::epsilon() != hoops::numeric_limits<unsigned int>::epsilon()) {
    status = 1;
    std::cerr << "numeric_limits<unsigned int>::epsilon() == " << std::numeric_limits<unsigned int>::epsilon() << " not " << hoops::numeric_limits<unsigned int>::epsilon() << std::endl;
  }
  if (std::numeric_limits<unsigned int>::round_error() != hoops::numeric_limits<unsigned int>::round_error()) {
    status = 1;
    std::cerr << "numeric_limits<unsigned int>::round_error() == " << std::numeric_limits<unsigned int>::round_error() << " not " << hoops::numeric_limits<unsigned int>::round_error() << std::endl;
  }
  if (std::numeric_limits<unsigned long>::is_specialized != hoops::numeric_limits<unsigned long>::is_specialized) {
    status = 1;
    std::cerr << "numeric_limits<unsigned long>::is_specialized == " << std::numeric_limits<unsigned long>::is_specialized << " not " << hoops::numeric_limits<unsigned long>::is_specialized << std::endl;
  }
  if (std::numeric_limits<unsigned long>::min() != hoops::numeric_limits<unsigned long>::min()) {
    status = 1;
    std::cerr << "numeric_limits<unsigned long>::min() == " << std::numeric_limits<unsigned long>::min() << " not " << hoops::numeric_limits<unsigned long>::min() << std::endl;
  }
  if (std::numeric_limits<unsigned long>::max() != hoops::numeric_limits<unsigned long>::max()) {
    status = 1;
    std::cerr << "numeric_limits<unsigned long>::max() == " << std::numeric_limits<unsigned long>::max() << " not " << hoops::numeric_limits<unsigned long>::max() << std::endl;
  }
  if (std::numeric_limits<unsigned long>::digits10 != hoops::numeric_limits<unsigned long>::digits10) {
    status = 1;
    std::cerr << "numeric_limits<unsigned long>::digits10 == " << std::numeric_limits<unsigned long>::digits10 << " not " << hoops::numeric_limits<unsigned long>::digits10 << std::endl;
  }
  if (std::numeric_limits<unsigned long>::is_signed != hoops::numeric_limits<unsigned long>::is_signed) {
    status = 1;
    std::cerr << "numeric_limits<unsigned long>::is_signed == " << std::numeric_limits<unsigned long>::is_signed << " not " << hoops::numeric_limits<unsigned long>::is_signed << std::endl;
  }
  if (std::numeric_limits<unsigned long>::is_integer != hoops::numeric_limits<unsigned long>::is_integer) {
    status = 1;
    std::cerr << "numeric_limits<unsigned long>::is_integer == " << std::numeric_limits<unsigned long>::is_integer << " not " << hoops::numeric_limits<unsigned long>::is_integer << std::endl;
  }
  if (std::numeric_limits<unsigned long>::epsilon() != hoops::numeric_limits<unsigned long>::epsilon()) {
    status = 1;
    std::cerr << "numeric_limits<unsigned long>::epsilon() == " << std::numeric_limits<unsigned long>::epsilon() << " not " << hoops::numeric_limits<unsigned long>::epsilon() << std::endl;
  }
  if (std::numeric_limits<unsigned long>::round_error() != hoops::numeric_limits<unsigned long>::round_error()) {
    status = 1;
    std::cerr << "numeric_limits<unsigned long>::round_error() == " << std::numeric_limits<unsigned long>::round_error() << " not " << hoops::numeric_limits<unsigned long>::round_error() << std::endl;
  }
  if (std::numeric_limits<float>::is_specialized != hoops::numeric_limits<float>::is_specialized) {
    status = 1;
    std::cerr << "numeric_limits<float>::is_specialized == " << std::numeric_limits<float>::is_specialized << " not " << hoops::numeric_limits<float>::is_specialized << std::endl;
  }
  if (std::numeric_limits<float>::min() != hoops::numeric_limits<float>::min()) {
    status = 1;
    std::cerr << "numeric_limits<float>::min() == " << std::numeric_limits<float>::min() << " not " << hoops::numeric_limits<float>::min() << std::endl;
  }
  if (std::numeric_limits<float>::max() != hoops::numeric_limits<float>::max()) {
    status = 1;
    std::cerr << "numeric_limits<float>::max() == " << std::numeric_limits<float>::max() << " not " << hoops::numeric_limits<float>::max() << std::endl;
  }
  if (std::numeric_limits<float>::digits10 != hoops::numeric_limits<float>::digits10) {
    status = 1;
    std::cerr << "numeric_limits<float>::digits10 == " << std::numeric_limits<float>::digits10 << " not " << hoops::numeric_limits<float>::digits10 << std::endl;
  }
  if (std::numeric_limits<float>::is_signed != hoops::numeric_limits<float>::is_signed) {
    status = 1;
    std::cerr << "numeric_limits<float>::is_signed == " << std::numeric_limits<float>::is_signed << " not " << hoops::numeric_limits<float>::is_signed << std::endl;
  }
  if (std::numeric_limits<float>::is_integer != hoops::numeric_limits<float>::is_integer) {
    status = 1;
    std::cerr << "numeric_limits<float>::is_integer == " << std::numeric_limits<float>::is_integer << " not " << hoops::numeric_limits<float>::is_integer << std::endl;
  }
  if (std::numeric_limits<float>::epsilon() != hoops::numeric_limits<float>::epsilon()) {
    status = 1;
    std::cerr << "numeric_limits<float>::epsilon() == " << std::numeric_limits<float>::epsilon() << " not " << hoops::numeric_limits<float>::epsilon() << std::endl;
  }
  if (std::numeric_limits<float>::round_error() != hoops::numeric_limits<float>::round_error()) {
    status = 1;
    std::cerr << "numeric_limits<float>::round_error() == " << std::numeric_limits<float>::round_error() << " not " << hoops::numeric_limits<float>::round_error() << std::endl;
  }
  if (std::numeric_limits<double>::is_specialized != hoops::numeric_limits<double>::is_specialized) {
    status = 1;
    std::cerr << "numeric_limits<double>::is_specialized == " << std::numeric_limits<double>::is_specialized << " not " << hoops::numeric_limits<double>::is_specialized << std::endl;
  }
  if (std::numeric_limits<double>::min() != hoops::numeric_limits<double>::min()) {
    status = 1;
    std::cerr << "numeric_limits<double>::min() == " << std::numeric_limits<double>::min() << " not " << hoops::numeric_limits<double>::min() << std::endl;
  }
  if (std::numeric_limits<double>::max() != hoops::numeric_limits<double>::max()) {
    status = 1;
    std::cerr << "numeric_limits<double>::max() == " << std::numeric_limits<double>::max() << " not " << hoops::numeric_limits<double>::max() << std::endl;
  }
  if (std::numeric_limits<double>::digits10 != hoops::numeric_limits<double>::digits10) {
    status = 1;
    std::cerr << "numeric_limits<double>::digits10 == " << std::numeric_limits<double>::digits10 << " not " << hoops::numeric_limits<double>::digits10 << std::endl;
  }
  if (std::numeric_limits<double>::is_signed != hoops::numeric_limits<double>::is_signed) {
    status = 1;
    std::cerr << "numeric_limits<double>::is_signed == " << std::numeric_limits<double>::is_signed << " not " << hoops::numeric_limits<double>::is_signed << std::endl;
  }
  if (std::numeric_limits<double>::is_integer != hoops::numeric_limits<double>::is_integer) {
    status = 1;
    std::cerr << "numeric_limits<double>::is_integer == " << std::numeric_limits<double>::is_integer << " not " << hoops::numeric_limits<double>::is_integer << std::endl;
  }
  if (std::numeric_limits<double>::epsilon() != hoops::numeric_limits<double>::epsilon()) {
    status = 1;
    std::cerr << "numeric_limits<double>::epsilon() == " << std::numeric_limits<double>::epsilon() << " not " << hoops::numeric_limits<double>::epsilon() << std::endl;
  }
  if (std::numeric_limits<double>::round_error() != hoops::numeric_limits<double>::round_error()) {
    status = 1;
    std::cerr << "numeric_limits<double>::round_error() == " << std::numeric_limits<double>::round_error() << " not " << hoops::numeric_limits<double>::round_error() << std::endl;
  }
  if (std::numeric_limits<long double>::is_specialized != hoops::numeric_limits<long double>::is_specialized) {
    status = 1;
    std::cerr << "numeric_limits<long double>::is_specialized == " << std::numeric_limits<long double>::is_specialized << " not " << hoops::numeric_limits<long double>::is_specialized << std::endl;
  }
  if (std::numeric_limits<long double>::min() != hoops::numeric_limits<long double>::min()) {
    status = 1;
    std::cerr << "numeric_limits<long double>::min() == " << std::numeric_limits<long double>::min() << " not " << hoops::numeric_limits<long double>::min() << std::endl;
  }
  if (std::numeric_limits<long double>::max() != hoops::numeric_limits<long double>::max()) {
    status = 1;
    std::cerr << "numeric_limits<long double>::max() == " << std::numeric_limits<long double>::max() << " not " << hoops::numeric_limits<long double>::max() << std::endl;
  }
  if (std::numeric_limits<long double>::digits10 != hoops::numeric_limits<long double>::digits10) {
    status = 1;
    std::cerr << "numeric_limits<long double>::digits10 == " << std::numeric_limits<long double>::digits10 << " not " << hoops::numeric_limits<long double>::digits10 << std::endl;
  }
  if (std::numeric_limits<long double>::is_signed != hoops::numeric_limits<long double>::is_signed) {
    status = 1;
    std::cerr << "numeric_limits<long double>::is_signed == " << std::numeric_limits<long double>::is_signed << " not " << hoops::numeric_limits<long double>::is_signed << std::endl;
  }
  if (std::numeric_limits<long double>::is_integer != hoops::numeric_limits<long double>::is_integer) {
    status = 1;
    std::cerr << "numeric_limits<long double>::is_integer == " << std::numeric_limits<long double>::is_integer << " not " << hoops::numeric_limits<long double>::is_integer << std::endl;
  }
  if (std::numeric_limits<long double>::epsilon() != hoops::numeric_limits<long double>::epsilon()) {
    status = 1;
    std::cerr << "numeric_limits<long double>::epsilon() == " << std::numeric_limits<long double>::epsilon() << " not " << hoops::numeric_limits<long double>::epsilon() << std::endl;
  }
  if (std::numeric_limits<long double>::round_error() != hoops::numeric_limits<long double>::round_error()) {
    status = 1;
    std::cerr << "numeric_limits<long double>::round_error() == " << std::numeric_limits<long double>::round_error() << " not " << hoops::numeric_limits<long double>::round_error() << std::endl;
  }
#else
  std::cout << "This implementation doesn't have <limits>; not really testing anything!" << std::endl;
#endif

  if (0 == status) std::cout << "Test succeeded." << std::endl;
  return status;
}
